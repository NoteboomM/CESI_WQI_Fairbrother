##############################################################-
#                     QUEBEC PERCENTILES
#
# Objective: Same percentile analysis as the previous script,
# with altered normal period years for Quebec flow stations.
# 
# 
# Date Modified: November 2018
##############################################################-

## Calculate northern Quebec stations' percentiles (different normal period) ## 

  northern_qc_stations <- c("03AB002", "03AC004", "03BD002", "03BF001",
      "03ED001","03FA003","03JB004", "03KC004", "03LF002", "03MB002", "03MD001")

# Remove North Quebec sites from the overall list if any have made it this far
  all_select <- all_select[! all_select$STATION_NUMBER %in% northern_qc_stations, ]  

# Get flow data for 11 northern Quebec sites
  qc_flow <- hy_daily_flows(hydat_path = "./Dependencies/Hydat/Hydat.sqlite3",
                            station_number = northern_qc_stations)

# Check if there is any flow data for the given year for these stations
  qc_yoi <- qc_flow %>% filter(!is.na(Value), Date>=as.Date(paste0(year_id, "-01-01")), 
                               Date<=as.Date(paste0(year_id, "-12-31")))
  qcyoi_counter <- as.numeric(nrow(qc_yoi))
  
# If there are no northern Quebec data for the specified year, continue with the existing table
  if(nrow(qc_yoi) == 0) {
  
      all_tally <- all_select # Reverts to the table without northern QC stations 
  
# Remove previous objects to free up memory  
  rm(list = c("qc_flow", "qc_yoi")) 
  gc()
  
      } else {
  
    # Extract the normal.period
      qc_norm <- qc_flow %>% filter(Date >= qc_nrm_start, Date<= qc_nrm_end, !is.na(Value)) # %>% # Extract the normal period

    # Eliminate stations that do not have enough flow data in the normal.period
      qc_norm$MDay <- format(qc_norm$Date, "%m-%d")
      qc_enough <- qc_norm %>% group_by(STATION_NUMBER, MDay) %>% mutate(nrmprdyrs = n())
      qc_counter1 <- nrow(qc_enough)
      qc_enough <- qc_enough %>% filter(nrmprdyrs >= normal_threshold)
      qc_counter2 <- nrow(qc_enough)
      qc_counter3 <- select(qc_enough, STATION_NUMBER) %>% unique() %>% nrow() # Number of stations with enough normal period data

  # Calculate percentiles for each station for each date
  # As outlined in the USGS Statistical Methods in Water Resources document
  # the percentile values are designated based on the 1/4 and 3/4 position values in the 
  # 30 year normal period
  
    excel_round <- function(x, digits) round(x*(1+1e-15), digits)
  
   qc_enough <- qc_enough %>% mutate(PercentileValue = excel_round((nrmprdyrs + 1)* 0.25))

    qc25 <- qc_enough %>%
      group_by(STATION_NUMBER,MDay) %>%
      dplyr::arrange(Value) %>%
      slice(PercentileValue) %>%
      unique() %>%
      select(STATION_NUMBER, MDay, p25 = Value)
  
    qc75 <- qc_enough %>% 
      group_by(STATION_NUMBER,MDay) %>%
      dplyr::arrange(desc(Value)) %>%
      slice(PercentileValue) %>%
      unique() %>%
      select(STATION_NUMBER, MDay, p75 = Value)
  
  # Join the two tables into one percentile range table
    qc_quantile <- inner_join(qc25, qc75, by = c("MDay", "STATION_NUMBER"))
  
  # Sort and combine the year of interest values with the percentile values
    qc_yoi$MDay <- format(qc_yoi$Date, "%m-%d")
    qc_yoi <- qc_yoi %>% group_by(MDay, STATION_NUMBER)
    compared <- inner_join(qc_yoi, qc_quantile, by = c("MDay", "STATION_NUMBER"))
  
  # Label each station and date based on percentile values 
    compared <- as.data.table(compared)
    compared[ Value < `p25`, Label := "Low"]
    compared[ Value >= `p25` & Value <= `p75`, Label := "Normal"]
    compared[ Value > `p75`, Label := "High"]
  
  # Create summary table for classifications of days by station
    qc_summary <- compared %>%
      group_by(STATION_NUMBER, Label) %>%
      tally() %>%
      ungroup() %>%
      spread(Label, n) 

  # Get overall classification for the site for the year based on highest value
    qc_sub <- select(qc_summary, High, Normal, Low)  
    qc_sub <- mutate(qc_sub, max = apply(qc_sub, 1, which.max))
    qc_max <- select(qc_sub, max)
  
  # Apply labels
    qc_max <- as.data.table(qc_max)
    max_value <- as.numeric(qc_max$max)
    qc_max[ max_value == 1, Label := "High"]
    qc_max[ max_value == 2, Label := "Normal"]
    qc_max[ max_value == 3, Label := "Low"]
  
  # Recombine the station numbers, data, and labels
    all_qc <- bind_cols(qc_summary, qc_max)
    all_qc[is.na(all_qc)] <- 0
  
  # Select final table columns 
    qc_select <- select(all_qc, STATION_NUMBER, Low, Normal, High, Label) 

  # Combine the Quebec table with all other stations to create an overall table
    all_tally <- bind_rows(all_select, qc_select)

  # Remove previous objects to free up memory  
    rm(list = c("all_qc", "compared", "qc25", "qc75", "qc_quantile", "qc_max", "qc_norm", 
              "qc_summary", "qc_sub", "qc_yoi", "qc_flow", "qc_enough", "qc_select", "all_select", 
              "northern_qc_stations", "max_value", "excel_round")) 
    gc()

  }

  