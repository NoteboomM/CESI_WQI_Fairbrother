##############################################################-
#                     PERCENTILES
#
# Objective: Ranks the flow data values for each day and
# extracts the 25th and 75th percentile values. Labels of low,
# normal, and high are applied based on the current year.
#
# Date Modified: July 2019
##############################################################-

# Get the number of normal period years and keep only those that meet the threshold
  normal_period$MDay <- format(normal_period$Date, "%m-%d")
  norm_enough <- normal_period %>% group_by(STATION_NUMBER, MDay) %>% mutate(nrmprdyrs=n())
  norm_counter1 <- nrow(norm_enough) # Number of years with normal period data
  norm_enough <- norm_enough %>% filter(nrmprdyrs >= normal_threshold)
  norm_counter2 <- nrow(norm_enough)  # Number of years with enough normal period data
  norm_counter3 <- length(unique(norm_enough$STATION_NUMBER)) # Number of stations with enough normal period data

# Remove previous objects to free up memory
  rm(list = c("normal_period"))
  gc()

# Calculate percentiles for each station for each date
# As outlined in the USGS Statistical Methods in Water Resources document
# the percentile values are designated based on the 1/4 and 3/4 position values in the
# 30 year normal period

  # This function ensures that the 1/4 value is selected
  excel_round <- function(x, digits) round(x*(1+1e-15), digits)

  norm_enough$PercentileValue <- excel_round((norm_enough$nrmprdyrs + 1)*0.25)

# Extract the 25th and 75th percentile rows
  station.list <-as.data.frame(unique(norm_enough$STATION_NUMBER))
  quantile.fn <- function(stn) {
    
       sub <- norm_enough[norm_enough$STATION_NUMBER == stn,]

       qu25 <- sub %>%
         group_by(MDay) %>%
         dplyr::arrange(Value) %>%
         slice(PercentileValue) %>%
         unique() %>%
         select(STATION_NUMBER, MDay, p25 = Value)

       qu75 <- sub %>%
         group_by(MDay) %>%
         dplyr::arrange(desc(Value)) %>%
         slice(PercentileValue) %>%
         unique() %>%
         select(STATION_NUMBER, MDay, p75 = Value)

       data <- inner_join(qu25, qu75, by = c("MDay", "STATION_NUMBER"))

       return(data)
  }
# Apply quantile.fn to list of stations. Progress status bar will appear in console.  
  data.list <- pbapply(station.list, 1, quantile.fn)
  quantile <- bind_rows(data.list) # Combine all outputs into one table

# Sort and combine the year of interest values with the percentile values
  current_year$MDay <- format(current_year$Date, "%m-%d")
  current_year <- current_year %>% group_by(STATION_NUMBER, MDay)
  compared <- inner_join(current_year, quantile, by = c("MDay", "STATION_NUMBER"))

# Label each station and date based on percentile values 
  compared <- as.data.table(compared)
  compared[ Value < `p25`, Label := "Low"]
  compared[ Value >= `p25` & Value <= `p75`, Label := "Normal"]
  compared[ Value > `p75`, Label := "High"]

# Create summary table for classifications of days by station
  yoi_summary <- compared %>%
    group_by(STATION_NUMBER, Label) %>%
    tally() %>%
    ungroup() %>%
    spread(Label, n) 

# Get overall classification for the site for the year based on highest value
  yoi_sub <- select(yoi_summary, High, Normal, Low)  
  yoi_sub <- mutate(yoi_sub, max = apply(yoi_sub, 1, which.max))
  yoi_max <- select(yoi_sub, max)

# Apply labels
  yoi_max <- as.data.table(yoi_max)
  max_value <- as.numeric(yoi_max$max)
  yoi_max[ max_value == 1, Label := "High"]
  yoi_max[ max_value == 2, Label := "Normal"]
  yoi_max[ max_value == 3, Label := "Low"]

# Recombine the station numbers, data, and labels
  all_yoi <- bind_cols(yoi_summary, yoi_max)
  all_yoi[is.na(all_yoi)] <- 0

# Select final table columns 
  all_select <- select(all_yoi, STATION_NUMBER, Low, Normal, High, Label) 

# Remove previous objects to free up memory  
  rm(list = c("all_yoi", "compared", "quantile", "norm_enough", 
            "yoi_max", "yoi_sub", "yoi_summary", "current_year", 
            "max_value","data.list")) 
  gc()

  