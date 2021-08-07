###############################################################-
#                       GET FLOW DATA
#
# Objective: Queries the HYDAT database for flow data from the
# current year and normal period years. 
# 
# Date Modified: November 2018
###############################################################-

### Get flow data for normal period and indicator calculation ###

# Get flow data for the current year
  current_year <- hy_daily_flows(hydat_path = "./Dependencies/Hydat/Hydat.sqlite3",
                                 start_date = as.Date(paste0(year_id, "-01-01")), 
                                                      end_date = as.Date(paste0(year_id, "-12-31")))
  
# Count how many stations have data for the internal report
  flow_records <- as.numeric(nrow(distinct(select(current_year, STATION_NUMBER))))

# Select relevant columns and remove all "NA" flow values
  current_year <- current_year %>% filter(!is.na(Value))

# Get flow data for the normal period
  normal_period <- hy_daily_flows(hydat_path = "./Dependencies/Hydat/Hydat.sqlite3",
                                  station_number = current_year$STATION_NUMBER, 
                                  start_date = normal_start, end_date = normal_end)

# Select relevant columns and remove all "NA" flow values
  normal_period <- normal_period %>% filter(!is.na(Value))
    