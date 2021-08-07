##############################################################-
#              CONTINUOUS AND SEASONAL CHECK
#
# Objective: Differentiates seasonal and continuous stations 
# to to determine whether the user specified thresholds are
# met (default is 80% per year).
# 
# Date Modified: November 2018
##############################################################-

# Select sites that have station information at least until the year of interest
  operation <- hy_stn_data_coll(hydat_path = "./Dependencies/Hydat/Hydat.sqlite3",
                                station_number = all_tally$STATION_NUMBER)
  operation <- operation %>% filter(DATA_TYPE == "Flow", Year_to>=year_id)

# Keep only records common to the selection and the year of interest stations
  op_select <- operation %>% select(STATION_NUMBER, OPERATION) %>% distinct()

# Join the year of interest data and the operation type
  joined <- inner_join(all_tally, op_select, by = "STATION_NUMBER")  # Final object from 3a

# Separate seasonal and continuous stations
  seasonal <- subset(joined, OPERATION == 'Seasonal') %>%
                        mutate(sum = Low + Normal + High) %>% # Add days
                        subset(sum > seasonal_threshold)  # Keep records with enough
  continuous <- subset(joined, OPERATION == 'Continuous') %>% 
                    mutate(sum = Low + Normal + High) %>%
                    subset(sum > continuous_threshold)

# Re-combine the two types
  all_data <- bind_rows(seasonal, continuous)
  
  seasonal <- nrow(seasonal)
  continuous <- nrow(continuous)
 
# Remove previous objects to free up memory  
  rm(list = c("joined", "operation", "op_select", "all_tally")) 
  gc() 
  
