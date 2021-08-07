#############################################################-
#                     CONFIGURATION
#
# Objective: Update the customizeable year values and 
# thresholds before saving and running this script. Connects
# to the HYDAT database or downloads the file if not present.
# 
# Date Modified: November 2018
#############################################################-

### (1) Date specifications ###

# Specify the year of interest
  year_id <- 2010
  
# Set the normal period dates
  normal_start <- as.Date("1981-01-01") 
  normal_end <- as.Date("2010-12-31") 

### (2) Threshold values ###
  
# The number of years of data required for each day in the normal period
# For instance, January 1st must have at least 25/30 flow values in 30 years of data
  normal_threshold <- 25
  
# The number of days with flow values required in the analysis for different types of stations
  seasonal_threshold <- 174 # Missing no more than 43 days of 217 for seasonal stations 
  continuous_threshold <- 292 # Missing no more than 73 days of 365 for continuous stations
  
### (3) Quebec dates ###
  
# Set the normal period dates for northern Quebec stations
  qc_nrm_start <- as.Date("1981-01-01")
  qc_nrm_end <- as.Date("2010-12-31")

### (4) Connect to the database ###
  
  # If the most recent Hydat.sqlite file is not already in the Packages folder, it will be downloaded
  if( length( grep("Hydat", list.files("./Dependencies/Hydat/")))==0){
    
    # hydat file will be downloaded in dependencies and subsequent tidyhydat calls will use this file
    # Highly suggest keeping a file folder with old Hydat versions to be able to reproduce old work
    hy_file <- "./Dependencies/Hydat"
    download_hydat(dl_hydat_here = hy_file)
    hy_db <- paste0(hy_file, "/Hydat.sqlite3")
    hy_set_default_db(hydat_path = hy_db)
    
  } else {
    hy_file <- "./Dependencies/Hydat"
    hy_db <- paste0(hy_file, "/Hydat.sqlite3")
    hy_set_default_db(hydat_path = hy_db)
  }
  
### (5) No changes required to this section, necessary for output file naming ###
###  and identification for the R Markdown report ###
  
# Version number for file naming
  version <- hy_version(hydat_path = "./Dependencies/Hydat/Hydat.sqlite3")
  version <- substring(version$Date,0,7)
  
  