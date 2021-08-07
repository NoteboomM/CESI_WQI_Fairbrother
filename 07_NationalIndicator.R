#######################################################-
#                  NATIONAL INDICATOR
#
# Objective: Counts the number of high low and normal
# station to compute the national indicator values. 
# 
# 
# Date Modified: November 2018
#######################################################-

# Find the proportions of High, Normal, and Low
  high <- filter(all_data, Label == "High") %>% nrow %>% print
  normal <- filter(all_data, Label == "Normal") %>% nrow %>% print
  low <- filter(all_data, Label == "Low") %>% nrow %>% print

  all <- nrow(all_data) %>% print

# Create the values for a pie chart
  prophigh <- high/all
  propnorm <- normal/all
  proplow <- low/all

# Create simple pie chart
  slices <- c(prophigh, propnorm, proplow)
  lbls <- c("High", "Normal", "Low")
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # add % to labels

# MSN 18-5-2021 - added colour definitions from CESI colours
  blue   <- rgb(15,76,106, maxColorValue = 255)
  green  <- rgb(133,161,66, maxColorValue = 255)
  orange <- rgb(242,144,0, maxColorValue = 255)

# MSN 26/5/2021 - changed to png instead of crappy jpg
  png(file = paste("Output/", year_id, "/", year_id, "_national_indicator_HYDAT_", version, ".png", sep = ""))
  pie(slices,labels = lbls, col= c(blue, green, orange) ,main= paste(year_id, " Station Classifications", sep = ""))
    dev.off()

# MSN 25-5-2021 - added text output of stations and percentages for reporting
  dataout <- paste("Output/", year_id, "/", year_id, "_national_indicator_HYDAT_", version, ".csv", sep = "")
  cat(paste("year", "valid_stns", "proplow", "propnorm", "prophigh", sep=","), file = dataout, append = F, fill = T)
  cat(paste(year_id, all, round(proplow*100), round(propnorm*100), round(prophigh*100), sep=","), file = dataout, append = T, fill = T)

# Remove intermediate objects
    rm(all, high, low, normal, pct, prophigh, proplow, propnorm, slices, lbls)
    gc()
    