#######################################################-
#              MAP ITEMS
#
# Objective: Produce maps of local and regional CESI WQI
# results to be included in CESI_Internal_Report. 
# 
# Date Modified: July 2019
#######################################################-

# Load libraries
library("raster")
library("rgdal")
library("rgeos")

# Load Pearse drainage area and water body shapefiles
WaterBodies<- readOGR(dsn = "./Map_Creation", "MainWaterBodiest")
Pearse<-readOGR(dsn = "./Map_Creation", "PDAs")

# Load results tables
Local <- read.csv(file = paste("./Output/", year_id, "/", year_id, "_local_entry_HYDAT_", version, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)  
Regional <- read.csv(file = paste("./Output/", year_id, "/", year_id, "_regional_map_HYDAT_", version, ".csv", sep = ""), header = TRUE)  

### Create Regional Results map

# Create colour vector for regional polygons based on regional results csv file
ID <- as.data.frame(Pearse@data$PEARSENMB)
colnames(ID)<-"PERSEDA_CODE"
ID2 <-left_join(ID, Regional[c("PERSEDA_CODE", "Flow_Type", "TotalGauged")], by = "PERSEDA_CODE")

# MSN 18-5-2021 - added definition of orange from CESI colours
blue   <- rgb(15,76,106, maxColorValue = 255)
green  <- rgb(133,161,66, maxColorValue = 255)
orange <- rgb(242,144,0, maxColorValue = 255)

# MSN 18-5-2021 - added condition that gauged area must be greater than 10% of region to colour
IDcolour <- rep("#C0C0C0", 25)
IDcolour[ID2$TotalGauged >= 0.1 & ID2$Flow_Type=="Low"] <- orange
IDcolour[ID2$TotalGauged >= 0.1 & ID2$Flow_Type=="Normal"] <- green
IDcolour[ID2$TotalGauged >= 0.1 & ID2$Flow_Type=="High"] <- blue

# Create vector of Pearse drainage names for polygon legend
legend1 <- Pearse@data[c("PEARSENMB", "PEARSEDA")]
legend1<- legend1[order(legend1$PEARSENMB),]
legend1$items <- paste(legend1$PEARSENMB, legend1$PEARSEDA, sep = ". ")

# Save as pdf with same dimensions as png
pdf(file = paste("Output/", year_id, "/Regional_",year_id,".pdf", sep = ""), 
    width = 8.5, height = 5.5)
  par(mar=c(1,1,1,1))
  par(xpd = T, mar = par()$mar + c(0,0,0,12))
  plot(Pearse, col=IDcolour, border = "grey20", lwd = 0.3)
  plot(WaterBodies, add=TRUE, col="slategray1", border= "grey20", lwd = 0.3)
  polygonsLabel(Pearse, labels = as.character(ID$PERSEDA_CODE), method = "centroid", cex = 0.6)
  legend("topright", inset = c(-0.4,0.15), xpd = TRUE,
       legend = c("Drainage Regions","", legend1$items),
       col = "black", cex = 0.9, y.intersp = 0.75, bty = "n")
  legend("topright", inset = c(0.05,0.15), cex=0.8, bty="n",
       legend = c("Main Water Bodies", "Low", "Normal", "High", "N/A"),
       fill = c("slategray1", orange, green, blue, "#C0C0C0"))
  mtext(paste("HYDAT version date:", version, sep=" "), side=1, line=-2, adj = 0, cex = 0.9)
  title(paste("Water Quantity Status of Drainage Regions (", year_id, ")", sep=" "), font=2, adj =0, line = -1)
  par(mar=c(1,1,1,1) + 0.1)
dev.off()

# Save as png
png(file = paste("Output/", year_id, "/Regional_",year_id,".png", sep = ""), 
    width = 8.5, height = 5.5, units = "in", res = 600)
  par(mar=c(1,1,1,1))
  par(xpd = T, mar = par()$mar + c(0,0,0,12))
  plot(Pearse, col=IDcolour, border = "grey20", lwd = 0.3)
  plot(WaterBodies, add=TRUE, col="slategray1", border= "grey20", lwd = 0.3)
  polygonsLabel(Pearse, labels = as.character(ID$PERSEDA_CODE), method = "centroid", cex = 0.6)
  legend("topright", inset = c(-0.4,0.15), xpd = TRUE,
       legend = c("Drainage Regions","", legend1$items),
       col = "black", cex = 0.9, y.intersp = 0.75, bty = "n")
  legend("topright", inset = c(0.05,0.15), cex=0.8, bty="n",
       legend = c("Main Water Bodies", "Low", "Normal", "High", "N/A"),
       fill = c("slategray1", orange, green, blue, "#C0C0C0"))
  mtext(paste("HYDAT version date:", version, sep=" "), side=1, line=-2, adj = 0, cex = 0.9)
  title(paste("Water Quantity Status of Drainage Regions (", year_id, ")", sep=" "), font=2, adj =0, line = -1)
  par(mar=c(1,1,1,1) + 0.1)
dev.off()


### Create local results maps

# Create point shapefile using lat and long of local results 
#  and convert to standard projection (Lambert, lat = (49,77), same as polygon shapefile)
lambert<- crs(Pearse)
LocalSpatial <-SpatialPointsDataFrame(Local[,c("Longitude", "Latitude")], data= Local)
crs(LocalSpatial)<- "+init=epsg:4326"
LocalSpatial <-spTransform(LocalSpatial, CRSobj = lambert)

# Create symbol vector
symbol<-rep(21, nrow(Local))
symbol[Local$Reg.==1] <- 24

# Create colour vectors
Loc.col<- rep("#000000", nrow(Local))
# Loc.col[Local$OperSched=="Seasonal" & Local$Flow_Type=="Low"]<- orange
# Loc.col[Local$OperSched=="Seasonal" & Local$Flow_Type=="Normal"]<- green
# Loc.col[Local$OperSched=="Seasonal" & Local$Flow_Type=="High"]<- blue
Loc.col[Local$Flow_Type=="Low"]<- orange
Loc.col[Local$Flow_Type=="Normal"]<- green
Loc.col[Local$Flow_Type=="High"]<- blue

Loc.bg<- rep("grey95", nrow(Local))
Loc.bg[Local$OperSched=="Continuous" & Local$Flow_Type=="Low"]<- orange
Loc.bg[Local$OperSched=="Continuous" & Local$Flow_Type=="Normal"]<- green
Loc.bg[Local$OperSched=="Continuous" & Local$Flow_Type=="High"]<- blue

# Legend Items
local.items<- c("Yearly", "Low", "Normal", "High", "Seasonal", "Low", "Normal", "High")
# local.leg.col <- c(NA, rep("#000000", 3), NA, orange, green, blue, NA)
local.leg.col <- c(NA, orange, green, blue, NA, orange, green, blue, NA)
local.leg.pt.bg<- c(NA,orange, green, blue, NA,rep("grey95", 3), NA)


# Save as pdf with same dimensions as png
pdf(file = paste("Output/", year_id, "/Local_",year_id,".pdf", sep = ""), 
    width = 8.5, height = 5.5)
  par(mar=c(1,1,1,1))
  par(xpd = T, mar = par()$mar + c(0,0,1,8))
  plot(Pearse, col="grey95", border = "grey20", lwd = 0.3)
  plot(WaterBodies, add=TRUE, col="slategray1", border= "grey20", lwd = 0.3)
  plot(LocalSpatial, add=TRUE, pch=symbol, col=Loc.col, bg=Loc.bg, cex=0.6)
  legend("topright", inset = c(0.05, 0.08), cex=0.6, bty="n", fill = c(rep(NA, 8), "slategray1"),
         title="Natural", legend=c(local.items, "Main Water Bodies"),
         pch = 21, col = local.leg.col, border="white", pt.bg = local.leg.pt.bg)
  legend("topright", inset = c(-0.25, 0.08),xpd=TRUE,  cex=0.6, bty="n", fill = c(rep(NA, 8), "grey95"),
         title="Regulated", legend=c(local.items, "Pearse Drainage Area"),
         pch = 24, col = local.leg.col, border="white", pt.bg = local.leg.pt.bg)
  mtext(paste("HYDAT version date:", version, sep=" "), side=1, line=-2, adj = 0, cex = 0.9)
  title(paste("CESI Local Water Quantity Indicator (", year_id, ")", sep=" "), font=2, adj =0)
  par(mar=c(1,1,2,1) + 0.1)
dev.off()

# Save as png
png(file = paste("Output/", year_id, "/Local_",year_id,".png", sep = ""), 
    width = 8.5, height = 5.5, units = "in", res = 600)
  par(mar=c(1,1,1,1))
  par(xpd = T, mar = par()$mar + c(0,0,1,8))
  plot(Pearse, col="grey95", border = "grey20", lwd = 0.3)
  plot(WaterBodies, add=TRUE, col="slategray1", border= "grey20", lwd = 0.3)
  plot(LocalSpatial, add=TRUE, pch=symbol, col=Loc.col, bg=Loc.bg, cex=0.6)
  legend("topright", inset = c(0.05, 0.08), cex=0.8, bty="n", fill = c(rep(NA, 8), "slategray1"),
         title="Natural", legend=c(local.items, "Main Water Bodies"),
         pch = 21, col = local.leg.col, border="white", pt.bg = local.leg.pt.bg)
  legend("topright", inset = c(-0.25, 0.08),xpd=TRUE,  cex=0.8, bty="n", fill = c(rep(NA, 8), "grey95"),
         title="Regulated", legend=c(local.items, "Pearse Drainage Area"),
         pch = 24, col = local.leg.col, border="white", pt.bg = local.leg.pt.bg)
  mtext(paste("HYDAT version date:", version, sep=" "), side=1, line=-2, adj = 0, cex = 0.9)
  title(paste("CESI Local Water Quantity Indicator (", year_id, ")", sep=" "), font=2, adj =0)
  par(mar=c(1,1,2,1) + 0.1)
dev.off()

# Clean up files
rm(list = c("ID", "ID2", "lambert", "legend1", "Local", "LocalSpatial", "Pearse", "Regional",
            "WaterBodies", "IDcolour", "Loc.bg", "Loc.col", "local.items", "local.leg.col",
            "local.leg.pt.bg", "symbol"))
gc()

# unload map packages and reload data.table (raster package masks some functions in data.table)
detach("package:raster", unload = TRUE)
detach("package:rgdal", unload=TRUE)
detach("package:rgeos", unload=TRUE)
library("data.table")