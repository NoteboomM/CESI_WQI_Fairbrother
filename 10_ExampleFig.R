##### Create an example figure for the CESI WQI Methods section

library(sp) #
library(rgdal) #
library(rgeos) #
library(raster) #
library(maptools) #
library(gdalUtils) #


PDA <- readOGR("./Map_Creation/PDAs.shp")
N.Sask <- PDA[PDA$PEARSEDA=="North Saskatchewan",]
crs<-N.Sask@proj4string

data <- read.csv("./Output/2019/2019_regional_entry_HYDAT_2021-07.csv", header = TRUE)
data.sask <- data[data$PEARSEDA=="North Saskatchewan",]
sask.xy <- data.sask[,c("Longitude", "Latitude")]
crs_wgs <- CRS( "+init=epsg:4326")
stn_plot<- SpatialPointsDataFrame(coords = sask.xy, data = data.sask, proj4string = crs_wgs)
stn_plot <- sp::spTransform(stn_plot, CRSobj = crs)

river <- readOGR("./Map_Creation/N.Sask_River.shp")
river_proj <- sp::spTransform(river, CRSobj = crs)


# directory for shapefiles
shp_dir <- "./Dependencies/All_Polygons/"
# extract shapefile names
shapefiles <- list.files(shp_dir, pattern = "*.shp$", full.names = T)

list <- data.sask$Station
index <- apply((sapply(list, function(x) grepl(x, shapefiles))), 1,
               function(y) ifelse(any(y), TRUE, FALSE))
stn_shp <- shapefiles[index]
blue<-rgb(15,76,106, maxColorValue = 255)
green<-rgb(133,161,66, maxColorValue = 255)

colour.vec <- c(blue, blue, blue, green, blue, green, green, blue)

source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R") 
# gives box.cex function to increase size of legend colour boxes.

# legend items
txt <- c("High WQI", "Normal WQI", "Monitoring Station", "River")
pch<- c(NA, NA, 21, NA)
col <- c("#000000", "#000000", "#000000", "#777777")
lty <- c(0,0,0,1)
pt.bg <- c(NA, NA, "#000000", NA)
fill <- c(blue, green, NA, NA)
border <- c("#111111", "#111111", NA, NA)

png("test.png", width = 3000, height = 2000, res=360)
plot(N.Sask)
for (i in 1:length(stn_shp)){
  watershed <- readOGR(stn_shp[i])
  watershed2 <- spTransform(watershed, CRSobj = crs)
  colour<-colour.vec[i]
  plot(watershed2, col= colour, border="#111111", add=TRUE)
}
plot(river_proj, col="#777777", add=TRUE)
plot(stn_plot,pch=21, add=TRUE)
par(mar=c(0,0,0,0))
legend("bottomleft", inset = c(0.05, 0.1), txt, lty=lty, pch=pch,pt.bg=pt.bg,col=col, fill=fill,
       border=border, bty="n", y.intersp = 2, box.cex=c(2,1.5))
title("North Saskatchewan Drainage Region - 2019 WQI Results", line = -3, cex=1)
dev.off()

