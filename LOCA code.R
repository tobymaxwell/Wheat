setwd("/Users/maxwell1/Documents/GitHub/Wheat/")
library(raster)
library(sp)
library(latticeExtra)
library(rgdal)
library(rgeos)
library(GISTools)

loca<-read("/Users/maxwell1/Documents/LOCA model/County/tasmax_models_max_rcp45-2016.tif")
plot(loca)



