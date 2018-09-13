library(soilDB)
library(FedData)
library(plyr)
library(foreign)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
dbf<-read.dbf("/Users/maxwell1/Documents/GitHub/Wheat/Layer.dbf")
str(dbf)
#st_read(dsn="/Users/tobymaxwell/Downloads/CA SSURGO Shape/", layer = "Untitled")
#ssurgo<-readOGR(dsn="/Users/tobymaxwell/Downloads/CA SSURGO Shape/", layer = "Untitled")
#ssurgo
#ssurgo<-merge(ssurgo, dbf)
#wheatcounties.qgis<-wheatcounties
#wheatconties.qgis <- CRS("+proj=longlat +datum=WGS84 +no_defs") #make CRS
#writeOGR(obj=wheatcounties.qgis, dsn="/Users/tobymaxwell/Downloads/CA SSURGO Shape/", layer="wheatcounties.qgis", driver="ESRI Shapefile") # this is in geographical projection

# the query point is in geographic coordinates with WGS84 datum
result<-NULL
data<-NULL
data.rm<-NULL
data.avg<-NULL
for(i in 1:46){
p <- SpatialPoints(sites.df[i,3:4], proj4string = CRS('+proj=longlat +datum=WGS84')) # transform to planar coordinate system for buffering
p.aea <- spTransform(p, CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ')) # create 1000 meter buffer
p.aea <- gBuffer(p.aea, width = 1000) # transform back to WGS84 GCS
p.buff <- spTransform(p.aea, CRS('+proj=longlat +datum=WGS84')) # extract bounding-box from last point
# coordinates are in WGS84 GCS
b <- as.vector(bbox(p.buff)) # download map unit polygons that overlap with bbox
p.mu.polys <- mapunit_geom_by_ll_bbox(b)
pbuff.awc<-merge(p.mu.polys, dbf)
data<-data.frame(pbuff.awc)$AVG_AWC
data[data<0]<-NA
data.rm<-na.omit(data)
data.avg<-mean(data.rm)
result<-cbind(c(result, data.avg))
}
awc<-data.frame(sites.df$Location, result)
awc$awc.cm<-awc$result*200
awc<-(merge(sites.df, awc, by.x="Location", by.y="sites.df.Location"))
awc
colnames(awc)<-c("Name", "County", "long", "Lat", "awc", "awc.200")
awc
plot(vca)
library(tmap)
# download map unit polygons that overlap with bbox
tm_shape(CA)+tm_borders(col = "black", alpha = 1)
tm_shape(pbuff.awc)+tm_polygons(alpha = 1, col = "AVG_AWC", palette = "Greens")
tm_shape(vca)+tm_polygons(alpha = 1, col = "awc", palette = "Greens")
plot(pbuff.awc, col = AVG_AWC)
par(mar=c(0,0,0,0))
plot(p.mu.polys)
plot(p.mu.polys[which(p.mu.polys$mukey %in% setdiff(p.mu.polys$mukey, res$mukey)), ], add=TRUE, col='grey')
lines(p.buff, col='red', lwd=2)
plot(extent(bbox(p.buff)), add=TRUE, col='RoyalBlue')
points(p, col='orange', pch=15)
legend('bottomleft', legend=c('query point', '1000m buffer', 'buffer BBOX', 'intersected polygons', 'overlapping polygons'), col=c('orange', 'red', 'royalblue', 'black', 'grey'), lwd=c(NA, 2, 2, 2, 2), pch=c(15, NA, NA, NA, NA))
box()



CA<-readRDS("/Users/tobymaxwell/Google Drive/documents/Grad/GEO200/Lab 10/counties.rds")
plot(CA)
SSURGO.CA <- get_ssurgo(template=wheatcounties)
plot(SSURGO.CA, lwd=0.1, add=T)

b <- c(-122,40,-118,35)
extent(CA)
# query geometry
x <- mapunit_geom_by_ll_bbox(b) # about 20 seconds
data.frame(x)
# reset margins, and plot the results
par(mar=c(0,0,0,0))
plot(x)
# add our original bounding box in red
rect(b[1], b[2], b[3], b[4], border='red', lwd=2)
# add a title at the bottom of the figure
title(sub='SSURGO Map Unit Delineations', line=-2, font=2)





vepPolygon <- polygon_from_extent(raster::extent(672800,740000,4102000,4170000), 
                                  proj4string='+proj=utm +datum=NAD83 +zone=12')

# Get the NRCS SSURGO data (USA ONLY)
#SSURGO.VEPIIN <- get_ssurgo(template=vepPolygon, label='VEPIIN')

# Plot the VEP polygon
#plot(vepPolygon)

# Plot the SSURGO mapunit polygons
#plot(SSURGO.VEPIIN$spatial, lwd=0.1, add=T)