setwd("/Users/Maxwell/Documents/GitHub/Wheat/")
library(raster)
library(sp)
library(latticeExtra)
library(rgdal)
library(rgeos)
library(GISTools)
CA<-readRDS("counties.rds")
plot(CA)

world.bio<-getData(name = 'worldclim', var = 'bio', res=2.5)
CA.precip<-world.bio[[12]]
CA.mat<-world.bio[[1]]

sites<-readOGR(dsn="Wheat.cut.kml", layer='Wheat' ) 
sites<-sites[-2]
sites.df<-as.data.frame(sites)
sites.df
sites.df$Name
levels(commonsite.yield.avg$Location)
dsp<-SpatialPoints(sites.df[3:4], proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 "))
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0") #make CRS

library(dismo)
dta<-spTransform(dsp, crs(CA.precip))
v <- voronoi(sites) #make voronoi polygons, = nearest neighbor polygons
plot(v)
crs(CA)
crs(CA.precip)
#confine it to california
plot(CA)
plot(sites, add = TRUE)


ca <- aggregate(CA) #combine the shapes into a simpler polygon
plot(ca)
CA<-spTransform(CA, crs(CA.precip)) #put dsp in TA CRS
sites<-spTransform(sites, crs(CA)) #put dsp in TA CRS
wheatcounties<-(CA[sites,])

## Loading required namespace: rgeos
vca <- crop(v, CA)#essentially cropping extent of v to the bounds of ca
plot(vca)
plot(wheatcounties)
vca.1<-crop(vca, wheatcounties)
plot(vca.1)
vca.1$Name<-sites.df$Name
par(mar=c(0,0,0,0))
plot(CA)
#plot(vca.1, add=T, col = "lightblue")
plot(wheatcounties, add=T, col = "lightblue" )
plot(sites, add = TRUE, pch = 16, cex=0.5)
library(maptools)
pointLabel(coordinates(wheatcounties),labels=wheatcounties$NAME, cex = 0.75, font = 2, method="SANN", doPlot=T)


library(spatialEco)
LtoCounty<-data.frame((point.in.poly(sites, CA)))
LtoCounty<-LtoCounty[-2:-3]
LtoCounty<-LtoCounty[-3:-4]
LtoCounty<-LtoCounty[-5:-6]
colnames(LtoCounty)<-c("Location", "County","long","Lat")
str(LtoCounty)
####Yield DF####
str(sites.df)
class(commonsite.yield.avg$County)
str(commonsite.yield.avg)
sites.df<-as.data.frame(LtoCounty)
YieldSites<-merge(sites.df, commonsite.yield.avg, by.x ="County", by.y = "County", all = T )
YieldSites<-commonsite.yield.avg
YieldSites.Yield<-NULL
YieldSites.Yield$Location<-commonsite.yield.avg$Location
YieldSites.Yield$Yield<-commonsite.yield.avg$Yield
YieldSites.Yield<-as.data.frame(YieldSites.Yield)
library(plyr)
YieldSites.Yield<-ddply(na.omit(YieldSites.Yield), .(Location), summarise,
                        Yield.sd = sd(Yield),
                        Yield = mean(Yield))
class(YieldSites.Yield)
str(YieldSites.Yield)
plot(CA)
CA$NAME
vca.merge<-merge(wheatcounties, YieldSites.Yield, all = TRUE, by.x=c("NAME"), by.y=c("Location"))
plot(vca.merge)

YieldSites$LocYear<-paste0(YieldSites$Location, YieldSites$Year)
vca.merge.all<-merge(vca.1, YieldSites.Yield, all=T, by.x="Name", by.y="Location")
plot(CA)
plot(vca.merge.all, col = vca.merge.all$Yield, add=T)
plot(sites, add=T)
plot(wheatcounties, add=T)
data.frame(vca.merge.all)
wheatinterp<-merge(sites, YieldSites.Yield, all=T, by.x="Name", by.y="Location")


####WUE####
commonsite.wue.avg
levels(commonsite.wue.avg$Location)
sites.df<-merge(sites.df,LtoCounty, by.x="Name", by.y="Location")
WUESites<-merge(sites.df, commonsite.wue.avg, by="County", all = T )
str(WUESites)
WUESites.WUE<-NULL
WUESites.WUE$County<-WUESites$County
WUESites.WUE$Location<-WUESites$Location
WUESites.WUE$WUE<-WUESites$wue
WUESites.WUE<-as.data.frame(WUESites.WUE)
library(plyr)
WUESites.WUE<-ddply(na.omit(WUESites.WUE), .(Location), summarise,
                        WUE.sd = sd(WUE),
                        WUE = mean(WUE))
WUESites.WUE
wheatcounties
vca.wue<-merge(wheatcounties, WUESites.WUE, by.x="NAME", by.y="County")

wueinterp<-merge(sites, WUESites.WUE, all=T, by.x="Name", by.y="Location")

####

#####Make Polygons For NUE####

commonsite.NUE.avg<-commonsite.nue.avg
levels(commonsite.NUE.avg$Location)
commonsite.NUE.avg<-merge(commonsite.NUE.avg, LtoCounty)
NUESites<-merge(sites.df, commonsite.NUE.avg, by= "Location", all = T )
NUESites

NUESites.NUE<-NULL
NUESites.NUE$County<-NUESites$County
NUESites.NUE$Location<-NUESites$Location
NUESites.NUE$NUE<-NUESites$nue
NUESites.NUE<-as.data.frame(NUESites.NUE)
library(plyr)
NUESites.NUE<-NUESites.NUE[!is.infinite(NUESites.NUE$NUE),]
NUESites.NUE<-ddply(na.omit(NUESites.NUE), .(Location), summarise,
                    NUE.sd = sd(NUE),
                    NUE = mean(NUE))
str(NUESites.NUE)
vca.NUE<-merge(wheatcounties, NUESites.NUE, by.x="NAME", by.y="County")
nueinterp<-merge(sites, NUESites.NUE, all=T, by.x="Name", by.y="Location")


MERGE<-merge(NUESites.NUE, WUESites.WUE, by = c("Name"), all = T)
merge.2<-merge(MERGE,YieldSites.Yield, by = "Name", all = T)
write.csv(merge.2, "/Users/tobymaxwell/desktop/merge.2.csv")

sites.NUE<-readOGR(dsn="/Users/tobymaxwell/Google Drive/documents/Grad/Research/NSF WSC/Wheat R/Wheat.cut.kml", layer='Wheat') 
sites.NUE<-sites.NUE[sites.NUE$Name %in% NUESites.NUE$Name,]
sites.df.NUE<-as.data.frame(sites.NUE)
sites.df.NUE
sites.df.NUE$Name
levels(commonsite.NUE.avg$Location)
dsp.NUE<-SpatialPoints(sites.df.NUE[3:4], proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 "))
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0") #make CRS

library(dismo)
dta.NUE<-spTransform(dsp.NUE, crs(CA.precip))
v.NUE <- voronoi(dta.NUE) #make voronoi polygons, = nearest neighbor polygons
plot(v.NUE)
#confine it to california
plot(CA)
plot(sites.NUE, add = TRUE)
sites.NUE<-spTransform(sites.NUE, crs(CA)) #put dsp in TA CRS
wheatcounties.NUE<-(CA[sites.NUE,])
vca.NUE <- crop(v.NUE, ca)
plot(vca.NUE)
plot(wheatcounties)
vca.1.NUE<-crop(vca.NUE, wheatcounties) #NEED TO FIX CRS
plot(CA)
plot(vca.1.NUE, add=T, col = "lightblue")
vca.NUE.intersect <- crop(vca.1.NUE, ca)
plot(vca.NUE.intersect)
vca.1.NUE$Name<-sites.df.NUE$Name
vca.NUE<-merge(vca.1.NUE, NUESites.NUE)
plot(vca.NUE)
####
library(tmap)

####Yield maps####
plot(vca.merge, col=vca.merge$Yield)

yield.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.merge)+tm_polygons(alpha = 1, col = "Yield", palette = "Greens", legend.hist = T)+tm_text("NAME", size = .5, col = "black")
yield.map

yield.sites<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(wheatinterp)+tm_symbols(alpha = 1, size = "Yield", palette = "Greens", col = "Yield.sd", scale = 3)+tm_text("Name", size = 1, col = "black", auto.placement = T)
yield.sites

yield.sd<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.merge)+tm_polygons(alpha = 1, col = "Yield.sd", palette = "Reds", legend.hist = T)

tmap_arrange(yield.map,yield.sd)

tm_shape(vca.merge)
map1<-qtm(shp=vca.merge, fill="Yield", fill.palette="Greens", basemaps = T, alpha = 0.7)
map1
map1+tm_view(basemaps = "Stamen.Watercolor")
plot(vca.merge, col = vca.merge$Yield)
text(x = centroid$x, y=centroid$y, label=(vca.1$Name), cex = .4, font = 2, col = 'Black')
plot(sites, add= TRUE, pch = 16, col = "grey", cex = .75)

###WUEmap####

wue.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.wue)+tm_polygons(alpha = 1, col = "WUE", palette = "Blues", legend.hist = T, breaks = c(0, 75, 150, 225, 300, 375, 600))
wue.map
wue.sd<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.wue)+tm_polygons(alpha = 1, col = "WUE.sd", palette = "Reds", legend.hist = T)

tmap_arrange(wue.map,wue.sd)

wue.sites<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(wueinterp)+tm_symbols(alpha = 1, size = "WUE", palette = "Blues", col = "WUE.sd", scale = 3)+tm_text("Name", size = 1, col = "black", auto.placement = T)
wue.sites
#NUEmap####

NUE.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.NUE)+tm_polygons(alpha = 1, col = "NUE", palette = "Purples", legend.hist = T)
NUE.map
NUE.sd<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.NUE)+tm_polygons(alpha = 1, col = "NUE.sd", palette = "Reds", legend.hist = T)
tmap_arrange(NUE.map,NUE.sd)

tmap_arrange(yield.map,wue.map,NUE.map, nrow=1)

nue.sites<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(nueinterp)+tm_symbols(alpha = 1, size = "NUE", palette = "Purples", col = "NUE.sd", scale = 3)+tm_text("Name", size = 1, col = "black", auto.placement = T)
nue.sites

tmap_arrange(yield.sites, wue.sites, nue.sites, nrow =1)
###CHeck for spatial autocorrelation in Yield
library(spdep)
library(spatstat)
library(sp)
library(ncf)
YieldSites
str(YieldSites)
yieldmat<-YieldSites[-2]
str(YieldSites)
yieldmat<-yieldmat[-5]
yieldmat<-na.omit(yieldmat)
str(yieldmat)
w <- 1/as.matrix(dist(coordinates(vca.merge)))
diag(w) <- 0
w
moran.test(vca.merge$Yield,mat2listw(w))
moran.mc(vca.merge$Yield,mat2listw(w), nsim = 10000)

#Rook
vca.merge
wr <- poly2nb(vca.merge, queen=FALSE)
wr
wm <- nb2mat(wr, style='W', zero.policy = TRUE)
dim(wm)

moran.test(vca.merge$Yield,mat2listw(wm), zero.policy = TRUE)
moran.mc(vca.merge$Yield,mat2listw(wm), nsim = 10000, zero.policy = TRUE)

#Queen
vca.merge
wr <- poly2nb(vca.merge, queen=T)
wr
wm <- nb2mat(wr, style='W', zero.policy = TRUE)
dim(wm)

moran.test(vca.merge$Yield,mat2listw(wm), zero.policy = TRUE)
moran.mc(vca.merge$Yield,mat2listw(wm), nsim = 10000, zero.policy = TRUE)

#2nd order Queen
q2o<-nblag(wr,maxlag =2) #from 1st order matrix, can get a list of lists that defines the 1st and 2nd order neighbors
q2o.nb<-q2o[[2]]
wm <- nb2mat(q2o.nb, style='W', zero.policy = TRUE)
dim(wm)
moran.test(vca.merge$Yield,mat2listw(wm), zero.policy = TRUE)
moran.mc(vca.merge$Yield,mat2listw(wm), nsim = 10000, zero.policy = TRUE)

###SpatAutoCorr WUE####
WUESites
str(WUESites)
WUEmat<-WUESites[-2]
str(WUEmat)
WUEmat<-WUEmat[-4]
WUEmat<-WUEmat[is.finite(WUEmat$WUE),]
WUEmat<-na.omit(WUEmat)
str(WUEmat)
plot(vca.WUE, col = vca.WUE$WUE)
w <- 1/as.matrix(dist(coordinates(vca.WUE)))
diag(w) <- 0
w
moran.test(vca.WUE$WUE,mat2listw(w), zero.policy = TRUE)
moran.mc(vca.WUE$WUE,mat2listw(w), nsim = 10000, zero.policy = TRUE)

###Binary Approach
#Rook
vca.WUE
wr <- poly2nb(vca.WUE, queen=FALSE)
wr
wm <- nb2mat(wr, style='W', zero.policy = TRUE)
dim(wm)

moran.test(vca.WUE$WUE,mat2listw(wm), zero.policy = TRUE)
moran.mc(vca.WUE$WUE,mat2listw(wm), nsim = 10000, zero.policy = TRUE)

#Queen
vca.WUE
wr <- poly2nb(vca.WUE, queen=T)
wr
wm <- nb2mat(wr, style='W', zero.policy = TRUE)
dim(wm)

moran.test(vca.WUE$WUE,mat2listw(wm), zero.policy = TRUE)
moran.mc(vca.WUE$WUE,mat2listw(wm), nsim = 10000, zero.policy = TRUE)

##2nd order Queen###
q2o<-nblag(wr,maxlag =2) #from 1st order matrix, can get a list of lists that defines the 1st and 2nd order neighbors
q2o.nb<-q2o[[2]]
wm <- nb2mat(q2o.nb, style='W', zero.policy = TRUE)
dim(wm)
moran.test(vca.WUE$WUE,mat2listw(wm), zero.policy = TRUE)
moran.mc(vca.WUE$WUE,mat2listw(wm), nsim = 100000, zero.policy = TRUE)

###SpatAutoCorr NUE####
NUESites
str(NUESites)
NUEmat<-NUESites[-2]
str(NUEmat)
NUEmat<-NUEmat[-4]
NUEmat<-na.omit(NUEmat)
str(NUEmat)
plot(vca.NUE, col = vca.NUE$NUE)
w <- 1/as.matrix(dist(coordinates(vca.NUE)))
diag(w) <- 0
w
moran.test(vca.NUE$NUE,mat2listw(w))
moran.mc(vca.NUE$NUE,mat2listw(w), nsim = 10000)

#Nearest Neighbor Approach
dis<-dist(coordinates(sites.NUE))
dis
D <- as.matrix(dis) #transform into a normal full matrix
round(D)

plot(hclust(dis))
library(raster)
gdis <- pointDistance(coordinates(sites.NUE)[,1:2], lonlat=TRUE) #use pointDistance to calculate distances on spherical surface
gdis
cols <- apply(D, 1, order)
cols
cols <- t(cols) #Transpose
cols
cols.nearest <- cols[, 2:3] #take columns 2 and 3
cols.nearest

#Binary Matrices
#Rook
library(spdep)
vca.NUE
wr <- poly2nb(vca.NUE, queen=FALSE)
wr
wm <- nb2mat(wr, style='W', zero.policy = TRUE)
dim(wm)

moran.test(vca.NUE$NUE,mat2listw(wm), zero.policy = TRUE)
moran.mc(vca.NUE$NUE,mat2listw(wm), nsim = 10000, zero.policy = TRUE)

###QUEEN
vca.NUE
wr <- poly2nb(vca.NUE, queen = T)
wr
wm <- nb2mat(wr, style='W', zero.policy = TRUE)
dim(wm)

moran.test(vca.NUE$NUE,mat2listw(wm), zero.policy = TRUE)
moran.mc(vca.NUE$NUE,mat2listw(wm), nsim = 10000, zero.policy = TRUE)
##Queen 2nd Order####
q2o<-nblag(wr,maxlag =2) #from 1st order matrix, can get a list of lists that defines the 1st and 2nd order neighbors
q2o.nb<-q2o[[2]]
wm <- nb2mat(q2o.nb, style='W', zero.policy = TRUE)
dim(wm)
moran.test(vca.NUE$NUE,mat2listw(wm), zero.policy = TRUE)
moran.mc(vca.NUE$NUE,mat2listw(wm), nsim = 10000, zero.policy = TRUE)

####Interpolations####


####Yield by temp Correlations####
str(commonsite.yield)
commonsite.interpolate<-ddply(commonavg, .(Location, County, Year), summarise, 
                Yield = mean(Yield),
                MAT=mean(MAT),
                MAP=mean(MAP),
                pdsi = mean(pdsi))

commonsite.annualavg<-ddply(commonavg, .(Location, Year), summarise, 
                              Yield = mean(Yield),
                              MAT=mean(MAT),
                              MAP=mean(MAP),
                              pdsi = mean(pdsi))

commonsite.annualavg<-ddply(na.omit(commonsite.annualavg), .(Location), summarise, 
                            Yield = mean(Yield),
                            MAT=mean(MAT),
                            MAP=mean(MAP),
                            pdsi = mean(pdsi))

yield.ts.df<-merge(LtoCounty, commonsite.annualavg, by = "Location")
finite.interpolate<-yield.ts.df[1]
finite.interpolate$Yield<-yield.ts.df$Yield
sites.interpolate<-merge(sites, finite.interpolate, by.x="Name",by.y="Location")
# FUNCTION TO REMOVE NA's IN sp DataFrame OBJECT
#   x           sp spatial DataFrame object
#   margin      Remove rows (1) or columns (2) 
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
    if(margin == 1) {  
      cat("DELETING ROWS: ", na.index, "\n") 
        return( x[-na.index,]  ) 
    }
    if(margin == 2) {  
      cat("DELETING COLUMNS: ", na.index, "\n") 
        return( x[,-na.index]  ) 
    }
 }

sites.interpolate<-sp.na.omit(sites.interpolate)
sites.interpolate<-spTransform(sites.interpolate, crs(CA))
int.points<-sites.interpolate
int.points<-SpatialPoints(sites.interpolate, crs(CA))
int.points<-SpatialPoints(data.frame(int.points)[1:2], crs(CA))

library(gstat)
gs <- gstat(formula=sites.interpolate$Yield~1, locations=sites.interpolate) #create gstat object (krig with infinite points, 2nd order decay are the default values)
v <- variogram(gs, width = 0.2) #from gstat object, calculate variogram
head(v)
plot(v)


#now fit a model variogram function

fve <- fit.variogram(v, vgm("Gau"))#fit model from variogram data 'v' with model vgm(...), where 85 is the partil sill (asymptote), "exp" means the exponential model, range of 75 and 
fve
##   model    psill    range
## 1   Nug 21.96600  0.00000
## 2   Exp 85.52957 72.31404
plot(variogramLine(fve, 4), type='l')
points(v[,2:3], pch=20, col='red')

k <- gstat(formula=sites.interpolate$Yield~1, locations=int.points, model=fve) #formula is air quality by intercept, at locations of aq raster, and model is using the fve variogram calculated above (exponential model)
# predicted values
r <- raster(CA, ncols = 100, nrows = 100)
g <- as(r, 'SpatialGrid')
kp <- predict(k, g) #predict k function (intercept model with fve variogram) across field g
str(k)
spplot(kp)


data.frame(sites.interpolate)
gs <- gstat(formula=sites.interpolate$Yield~1, nmax=5, locations=int.points, set=list(idp=1)) #defines linear model as precipitation as a funciton of the intercept at locations described by the dta SpatialPointsDataFrame
r <- raster(CA, ncols = 100, nrows = 100)
idw <- interpolate(r, gs) #make a RasterLayer with interpolated values using the fitted model from gs which does inverse distance weighted interpolation
plot(idw)
crs(idw)<- crs(CA)
crs(idw)
idw<-mask(idw,CA)
plot(idw)
plot(ca, add = TRUE)
plot(wheatcounties, add=T, lwd=0.25)
plot(int.points, add=TRUE, cex=0.5, pch=16)


NUEsites<-SpatialPoints(vca.NUE, crs(CA.precip))
gs2 <- gstat(formula=NUESites.NUE$NUE~1, locations=NUEsites, nmax=4, set=list(idp=2)) #set inverse distance power to be to the power of 1 and also limit krigeing to the nmax=1 nearest neighbor. This limits the number of points involved with interpolation and also sets the decay rate to be to the first power, default is 2nd order decay. Higher order powers give more infludence to closer points, giving less smooth surface, lower orders give more influence to further points, giving more smooth surfaces
r <- raster(CA, ncols = 100, nrows = 100)
idw <- interpolate(r, gs2) #make a RasterLayer with interpolated values using the fitted model from gs which does inverse distance weighted interpolation
plot(idw)
idw<-mask(idw, wheatcounties)
plot(idw)
plot(ca, add=TRUE, cex=0.5)
plot(wheatcounties, add=T, lwd=0.25)
plot(NUEsites, pch=16, add=T, cex=0.5)
#Mapping
#Polygons
r <- raster(CA, ncols = 100, nrows = 100)
plot(CA)
#generate raster layer object with resolution 10000
vr <- rasterize(vca.NUE, r, 'NUE')
par(mfrow = c(1,1))
plot(vr)
plot(vca.NUE.intersect, add = T)
plot(CA, add = T)


NUE.map.2<-tm_shape(ca)+tm_borders(col = "black", alpha = 1)+tm_shape(vr)+tm_raster(alpha = 1, col = "NUE", palette = "Purples", breaks = c(0,0.05,0.1,0.15,0.2,0.25,0.5,0.75), title = c("NUE (Protein (%N)/ \nN applied (lb/acre)"))+tm_legend(position = c("center", "top"))+tm_shape(ca)+tm_borders(col = "black", alpha = 1)
NUE.map.2
#Weights Matrix
cp <- rasterToPoints(vr) #make matrix with xy and values of each point in the raster
# distance matrix
d <- pointDistance(cp[, 1:2], dta.NUE, lonlat=FALSE) #distance between points on xy plane
dim(d)
nn <- 5
ngb <- t(apply(d, 1, function(x) order(x)[1:nn])) #by row, order from least to most (which is equivalent to distance) and take the first 5 values (5 closest points). Transpose the result so each row of the new matrix is the 5 closest points to that point
head(ngb)

plot(CA)
points(cp[1, 1:2, drop=FALSE], col='blue', pch='x', cex=2)#plot blue X for 1st value
points(dta[ngb[1,], ], col='red', pch=20) #plot 5 defined points in red
points(cp[nrow(cp), 1:2, drop=FALSE], col='blue', pch='x', cex=2) # plot blue x for last value
points(dta[ngb[nrow(cp),], ], col='green', pch=20) #plot 5 closest in green

pairs <- cbind(rep(1:nrow(ngb), nn), as.vector(ngb)) #make sets of values, assigning an index to each point for each of its neighbors so that when it is in long format each event can be identified by its index, so there are 5 events for each event (as in 5 values for each location representing its 5 nearest neighbors)
values <- vca.NUE$NUE[pairs[,2]] #subset data by matching to second column of pairs
pn <- tapply(values, pairs[,1], mean) #get mean according to groups of values defined by pairs[,1], the index

nnr <- r #make a raster
nnr[!is.na(vr)] <- pn #put pn values into nnr
plot(nnr)#plot it
plot(CA, add=T)


#Inverse Weights
library(gstat)
gs <- gstat(formula=NUESites.NUE$NUE~1, locations=dta.NUE) #defines linear model as precipitation as a funciton of the intercept at locations described by the dta SpatialPointsDataFrame
idw <- interpolate(r, gs) #make a RasterLayer with interpolated values using the fitted model from gs which does inverse distance weighted interpolation
crop(idw,ca)
plot(idw)
#Spatial Lag Model?
str(commonsite.model.avg)
####Yield DF####
modelSites<-merge(sites.df, commonsite.model.avg, by.x ="Name", by.y = "Location", all = T)
modelSites
modelSites.model<-modelSites
modelSites.model<-as.data.frame(modelSites.model)
library(plyr)
str(modelSites.model)
modelSites.model<-ddply(na.omit(modelSites.model), .(Name), summarise,
                        Yield = mean(Yield),
                        Water.Total = mean(Water.Total),
                        Preplant.N = mean(Preplant.N),
                        CO2 = mean(CO2),
                        solar = mean(solar))
modelSites.model
#####
vca.model<-merge(vca.1, modelSites.model, all = TRUE)

library(tmap)
xy<-coordinates(vca.model)[,1:2]
nb2500 <- dnearneigh(xy, 0, 2500) #make neighbor list by distances, for coordinates xy, distances between 0 and 2500
w2500 <- nb2listw(nb2500, zero.policy=TRUE)
w2500
m.yield<-lm(Yield~Water.Total+Preplant.N+Name*CO2+solar, modelSites.model)
summary(m.yield)
lm.LMtests(m.yield, w2500, zero.policy=TRUE, test='LMerr')

vca.MAT<-merge(vca.1,importance.all[importance.all$ID=="MAT",], by.x="Name", by.y="Location")
MAT.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.MAT)+tm_polygons(title="MAT",col = "importance",palette = "Reds", textNA ="Importance=0", textNA ="Importance=0", textNA ="Importance=0")+tm_text("sign", size =0.5, col = "black")

vca.pdsi.1<-merge(vca.1, importance.all[importance.all$ID=="pdsi.q1",], by.x="Name", by.y="Location")
pdsi.map.1<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.pdsi.1)+tm_polygons(col = "importance", title = "PDSI Q1", textNA ="Importance=0", textNA ="Importance=0", textNA ="Importance=0")+tm_text("sign", size = .5, col = "black")

vca.MAP<-merge(vca.1, importance.all[importance.all$ID=="MAP",], by.x="Name", by.y="Location")
MAP.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.MAP)+tm_polygons(col = "importance",title = "MAP", palette = "Blues", textNA ="Importance=0", textNA ="Importance=0", textNA ="Importance=0")+tm_text("sign", size = .5, col = "black")

vca.CO2<-merge(vca.1, importance.all[importance.all$ID=="CO2",], by.x="Name", by.y="Location")
CO2.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.CO2)+tm_polygons(col = "importance",title = "CO2", palette = "Purples", textNA ="Importance=0", textNA ="Importance=0", textNA ="Importance=0")+tm_text("sign", size = .5, col = "black")

tmap_arrange(MAT.map, pdsi.map.1, MAP.map,CO2.map, nrow=2)

vca.Pre<-merge(vca.1, importance.all[importance.all$ID=="Preplant.N",][-1,], by.x="Name", by.y="Location")
Pre.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.Pre)+tm_polygons(col = "importance",title = "Preplant N", palette = "Greens", textNA ="Importance=0")+tm_text("sign", size = .5, col = "black")

vca.N<-merge(vca.1, importance.all[importance.all$ID=="Total.N",][-1,], by.x="Name", by.y="Location")
N.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.N)+tm_polygons(col = "importance",title = "Total N", palette = "Greens", textNA ="Importance=0")+tm_text("sign", size = .5, col = "black")

vca.Anth<-merge(vca.1, importance.all[importance.all$ID=="Anthesis.N",][-1,], by.x="Name", by.y="Location")
Anth.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.Anth)+tm_polygons(col = "importance",title = "Anthesis N", palette = "Greens", textNA ="Importance=0")+tm_text("sign", size = .5, col = "black")

vca.Till<-merge(vca.1, importance.all[importance.all$ID=="Tillering",][-1,], by.x="Name", by.y="Location")
Till.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(vca.Till)+tm_polygons(col = "importance",title = "Tillering N", palette = "Greens", textNA ="Importance=0", textNA ="Importance=0")+tm_text("sign", size = .5, col = "black")


tmap_arrange(Pre.map, N.map, Anth.map, Till.map, nrow=2)


CA.MAT<-merge(wheatcounties,importance.counties[importance.counties$ID=="MAT",], by.x="NAME", by.y="Location")
MAT.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.MAT)+tm_polygons(title="MAT",col = "importance",palette = "Reds", textNA ="Importance=0", textNA ="Importance=0")+tm_text("sign", size = 1, col = "black")
MAT.map

CA.pdsi.1<-merge(wheatcounties, importance.counties[importance.counties$ID=="pdsi.q1",], by.x="NAME", by.y="Location")
PDSI.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.pdsi.1)+tm_polygons(col = "importance", title = "PDSI Q1", textNA ="Importance=0")+tm_text("sign", size = 1, col = "black")
PDSI.map

CA.MAP<-merge(wheatcounties, importance.counties[importance.counties$ID=="MAP",], by.x="NAME", by.y="Location")
MAP.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.MAP)+tm_polygons(col = "importance",title = "MAP", palette = "Blues", textNA ="Importance=0")+tm_text("sign", size = 1, col = "black")
MAP.map

CA.CO2<-merge(wheatcounties, importance.counties[importance.counties$ID=="CO2",], by.x="NAME", by.y="Location")
CO2.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.CO2)+tm_polygons(col = "importance",title = "CO2", palette = "Purples", textNA ="Importance=0", textNA ="Importance=0", textNA ="Importance=0",breaks = c(0,0.2,0.4,0.6,0.8,1))+tm_text("sign", size = 1, col = "black")

tmap_arrange(MAT.map, PDSI.map, MAP.map,CO2.map, nrow=2)

CA.Pre<-merge(wheatcounties, importance.counties[importance.counties$ID=="Preplant.N",], by.x="NAME", by.y="Location")
Pre.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.Pre)+tm_polygons(col = "importance",title = "Preplant N", palette = "Greens", textNA ="Importance=0",breaks = c(0,0.2,0.4,0.6,0.8,1))+tm_text("sign", size = 1, col = "black")
Pre.map
CA.N<-merge(wheatcounties, importance.counties[importance.counties$ID=="Total.N",], by.x="NAME", by.y="Location")
N.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.N)+tm_polygons(col = "importance",title = "Total N", palette = "Greens", textNA ="Importance=0",breaks = c(0,0.2,0.4,0.6,0.8,1))+tm_text("sign", size = 1, col = "black")
N.map
CA.Anth<-merge(wheatcounties, importance.counties[importance.counties$ID=="Anthesis.N",], by.x="NAME", by.y="Location")
Anth.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.Anth)+tm_polygons(col = "importance",title = "Anthesis N", palette = "Greens", textNA ="Importance=0",breaks = c(0,0.2,0.4,0.6,0.8,1))+tm_text("sign", size = 1, col = "black")
Anth.map
CA.Till<-merge(wheatcounties, importance.counties[importance.counties$ID=="Tillering",], by.x="NAME", by.y="Location")
Till.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.Till)+tm_polygons(col = "importance",title = "Tillering N", palette = "Greens", textNA ="Importance=0", textNA ="Importance=0",breaks = c(0,0.2,0.4,0.6,0.8,1))+tm_text("sign", size = 1, col = "black")

tmap_arrange(Pre.map, N.map, Anth.map, Till.map, nrow=2)

CA.Water<-merge(wheatcounties, importance.counties[importance.counties$ID=="Water.Total",], by.x="NAME", by.y="Location",breaks = c(0,0.2,0.4,0.6,0.8,1))
Water.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.Water)+tm_polygons(col = "importance",title = "Total Water", palette = "Blues", textNA ="Importance=0", textNA ="Importance=0",breaks = c(0,0.2,0.4,0.6,0.8,1))+tm_text("sign", size = 1, col = "black")
Water.map
CA.Rain<-merge(wheatcounties, importance.counties[importance.counties$ID=="Rain",], by.x="NAME", by.y="Location")
Rain.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.Rain)+tm_polygons(col = "importance",title = "Season Rain", palette = "Blues", textNA ="Importance=0", textNA ="Importance=0",breaks = c(0,0.2,0.4,0.6,0.8,1))+tm_text("sign", size = 1, col = "black")
Rain.map

tmap_arrange(MAT.map, PDSI.map,CO2.map, Pre.map, N.map, Till.map, Water.map, Rain.map, MAP.map, nrow=3)

wuepredict<-ddply(commonavg.county, .(County), summarise,
      Predict=mean(Predict),
      WUE=mean(WUE))
CA.mod<-merge(wheatcounties, wuepredict, by.x="NAME", by.y="County")
CA.modmap<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.mod)+tm_polygons(col = "WUE",title = "WUE", palette = "Blues", textNA ="No data", textNA ="Importance=0")+tm_text("WUE")
CA.modmap

temp.ts<-regression
colnames(temp.ts)<-c("County", "coef", "r2", "p", "p.bonf")
CA.Temp.ts<-merge(wheatcounties, temp.ts, by.x="NAME", by.y="County")
temp.ts.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.Temp.ts)+tm_polygons(col = "coef",title = "Temp Coefficient\n C/Year", palette = "Reds", textNA ="No Data")
temp.ts.map
,breaks = c(0,0.2,0.4,0.6,0.8,1)
+tm_text("sign", size = 1, col = "black")

YieldTemp.1<-ddply(na.omit(commonavg.env2), .(County), summarise,
                   Yield.MAT.increase = mean(Yield.MAT.increase))
                   
                   
CA.Temp.ts<-merge(wheatcounties, YieldTemp.1, by.x="NAME", by.y="County")
temp.inc.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.Temp.ts)+tm_polygons(col = "Yield.MAT.increase",title = "Yield % Change\n1 Degree Warming", palette = "Reds", textNA ="No Data")
temp.inc.map

Yieldppt.1<-ddply(na.omit(commonavg.env3), .(County), summarise,
                   Yield.MAP.increase = mean(Yield.MAP.increase))

CA.PPT.ts<-merge(wheatcounties, Yieldppt.1, by.x="NAME", by.y="County")
ppt.inc.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.PPT.ts)+tm_polygons(col = "Yield.MAP.increase",title = "Yield % Change\n95% PPT", palette = "Blues", textNA ="No Data")
ppt.inc.map
