setwd("/maxwell1/Documents/Github/Wheat/")
library(raster)
library(sp)
library(latticeExtra)
library(rgdal)
library(rgeos)
library(GISTools)
sites<-readOGR(dsn="/Users/maxwell1/Documents/GitHub/Wheat/Wheat.cut.kml", layer='Wheat' ) 
sitecoords<-data.frame(coordinates(sites)[,1:2])
tmin.rasters<-getData(name = 'worldclim', var = 'tmin', res=2.5)
tmin<-extract(tmin.rasters, sitecoords)

tmax.rasters<-getData(name = 'worldclim', var = 'tmax', res=2.5)
tmax<-extract(tmax.rasters, sitecoords)

prec.rasters<-getData(name = 'worldclim', var = 'prec', res=2.5)
prec<-extract(prec.rasters, sitecoords)

tmax.rcp85.AC.50.rast<-getData(name = 'CMIP5', var = 'tmax', res=2.5, lon=-115.5631, lat=32.792, rcp=85, model='AC', year=50)
plot(tmax.rcp85.AC.50.rast)[[1]]
tmax.rcp85.AC.50<-extract(tmax.rcp85.AC.50.rast, sitecoords)
tmax.rcp85.AC.50<-as.data.frame(tmax.rcp85.AC.50[,1:3])
tmax.rcp85.AC.50$Location<-sites$Name
tmax.rcp85.AC.50$Year<-2050
tmax.rcp85.AC.50$TMax.q1<-apply(tmax.rcp85.AC.50[,1:3],1,mean,na.rm=TRUE)

tmax.rcp85.AC.70.rast<-getData(name = 'CMIP5', var = 'tmax', res=2.5, lon=i, lat=j, rcp=85, model='AC', year=70)
tmax.rcp85.AC.70<-extract(tmax.rcp85.AC.70.rast, sitecoords)
tmax.rcp85.AC.70<-as.data.frame(tmax.rcp85.AC.70[,1:3])
tmax.rcp85.AC.70$Location<-sites$Name
tmax.rcp85.AC.70$Year<-2070
tmax.rcp85.AC.70$TMax.q1<-apply(tmax.rcp85.AC.70[,1:3],1,mean,na.rm=TRUE)


sitecoords<-data.frame(coordinates(sites)[,1:2])
sitextremes<-matrix(c(sitecoords[1,1], sitecoords[17,1],sitecoords[1,2], sitecoords[17,2]), nrow=2, ncol=2)
sitextremes

###MAT/BIO1, BIO12###
models<-c("BC","CC", "CN", "GS", "HD", "IP", "MI", "MR", "MC","MP", "MG", "NO")
cmipyear=c(50,70)
rcp<-c(26, 85)
bio1.future<-NULL
for(i in cmipyear){
  for(j in rcp){
    for(k in models){
      record<-getData(name = 'CMIP5', var = 'BIO', res=2.5, rcp=j, model=k, year=i)
      bio1.future<-c(bio1.future, record)
    }
  }
}
bio1.26.50<-NULL
int<-NULL
int.2<-NULL
for(i in 1:12){
  int<-extract(bio1.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1])
  bio1.26.50<-c(bio1.26.50, int.2)
}
bio1.26.50<-as.data.frame(bio1.26.50)
bio1.26.50.mean<-data.frame(rowMeans(bio1.26.50)/10)
bio1.26.50.mean$sd<-apply(bio1.26.50, 1, sd)/10

colnames(bio1.26.50.mean)<-c("MAT.26.50", "MATsd.26.50")
bio1.26.50.mean$Name<-sites$Name
#bio1.26.50.mean$Year<-2050
#bio1.26.50.mean$rcp<-2.6

bio1.85.50<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 13:24){
  int<-extract(bio1.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1])
  bio1.85.50<-c(bio1.85.50, int.2)
}

bio1.85.50<-as.data.frame(bio1.85.50)
bio1.85.50.mean<-data.frame(rowMeans(bio1.85.50)/10)
bio1.85.50.mean$sd<-apply(bio1.85.50, 1, sd)/10
colnames(bio1.85.50.mean)<-c("MAT.85.50", "MATsd.85.50")
bio1.85.50.mean$Name<-sites$Name
#bio1.85.50.mean$Year<-2050
#bio1.85.50.mean$rcp<-8.5

bio1.26.70<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 25:36){
  int<-extract(bio1.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1])
  bio1.26.70<-c(bio1.26.70, int.2)
}

bio1.26.70<-as.data.frame(bio1.26.70)
bio1.26.70.mean<-data.frame(rowMeans(bio1.26.70)/10)
bio1.26.70.mean$sd<-apply(bio1.26.70, 1, sd)/10
colnames(bio1.26.70.mean)<-c("MAT.26.70", "MATsd.26.70")
bio1.26.70.mean$Name<-sites$Name
#bio1.26.70.mean$Year<-2070
#bio1.26.70.mean$rcp<-2.6

bio1.85.70<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 37:48){
  int<-extract(bio1.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1])
  bio1.85.70<-c(bio1.85.70, int.2)
}

bio1.85.70<-as.data.frame(bio1.85.70)
bio1.85.70.mean<-data.frame(rowMeans(bio1.85.70)/10)
bio1.85.70.mean$sd<-apply(bio1.85.70, 1, sd)/10
colnames(bio1.85.70.mean)<-c("MAT.85.70", "MATsd.85.70")
bio1.85.70.mean$Name<-sites$Name
#bio1.85.70.mean$Year<-2070
#bio1.85.70.mean$rcp<-8.5

MAT.worldclim<-merge(bio1.85.50.mean, bio1.26.50.mean, all=T, by=c("Name"))
MAT.worldclim<-merge(MAT.worldclim, bio1.26.70.mean, all=T, by=c("Name"))
MAT.worldclim<-merge(MAT.worldclim, bio1.85.70.mean, all=T, by=c("Name"))
MAT.worldclim

###MAP/BIO12###
models<-c("BC","CC", "CN", "GS", "HD", "IP", "MI", "MR", "MC","MP", "MG", "NO")
cmipyear=c(50,70)
rcp<-c(26, 85)
bio12.future<-NULL
for(i in cmipyear){
  for(j in rcp){
    for(k in models){
      record<-getData(name = 'CMIP5', var = 'BIO', res=2.5, rcp=j, model=k, year=i)
      bio12.future<-c(bio12.future, record)
    }
  }
}
bio12.26.50<-NULL
int<-NULL
int.2<-NULL
for(i in 1:12){
  int<-extract(bio12.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,12])
  bio12.26.50<-c(bio12.26.50, int.2)
}
bio12.26.50<-as.data.frame(bio12.26.50)
bio12.26.50.mean<-data.frame(rowMeans(bio12.26.50))
bio12.26.50.mean$sd<-apply(bio12.26.50, 1, sd)

colnames(bio12.26.50.mean)<-c("MAP.26.50", "MAPsd.26.50")
bio12.26.50.mean$Name<-sites$Name
#bio12.26.50.mean$Year<-2050
#bio12.26.50.mean$rcp<-2.6

bio12.85.50<-NULL
int<-NULL
int.2<-NULL
for(i in 13:24){
  int<-extract(bio12.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,12])
  bio12.85.50<-c(bio12.85.50, int.2)
}

bio12.85.50<-as.data.frame(bio12.85.50)
bio12.85.50.mean<-data.frame(rowMeans(bio12.85.50))
bio12.85.50.mean$sd<-apply(bio12.85.50, 1, sd)
colnames(bio12.85.50.mean)<-c("MAP.85.50", "MAPsd.85.50")
bio12.85.50.mean$Name<-sites$Name
#bio12.85.50.mean$Year<-2050
#bio12.85.50.mean$rcp<-8.5

bio12.26.70<-NULL
int<-NULL
int.2<-NULL
for(i in 25:36){
  int<-extract(bio12.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,12])
  bio12.26.70<-c(bio12.26.70, int.2)
}

bio12.26.70<-as.data.frame(bio12.26.70)
bio12.26.70.mean<-data.frame(rowMeans(bio12.26.70))
bio12.26.70.mean$sd<-apply(bio12.26.70, 1, sd)
colnames(bio12.26.70.mean)<-c("MAP.26.70", "MAPsd.26.70")
bio12.26.70.mean$Name<-sites$Name
#bio12.26.70.mean$Year<-2070
#bio12.26.70.mean$rcp<-2.6

bio12.85.70<-NULL
int<-NULL
int.2<-NULL
for(i in 37:48){
  int<-extract(bio12.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,12])
  bio12.85.70<-c(bio12.85.70, int.2)
}

bio12.85.70<-as.data.frame(bio12.85.70)
bio12.85.70.mean<-data.frame(rowMeans(bio12.85.70))
bio12.85.70.mean$sd<-apply(bio12.85.70, 1, sd)
colnames(bio12.85.70.mean)<-c("MAP.85.70", "MAPsd.85.70")
bio12.85.70.mean$Name<-sites$Name
#bio12.85.70.mean$Year<-2070
#bio12.85.70.mean$rcp<-8.5

MAP.worldclim<-merge(bio12.85.50.mean, bio12.26.50.mean, all=T, by=c("Name"))
MAP.worldclim<-merge(MAP.worldclim, bio12.26.70.mean, all=T, by=c("Name"))
MAP.worldclim<-merge(MAP.worldclim, bio12.85.70.mean, all=T, by=c("Name"))
MAP.worldclim

###TMax###
models<-c("BC","CC", "CN", "GS", "HD", "IP", "MI", "MR", "MC","MP", "MG", "NO")
cmipyear=c(50,70)
rcp<-c(26, 85)
tmax.future<-NULL
for(i in cmipyear){
  for(j in rcp){
  for(k in models){
    record<-getData(name = 'CMIP5', var = 'tmax', res=2.5, rcp=j, model=k, year=i)
  tmax.future<-c(tmax.future, record)
  }
  }
}
tmax.26.50<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 1:12){
  int<-extract(tmax.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$TMax.q1<-apply(int[,1:3],1,mean,na.rm=TRUE)
  tmax.26.50<-c(tmax.26.50, int.3)
}
tmax.26.50<-as.data.frame(tmax.26.50)
tmax.26.50.mean<-data.frame(rowMeans(tmax.26.50)/10)
tmax.26.50.mean$sd<-apply(tmax.26.50, 1, sd)/10

colnames(tmax.26.50.mean)<-c("tmax.26.50", "tmaxsd.26.50")
tmax.26.50.mean$Name<-sites$Name
#tmax.26.50.mean$Year<-2050
#tmax.26.50.mean$rcp<-2.6

tmax.85.50<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 13:24){
  int<-extract(tmax.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$TMax.q1<-apply(int[,1:3],1,mean,na.rm=TRUE)
  tmax.85.50<-c(tmax.85.50, int.3)
}

tmax.85.50<-as.data.frame(tmax.85.50)
tmax.85.50.mean<-data.frame(rowMeans(tmax.85.50)/10)
tmax.85.50.mean$sd<-apply(tmax.85.50, 1, sd)/10
colnames(tmax.85.50.mean)<-c("tmax.85.50", "tmaxsd.85.50")
tmax.85.50.mean$Name<-sites$Name
#tmax.85.50.mean$Year<-2050
#tmax.85.50.mean$rcp<-8.5

tmax.26.70<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 25:36){
  int<-extract(tmax.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$TMax.q1<-apply(int[,1:3],1,mean,na.rm=TRUE)
  tmax.26.70<-c(tmax.26.70, int.3)
}

tmax.26.70<-as.data.frame(tmax.26.70)
tmax.26.70.mean<-data.frame(rowMeans(tmax.26.70)/10)
tmax.26.70.mean$sd<-apply(tmax.26.70, 1, sd)/10
colnames(tmax.26.70.mean)<-c("tmax.26.70", "tmaxsd.26.70")
tmax.26.70.mean$Name<-sites$Name
#tmax.26.70.mean$Year<-2070
#tmax.26.70.mean$rcp<-2.6

tmax.85.70<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 37:48){
  int<-extract(tmax.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$TMax.q1<-apply(int[,1:3],1,mean,na.rm=TRUE)
  tmax.85.70<-c(tmax.85.70, int.3)
}

tmax.85.70<-as.data.frame(tmax.85.70)
tmax.85.70.mean<-data.frame(rowMeans(tmax.85.70)/10)
tmax.85.70.mean$sd<-apply(tmax.85.70, 1, sd)/10
colnames(tmax.85.70.mean)<-c("tmax.85.70", "tmaxsd.85.70")
tmax.85.70.mean$Name<-sites$Name
#tmax.85.70.mean$Year<-2070
#tmax.85.70.mean$rcp<-8.5

tmax.worldclim<-merge(tmax.85.50.mean, tmax.26.50.mean, all=T, by=c("Name"))
tmax.worldclim<-merge(tmax.worldclim, tmax.26.70.mean, all=T, by=c("Name"))
tmax.worldclim<-merge(tmax.worldclim, tmax.85.70.mean, all=T, by=c("Name"))
tmax.worldclim

####TMIN####
models<-c("BC","CC", "CN", "GS", "HD", "IP", "MI", "MR", "MC","MP", "MG", "NO")
cmipyear=c(50,70)
rcp<-c(26, 85)
tmin.future<-NULL
for(i in cmipyear){
  for(j in rcp){
    for(k in models){
      record<-getData(name = 'CMIP5', var = 'tmin', res=2.5, rcp=j, model=k, year=i)
      tmin.future<-c(tmin.future, record)
    }
  }
}

tmin.26.50<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 1:12){
  int<-extract(tmin.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$Tmin.q1<-apply(int[,1:3],1,mean,na.rm=TRUE)
  tmin.26.50<-c(tmin.26.50, int.3)
}
tmin.26.50<-as.data.frame(tmin.26.50)
tmin.26.50.mean<-data.frame(rowMeans(tmin.26.50)/10)
tmin.26.50.mean$sd<-apply(tmin.26.50, 1, sd)/10
colnames(tmin.26.50.mean)<-c("tmin.26.50", "tminsd.26.50")
tmin.26.50.mean$Name<-sites$Name
#tmin.26.50.mean$Year<-2050
#tmin.26.50.mean$rcp<-2.6

tmin.85.50<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 13:24){
  int<-extract(tmin.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$Tmin.q1<-apply(int[,1:3],1,mean,na.rm=TRUE)
  tmin.85.50<-c(tmin.85.50, int.3)
}

tmin.85.50<-as.data.frame(tmin.85.50)
tmin.85.50.mean<-data.frame(rowMeans(tmin.85.50)/10)
tmin.85.50.mean$sd<-apply(tmin.85.50, 1, sd)/10
colnames(tmin.85.50.mean)<-c("tmin.85.50", "tminsd.85.50")
tmin.85.50.mean$Name<-sites$Name
#tmin.85.50.mean$Year<-2050
#tmin.85.50.mean$rcp<-8.5

tmin.26.70<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 25:36){
  int<-extract(tmin.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$Tmin.q1<-apply(int[,1:3],1,mean,na.rm=TRUE)
  tmin.26.70<-c(tmin.26.70, int.3)
}

tmin.26.70<-as.data.frame(tmin.26.70)
tmin.26.70.mean<-data.frame(rowMeans(tmin.26.70)/10)
tmin.26.70.mean$sd<-apply(tmin.26.70, 1, sd)/10
colnames(tmin.26.70.mean)<-c("tmin.26.70", "tminsd.26.70")
tmin.26.70.mean$Name<-sites$Name
#tmin.26.70.mean$Year<-2070
#tmin.26.70.mean$rcp<-2.6

tmin.85.70<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 37:48){
  int<-extract(tmin.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$Tmin.q1<-apply(int[,1:3],1,mean,na.rm=TRUE)
  tmin.85.70<-c(tmin.85.70, int.3)
}

tmin.85.70<-as.data.frame(tmin.85.70)
tmin.85.70.mean<-data.frame(rowMeans(tmin.85.70)/10)
tmin.85.70.mean$sd<-apply(tmin.85.70, 1, sd)/10
colnames(tmin.85.70.mean)<-c("tmin.85.70", "tminsd.85.70")
tmin.85.70.mean$Name<-sites$Name
#tmin.85.70.mean$Year<-2070
#tmin.85.70.mean$rcp<-8.5


tmin.worldclim<-merge(tmin.85.50.mean, tmin.26.50.mean, all=T, by=c("Name"))
tmin.worldclim<-merge(tmin.worldclim, tmin.26.70.mean, all=T, by=c("Name"))
tmin.worldclim<-merge(tmin.worldclim, tmin.85.70.mean, all=T, by=c("Name"))
tmin.worldclim

####Prec####
models<-c("BC","CC", "CN", "GS", "HD", "IP", "MI", "MR", "MC","MP", "MG", "NO")
cmipyear=c(50,70)
rcp<-c(26, 85)
prec.future<-NULL
for(i in cmipyear){
  for(j in rcp){
    for(k in models){
      record<-getData(name = 'CMIP5', var = 'prec', res=2.5, rcp=j, model=k, year=i)
      prec.future<-c(prec.future, record)
    }
  }
}

prec.26.50<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 1:12){
  int<-extract(prec.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$prec.q1<-apply(int[,1:3],1,sum,na.rm=TRUE)
  prec.26.50<-c(prec.26.50, int.3)
}
prec.26.50<-as.data.frame(prec.26.50)
prec.26.50.mean<-data.frame(apply(prec.26.50,1,mean))
prec.26.50.mean$sd<-apply(prec.26.50, 1, sd)
colnames(prec.26.50.mean)<-c("prec.26.50", "precsd.26.50")
prec.26.50.mean$Name<-sites$Name
#prec.26.50.mean$Year<-2050
#prec.26.50.mean$rcp<-2.6

prec.85.50<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 13:24){
  int<-extract(prec.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$prec.q1<-apply(int[,1:3],1,sum,na.rm=TRUE)
  prec.85.50<-c(prec.85.50, int.3)
}

prec.85.50<-as.data.frame(prec.85.50)
prec.85.50.mean<-data.frame(apply(prec.85.50,1,mean))
prec.85.50.mean$sd<-apply(prec.85.50, 1, sd)
colnames(prec.85.50.mean)<-c("prec.85.50", "precsd.85.50")
prec.85.50.mean$Name<-sites$Name
#prec.85.50.mean$Year<-2050
#prec.85.50.mean$rcp<-8.5

prec.26.70<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 25:36){
  int<-extract(prec.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$prec.q1<-apply(int[,1:3],1,sum,na.rm=TRUE)
  prec.26.70<-c(prec.26.70, int.3)
}

prec.26.70<-as.data.frame(prec.26.70)
prec.26.70.mean<-data.frame(apply(prec.26.70,1,mean))
prec.26.70.mean$sd<-apply(prec.26.70, 1, sd)
colnames(prec.26.70.mean)<-c("prec.26.70", "precsd.26.70")
prec.26.70.mean$Name<-sites$Name
#prec.26.70.mean$Year<-2070
#prec.26.70.mean$rcp<-2.6

prec.85.70<-NULL
int<-NULL
int.2<-NULL
int.3<-NULL
for(i in 37:48){
  int<-extract(prec.future[[i]], sitecoords)
  int.2<-as.data.frame(int[,1:3])
  int.3$prec.q1<-apply(int[,1:3],1,sum,na.rm=TRUE)
  prec.85.70<-c(prec.85.70, int.3)
}

prec.85.70<-as.data.frame(prec.85.70)
prec.85.70.mean<-data.frame(apply(prec.85.70,1,mean))
prec.85.70.mean$sd<-apply(prec.85.70, 1, sd)
colnames(prec.85.70.mean)<-c("prec.85.70", "precsd.85.70")
prec.85.70.mean$Name<-sites$Name
#prec.85.70.mean$Year<-2070
#prec.85.70.mean$rcp<-8.5


prec.worldclim<-merge(prec.85.50.mean, prec.26.50.mean, all=T, by=c("Name"))
prec.worldclim<-merge(prec.worldclim, prec.26.70.mean, all=T, by=c("Name"))
prec.worldclim<-merge(prec.worldclim, prec.85.70.mean, all=T, by=c("Name"))
prec.worldclim



####Currentq1Prec####
record<-getData(name = 'worldclim', var = 'prec', res=2.5)
record[[1]]
prec.modern<-NULL
int<-NULL
int.2<-NULL
  int<-extract(record, sitecoords)[,1:3]
  int.2<-as.data.frame(int)

prec.modern<-int.2
prec.modern.mean<-data.frame(apply(prec.modern, 1,sum))
colnames(prec.modern.mean)<-c("Rain.q1")
prec.modern.mean$Name<-sites$Name
#prec.modern.mean$Year<-2070
#prec.modern.mean$rcp<-8.5


prec.worldclim<-merge(prec.85.50.mean, prec.26.50.mean, all=T, by=c("Name"))
prec.worldclim<-merge(prec.worldclim, prec.26.70.mean, all=T, by=c("Name"))
prec.worldclim<-merge(prec.worldclim, prec.85.70.mean, all=T, by=c("Name"))
prec.worldclim<-merge(prec.worldclim, prec.modern.mean, by="Name", all=T)
data.frame(sites)

#Merge to one df

futurewheatclimate<-merge(tmax.worldclim, tmin.worldclim, by=c("Name"), all=T)
futurewheatclimate<-merge(futurewheatclimate, MAP.worldclim, by="Name", all=T)
futurewheatclimate<-merge(futurewheatclimate, MAT.worldclim, by="Name", all=T)
futurewheatclimate<-merge(futurewheatclimate, prec.worldclim, by=c("Name"), all=T)
futurewheatclimate
#write.csv(futurewheatclimate, "/Users/maxwell1/Documents/Github/Wheat/futurewheatclimate.csv")
futurewheatclimate<-read.csv("/Users/maxwell1/Documents/Github/Wheat/futurewheatclimate.csv")
library(tmap)
colnames(sites.df)
prec.worldclim<-merge(sites.df[-3:-4], futurewheatclimate, by.x=c("Location"), by.y="Name")
prec.tmap<-ddply(prec.worldclim, .(County), summarise,
                 prec.85.70=mean(prec.85.70/Rain.q1),
                 prec.26.70=mean(prec.26.70/Rain.q1),
                 prec.85.50=mean(prec.85.50/Rain.q1),
                 prec.26.50=mean(prec.26.50/Rain.q1))
CA.PPT.ts<-merge(wheatcounties, prec.tmap, by.x="NAME", by.y="County")
ppt.inc.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.PPT.ts)+tm_polygons(col = "prec.85.70",title = "PPT Change\n% of Current\nRCP 8.5, 2070", palette = "Blues", textNA ="No Data", breaks = c(0.85, 0.90, 0.95, 1.00, 1.05, 1.10)) 

ppt.inc.map.26.70<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.PPT.ts)+tm_polygons(col = "prec.26.70",title = "PPT Change\n% of Current\nRCP 2.6, 2070", palette = "Blues", textNA ="No Data", breaks = c(0.85, 0.90, 0.95, 1.00, 1.05, 1.10)) 

ppt.inc.map.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.PPT.ts)+tm_polygons(col = "prec.26.50",title = "PPT Change\n% of Current\nRCP 2.6, 2050", palette = "Blues", textNA ="No Data", breaks = c(0.85, 0.90, 0.95, 1.00, 1.05, 1.10)) 

ppt.inc.map.85.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.PPT.ts)+tm_polygons(col = "prec.85.50",title = "PPT Change\n% of Current\nRCP 8.5, 2050", palette = "Blues", textNA ="No Data", breaks = c(0.85, 0.90, 0.95, 1.00, 1.05, 1.10)) 

tmap_arrange(ppt.inc.map.26.50, ppt.inc.map.26.70, ppt.inc.map.85.50,ppt.inc.map)


#current temp####
record<-getData(name = 'worldclim', var = 'tmax', res=2.5)
record[[1]]
tmax.modern<-NULL
int<-NULL
int.2<-NULL
for(i in 1:3){
  int<-extract(record[[i]], sitecoords)
  int.2<-as.data.frame(int)
  tmax.modern<-c(tmax.modern, int.2)
}

tmax.modern<-as.data.frame(tmax.modern)
tmax.modern.mean<-data.frame(rowMeans(tmax.modern)/10)
colnames(tmax.modern.mean)<-c("tmax.q1")
tmax.modern.mean$Name<-sites$Name

tmax.worldclim<-merge(futurewheatclimate, tmax.modern.mean, by="Name", all=T)
data.frame(sites.df)
library(tmap)
tmax.worldclim<-merge(sites.df, tmax.worldclim, by.x="Location", by.y="Name")
tmax.tmap<-ddply(tmax.worldclim, .(County), summarise,
                 tmax.85.70=mean(tmax.85.70/tmax.q1),
                 tmax.26.70=mean(tmax.26.70/tmax.q1),
                 tmax.85.50=mean(tmax.85.50/tmax.q1),
                 tmax.26.50=mean(tmax.26.50/tmax.q1))
CA.tmax.shape<-merge(wheatcounties, tmax.tmap, by.x="NAME", by.y="County")
tmax.inc.map<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.tmax.shape)+tm_polygons(col = "tmax.85.70",title = "Tmax Change (C)\nRCP 8.5, 2070", palette = "Reds", textNA ="No Data",legend.hist.z=1,legend.hist=T) 

tmax.inc.map.26.70<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.tmax.shape)+tm_polygons(col = "tmax.26.70",title = "Tmax Change (C)\nRCP 2.6, 2070", palette = "Reds", textNA ="No Data",legend.hist.z=1,legend.hist=T) 

tmax.inc.map.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.tmax.shape)+tm_polygons(col = "tmax.26.50",title = "Tmax Change (C)\nRCP 2.6, 2050", palette = "Reds", textNA ="No Data",legend.hist=T, legend.hist.z=2) 

tmax.inc.map.85.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(CA.tmax.shape)+tm_polygons(col = "tmax.85.50",title = "Tmax Change (C)\nRCP 8.5, 2050", palette = "Reds", textNA ="No Data") 

tmap_arrange(tmax.inc.map.26.50, tmax.inc.map.26.70, tmax.inc.map.85.50,tmax.inc.map)
             
# load libraries
library(rgeos)
library(UScensus2000tract)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# plot Census Tract map
plot(CA.tmax.shape)

# create a unique character ID value:
CA.tmax.shape$id=as.character(1:nrow(CA.tmax.shape))

# Fortify on that ID and join attribute data:
ofort <- fortify(CA.tmax.shape, region="id")
ofort <- left_join(ofort, CA.tmax.shape@data, c("id"="id"))

# Shear/scale matrix [[2,1],[0,1]] obtained by some trial and error:
sm <- matrix(c(1,0.2,1.5,3),2,2)

# Get transformed coordinates:
xy <- as.matrix(ofort[,c("long","lat")]) %*% sm

# Add xy as extra columns in fortified data:
ofort$x <- xy[,1]; ofort$y = xy[,2]

# Plot !
ggplot(ofort, aes(x=x, y=y, group=id, fill=tmax.85.70)) + geom_polygon(color="White") + coord_fixed()+theme_classic()+
  geom_polygon( color="White", aes(x=x+8, y=y, group=id, fill=tmax.85.50))+          
  geom_polygon( color="White", aes(x=x+8, y=y, group=id, fill=tmax.85.50))+
  geom_polygon( color="White", aes(x=x+8, y=y, group=id, fill=tmax.85.50))


# plot Census Tract map
plot(shape.26.50)

# create a unique character ID value:
shape.26.50$id=as.character(1:nrow(shape.26.50))

# Fortify on that ID and join attribute data:
ofort <- fortify(shape.26.50, region="id")
ofort <- left_join(ofort, shape.26.50@data, c("id"="id"))

# Shear/scale matrix [[2,1],[0,1]] obtained by some trial and error:
sm <- matrix(c(1,0.2,1.5,3),2,2)

# Get transformed coordinates:
xy <- as.matrix(ofort[,c("long","lat")]) %*% sm

# Add xy as extra columns in fortified data:
ofort$x <- xy[,1]; ofort$y = xy[,2]

# Plot !

ggplot(ofort, aes(x=x, y=y, group=id, fill=l.N, colors = myPalette)) + geom_polygon(color="White") + coord_fixed()+theme_classic()+
  geom_polygon( color="White", aes(x=x+8, y=y, group=id, fill=l.D))+          
  geom_polygon( color="White", aes(x=x+16, y=y, group=id, fill=l.F))+
  + scale_fill_gradient(low='white', high='grey20')
