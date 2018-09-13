#Time Series
library(TTR)
NASS<-read.csv("/Users/tobymaxwell/Google Drive/documents/Grad/Research/NSF WSC/NASS/NASS CA Averages.csv")

NASS

tail(commoncimis)
library(plyr)
timeseries<-NULL
timeseries$Yield<-commoncimis$Yield
timeseries$Year<-commoncimis$Year
timeseries$Year<-as.factor(timeseries$Year)
timeseries.2<-ddply(commonall, .(Location, Year), summarise, 
                    Yield = mean(Yield),
                    WUE=mean(WUE))
timeseries.2
par(mar=c(4,4,4,4))
plot(WUE~Year, timeseries.2)
timeseries.3<-ddply(na.omit(timeseries.2[is.finite(timeseries.2$WUE),]), .(Year), summarise, Yield = mean(Yield), WUE=mean(WUE))
timeseries.3
plot(WUE~Year, timeseries.3)
ts.1<-ts(timeseries.3$Yield, start = 1981)
ts.2<-ts(timeseries.3$WUE, start = 1981)
plot.ts(SMA(ts.2, 3))
par(mfrow = c(1,2))
ts.plot(ts.1)
ts.plot(ts.2)
#NASS GGplots
library(reshape2)
df <- melt(NASS, id.vars = 'Year', variable.name = 'series')
df<-na.omit(df)
class(df$value)

cropslopes<-read.csv("/Users/tobymaxwell/Google Drive/documents/Grad/Research/NSF WSC/NASS/cropslopes.csv")

df<-merge(df,cropslopes, by.x="series", by.y="crop")

df.plot<-df[df$series=="Potatoes"|df$series=="Wheat"|df$series=="Tomato"|df$series=="Almond"|df$series=="Corn"|df$series=="Alfalfa"|df$series=="Lettuce"|df$series=="Beans"|df$series=="Cotton",]
library(ggplot2)
ggplot(df.plot, aes(value, x=Year))+geom_point()+facet_wrap(~series, scales = "free", nrow=2)+geom_vline(xintercept = c(1941, 1951, 1973), color = "LightBlue")+ylab("Yield (Tons/Acre)")+geom_abline(aes(slope=Slope, intercept = Intercept), color = "red")+geom_abline(aes(slope=slope1, intercept = intercept1), color = "forest green")+geom_abline(aes(slope=slope2, intercept = intercept2), color = "orange")+geom_vline(data = df.plot, aes(xintercept=B1), linetype = "dashed", color = "purple")+geom_vline(data = df.plot, aes(xintercept=B2), linetype = "dashed", color = "purple")+theme_classic()


par(mfrow=c(3,3), mar = c(2,4,1,4))
Potatoes<-ts(na.omit(NASS$Potatoes), start = 1867)
Potatoes.ts<-SMA(Potatoes, 5)
plot.ts(Potatoes.ts)
abline(v=c(1941,1951,1973), col = "Blue", lty=c(1:3))
abline(,col="Black")
Tomato<-ts(na.omit(NASS$Tomato), start = 1920)
Tomato.ts<-SMA(Tomato, 5)
plot.ts(Tomato.ts)
abline(v=c(1941,1951,1973), col = "Blue", lty=c(1:3))
Almond<-ts(na.omit(NASS$Almond), start = 1919)
Almond.ts<-SMA(Almond, 5)
plot.ts(Almond.ts)
abline(v=c(1941,1951,1973), col = "Blue", lty=c(1:3))
Wheat<-ts(na.omit(NASS$Wheat), start = 1866)
Wheat.ts<-SMA(Wheat, 3)
plot.ts(Wheat.ts)
abline(v=c(1941,1951,1973), col = "Blue", lty=c(1:3))
Corn<-ts(na.omit(NASS$Corn), start = 1866)
Corn.ts<-SMA(Corn, 3)
plot.ts(Corn.ts)
abline(v=c(1941,1951,1973), col = "Blue", lty=c(1:3))
Alfalfa<-ts(na.omit(NASS$Hay.Alf), start = 1919)
Alfalfa.ts<-SMA(Alfalfa, 5)
plot.ts(Alfalfa.ts)
abline(v=c(1941,1951,1973), col = "Blue", lty=c(1:3))
Lettuce<-ts(na.omit(NASS$Lettuce), start = 1920)
Lettuce.ts<-SMA(Lettuce, 5)
plot.ts(Lettuce.ts)
abline(v=c(1941,1951,1973), col = "Blue", lty=c(1:3))
Beans<-ts(na.omit(NASS$Beans), start = 1909)
Beans.ts<-SMA(Beans, 5)
plot.ts(Beans.ts)
abline(v=c(1941,1951,1973), col = "Blue", lty=c(1:3))
Cotton<-ts(NASS$Cotton, start = 1911)
Cotton.ts<-SMA(Cotton, 1)
plot.ts(Cotton)
abline(v=c(1941,1951,1973), col = "Blue", lty=c(1:3))
legend(x= "topleft", col = "lightblue", lty=1, legend=c("1941: Colorado Aqueduct","1951: Shasta Dam to SJV", "1973: SWP to SoCal"))
legend(x= "topleft", col = c("Red", "Orange", "Forest Green"), lty=1, legend=c("Recent Decades", "20th Century Boom", "Pre-Water Project"))
legend(x= "topleft", col = c("purple"), lty=2, legend=c("Breaks"))

par(mfrow=c(1,1))

require(bfast)
require(strucchange)
require(sandwich)
require(forecast)
require(raster)
require(sp)

crops<-c("Potatoes", "Wheat", "Tomato", "Almond", "Corn", "Alfalfa", "Lettuce", "Beans")
crops
ts.slopes<-function(ts){
breaks<-bfast(ts, season = "none", max.iter=2,breaks=2)
niter <- length(breaks$output)
slopes<-coef(breaks$output[[niter]]$bp.Vt)[,2]
intercepts<-coef(breaks$output[[niter]]$bp.Vt)[,1]
ci<-(breaks$output[[niter]]$ci.Vt)
nm <-deparse(substitute(ts))
return(c(slopes, intercepts, plot(breaks, type="trend", main = paste(nm))))
}
ts.slopes(Corn)
slopes<-NULL
result<-NULL
for (i in seq_along(noquote(crops))) {
d<- get(paste0(crops[i], ".ts")) # copy the object to an object named `d`
slopes<-ts.slopes(na.omit(d))
print(slopes)
result<-rbind(result,slopes)
row.names(result)<-crops[1:i]
colnames(result)<-c("S1", "S2", "S3", "I1", "I2","I3")
}
result
ts.slopes(na.omit(Corn.ts))

YieldSites.Yield
breaks<-bfast(na.omit(Wheat.ts), season = "none", max.iter=2,breaks=2)
niter <- length(breaks$output)
slopes<-coef(breaks$output[[niter]]$bp.Vt)[,2]
intercepts<-coef(breaks$output[[niter]]$bp.Vt)[,1]
ci<-(breaks$output[[niter]]$ci.Vt)
ci
slopes
intercepts
str(breaks)

#inflection point for yield/water
library(inflection)
yieldwater<-NULL
yieldwater$Yield<-commonavg$Yield
yieldwater$Water.Total<-commonavg$Water.Total
yieldwater$Year<-commonavg$Year
yieldwater$Location<-commonavg$Location
yieldwater<-as.data.frame(yieldwater)
yieldwater[is.na(yieldwater$Location),]
yieldwater<-na.omit(yieldwater)
yieldwater.avg<-ddply(yieldwater, .(Location, Year), summarise,
                      Yield=mean(Yield),
                      Water.Total = mean(Water.Total))

####nls logistic growth function####
library(car)

ccap<-function(phi1,phi2,phi3,x){
  (phi1/(1+exp(-(phi2+phi3*x))))#phi1 is carrying cap, phi2 is not interseting, phi3 is growth parameter, i.e. how fast reaches asymptote, x is variable which changes (dependent x var, often time)
}
ccap(6000,-3.47,.25,30)
coef(lm(logit(Yield)~Water.Total,data=yieldwater))

cc

water<-NULL
cc<-NULL

water1982<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
    start=list(phi1=6000,phi2=-5,phi3=0.25),
    data=yieldwater[yieldwater$Year==1982,], trace=TRUE,
    control = list(maxiter = 10000000, minFactor=.00001))
water1984<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
    start=list(phi1=4000,phi2=-5,phi3=0.6),
    data=yieldwater[yieldwater$Year==1984,], trace=TRUE,
    control = list(maxiter = 10000000, minFactor=.00001))
water1985<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=4000,phi2=-5,phi3=0.6),
               data=yieldwater[yieldwater$Year==1985,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
water1986<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=5000,phi2=-5,phi3=0.4),
               data=yieldwater[yieldwater$Year==1986,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1987<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #              start=list(phi1=7000,phi2=-8,phi3=0.8),
  #             data=yieldwater[yieldwater$Year==1987,], trace=TRUE,
   #            control = list(maxiter = 10000000, minFactor=.00001))
water1988<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000000,phi2=-5,phi3=0.4),
               data=yieldwater[yieldwater$Year==1988,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
water1989<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=4000,phi2=-5,phi3=0.6),
               data=yieldwater[yieldwater$Year==1989,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1990<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #             start=list(phi1=7000,phi2=-2,phi3=0.1),
  #            data=yieldwater[yieldwater$Year==1990,], trace=TRUE,
   #           control = list(maxiter = 10000000, minFactor=.00000001))
water1991<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=8000,phi2=-5,phi3=0.25),
               data=yieldwater[yieldwater$Year==1991,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1992<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #             start=list(phi1=6000,phi2=-5,phi3=0.15),
  #            data=yieldwater[yieldwater$Year==1992,], trace=TRUE,
   #           control = list(maxiter = 1000, minFactor=.0000001))
#water1993<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #              start=list(phi1=4400,phi2=-1,phi3=.1),
  #             data=yieldwater[yieldwater$Year==1993,], trace=TRUE,
   #            control = list(maxiter = 10000000, minFactor=.00001))
#water1994<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #              start=list(phi1=5000,phi2=-6,phi3=.15),
  #             data=yieldwater[yieldwater$Year==1994,], trace=TRUE,
   #            control = list(maxiter = 10000000, minFactor=.00000000000001))
water1995<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=5000,phi2=-5,phi3=0.4),
               data=yieldwater[yieldwater$Year==1995,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1997<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
              # start=list(phi1=6000,phi2=-5,phi3=0.1),
               #data=yieldwater[yieldwater$Year==1997,], trace=TRUE,
               #control = list(maxiter = 10000000, minFactor=.00001))
#water1998<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               #start=list(phi1=6000,phi2=-5,phi3=0.1),
               #data=yieldwater[yieldwater$Year==1998,], trace=TRUE,
               #control = list(maxiter = 10000000, minFactor=.00000000001))
water1999<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.8),
               data=yieldwater[yieldwater$Year==1999,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2000<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=7000,phi2=-5,phi3=0.8),
               data=yieldwater[yieldwater$Year==2000,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
#water2001<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
             #  start=list(phi1=6000,phi2=-5,phi3=0.8),
             #  data=yieldwater[yieldwater$Year==2001,], trace=TRUE,
             #  control = list(maxiter = 10000000, minFactor=.00000000001))
water2002<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.8),
               data=yieldwater[yieldwater$Year==2002,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2003<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.8),
               data=yieldwater[yieldwater$Year==2003,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2004<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=5500,phi2=-5,phi3=0.4),
               data=yieldwater[yieldwater$Year==2004,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2005<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=5000,phi2=-5,phi3=0.5),
               data=yieldwater[yieldwater$Year==2005,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
#water2006<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=4000,phi2=-5,phi3=0.99),
               data=yieldwater[yieldwater$Year==2006,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2007<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.8),
               data=yieldwater[yieldwater$Year==2007,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2008<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.4),
               data=yieldwater[yieldwater$Year==2008,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2009<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=5000,phi2=-5,phi3=0.5),
               data=yieldwater[yieldwater$Year==2009,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2010<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.5),
               data=yieldwater[yieldwater$Year==2010,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
#water2011<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
              # start=list(phi1=6000,phi2=-5,phi3=0.5),
              # data=yieldwater[yieldwater$Year==2011,], trace=TRUE,
              # control = list(maxiter = 10000000, minFactor=.00000000001))
#water2012<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
            #   start=list(phi1=5000,phi2=-5,phi3=0.5),
            #   data=yieldwater[yieldwater$Year==2012,], trace=TRUE,
            #  control = list(maxiter = 10000000, minFactor=.00000000001))
water2013<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.5),
               data=yieldwater[yieldwater$Year==2013,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2014<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.5),
               data=yieldwater[yieldwater$Year==2014,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2015<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.5),
               data=yieldwater[yieldwater$Year==2015,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))



cc<-data.frame(cbind(c(5356,5458, 7281, 5396, 5925, 5898,8844,7422,5599, 6180,7463, 5840, 6096, 7086, 4889,8511,6871, 5780, 6180 ), c(1982, 1984, 1985, 1986, 1988, 1989, 1991, 1999, 2000, 2002, 2003, 2005, 2007, 2008, 2009, 2010, 2013, 2014, 2015)))
colnames(cc)<-c("Yield", "Year")
cc
plot(Yield~Year, cc)
summary(lm(Yield~Year, cc))
plot(predict(water2013)~Water.Total, yieldwater[yieldwater$Year==2013,] )

list<-paste0(rep("water",19), cc$Year)
yearlist<-cc$Year
nlsinfl<-NULL
waterpredict<-NULL
watertot<-NULL
watertot.2<-NULL
thresh<-NULL
steep<-NULL
na.omit(yieldwater)
  for( j in yearlist){
    yield<-get(paste0("water", j))
    waterpredict<-c(waterpredict, predict(yield))
    thresh<-c(thresh, coef(yield)[1], use.names=F)
    steep<-c(steep, coef(yield)[3], use.names=F)
    watertot<-c(watertot,yieldwater[yieldwater$Year==j,]$Water.Total)
    watertot.2<-c(watertot.2,rep(j, length(yieldwater[yieldwater$Year==j,]$Water.Total)))
  wheatinfl<-findiplist(x=yieldwater[yieldwater$Year==j,]$Water.Total, y=predict(yield), index=1)
  nlsinfl<-cbind(c(nlsinfl,wheatinfl[1,3]))
  }
nlscoefs<-as.data.frame(cbind(thresh,steep,cc$Year))
plot(steep~V3, nlscoefs)
predictframe<-as.data.frame(cbind(waterpredict, watertot.2, watertot))
colnames(predictframe)<-c("yield.pred","Year", "Water.Total")
predictframe$Year<-as.factor(predictframe$Year)
nlsinfl<-as.data.frame(nlsinfl)
nlsinfl$Year<-yearlist
nlsinfl<-nlsinfl[is.finite(nlsinfl$V1),]
ggplot(predictframe[predictframe$Year==1982|predictframe$Year==1985|predictframe$Year==1988|predictframe$Year==1989|predictframe$Year==1999|predictframe$Year==2000|predictframe$Year==2002|predictframe$Year==2007|predictframe$Year==2008|predictframe$Year==2009|predictframe$Year==2013|predictframe$Year==2015,], aes(y=yield.pred, x=Water.Total))+geom_point()+geom_smooth(formula=y~log(x))+facet_wrap(~Year)+theme_classic()+geom_vline(data=nlsinfl,aes(xintercept=V1, color="red"))+theme(legend.position="none")


plot(lm(V1~Year, nlsinfl))
nlsinfl<-nlsinfl[-13:-14,]
nlsinfl<-nlsinfl[-10,]
summary(lm(V1~Year, nlsinfl))
ggplot(nlsinfl, aes(y=V1,Year))+geom_point()+geom_smooth(method="lm")
library(ggplot2)

waterinfl<-NULL
curves.x<-NULL
curves.y<-NULL
curves.2<-NULL
yield.watinfl<-NULL
for (i in c(1982:1995,1997:2015)){
  wheat.loess<-loess(lm(Yield~Water.Total,yieldwater.avg[yieldwater.avg$Year==i,]), family = "sym")
curves.y<-c(curves.y,wheat.loess$y)
curves.x<-c(curves.x, wheat.loess$x)
curves.2<-c(curves.2,rep(i,length(predict(wheat.loess))))
wheatinfl<-findiplist(x=yieldwater.avg[yieldwater.avg$Year==i,]$Water.Total, y=predict(wheat.loess), index=1)
yield.watinfl<-cbind(c(yield.watinfl, predict(wheat.loess,wheatinfl[1,3])))
waterinfl<-cbind(c(waterinfl,wheatinfl[1,3]))
}

curve.df<-as.data.frame(cbind(curves.y,curves.x, curves.2))
colnames(curve.df)<-c("Yield","Water.Total", "Year")
ggplot(curve.df, aes(y=Yield, x=Water.Total, group=Year))+geom_smooth(se=F, col=Year)+theme_classic()+geom_point()

waterinfl
waterinfl<-as.data.frame(cbind(waterinfl, yield.watinfl))
waterinfl$Year<-c(1982:1995,1997:2015)
waterinfl
loess.years<-waterinfl[is.finite(waterinfl$V1),]
library(ggplot2)
summary(lm(V1~Year, waterinfl))
curve.cut<-curve.df[curve.df$Year==1984|curve.df$Year==1989|curve.df$Year==1993|curve.df$Year==1998|curve.df$Year==1999|curve.df$Year==2003|curve.df$Year==2010|curve.df$Year==2012|curve.df$Year==2015,]
watercurves<-ggplot(curve.cut, aes(y=Yield, x=Water.Total, group=Year))+geom_smooth(se=FALSE)+theme_classic()+facet_wrap(~Year)+geom_point()+geom_vline(data=loess.years, aes(xintercept=V1, color="red"))+theme(legend.position = "none")
#+geom_point(data=loess.years, aes(y=V2,x=V1, color="red"))

waterthresh<-ggplot(waterinfl,aes(V1,x=Year))+geom_point()+geom_smooth(method="lm", fill="lightgrey")+theme_classic()+ylab("Water Threshold (Inches Applied)")
multiplot(watercurves, waterthresh)

water1985<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=4000,phi2=-5,phi3=0.6),
               data=nitwater[nitwater$Year==1985,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
water1986<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=5000,phi2=-5,phi3=0.4),
               data=nitwater[nitwater$Year==1986,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1987<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
#              start=list(phi1=7000,phi2=-8,phi3=0.8),
#             data=nitwater[nitwater$Year==1987,], trace=TRUE,
#            control = list(maxiter = 10000000, minFactor=.00001))
water1988<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000000,phi2=-5,phi3=0.4),
               data=nitwater[nitwater$Year==1988,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
water1989<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=4000,phi2=-5,phi3=0.6),
               data=nitwater[nitwater$Year==1989,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1990<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
#             start=list(phi1=7000,phi2=-2,phi3=0.1),
#            data=nitwater[nitwater$Year==1990,], trace=TRUE,
#           control = list(maxiter = 10000000, minFactor=.00000001))
water1991<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=8000,phi2=-5,phi3=0.25),
               data=nitwater[nitwater$Year==1991,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1992<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
#             start=list(phi1=6000,phi2=-5,phi3=0.15),
#            data=nitwater[nitwater$Year==1992,], trace=TRUE,
#           control = list(maxiter = 1000, minFactor=.0000001))
#water1993<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
#              start=list(phi1=4400,phi2=-1,phi3=.1),
#             data=nitwater[nitwater$Year==1993,], trace=TRUE,
#            control = list(maxiter = 10000000, minFactor=.00001))
#water1994<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
#              start=list(phi1=5000,phi2=-6,phi3=.15),
#             data=nitwater[nitwater$Year==1994,], trace=TRUE,
#            control = list(maxiter = 10000000, minFactor=.00000000000001))
water1995<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=5000,phi2=-5,phi3=0.4),
               data=nitwater[nitwater$Year==1995,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1997<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
# start=list(phi1=6000,phi2=-5,phi3=0.1),
#data=nitwater[nitwater$Year==1997,], trace=TRUE,
#control = list(maxiter = 10000000, minFactor=.00001))
#water1998<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
#start=list(phi1=6000,phi2=-5,phi3=0.1),
#data=nitwater[nitwater$Year==1998,], trace=TRUE,
#control = list(maxiter = 10000000, minFactor=.00000000001))
water1999<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000,phi2=-5,phi3=0.8),
               data=nitwater[nitwater$Year==1999,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2000<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=7000,phi2=-5,phi3=0.8),
               data=nitwater[nitwater$Year==2000,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
#water2001<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
#  start=list(phi1=6000,phi2=-5,phi3=0.8),
#  data=nitwater[nitwater$Year==2001,], trace=TRUE,
#  control = list(maxiter = 10000000, minFactor=.00000000001))
water2002<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000,phi2=-5,phi3=0.8),
               data=nitwater[nitwater$Year==2002,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2003<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000,phi2=-5,phi3=0.8),
               data=nitwater[nitwater$Year==2003,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2004<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=5500,phi2=-5,phi3=0.4),
               data=nitwater[nitwater$Year==2004,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2005<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=5000,phi2=-5,phi3=0.5),
               data=nitwater[nitwater$Year==2005,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
#water2006<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
start=list(phi1=4000,phi2=-5,phi3=0.99),
data=nitwater[nitwater$Year==2006,], trace=TRUE,
control = list(maxiter = 10000000, minFactor=.00000000001))
water2007<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000,phi2=-5,phi3=0.8),
               data=nitwater[nitwater$Year==2007,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2008<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000,phi2=-5,phi3=0.4),
               data=nitwater[nitwater$Year==2008,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2009<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=5000,phi2=-5,phi3=0.5),
               data=nitwater[nitwater$Year==2009,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2010<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000,phi2=-5,phi3=0.5),
               data=nitwater[nitwater$Year==2010,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
#water2011<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
# start=list(phi1=6000,phi2=-5,phi3=0.5),
# data=nitwater[nitwater$Year==2011,], trace=TRUE,
# control = list(maxiter = 10000000, minFactor=.00000000001))
#water2012<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
#   start=list(phi1=5000,phi2=-5,phi3=0.5),
#   data=nitwater[nitwater$Year==2012,], trace=TRUE,
#  control = list(maxiter = 10000000, minFactor=.00000000001))
water2013<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000,phi2=-5,phi3=0.5),
               data=nitwater[nitwater$Year==2013,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2014<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000,phi2=-5,phi3=0.5),
               data=nitwater[nitwater$Year==2014,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2015<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
               start=list(phi1=6000,phi2=-5,phi3=0.5),
               data=nitwater[nitwater$Year==2015,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))

nitwater<-NULL
nitwater$Yield<-commonavg$Yield
nitwater$Year<-commonavg$Year
nitwater$Total.N<-commonavg$Total.N
nitwater<-as.data.frame(nitwater)
nitwater$Location<-commonavg$Location
nitwater<-na.omit(nitwater)


N1982<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
           start=list(phi1=5000,phi2=-5,phi3=0.5),
           data=nitwater[nitwater$Year==1982,], trace=TRUE,
           control = list(maxiter = 10000000, minFactor=.00001))
N1984<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
           start=list(phi1=5000,phi2=-5,phi3=0.2),
           data=nitwater[nitwater$Year==1984,], trace=TRUE,
           control = list(maxiter = 10000000, minFactor=.00001)



Ninfl<-NULL
yield.ninfl<-NULL
curves.y<-NULL
curves.x<-NULL
curves.2<-NULL
for (i in c(1982:1995,1997:2015)){
wheat.loess<-loess(lm(Yield~Total.N,nitwater[nitwater$Year==i,]), family = "sym")

curves.y<-c(curves.y,wheat.loess$y)
curves.x<-c(curves.x, wheat.loess$x)
curves.2<-c(curves.2,rep(i,length(predict(wheat.loess))))
wheatinfl<-findiplist(x=nitwater[nitwater$Year==i,]$Total.N, y=predict(wheat.loess), index=0)

yield.ninfl<-cbind(c(yield.ninfl, predict(wheat.loess,wheatinfl[1,3])))
Ninfl<-cbind(c(Ninfl,wheatinfl[1,3]))
}
Ninfl<-cbind(Ninfl,yield.ninfl)
Ninfl<-as.data.frame(Ninfl)
Ninfl$Year<-c(1982:1995,1997:2015)
Ninfl
Ninfl.cut<-Ninfl[is.finite(Ninfl$V1),]
colnames(Ninfl.cut)<-c("V1","Yield", "Year")


Ncurve.df<-as.data.frame(cbind(curves.y,curves.x, curves.2))
colnames(Ncurve.df)<-c("Yield","Total.N", "Year")
Ncurve.cut<-Ncurve.df[Ncurve.df$Year==1983|Ncurve.df$Year==1987|Ncurve.df$Year==1988|Ncurve.df$Year==1992|Ncurve.df$Year==1993|Ncurve.df$Year==1994|Ncurve.df$Year==1995|Ncurve.df$Year==2000|Ncurve.df$Year==2001|Ncurve.df$Year==2002|Ncurve.df$Year==2005|Ncurve.df$Year==2008|Ncurve.df$Year==2011|Ncurve.df$Year==2012|Ncurve.df$Year==2013|Ncurve.df$Year==2014,]

nitcurve<-ggplot(Ncurve.cut, aes(y=Yield, x=Total.N, group=Year))+geom_smooth(se=F)+theme_classic()+geom_point()+facet_wrap(~Year)+geom_vline(data=Ninfl.cut, aes(xintercept=V1, color="Red"))+theme(legend.position="none")

library(ggplot2)
summary(lm(V1~Year, Ninfl))
nittrend<-ggplot(Ninfl,aes(y=V1,x=Year))+geom_point()+geom_smooth(method="lm", fill="lightgrey")+theme_classic()+ylab("Nitrogen Threshold (lbs/acre)")

multiplot(nitcurve,nittrend)

nitcurves<-ggplot(Ninfl, aes(y=Yield, x=Total.N, group=Year))+geom_smooth(se=FALSE)+theme_classic()+facet_wrap(~Year)+geom_point()
nitcurves
+geom_vline(data=loess.years, aes(xintercept=V1, color="red"))+theme(legend.position = "none")

inflall<-rbind(Ninfl, waterinfl)
inflall$source<-c(rep(c("Total N (lb/acre)"),30), rep(c("Total Water (inches applied)"),30))
ggplot(inflall,aes(V1,x=Year))+geom_point()+geom_smooth(method="lm", fill="lightgrey", col="Black")+facet_wrap(~source, scales="free")+ylab("Input Threshold of Diminishing Returns in Yield")+theme_classic()

library(ggplot2)

ggplot(commonavg, aes(y=Yield, x=Irrigation.Total))+geom_point()+geom_smooth(method="loess")
ggplot(commonavg, aes(y=Yield, x=Total.N))+geom_point()+geom_smooth(method="loess")
ggplot(commonavg, aes(y=Yield, x=Total.K))+geom_point()+geom_smooth(method="loess")
protrend<-ggplot(commonavg, aes(y=Protein, x=Irrigation.Total))+geom_point()+geom_smooth(method="lm")+xlim(c(0,50))+theme_classic()
commonavg$Irrigation.Method<-factor(commonavg$Irrigation.Method, levels = c("None", "Drip", "Mixed", "Sprinkler", "Flood", "Furrow", "Strip", "Spud"))

protlabs<-NULL
protlabs$group<-rownames(tukey$groups)
protlabs$label<-c("a", "a", "a","b","bc", "c", "d","d")
protlabs<-as.data.frame(protlabs)
protbox<-ggplot(commonavg, aes(y=Protein, x=Irrigation.Method))+geom_boxplot()+theme_classic()+annotate("text", x=1:8, y=6, label=protlabs$label)
multiplot(protrend, protbox)
library(agricolae)
options(na.action = "na.omit")
mod<-lm(Protein~Irrigation.Method, commonavg)
anova(mod)
tukey<-HSD.test(mod, trt="Irrigation.Method", DFerror=7709, MSerror=2.314, group=TRUE)
tukey
proteinmodel1<-lm(Protein~Water.Total*Irrigation.Method*Total.K, commonavg)
proteinmodel2<-lm(Protein~Irrigation.Total*Irrigation.Method*Total.K, commonavg)
proteinmodel3<-lm(Protein~Rain*Irrigation.Method*Total.K, commonavg)
summary(proteinmodel1)
summary(proteinmodel2)
summary(proteinmodel3)

protdf<-NULL
protdf$Protein<-commonavg$Protein
protdf$Water.Total<-commonavg$Water.Total
protdf$Irrigation.Total<-commonavg$Irrigation.Total
protdf$Irrigation.Method<-commonavg$Irrigation.Method
protdf$Total.K<-commonavg$Total.K
protdf<-as.data.frame(protdf)
protdf<-na.omit(protdf)


kmod<-lm(WUE~pdsi.q4*Irrigation.Method+Irrigation.Method*Total.K+County, commonavg[is.finite(commonavg$WUE),])
summary(kmod)
plot(protdf$Protein~predict(proteinmodel1))


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#OTher Variables
library(plyr)
commonavg.year<-ddply(commonavg, .(Year, County), summarise,
                        Yield = mean(Yield),
                        WUE=mean(WUE),
                        pdsi.q1=mean(pdsi.q1),
                        MAT=mean(MAT),
                        MAP=mean(MAP),
                        CO2=mean(CO2),
                        Water.Total=mean(Water.Total),
                        Rain=mean(Rain),
                        Total.N=mean(Total.N),
                        Preplant.N=mean(Preplant.N),
                        Anthesis.N = mean(Anthesis.N),
                        Tillering = mean(Tillering))
str(commonavg.year)
commonavg.year<-na.omit(commonavg.year)
commonavg.year<-ddply(commonavg.year, .(Year), summarise,
                      Yield = mean(Yield),
                      WUE=mean(WUE),
                      pdsi.q1=mean(pdsi.q1),
                      MAT=mean(MAT),
                      MAP=mean(MAP),
                      CO2=mean(CO2),
                      Water.Total=mean(Water.Total),
                      Rain=mean(Rain),
                      Total.N=mean(Total.N),
                      Preplant.N=mean(Preplant.N),
                      Anthesis.N = mean(Anthesis.N),
                      Tillering = mean(Tillering))
nassacres<-read.csv("/Users/maxwell1/Documents/GitHub/Wheat/nass acres.csv")
str(commonavg.year)
commonavg.year<-merge(nassacres, commonavg.year)

commonavg.year[2:15]<-scale(commonavg.year[2:15])
library(ggplot2)
ggplot(commonavg.year, aes(y=Yield, Year))+geom_line(size =2)+geom_line(aes(MAT, x=Year), color = "Red", lwd=.5)+geom_line(aes(MAP, x=Year), color = "Blue", lwd=.5)+theme_classic()+geom_line(aes(y=Planted.Acres, x=Year), color = "Dark Grey", size = 2)


require(randomForest)
require(MASS)
commonavg.rf<-ddply(commonavg, .(County, Year, Texture, ETzone, Irrigation.Method), summarise,
                      Yield = mean(Yield),
                      pdsi.q1=mean(pdsi.q1),
                      MAT=mean(MAT),
                      MAP=mean(MAP),
                      CO2=mean(CO2),
                      Water.Total=mean(Water.Total),
                      Rain=mean(Rain),
                      Total.N=mean(Total.N),
                    Total.P=mean(Total.P),
                    Total.K=mean(Total.K),
                      Preplant.N=mean(Preplant.N),
                      Anthesis.N = mean(Anthesis.N),
                    With.Seed=mean(With.Seed),
                      Tillering = mean(Tillering))
str(commonavg.rf)
commonavg.rf<-na.omit(commonavg.rf)
train<-sample(1:nrow(commonavg.rf))
rf<-randomForest(Yield~., data =commonavg.rf)
rf
rf$importance
plot(rf)
predict(rf)
commonavg.rf$rf.mod<-predict(rf)
commonavg.rf$resid.rf<-resid(lm(rf.mod~Yield, commonavg.rf))
plot(rf.mod~Yield, commonavg.rf)
resid.df<-ddply(commonavg.rf, .(Year), summarise, 
      resid.rf=mean(resid.rf))
ggplot(resid.df, aes(resid.rf, x=Year))+geom_line()+stat_smooth(method="loess", span=0.1, se=TRUE,fill alpha=0.3)
