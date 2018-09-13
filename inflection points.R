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
library(minpack.lm)

water1982<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.25),
               data=yieldwater[yieldwater$Year==1982,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
water1983<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=4200,phi2=-5,phi3=0.2),
               data=yieldwater[yieldwater$Year==1983,], trace=TRUE,
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
water1987<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
             start=list(phi1=6100,phi2=-5,phi3=0.2),
           data=yieldwater[yieldwater$Year==1987,], trace=TRUE)
water1988<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000000,phi2=-5,phi3=0.1),
               data=yieldwater[yieldwater$Year==1988,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
water1989<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=4000,phi2=-5,phi3=0.6),
               data=yieldwater[yieldwater$Year==1989,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1990<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #            start=list(phi1=7000,phi2=-2,phi3=0.1),
  #          data=yieldwater[yieldwater$Year==1990,], trace=TRUE,
   #        control = list(maxiter = 10000000, minFactor=.00000001))
water1991<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=8000,phi2=-5,phi3=0.25),
               data=yieldwater[yieldwater$Year==1991,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1992<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #              start=list(phi1=6000,phi2=-5,phi3=0.15),
  #          data=yieldwater[yieldwater$Year==1992,], trace=TRUE,
   #         control = list(maxiter = 1000, minFactor=.0000001))
#water1993<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
    #          start=list(phi1=4400,phi2=-1,phi3=.5),
      #       data=yieldwater[yieldwater$Year==1993,], trace=TRUE,
       #     control = list(maxiter = 10000000, minFactor=.00001))
#water1994<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
#              start=list(phi1=5000,phi2=-6,phi3=.15),
#             data=yieldwater[yieldwater$Year==1994,], trace=TRUE,
#            control = list(maxiter = 10000000, minFactor=.00000000000001))
water1995<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=5000,phi2=-5,phi3=0.4),
               data=yieldwater[yieldwater$Year==1995,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00001))
#water1997<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #start=list(phi1=6000,phi2=-5,phi3=0.1),
#data=yieldwater[yieldwater$Year==1997,], trace=TRUE,
#ontrol = list(maxiter = 10000000, minFactor=.00001))

water1998<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
start=list(phi1=6000,phi2=-5,phi3=0.1),
data=yieldwater[yieldwater$Year==1998,], trace=TRUE,
control = list(maxiter = 10000000, minFactor=.00000000001))

water1999<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=6000,phi2=-5,phi3=0.8),
               data=yieldwater[yieldwater$Year==1999,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
water2000<-nls(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
               start=list(phi1=7000,phi2=-5,phi3=0.8),
               data=yieldwater[yieldwater$Year==2000,], trace=TRUE,
               control = list(maxiter = 10000000, minFactor=.00000000001))
#water2001<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 # start=list(phi1=6000,phi2=-5,phi3=0.8),
  #data=yieldwater[yieldwater$Year==2001,], trace=TRUE,
  #control = list(maxiter = 10000000, minFactor=.00000000001))
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
#water2006<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 # start=list(phi1=4700,phi2=-5,phi3=0.99),
  #data=yieldwater[yieldwater$Year==2006,], trace=TRUE,
  #control = list(maxiter = 10000000, minFactor=.00000000001))
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
#water2011<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #start=list(phi1=6000,phi2=-5,phi3=0.5),
 #data=yieldwater[yieldwater$Year==2011,], trace=TRUE,
 #control = list(maxiter = 10000000, minFactor=.00000000001))
#water2012<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Water.Total))),
 #  start=list(phi1=5000,phi2=-20,phi3=0.01),
  # data=yieldwater[yieldwater$Year==2012,], trace=TRUE,
  #control = list(maxiter = 10000000, minFactor=.00000000001))
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


cc<-data.frame(cbind(c(5356,5458, 7281, 5396,5354,5925, 5898,8844,4719,4219,7422,5599, 6180,7463, 5840, 6096, 7086, 4889,8511,6871, 5780, 6180 ), c(1982, 1984, 1985, 1986, 1987, 1988, 1989, 1991,1995,1998, 1999, 2000, 2002, 2003, 2005, 2007, 2008, 2009, 2010, 2013, 2014, 2015)))
colnames(cc)<-c("Yield", "Year")
cc
ggplot(cc, aes(Yield, x=Year))+geom_point()+geom_smooth(method = "lm",fill="lightgrey", color = "Black")+theme_classic()+ylab("Yield Plateau (lbs/acre)")
summary(lm(Yield~Year, cc))
plot(predict(water2013)~Water.Total, yieldwater[yieldwater$Year==2013,])

list<-paste0(rep("water",22), cc$Year)
yearlist<-cc$Year
nlsinfl.w<-NULL
waterpredict<-NULL
watertot<-NULL
watertot.2<-NULL
na.omit(yieldwater)
for( j in yearlist){
  yield<-get(paste0("water", j))
  waterpredict<-c(waterpredict, predict(yield))
  watertot<-c(watertot,yieldwater[yieldwater$Year==j,]$Water.Total)
  watertot.2<-c(watertot.2,rep(j, length(yieldwater[yieldwater$Year==j,]$Water.Total)))
  wheatinfl<-findiplist(x=yieldwater[yieldwater$Year==j,]$Water.Total, y=predict(yield), index=0)
  nlsinfl.w<-cbind(c(nlsinfl.w,wheatinfl[1,3]))
}
predictframe<-as.data.frame(cbind(waterpredict, watertot.2, watertot))
colnames(predictframe)<-c("yield.pred","Year", "Water.Total")
predictframe$Year<-as.factor(predictframe$Year)

nlsinfl.w<-as.data.frame(nlsinfl.w)
nlsinfl.w$Year<-yearlist
plot(lm(V1~Year, nlsinfl.w))
nlsinfl.w<-nlsinfl.w[-16,]
nlsinfl.w<-nlsinfl.w[-13,]
nlsinfl.w<-nlsinfl.w[-5,]
nlsinfl.w<-nlsinfl.w[is.finite(nlsinfl.w$V1),]
summary(lm(V1~Year, nlsinfl.w))
waterplot.dat<-predictframe[predictframe$Year %in% nlsinfl.w$Year,]
yieldwater.dat<-yieldwater[yieldwater$Year %in% nlsinfl.w$Year,]
waterplot<-ggplot(waterplot.dat, aes(y=yield.pred, x=Water.Total))+geom_smooth(col="grey")+facet_wrap(~Year, nrow=3)+theme_classic()+geom_vline(data=nlsinfl.w,color="black",aes(xintercept=V1))+theme(legend.position="none")+ylab("Predicted Yield (lbs/acre)")+xlab("Total Water  (Inches)")+geom_point(data=yieldwater.dat, aes(y=Yield, x=Water.Total))+geom_point(color="grey", size=2)
waterplot
summary(lm(V1~Year, nlsinfl.w))
waterlm<-ggplot(nlsinfl.w, aes(y=V1,Year))+geom_point()+geom_smooth(method="lm", fill="lightgrey", color="black")+theme_classic()+ylab("Water Threshold (Inches Applied)")+xlab("Year")
waterlm
library(ggplot2)
multiplot(water)

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

waterinfl
waterinfl<-as.data.frame(cbind(waterinfl, yield.watinfl))
waterinfl$Year<-c(1982:1995,1997:2015)
waterinfl
loess.years<-waterinfl[is.finite(waterinfl$V1),]
library(ggplot2)
summary(lm(V1~Year, waterinfl))
curve.cut<-curve.df[curve.df$Year==1984|curve.df$Year==1989|curve.df$Year==1993|curve.df$Year==1998|curve.df$Year==1999|curve.df$Year==2003|curve.df$Year==2010|curve.df$Year==2012|curve.df$Year==2015,]
watercurves<-ggplot(yieldwater, aes(y=Yield, x=Water.Total, group=Year))+geom_smooth(se=FALSE)+theme_classic()+facet_wrap(~Year)+geom_point()+geom_vline(data=loess.years, aes(xintercept=V1, color="red"))+theme(legend.position = "none")
#+geom_point(data=loess.years, aes(y=V2,x=V1, color="red"))
summary(lm(V1~Year, waterinfl))
waterthresh<-ggplot(waterinfl,aes(V1,x=Year))+geom_point()+geom_smooth(method="lm", fill="lightgrey")+theme_classic()+ylab("Water Threshold (Inches Applied)")
multiplot(watercurves, waterthresh)


nitwater<-NULL
nitwater$Yield<-commonavg$Yield
nitwater$Year<-commonavg$Year
nitwater$Total.N<-commonavg$Total.N
nitwater<-as.data.frame(nitwater)
nitwater$Location<-commonavg$Location
nitwater<-na.omit(nitwater)
#nitwater<-nitwater[nitwater$Location!="Delta.Tyler",]
#nitwater<-nitwater[nitwater$Location!="Delta.Tyler"&nitwater$Total.N>0,]
library(minpack.lm)
#N1982<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
 #             start=list(phi1=4000,phi2=-2,phi3=0.4),
  #           data=nitwater[nitwater$Year==1982,], trace=TRUE,
   #         control = list(maxiter = 10000000, minFactor=.00001))
N1983<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
           start=list(phi1=5000,phi2=-2,phi3=0.1),
           data=nitwater[nitwater$Year==1983,], trace=TRUE,
           control = list(maxiter = 10000000, minFactor=.00001))
N1984<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
          start=list(phi1=5000,phi2=-2,phi3=0.01),
        data=nitwater[nitwater$Year==1984,], trace=TRUE,
       control = list(maxiter = 10000000, minFactor=.00000001))
N1985<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
           start=list(phi1=5000,phi2=-2,phi3=0.01),
           data=nitwater[nitwater$Year==1985,], trace=TRUE,
           control = list(maxiter = 10000000, minFactor=.00001))
N1986<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=5000,phi2=-2,phi3=0.01),
             data=nitwater[nitwater$Year==1986,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
#N1987<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
 #            start=list(phi1=5000,phi2=-8,phi3=0.5),
  #           data=nitwater[nitwater$Year==1987,], trace=TRUE,
   #          control = list(maxiter = 10000000, minFactor=.00001))
N1988<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==1988,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N1989<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=5000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==1989,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N1990<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==1990,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
#N1991<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
 #            start=list(phi1=6000,phi2=-2,phi3=0.2),
  #           data=nitwater[nitwater$Year==1991,], trace=TRUE,
   #          control = list(maxiter = 10000000, minFactor=.00001))
#N1992<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
 #            start=list(phi1=6000,phi2=-2,phi3=0.2),
  #           data=nitwater[nitwater$Year==1992,], trace=TRUE,
   #          control = list(maxiter = 10000000, minFactor=.00001))
#N1993<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
 #            start=list(phi1=6000,phi2=-2,phi3=0.4),
  #           data=nitwater[nitwater$Year==1993,], trace=TRUE,
   #          control = list(maxiter = 10000000, minFactor=.00001))
N1994<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.4),
             data=nitwater[nitwater$Year==1994,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N1995<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=5000,phi2=-2,phi3=0.01),
             data=nitwater[nitwater$Year==1995,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
#N1997<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
 #            start=list(phi1=6000,phi2=-2,phi3=0.2),
  #           data=nitwater[nitwater$Year==1997,], trace=TRUE,
   #          control = list(maxiter = 10000000, minFactor=.00001))
N1998<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==1998,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N1999<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==1999,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2000<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2000,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
#N2001<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
 #            start=list(phi1=6000,phi2=-2,phi3=0.2),
  #           data=nitwater[nitwater$Year==2001,], trace=TRUE,
   #          control = list(maxiter = 10000000, minFactor=.00001))
N2002<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2002,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2003<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2003,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2004<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2004,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2005<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2005,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2006<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2006,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2007<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2007,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
#N2008<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
 #            start=list(phi1=6000,phi2=-2,phi3=0.2),
  #           data=nitwater[nitwater$Year==2008,], trace=TRUE,
   #          control = list(maxiter = 10000000, minFactor=.00001))
N2009<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2009,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2010<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2010,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2011<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2011,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2012<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2012,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2013<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.2),
             data=nitwater[nitwater$Year==2013,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2014<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=6000,phi2=-2,phi3=0.01),
             data=nitwater[nitwater$Year==2014,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
N2015<-nlsLM(Yield~phi1/(1+exp(-(phi2+phi3*Total.N))),
             start=list(phi1=5500,phi2=-2,phi3=0.01),
             data=nitwater[nitwater$Year==2015,], trace=TRUE,
             control = list(maxiter = 10000000, minFactor=.00001))
plot(Yield~Total.N,nitwater[nitwater$Year==1984,])

cc.N<-c(c(1983,1985, 1988:1990,1994,1995,1998:2000, 2002, 2004:2007,2009:2013))
cc.N<-as.data.frame(cc.N)
cc.N$Yield<-c(4696,7411, 13030, 4950, 6916, 6706, 5076, 5307, 7146, 5749, 8457, 5500, 5045, 7089, 5746, 6430, 6465, 6145, 5864, 7007)
colnames(cc.N)<-c("Year", "Yield")
ggplot(cc.N, aes(Yield,x=Year, cc.N, ylab = "Yield (lbs/acre)"))+geom_point()+geom_smooth(method="lm", color = "black")+theme_classic()
list<-paste0(rep("water",20), cc$Year)
yearlist<-cc.N$Year
nlsinfl<-NULL
Npredict<-NULL
Ntot<-NULL
Ntot.2<-NULL
for( j in yearlist){
  yield<-get(paste0("N", j))
  Npredict<-c(Npredict, predict(yield))
  Ntot<-c(Ntot,nitwater[nitwater$Year==j,]$Total.N)
  Ntot.2<-c(Ntot.2,rep(j, length(nitwater[nitwater$Year==j,]$Total.N)))
  wheatinfl<-findiplist(x=nitwater[nitwater$Year==j,]$Total.N, y=predict(yield), index=1)
  nlsinfl<-cbind(c(nlsinfl,wheatinfl[1,3]))
}
predictframe<-as.data.frame(cbind(Npredict, Ntot.2, Ntot))
colnames(predictframe)<-c("yield.pred","Year", "Total.N")

nlsinfl<-as.data.frame(nlsinfl)
nlsinfl$Year<-yearlist
plot(lm(V1~Year, nlsinfl))
nlsinfl<-nlsinfl[-17:-18,]
nlsinfl<-nlsinfl[-4,]
plot(V1~Year, nlsinfl)
summary(lm(V1~Year, nlsinfl))
nlsinfl<-nlsinfl[is.finite(nlsinfl$V1),]

nitwater.cut<-nitwater[nitwater$Year %in% nlsinfl$Year,]

nplot<-ggplot(data=predictframe[predictframe$Year %in% nlsinfl$Year,], aes(y=yield.pred, x=Total.N))+geom_point(data=nitwater.cut[nitwater.cut$Year %in% nlsinfl$Year,], aes(y=Yield, x=Total.N))+geom_point(color="darkgrey", size = 2)+facet_wrap(~Year)+geom_smooth(fill="lightgrey", color = "black", lwd=0.5)+theme(legend.position="none")+theme_classic()+geom_vline(data=nlsinfl, aes(xintercept=V1))+ylab("Yield (lbs/acre)")+xlab("N applied (lbs/acre)")
nplot

nittrend<-ggplot(nlsinfl,aes(y=V1,x=Year))+geom_point()+geom_smooth(method="lm", fill="lightgrey", col = "black")+theme_classic()+ylab("Nitrogen Threshold (lbs/acre)")
nittrend

nitwater<-NULL
nitwater$Yield<-commonavg$Yield
nitwater$Year<-commonavg$Year
nitwater$Total.N<-commonavg$Total.N
nitwater<-as.data.frame(nitwater)
nitwater$Location<-commonavg$Location
nitwater<-na.omit(nitwater)
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
  wheatinfl<-findiplist(x=nitwater[nitwater$Year==i,]$Total.N, y=predict(wheat.loess), index=1)
  
  yield.ninfl<-cbind(c(yield.ninfl, predict(wheat.loess,wheatinfl[1,3])))
  Ninfl<-cbind(c(Ninfl,wheatinfl[1,3]))
}
Ninfl<-cbind(Ninfl,yield.ninfl)
Ninfl<-as.data.frame(Ninfl)
Ninfl$Year<-c(1982:1995,1997:2015)
Ninfl
Ninfl.cut<-Ninfl[is.finite(Ninfl$V1),]
colnames(Ninfl.cut)<-c("V1","Yield", "Year")
plot(V1~Year,Ninfl)

Ncurve.df<-as.data.frame(cbind(curves.y,curves.x, curves.2))
colnames(Ncurve.df)<-c("Yield","Total.N", "Year")
Ncurve.cut<-Ncurve.df[Ncurve.df$Year==1983|Ncurve.df$Year==1987|Ncurve.df$Year==1988|Ncurve.df$Year==1992|Ncurve.df$Year==1993|Ncurve.df$Year==1994|Ncurve.df$Year==1995|Ncurve.df$Year==2000|Ncurve.df$Year==2001|Ncurve.df$Year==2002|Ncurve.df$Year==2005|Ncurve.df$Year==2008|Ncurve.df$Year==2011|Ncurve.df$Year==2012|Ncurve.df$Year==2013|Ncurve.df$Year==2014,]

nitcurve<-ggplot(Ncurve.cut, aes(y=Yield, x=Total.N, group=Year))+geom_smooth(color="grey", se=F)+theme_classic()+geom_point()+facet_wrap(~Year)+geom_vline(data=Ninfl.cut,color="goldenrod4", aes(xintercept=V1, color="goldenrod4"))+theme(legend.position="none")
nitcurve
library(ggplot2)
summary(lm(V1~Year, Ninfl))
nittrend<-ggplot(Ninfl,aes(y=V1,x=Year))+geom_point()+geom_smooth(method="lm", fill="lightgrey", col = "goldenrod4")+theme_classic()+ylab("Nitrogen Threshold (lbs/acre)")
nittrend
multiplot(nitcurve,nittrend)

nitcurves<-ggplot(Ninfl, aes(y=Yield, x=Total.N, group=Year))+geom_smooth(se=FALSE)+theme_classic()+facet_wrap(~Year)+geom_point()
nitcurves


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

nassacres<-read.csv("/Users/maxwell1/Documents/GitHub/Wheat-Updated/nass acres.csv")
nassacres$plant.harvest<-nassacres$Harvested.Acres/nassacres$Planted.Acres
nassacres
nacres<-merge(nlsinfl, nassacres)
wacres<-merge(nlsinfl.w, nassacres)

commonavg.co2<-ddply(commonavg, .(Year), summarise,
                     CO2=mean(CO2),
                     MAT=mean(MAT),
                     MAP=mean(MAP),
                     PDSI=mean(pdsi.q1))
summary(lm(tons.acre~plant.harvest, nassacres))
plot(tons.acre~plant.harvest, nassacres)
nacres<-merge(nacres, commonavg.co2)
nacres$plant.harvest<-nacres$Harvested.Acres/nacres$Planted.Acres
summary(lm(V1~tons.acre, nacres))
wacres<-merge(wacres, commonavg.co2)
wacres$harvest.plant<-wacres$Harvested.Acres/wacres$Planted.Acres
summary(lm(V1~tons.acre, wacres))

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
