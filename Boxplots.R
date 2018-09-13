#Wheat Bargraph
commoncult
str(commonavg)
yieldall<-subset(commonall, select=c("Yield", "Cultivar"))
yieldall$Cultivar<-"All"
yield.cult<-subset(commoncult, select=c("Yield", "Cultivar"))
yield.cult$Cultivar<-"Cultivar"
yield.site<-subset(commoncount, select = c("Yield", "County"))
yield.site$County<-"County"
yield.Loc<-subset(commonloc, select = c("Yield", "Location"))
yield.Loc$Location<-"Location"
yieldtex<-ddply(na.omit(subset(commontex, select=c("Texture", "Yield"))), .(Texture), summarise, Yield = mean(Yield))
yieldtex$Texture<-"Texture"
yieldirr<-ddply(na.omit(subset(commonirr, select=c("Irrigation.Method", "Yield"))), .(Irrigation.Method), summarise, Yield = mean(Yield))
yieldirr$Irrigation.Method<-"Irrgation Method"
yieldyr<-ddply(na.omit(subset(commonyr, select=c("Year", "Yield"))), .(Year), summarise, Yield = mean(Yield))
yieldyr$Year<-"Year"
yieldpr<-ddply(na.omit(subset(commonall, select=c("Previous", "Yield"))), .(Previous), summarise, Yield = mean(Yield))
yieldpr$Previous<-"Previous"

#WUE
WUEall<-subset(commonall, select=c("WUE", "Cultivar"))
WUEall$Cultivar<-"All"
WUE.cult<-subset(commoncult, select=c("WUE", "Cultivar"))
WUE.cult$Cultivar<-"Cultivar"
WUE.site<-subset(commoncount, select = c("WUE", "County"))
WUE.site$County<-"County"
WUE.Loc<-subset(commonloc, select = c("WUE", "Location"))
WUE.Loc$Location<-"Location"
WUEtex<-ddply(na.omit(subset(commontex, select=c("Texture", "WUE"))), .(Texture), summarise, WUE = mean(WUE))
WUEtex$Texture<-"Texture"
WUEirr<-ddply(na.omit(subset(commonirr, select=c("Irrigation.Method", "WUE"))), .(Irrigation.Method), summarise, WUE = mean(WUE))
WUEirr$Irrigation.Method<-"Irrgation Method"
WUEyr<-ddply(na.omit(subset(commonyr, select=c("Year", "WUE"))), .(Year), summarise, WUE = mean(WUE))
WUEyr$Year<-"Year"
WUEpr<-ddply(na.omit(subset(commonall[is.finite(commonall$WUE),], select=c("Previous", "WUE"))), .(Previous), summarise, WUE = mean(WUE))
WUEpr$Previous<-"Previous"
#NUE
NUEall<-subset(commonall, select=c("NUE", "Cultivar"))
NUEall$Cultivar<-"All"
NUE.df.cult<-na.omit(subset(commonavg, select=c("NUE", "Cultivar")))
NUE.cult<-ddply(NUE.df.cult[is.finite(NUE.df.cult$NUE),], .(Cultivar), summarise, NUE=mean(NUE))
NUE.cult
NUE.cult$Cultivar<-"Cultivar"
NUE.df.count<-na.omit(subset(commonavg, select=c("NUE", "County")))
NUE.count<-ddply(NUE.df.count[is.finite(NUE.df.count$NUE),], .(County), summarise, NUE=mean(NUE))
NUE.count
NUE.count$County<-"County"
NUE.df.loc<-na.omit(subset(commonavg, select=c("NUE", "Location")))
NUE.loc<-ddply(NUE.df.loc[is.finite(NUE.df.loc$NUE),], .(Location), summarise, NUE=mean(NUE))
NUE.loc
NUE.loc$Location<-"Location"
NUEtex<-ddply(na.omit(subset(commontex, select=c("Texture", "NUE"))), .(Texture), summarise, NUE = mean(NUE))
NUEtex$Texture<-"Texture"
NUEirr<-ddply(na.omit(subset(commonirr, select=c("Irrigation.Method", "NUE"))), .(Irrigation.Method), summarise, NUE = mean(NUE))
NUEirr$Irrigation.Method<-"Irrgation Method"
NUEyr<-ddply(na.omit(subset(commonyr, select=c("Year", "NUE"))), .(Year), summarise, NUE = mean(NUE))
NUEyr$Year<-"Year"
NUEpr<-ddply(na.omit(subset(commonall[is.finite(commonall$NUE),], select=c("Previous", "NUE"))), .(Previous), summarise, NUE = mean(NUE))
NUEpr$Previous<-"Previous"

# put the data.frames into a named list, where names correspond to data.frame names

yielddfs<-list(na.omit(yieldall[1]), yield.cult[1], yield.site[1:22,1], yield.Loc[1], yieldtex[2], yieldirr[2], yieldyr[2], yieldpr[2])
WUEdfs<-list(WUEall[is.finite(WUEall$WUE),1], WUE.cult[is.finite(WUE.cult$WUE),1], WUE.site[is.finite(WUE.site$WUE), 1], WUE.Loc[is.finite(WUE.Loc$WUE),1], WUEtex[is.finite(WUEtex$WUE),2], WUEirr[is.finite(WUEirr$WUE),2], WUEyr[is.finite(WUEyr$WUE),2], WUEpr[is.finite(WUEpr$WUE),2])
NUEdfs<-list(NUEall[is.finite(NUEall$NUE),1], NUE.cult[is.finite(NUE.cult$NUE),2], NUE.site[is.finite(NUE.site$NUE), 2], NUE.Loc[is.finite(NUE.Loc$NUE),2], NUEtex[is.finite(NUEtex$NUE),2], NUEirr[is.finite(NUEirr$NUE),2], NUEyr[is.finite(NUEyr$NUE),2], NUEpr[is.finite(NUEpr$NUE),2])


#apply function
cvyield <- data.frame(lapply(yielddfs, function(i) sd(unlist(i))/mean(unlist(i))))
cvwue <- data.frame(lapply(WUEdfs, function(i) sd(unlist(i))/mean(unlist(i))))
cvnue <- data.frame(lapply(NUEdfs, function(i) sd(unlist(i))/mean(unlist(i))))
#or loop method
cv<-NULL
cvframe<-NULL
for(i in yielddfs){
  cv<-sd(unlist(i))/mean(unlist(i))
  cvframe<-c(cvframe, cv)
  
}



library(ggplot2)
ybox<-ggplot(yieldall, aes(y=Yield, x=Cultivar))+
  geom_boxplot()+
  geom_boxplot(data=yield.cult, aes(y=Yield, x=Cultivar))+
  geom_boxplot(data=yield.site, aes(y=Yield, x=County))+
  geom_boxplot(data=yield.Loc, aes(y=Yield, x=Location))+
  geom_boxplot(data=yieldtex, aes(y=Yield, x=Texture))+
  geom_boxplot(data=yieldirr, aes(y=Yield, x=Irrigation.Method))+
  geom_boxplot(data=yieldyr, aes(y=Yield, x=Year))+
  geom_boxplot(data=yieldpr, aes(y=Yield, x=Previous))+
  coord_flip()+theme_classic()+xlab("")+ylab("Yield (lbs/acre)")+
  annotate("text", x=(1:8)+.25, y=0, label = paste("CV=",round(cvyield, 2)))

wbox<-ggplot(WUEall, aes(y=WUE, x=Cultivar))+
  geom_boxplot()+
  geom_boxplot(data=WUE.cult, aes(y=WUE, x=Cultivar))+
  geom_boxplot(data=WUE.site, aes(y=WUE, x=County))+
  geom_boxplot(data=WUE.Loc, aes(y=WUE, x=Location))+
  geom_boxplot(data=WUEtex, aes(y=WUE, x=Texture))+
  geom_boxplot(data=WUEirr, aes(y=WUE, x=Irrigation.Method))+
  geom_boxplot(data=WUEyr, aes(y=WUE, x=Year))+
  geom_boxplot(data=WUEpr, aes(y=WUE, x=Previous))+
  coord_flip()+theme_classic()+ylim(0,1000)+xlab("")+ylab("WUE (lbs Yield/Water applied (in))")+
  annotate("text", x=(1:8)+.25, y=0, label = paste("CV=",round(cvwue, 2)))

nbox<-ggplot(NUEall, aes(y=NUE, x=Cultivar))+
  geom_boxplot()+
  geom_boxplot(data=NUE.cult, aes(y=NUE, x=Cultivar))+
  geom_boxplot(data=NUE.count, aes(y=NUE, x=County))+
  geom_boxplot(data=NUE.Loc, aes(y=NUE, x=Location))+
  geom_boxplot(data=NUEtex, aes(y=NUE, x=Texture))+
  geom_boxplot(data=NUEirr, aes(y=NUE, x=Irrigation.Method))+
  geom_boxplot(data=NUEyr, aes(y=NUE, x=Year))+
  geom_boxplot(data=NUEpr, aes(y=NUE, x=Previous))+
  coord_flip()+theme_classic()+ylim(0,2000)+xlab("")+ylab("NUE (lbs Protein/acre)/(lb N/acre)")+
  annotate("text", x=(1:8)+.25, y=0, label = paste("CV=",round(cvnue, 2)))

library(gridExtra)
grid.arrange(ybox, wbox, nbox)

bar$Aggregation<-reorder(bar$Aggregation, bar$cv)
ybar<-ggplot(bar, aes(x=Aggregation, y=Yield.Avg))+geom_col()+geom_errorbar(data=bar, aes(ymin=Yield.Avg-sd, ymax = Yield.Avg+sd, width = 0.25))+coord_flip()+
  geom_text(label = paste("CV=",round(bar$cv, 3)), aes(y=1700), color="White")+theme_classic()+xlab("Aggregation Level")+ylab("Yield (lbs/acre)")

bar$Aggregation<-reorder(bar$Aggregation, bar$wue.cv)
wuebar<-ggplot(bar, aes(x=Aggregation, y=wue))+geom_col()+geom_errorbar(data=bar, aes(ymin=wue-wue.sd, ymax = wue+wue.sd, width = 0.25))+coord_flip()+
  geom_text(label = paste("CV=",round(bar$wue.cv, 3)), aes(y=125), color="White")+theme_classic()+xlab("Aggregation Level")+ylab("WUE (lbs Yield/Water applied (in))")

bar$Aggregation<-reorder(bar$Aggregation, bar$nue.cv)
nuebar<-ggplot(bar, aes(x=Aggregation, y=nue))+geom_col()+geom_errorbar(data=bar, aes(ymin=nue-nue.sd, ymax = nue+nue.sd, width = 0.25))+coord_flip()+
  geom_text(label = paste("CV=",round(bar$nue.cv, 3)), aes(y=400), color="White")+theme_classic()+xlab("Aggregation Level")+ylab("NUE (lbs Protein/acre)/(lb N/acre)")

library(gridExtra)
grid.arrange(ybar, wuebar, nuebar)
grid.arrange(ybox, wbox, nbox)
