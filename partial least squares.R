#PLS regression

library(MASS)
library(caret)
library(AppliedPredictiveModeling)
library(lars)
library(pls)
library(elasticnet)
library(plyr)
str(commonavg)
commonavg.pls<-ddply(commonavg, .(Location, Year, Texture, Irrigation.Method), summarise,
                    Yield = mean(Yield),
                    CO2=mean(CO2),
                    PDSI=mean(pdsi.q1),
                    MAT=mean(MAT),
                    MAP=mean(MAP),
                    tmax.q1=mean(TMAX),
                    tmin.q1=mean(TMIN),
                    Rain=mean(Rain),
                    Water.Total=mean(Water.Total),
                    Preplant.N=mean(Preplant.N),
                    Anthesis.N = mean(Anthesis.N),
                    Tillering.N = mean(Tillering),
                    With.Seed = mean(With.Seed),
                    Total.N = mean(Total.N),
                    Total.P = mean(Total.P),
                    Total.K=mean(Total.K))
commonavg.pls<-na.omit(commonavg.pls)
#commonavg.pls<-commonavg.pls[commonavg.pls$Irrigation.Method!="None",]
commonavg.pls[6:20]<-scale(commonavg.pls[6:20])
commonavg.pls$Year<-as.factor(commonavg.pls$Year)
str(commonavg.pls)
set.seed(2167)
plsFit<-plsr(Yield~.,data=commonavg.pls, validation = "CV", method = "oscorespls", jackknife=TRUE, segments = 5)
summary(plsFit)
coef(plsFit)

validationplot(plsFit, val.type="RMSEP")
pls.RMSEP = RMSEP(plsFit, estimate="CV")
plot(pls.RMSEP, main="RMSEP PLS YIELD", xlab="components")
min_comp = which.min(pls.RMSEP$val)
points(min_comp, min(pls.RMSEP$val), pch=1, col="red", cex=1.5)

plot(commonavg.pls$MAP~resid(plsFit)[,,9])
wpls.pred = predict(plsFit,commonavg.pls, ncomp=1:9, type="response")
ggplot(,aes(wpls.pred[,,9], y=commonavg.pls$Yield))+geom_point()+theme_classic()+theme(text=element_text(size=16,  family="Georgia"))+xlab("Partial Least Squares Prediction")+ylab("Yield")
wpls.pred[,,9]
plot(wpls.pred~commonavg.pls$Yield)
coef(jack.test(plsFit, ncomp=9))[91:105]


plot(plsFit, ncomp=9, asp=1, line=TRUE)
plscoef<-coef(jack.test(plsFit, ncomp=10))[91:105]
importance<-VIP(plsFit)
str(importance)
ax1imp<-colMeans(importance[1:9,])[91:105]
axnorm<-ax1imp/ax1imp[13]
imp.df<-data.frame(plscoef, axnorm, names(axnorm))
ggplot(imp.df, aes(axnorm, x=names.axnorm.,fill=plscoef))+geom_bar(stat="identity")+theme_classic()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("")+ylab("Normalized PLSR Importance")+coord_flip()+scale_fill_gradient(low="red", high="forestgreen")+theme(text=element_text(size=16,  family="Georgia"))+guides(fill=guide_legend(title="Standardized\nCoefficient"))+theme(legend.position="bottom")

plscoef<-scale(coef(jack.test(plsFit, ncomp=9))[77:105])
importance<-VIP(plsFit)
str(importance)
ax1imp<-colMeans(importance[1:9,])[77:105]
axnorm<-ax1imp/ax1imp[21]
imp.df<-data.frame(plscoef, axnorm, names(axnorm))
rownames(imp.df)<-c("Loam", "Loamy Sand", "Muck", "Sandy Loam", "Silt Loam", "Silty Clay", "Silty Clay Loam", "Flood", "Furrow", "Mixed", "None", "Sprinkler", "Spud", "Strip", "CO2", "PDSI", "MAT", "MAP", "Rain", "Water Total", "Preplant N", "Anthesis N", "Tillering N", "With Seed", "Total N", "Total P", "Total K")
imp.df$names.axnorm.<-factor(rownames(imp.df), levels = c("Loam", "Loamy Sand", "Muck", "Sandy Loam", "Silt Loam", "Silty Clay", "Silty Clay Loam", "Flood", "Furrow", "Mixed", "None", "Sprinkler", "Spud", "Strip", "CO2", "PDSI", "MAT", "MAP", "Rain", "Water Total", "Preplant N", "Anthesis N", "Tillering N", "With Seed", "Total N","Total P", "Total K"))

ggplot(imp.df, aes(plscoef, x=names.axnorm.,fill=axnorm))+geom_bar(stat="identity", color="black")+theme_classic()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("")+ylab("Standardized Coefficient")+coord_flip()+scale_fill_gradient2(low= "Red", mid="white",high="forestgreen", guide="colourbar")+theme(text=element_text(size=16,  family="Georgia"))+guides(fill=guide_legend(title="Standardized\nImportance"))+theme(legend.position="bottom")



VIP <- function(object) {
  if (object$method != "oscorespls")
    stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
  if (nrow(object$Yloadings) > 1)
    stop("Only implemented for single-response models")
  
  SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
  Wnorm2 <- colSums(object$loading.weights^2)
  SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}


## VIPjh returns the VIP of variable j with h components
VIPjh <- function(object, j, h) {
  if (object$method != "oscorespls")
    stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
  if (nrow(object$Yloadings) > 1)
    stop("Only implemented for single-response models")
  
  b <- c(object$Yloadings)[1:h]
  T <- object$scores[,1:h, drop = FALSE]
  SS <- b^2 * colSums(T^2)
  W <- object$loading.weights[,1:h, drop = FALSE]
  Wnorm2 <- colSums(W^2)
  sqrt(nrow(W) * sum(SS * W[j,]^2 / Wnorm2) / sum(SS))
}






#####PLS Fit Only Env. Variables
library(plyr)
commonavg.env<-ddply(commonavg, .(County, Texture, Irrigation.Method), summarise,
                     Yield=mean(Yield),
                     CO2=mean(CO2),
                     TMax.q1=mean(TMAX),
                     TMin.q1=mean(TMIN),
                     MAT=mean(MAT),
                     MAP=mean(MAP),
                     Rain=mean(Rain))
commonavg.env<-na.omit(commonavg.env)
#commonavg.env$Year<-factor(commonavg.env$Year, levels =c("1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2050"))
#commonavg.pls<-commonavg.pls[commonavg.pls$Irrigation.Method!="None",]
#commonavg.env[4:8]<-scale(commonavg.env[4:8])
#commonavg.env$Year<-as.factor(commonavg.env$Year)
str(commonavg.env)
set.seed(2167)
plsFit<-plsr(Yield~Irrigation.Method*Texture+.,data=commonavg.env, validation = "CV", method = "oscorespls", jackknife=TRUE, segments = 5)
summary(plsFit)
pls.RMSEP = RMSEP(plsFit, estimate="CV")
plot(RMSEP(plsFit), legendpos = "topright")
min_comp = which.min(pls.RMSEP$val)
points(min_comp, min(pls.RMSEP$val), pch=1, col="red", cex=1.5)

plot(plsFit, ncomp = 8, line = TRUE)
plot(plsFit, plottype = "scores", comps = 1:8)
validationplot(plsFit, val.type="RMSEP")
plot(pls.RMSEP, main="RMSEP PLS YIELD", xlab="components")
plot(plsFit, "loadings", comps = 1:8, legendpos ="topleft", xlab = "nm") 
abline(h = 0)




wpls.pred = predict(plsFit,commonavg.env, ncomp=1:8, type="response")
ggplot(commonavg.env,aes(wpls.pred[,,8], y=commonavg.env$Yield))+geom_point()+theme_classic()+xlab("Partial Least Squares Prediction")+ylab("Yield")
plot(wpls.pred[,,8]~commonavg.env$Yield)
L<-length(coef(jack.test(plsFit, ncomp=8)))
coef(jack.test(plsFit, ncomp=8))[36:(41)]

plscoef<-scale(coef(jack.test(plsFit, ncomp=1:8))[36:41])
importance<-VIP(plsFit)
str(importance)
ax1imp<-NULL
ax1imp<-colMeans(importance[1:8,])[36:41]
axnorm<-ax1imp/ax1imp[5]
imp.df<-data.frame(plscoef[1:6], axnorm[1:6], names(axnorm)[1:6])
colnames(imp.df)<-c("coef", "normimp", "names")
ggplot(imp.df, aes(coef, x=names,fill=normimp))+geom_bar(stat="identity", color="black")+theme_classic()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("")+ylab("Scaled Coefficient")+coord_flip()+scale_fill_gradient(low= "white",high="forestgreen", guide="colourbar")+guides(fill=guide_legend(title="Standardized\nImportance"))+theme(legend.position="bottom")


str(futurewheatclimate)
str(commonavg.env)
p85.50.test<-prec.85.50.mean[-2]

p.85.50.test<-merge(sites.df, p85.50.test, by.x="Location",by.y="Name", all=T)[-3:-4]
p.85.50.test<-p.85.50.test[-1]
colnames(p.85.50.test)<-c("County", "Rain")
p.85.50.test$TMin.q1<-futurewheatclimate$tmin.85.50
p.85.50.test$TMax.q1<-futurewheatclimate$tmax.85.50
p.85.50.test$MAP<-futurewheatclimate$MAP.85.50
p.85.50.test$MAT<-futurewheatclimate$MAT.85.50
p.85.50.test$Texture<-factor("loamy sand", levels = levels(commonavg.env$Texture))


str(p.85.50.test)
str(commonavg.env)
p.85.50.test$County<-factor(p.85.50.test$County)
#p.85.50.test$Year<-factor(c(rep(2050, 46)), levels =c("1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2050"))
p.85.50.test$CO2<-640
levels(p.85.50.test$Year)
levels(commonavg.env$Year)

p.85.50.test<-p.85.50.test[-38,]
p.85.50.test$County<-factor(p.85.50.test$County, levels=c(levels(commonavg.env$County)))
levels(p.85.50.test$County)
levels(commonavg.env$County)

levels(commonavg.env$Irrigation.Method)


p.85.50.test$Irrigation.Method<-factor(rep("Flood",45), levels = c(levels(commonavg.env$Irrigation.Method)))
p.85.50.test$Texture<-factor("loamy sand", levels = levels(commonavg.env$Texture))
yield.85.50<-predict(plsFit, newdata=p.85.50.test, ncomp=8)

####85.70####

p85.70.test<-prec.85.70.mean[-2]

p.85.70.test<-merge(sites.df, p85.70.test, by.x="Location",by.y="Name", all=T)[-3:-4]
p.85.70.test<-p.85.70.test[-1]
colnames(p.85.70.test)<-c("County", "Rain")
p.85.70.test$TMin.q1<-futurewheatclimate$tmin.85.70
p.85.70.test$TMax.q1<-futurewheatclimate$tmax.85.70
p.85.70.test$MAP<-futurewheatclimate$MAP.85.70
p.85.70.test$MAT<-futurewheatclimate$MAT.85.70
p.85.70.test$Texture<-"loamy sand"
p.85.70.test$Texture<-factor(p.85.70.test$Texture, levels =c("clay", "clay loam", "loam", "loamy sand", "muck", "sandy loam","sily loam", "silty clay", "silty clay loam"))

str(p.85.70.test)
str(commonavg.env)
p.85.70.test$County<-factor(p.85.70.test$County)
#p.85.70.test$Year<-factor(c(rep(2050, 46)), levels =c("1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2050"))

levels(p.85.70.test$Year)
levels(commonavg.env$Year)

p.85.70.test<-p.85.70.test[-38,]
p.85.70.test$County<-factor(p.85.70.test$County, levels=c(levels(commonavg.env$County)))
levels(p.85.70.test$County)
levels(commonavg.env$County)

levels(commonavg.env$Irrigation.Method)
p.85.70.test$Irrigation.Method<-factor(rep("Flood",45), levels = c(levels(commonavg.env$Irrigation.Method)))
#p.85.70.test<-p.85.70.test[-7,]
#p.85.70.test$Location<-factor(p.85.70.test$Location, levels=c(levels(commonavg.env$Location)))
#levels(p.85.70.test$Location)
#levels(commonavg.env$Location)

p.85.70.test$CO2<-1240
predict(plsFit, newdata=p.85.70.test, ncomp=8)



####26.50####
p26.50.test<-prec.26.50.mean[-2]

p.26.50.test<-merge(sites.df, p26.50.test, by.x="Location",by.y="Name", all=T)[-3:-4]
p.26.50.test<-p.26.50.test[-1]
colnames(p.26.50.test)<-c("County", "Rain")
p.26.50.test$TMin.q1<-futurewheatclimate$tmin.26.50
p.26.50.test$TMax.q1<-futurewheatclimate$tmax.26.50
p.26.50.test$MAP<-futurewheatclimate$MAP.26.50
p.26.50.test$MAT<-futurewheatclimate$MAT.26.50

str(p.26.50.test)
str(commonavg.env)
p.26.50.test$County<-factor(p.26.50.test$County)
#p.26.50.test$Year<-factor(c(rep(2050, 46)), levels =c("1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2050"))
#levels(p.26.50.test$Year)
#levels(commonavg.env$Year)

p.26.50.test<-p.26.50.test[-38,]
p.26.50.test$County<-factor(p.26.50.test$County, levels=c(levels(commonavg.env$County)))
levels(p.26.50.test$County)
levels(commonavg.env$County)

levels(commonavg.env$Irrigation.Method)
p.26.50.test$Irrigation.Method<-factor(rep("Flood",45), levels = c(levels(commonavg.env$Irrigation.Method)))
#p.26.50.test<-p.26.50.test[-7,]
#p.26.50.test$Location<-factor(p.26.50.test$Location, levels=c(levels(commonavg.env$Location)))
#levels(p.26.50.test$Location)
#levels(commonavg.env$Location)
p.26.50.test$CO2<-450

p.26.50.test$Texture<-factor("loamy sand", levels = levels(commonavg.env$Texture))
p.26.50.test$Irrigation.Method<-factor(rep("Flood",45), levels = c(levels(commonavg.env$Irrigation.Method)))
predict(plsFit, newdata=p.26.50.test, ncomp=8)

####26.70####
p26.70.test<-prec.26.70.mean[-2]

p.26.70.test<-merge(sites.df, p26.70.test, by.x="Location",by.y="Name", all=T)[-3:-4]
p.26.70.test<-p.26.70.test[-1]
colnames(p.26.70.test)<-c("County", "Rain")
p.26.70.test$TMin.q1<-futurewheatclimate$tmin.26.70
p.26.70.test$TMax.q1<-futurewheatclimate$tmax.26.70
p.26.70.test$MAP<-futurewheatclimate$MAP.26.70
p.26.70.test$MAT<-futurewheatclimate$MAT.26.70
p.26.70.test$Texture<-"clay"
p.26.70.test$Texture<-factor(p.26.70.test$Texture, levels =c("clay", "clay loam", "loam", "loamy sand", "muck", "sandy loam","sily loam", "silty clay", "silty clay loam"))

str(p.26.70.test)
str(commonavg.env)
p.26.70.test$County<-factor(p.26.70.test$County)
#p.26.70.test$Year<-factor(c(rep(2050, 46)), levels =c("1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2050"))
#levels(p.26.70.test$Year)
#levels(commonavg.env$Year)

p.26.70.test<-p.26.70.test[-38,]
p.26.70.test$County<-factor(p.26.70.test$County, levels=c(levels(commonavg.env$County)))
levels(p.26.70.test$County)
levels(commonavg.env$County)

levels(commonavg.env$Irrigation.Method)
p.26.70.test$Irrigation.Method<-factor(rep("Drip",45), levels = c(levels(commonavg.env$Irrigation.Method)))
#p.26.70.test<-p.26.70.test[-7,]
#p.26.70.test$Location<-factor(p.26.70.test$Location, levels=c(levels(commonavg.env$Location)))
#levels(p.26.70.test$Location)
#levels(commonavg.env$Location)

p.26.70.test$CO2<-440
predict(plsFit, newdata=p.26.70.test, ncomp=8)

soils<-c("loamy sand", "loam", "clay")
irrigation<-c("None", "Drip", "Flood")
pred<-NULL
yield.26.50<-NULL
for(j in irrigation){
  for(i in soils){
    p.26.50.test$Texture<-factor(i, levels = levels(commonavg.env$Texture))
    p.26.50.test$Irrigation.Method<-factor(rep(j,45), levels = c(levels(commonavg.env$Irrigation.Method)))
    pred<-data.frame(predict(plsFit, newdata=p.26.50.test, ncomp=8))
    yield.26.50<-c(yield.26.50, pred)
  }
}

yield.26.50<-data.frame(yield.26.50)
colnames(yield.26.50)<-c("ls.N", "l.N", "c.N", "ls.D", "l.D", "c.D", "ls.F", "l.F", "c.F")
yield.26.50$County<-p.26.50.test$County
yield.26.50<-ddply(yield.26.50, .(County), summarise, 
                   Yield.26.50=mean(Yield.26.50))
yield.26.50<-ddply(yield.26.50, .(County),summarise,
                           ls.N=mean(ls.N),
                           l.N=mean(l.N),
                           c.N = mean(c.N),
                           ls.D=mean(ls.D),
                           l.D = mean(l.D),
                           c.D = mean(c.D),
                           ls.F=mean(ls.F),
                           l.F = mean(l.F),
                           c.F = mean(c.D))

shape.26.50<-merge(CA, yield.26.50[1:4], by.x="NAME", by.y="County")
lsN.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(shape.26.50)+tm_polygons(col = "ls.N",title = "Yield (lbs/acre)\nlsN RCP 2.6, 2050", palette = "BrBG", textNA ="No Data", breaks=c(-1000, 0, 1000, 2000, 3000,4000, 5000, 6000, 7000)) 
lsD.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(shape.26.50)+tm_polygons(col = "ls.D",title = "Yield (lbs/acre)\nlsD RCP 2.6, 2050", palette = "BrBG", textNA ="No Data", breaks=c(-1000, 0, 1000, 2000, 3000,4000, 5000, 6000, 7000)) 
lsF.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(shape.26.50)+tm_polygons(col = "ls.F",title = "Yield (lbs/acre)\nlsF RCP 2.6, 2050", palette = "BrBG", textNA ="No Data", breaks=c(-1000, 0, 1000, 2000, 3000,4000, 5000, 6000, 7000)) 
lN.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(shape.26.50)+tm_polygons(col = "l.N",title = "Yield (lbs/acre)\nlN RCP 2.6, 2050", palette = "BrBG", textNA ="No Data", breaks=c(-1000, 0, 1000, 2000, 3000,4000, 5000, 6000, 7000)) 
lD.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(shape.26.50)+tm_polygons(col = "l.D",title = "Yield (lbs/acre)\nlD RCP 2.6, 2050", palette = "BrBG", textNA ="No Data", breaks=c(-1000, 0, 1000, 2000, 3000,4000, 5000, 6000, 7000)) 
lF.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(shape.26.50)+tm_polygons(col = "l.F",title = "Yield (lbs/acre)\nlF RCP 2.6, 2050", palette = "BrBG", textNA ="No Data", breaks=c(-1000, 0, 1000, 2000, 3000,4000, 5000, 6000, 7000)) 
cN.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(shape.26.50)+tm_polygons(col = "c.N",title = "Yield (lbs/acre)\nCN RCP 2.6, 2050", palette = "BrBG", textNA ="No Data", breaks=c(-1000, 0, 1000, 2000, 3000,4000, 5000, 6000, 7000)) 
cD.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(shape.26.50)+tm_polygons(col = "c.D",title = "Yield (lbs/acre)\nCD RCP 2.6, 2050", palette = "BrBG", textNA ="No Data", breaks=c(-1000, 0, 1000, 2000, 3000,4000, 5000, 6000, 7000)) 
cF.26.50<-tm_shape(CA)+tm_borders(col = "black", alpha = 1)+tm_shape(shape.26.50)+tm_polygons(col = "c.F",title = "Yield (lbs/acre)\nCF RCP 2.6, 2050", palette = "BrBG", textNA ="No Data", breaks=c(-1000, 0, 1000, 2000, 3000,4000, 5000, 6000, 7000)) 
tmap_arrange(lsN.26.50,lsD.26.50,lsF.26.50,lN.26.50,lD.26.50,lF.26.50,cN.26.50,cD.26.50,cF.26.50)

####85.50####

soils<-c("loamy sand", "loam", "clay")
irrigation<-c("None", "Drip", "Flood")
pred<-NULL
yield.85.50<-NULL
for(j in irrigation){
  for(i in soils){
    p.85.50.test$Texture<-factor(i, levels = levels(commonavg.env$Texture))
    p.85.50.test$Irrigation.Method<-factor(rep(j,45), levels = c(levels(commonavg.env$Irrigation.Method)))
    pred<-data.frame(predict(plsFit, newdata=p.85.50.test, ncomp=8))
    yield.85.50<-c(yield.85.50, pred)
  }
}

yield.85.50<-data.frame(yield.85.50)
colnames(yield.85.50)<-c("ls.N", "l.N", "c.N", "ls.D", "l.D", "c.D", "ls.F", "l.F", "c.F")
yield.85.50$County<-p.85.50.test$County
yield.85.50<-ddply(yield.85.50, .(County),summarise,
                   ls.N=mean(ls.N),
                   l.N=mean(l.N),
                   c.N = mean(c.N),
                   ls.D=mean(ls.D),
                   l.D = mean(l.D),
                   c.D = mean(c.D),
                   ls.F=mean(ls.F),
                   l.F = mean(l.F),
                   c.F = mean(c.D))

shape.85.50<-merge(CA, yield.85.50[1:4], by.x="NAME", by.y="County")

####26.70####

soils<-c("loamy sand", "loam", "clay")
irrigation<-c("None")
rcp<-c("p.26.50.test","p.85.50.test","p.26.70.test","p.85.70.test")
rcp
pred<-NULL
yield.future<-NULL
rcpyield<-NULL
rcpdecile<-NULL
rcpindex<-NULL
for(j in irrigation){
  for(i in soils){
    for(k in rcp){
   yield.future$Texture<-factor(i, levels = levels(commonavg.env$Texture))
    yield.future$Irrigation.Method<-factor(rep(j,45), levels = c(levels(commonavg.env$Irrigation.Method)))
    pred<-data.frame(predict(plsFit, newdata=eval(parse(text = paste(k))), ncomp=8))
    pred.decile<-data.frame(pred, p.26.70.test$County)
    pred.decile<-ddply(pred.decile, .(p.26.70.test.County),summarise,
                       test=mean(Yield.8.comps))
    rcpdecile<- data.frame(ntile(pred.decile[2], 5)) 
    rcpindex<-data.frame(c(rcpindex, rcpdecile))
    rcpyield<-c(rcpyield, pred)
    }
  }
}

rcpyield<-data.frame(rcpyield)
colnames(rcpyield)<-c("ls2650", "ls8550", "ls2670", "ls8570","l2650", "l8550", "l2670", "l8570","c2650", "c8550", "c2670", "c8570")
rcpyield$County<-p.26.70.test$County
rcpyield<-ddply(rcpyield, .(County),summarise,
ls2650=mean(ls2650),
ls8550=mean(ls8550),
ls2670 = mean(ls2670),
ls8570=mean(ls8570),
l2650=mean(l2650),
l8550=mean(l8550),
l2670 = mean(l2670),
l8570=mean(l8570),
c2650=mean(c2650),
c8550=mean(c8550),
c2670 = mean(c2670),
c8570=mean(c8570))

library(dplyr)
rcpyield$ls2650quartile <- ntile(rcpyield$ls2650, 4)  
temp <- temp %>% mutate(quartile = ntile(value, 4))
                
temp$quartile <- with(temp, cut(value, 
                breaks=quantile(value, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                include.lowest=TRUE))

shape.26.70<-merge(CA, yield.26.70[1:4], by.x="NAME", by.y="County")



####85.70####

soils<-c("loamy sand", "loam", "clay")
irrigation<-c("None", "Drip", "Flood")
pred<-NULL
yield.85.70<-NULL
for(j in irrigation){
  for(i in soils){
    p.85.70.test$Texture<-factor(i, levels = levels(commonavg.env$Texture))
    p.85.70.test$Irrigation.Method<-factor(rep(j,45), levels = c(levels(commonavg.env$Irrigation.Method)))
    pred<-data.frame(predict(plsFit, newdata=p.85.70.test, ncomp=8))
    yield.85.70<-c(yield.85.70, pred)
  }
}

yield.85.70<-data.frame(yield.85.70)
colnames(yield.85.70)<-c("ls.N", "l.N", "c.N", "ls.D", "l.D", "c.D", "ls.F", "l.F", "c.F")
yield.85.70$County<-p.85.70.test$County
yield.85.70<-ddply(yield.85.70, .(County),summarise,
                   ls.N=mean(ls.N),
                   l.N=mean(l.N),
                   c.N = mean(c.N),
                   ls.D=mean(ls.D),
                   l.D = mean(l.D),
                   c.D = mean(c.D),
                   ls.F=mean(ls.F),
                   l.F = mean(l.F),
                   c.F = mean(c.D))

shape.85.70<-merge(CA, yield.85.70[1:4], by.x="NAME", by.y="County")

