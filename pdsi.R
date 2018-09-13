#PDSI
library(devtools)
install_github("cszang/pdsi")
#(on Linux)
library(pdsi)
pdsi::build_linux_binaries()

# from PRISM extracts script
str(PPT.df)
str(results.df)
#from SSURGO script
str(awc)

list<-levels(awc$Name)
list
TEMP.df<-results.df[1:420,]
PPT.df<-PPT.df[1:420,]
input<-NULL
result<-NULL
avg<-NULL
pdsi.all<-NULL
for(i in 1:46){
  input<- data.frame(PPT.df$Year, rep(1:12, 35), TEMP.df[i],PPT.df[i])
  colnames(input)<-c("year", "month", "temp", "prec")
  result<-pdsi(awc=awc[i,5], lat=awc[i,3], input,1982, 2015, "scpdsi")
  avg<-rowMeans(result[-1])
  pdsi.all<-data.frame(cbind(avg, pdsi.all))
  assign(noquote(paste0("pdsi.", list[i])), data.frame(avg, result[1]))
}
pdsi.df<-data.frame(pdsi.all)
colnames(pdsi.df)<-list
pdsi.df$Year<-1982:2015
str(pdsi.df)

pdsi.reshape<-reshape(pdsi.df, 
                     idvar="Year", ids = "Year",
                     times=names(pdsi.df[-47]), timevar = "Location",
                     varying=list(names(pdsi.df[-47])), v.names="pdsi", 
                     direction = "long")
str(pdsi.reshape)
pdsi.reshape<-merge(pdsi.reshape, LtoCounty)
plot(pdsi~Year, pdsi.reshape[pdsi.reshape$Location=="Kings",])

####PDSI q1####
input<-NULL
result<-NULL
avg<-NULL
pdsi.all<-NULL
for(i in 1:46){
  input<- data.frame(PPT.df$Year, rep(1:12, 35), TEMP.df[i],PPT.df[i])
  colnames(input)<-c("year", "month", "temp", "prec")
  result<-pdsi(awc=awc[i,5], lat=awc[i,3], input,1982, 2015, "scpdsi")
  avg<-rowMeans(result[2:4])
  pdsi.all<-data.frame(cbind(avg, pdsi.all))
  assign(noquote(paste0("pdsi.", list[i])), data.frame(avg, result[1]))
}
pdsi.df<-data.frame(pdsi.all)
colnames(pdsi.df)<-list
pdsi.df$Year<-1982:2015
str(pdsi.df)

pdsi.q1<-reshape(pdsi.df, 
                      idvar="Year", ids = "Year",
                      times=names(pdsi.df[-47]), timevar = "Location",
                      varying=list(names(pdsi.df[-47])), v.names="pdsi.q1", 
                      direction = "long")
str(pdsi.q1)
pdsi.q1<-merge(pdsi.q1, LtoCounty)
plot(pdsi.q1$pdsi~pdsi.reshape$pdsi)

####PDSI q4####
input<-NULL
result<-NULL
avg<-NULL
pdsi.all<-NULL
for(i in 1:46){
  input<- data.frame(PPT.df$Year, rep(1:12, 35), TEMP.df[i],PPT.df[i])
  colnames(input)<-c("year", "month", "temp", "prec")
  result<-pdsi(awc=awc[i,5], lat=awc[i,3], input,1982, 2015, "scpdsi")
  avg<-rowMeans(result[11:13])
  pdsi.all<-data.frame(cbind(avg, pdsi.all))
  assign(noquote(paste0("pdsi.", list[i])), data.frame(avg, result[1]))
}
pdsi.df<-data.frame(pdsi.all)
colnames(pdsi.df)<-list
pdsi.df$Year<-1982:2015
str(pdsi.df)

pdsi.q4<-reshape(pdsi.df, 
                 idvar="Year", ids = "Year",
                 times=names(pdsi.df[-47]), timevar = "Location",
                 varying=list(names(pdsi.df[-47])), v.names="pdsi.q4", 
                 direction = "long")
str(pdsi.q4)
pdsi.q4<-merge(pdsi.q4, LtoCounty)
plot(pdsi.q1$pdsi~pdsi.q4$pdsi.q4)

pdsi.all<-merge(pdsi.reshape, pdsi.q1)
pdsi.all<-merge(pdsi.all, pdsi.q4)
str(pdsi.all)
