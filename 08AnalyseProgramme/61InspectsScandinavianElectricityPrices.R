#61InspectsScandinavianElectricityPrices.R
setwd('C:/Subversion/PRO/MCT/25Forecasting/09Originale')
V=ReadDates('ScandinavianElectricityPrices')
DF=as.data.frame(V)
FullTime=strptime(paste(as.character(DF$Time),DF$Hours),'%Y-%m-%d %H',tz = 'UTC')
Data=as.matrix(DF[,-c(1,2)])
mode(Data)="numeric"
PlotMissingvalues(Data)
colnames(Data)
Data=Data[,-16]#LV
apply(Data, 2, range,na.rm=T)
Data=Data[,-c(7,8)]

Pixelmatrix(Data)

apply(Data, 2, FUN = function(x) return(sum(!is.finite(x))))

ind=which(!is.finite(Data[,'SYS']))
Data[ind,]
FullTime[ind]
#ForwardFill
Data[ind,]=Data[ind-1,]

##Seasonalities ----
Restructure_Split=function(Liste){
  return(lapply(Liste,function(x){
    hours=1:25
    n=length(x)
    x=t(as.matrix(x))
    colnames(x)=hours[1:n]
    DF=as.data.frame(x)
    return(DF)
  }))
}

days=lubridate::wday(FullTime,week_start = 1)
SysPerDay=c()
for(i in 1:7){
  ind_mo=which(days==i)
  wTime=FullTime[ind_mo]
  wData=Data[ind_mo,]
  Time = cut(wTime, breaks = "days")
  wSYS=split(x = wData[,"SYS"],f = Time,drop = T)
  wSYS_df=Restructure_Split(wSYS)
  SysPerDay[[i]]=data.table::rbindlist(l = wSYS_df,fill=TRUE)
}
sapply(SysPerDay,dim)#Zeitumstellung!
AverageWeek=unlist(lapply(SysPerDay,function(x) apply(x,2,median,na.rm=T)))

p <- plotly::plot_ly() 
p <- plotly::add_lines(p,x = 1:length(AverageWeek), y = ~AverageWeek,name='Mondays')
p <-  plotly::layout(p,
                     title = 'Sys Seasonality Pattern: Week', 
                     xaxis = list(title='Mo-So in Hours'),
                     yaxis= list(
                       title='Euro/MWh',
                       showgrid=FALSE
                     )
)
p
X=c()
Y=c()
for(i in 1:7){
  x=SysPerDay[[i]]
  x$Id=1:nrow(x)
  dfmweek=reshape2::melt(x,id="Id")
  dfmweek$variable=as.numeric(as.character(dfmweek$variable))
  X=c(X,dfmweek$variable+(i-1)*24)
  Y=c(Y,dfmweek$value)
}  

out=DataVisualizations::PDEscatter(X,Y,na.rm = T,ylim=c(min(Y,na.rm=T),65),xlab='Mo-So in Hours',ylab="Euro/MWh",main='Density Estimation of Saisonality Pattern',DrawTopView=FALSE,Plotter = 'plotly')
out$Handle
plotly::layout(out$Handle,scene = list(yaxis=list(range=c(0,50))))
# plot(MondaysSYS[[1]],type='l',xlim = c(1,24),ylim=c(10,70))
# for(i in 1:length(MondaysSYS)){
#   points(MondaysSYS[[i]],type='l',xlim = c(1,24),ylim=c(10,70))
# }
# GGplotDF=as.data.frame(do.call(rbind,MondaysSYS))
# 
# AverageMonday=apply(GGplotDF,2,median)
# GGplotDF$ID=1:nrow(GGplotDF)
# dfm=reshape2::melt(GGplotDF,id="ID")
# library(ggplot2)
# ggobject=ggplot(dfm, aes_string(y = 'value', x = 'variable',group='ID'))+geom_line()
# ggobject



ind_so=which(days==7)
SundaysTime=FullTime[ind_so]
SundaysData=Data[ind_so,]
Time = cut(SundaysTime, breaks = "days")
SundaysSYS=split(x = SundaysData[,"SYS"],f = Time,drop = T)
plot(SundaysSYS[[1]],type='l',xlim = c(1,24),ylim=c(10,70))
for(i in 1:length(SundaysSYS)){
  points(SundaysSYS[[i]],type='l',xlim = c(1,24),ylim=c(10,70))
}
GGplotDF=as.data.frame(do.call(rbind,SundaysSYS))
AverageSunday=apply(GGplotDF,2,median)
GGplotDF$ID=1:nrow(GGplotDF)
dfm=reshape2::melt(GGplotDF,id="ID")
library(ggplot2)
ggobject=ggplot(dfm, aes_string(y = 'value', x = 'variable',group='ID'))+geom_line()
ggobject

plot(AverageMonday[1:24],type='l',col='red',ylab='Euro/MWh',xlab='Hours',main='Monday (red) versus Sunday(blue))')
points(AverageSunday[1:24],type='l',col='blue')

p <- plotly::plot_ly() 
p <- plotly::add_lines(p,x = 1:24, y = ~AverageMonday[1:24],name='Mondays')
p <- plotly::add_lines(p,x = 1:24, y = ~AverageSunday[1:24],name='Sundays')
p <-  plotly::layout(p,
                     title = 'Average Daily Seasonalitiy', 
                     xaxis = list(title='Hours'),
                     yaxis= list(
                       title='Euro/MWh',
                       showgrid=FALSE
                     )
)
p
# Daily Resolution ----
DailyData=TSAT::aggregateTime2Days(Time = FullTime,Data = Data,FUN = mean)

Time = cut(DailyData$Time, breaks = "weeks")
WeeksSYS=split(x = DailyData$SYS,f = Time,drop = T)

#Woche startet immer am Montag
unique(lubridate::wday(as.Date(names(WeeksSYS)),week_start =1))

#Annahme: Regulaere Zeitreihe
WeeksSYS2=lapply(WeeksSYS,function(x){
  string=c('MO','Di','Mi','DO','FR','SA','SO')
  n=length(x)
  # names(x)=string[1:n]
  x=t(as.matrix(x))
  colnames(x)=string[1:n]
  DF=as.data.frame(x)
  return(DF)
})
PerDay=data.table::rbindlist(l = WeeksSYS2,fill=TRUE)
PerDay$Weekstart=as.Date(names(WeeksSYS))
#rowr::cbind.fill()plyr
#GGplotDFweeks=rapply(f = function(x,y) return(merge(x,y,'Weekday','Weekday')),object = WeeksSYS2)
# GGplotDFweeks=rapply(f = function(x) return(merge(x,WeeksSYS2[[1]],by.x='Weekday',by.y='Weekday')),object = WeeksSYS2)

# x=data.table::rbindlist(l = WeeksSYS2,fill=TRUE)

x=TSAT::Time2Classification(dfmweek$variable)
#x=JitterUniqueValues(x,NULL,0.9,1.1)
y=dfmweek$value
# plot(xprior,y)
out=DataVisualizations::PDEscatter(x,y,na.rm = T,xlab = 'Weekdays,1=Mo,...,7=SO',ylab='Euro/MWh',main = 'Density Estimation of Weekly Saisonality')


DataVisualizations::PDEscatter()

AverageWeek=apply(PerDay[,1:7],2,median,na.rm=T)
PerDay$ID=1:nrow(PerDay)
dfmweek=reshape2::melt(PerDay,id="Weekstart")
x=TSAT::Time2Classification(dfmweek$variable)
#x=JitterUniqueValues(x,NULL,0.9,1.1)
y=dfmweek$value
# plot(xprior,y)
out=DataVisualizations::PDEscatter(x,y,na.rm = T,xlab = 'Weekdays,1=Mo,...,7=SO',ylab='Euro/MWh',main = 'Density Estimation of Weekly Saisonality')
# points(1:7,AverageWeek,col='blue',type='l')
# library(ggplot2)
# ggobject=ggplot(dfmweek, aes_string(y = 'value', x = 'variable',group='Weekstart'))+geom_line()
# ggobject

p <- plotly::plot_ly() 
p <- plotly::add_lines(p,x = 1:7, y = ~AverageWeek,name='Sys')
p <-  plotly::layout(p,
                     title = 'Average Weekly Seasonalitiy', 
                     xaxis = list(title='Weekdays,1=Mo,...,7=SO'),
                     yaxis= list(
                       title='Euro/MWh',
                       showgrid=FALSE
                     )
)
p

DataVisualizations::DualaxisLinechart(DailyData$Time,DailyData$SYS,DailyData$SE1,y1lab = 'SYS, EUR/MWh',y2lab = 'SE1, EUR/MWh')

Pixelmatrix(DailyData[,-1],main = 'Average per Day is Higly Similar')

setwd('C:/Subversion/PRO/MCT/25Forecasting/01Transformierte')
WriteDates('Daily_ScandinavianElectricityPrices',DailyData,Comments = 'Averaged per day, units: EUR/MWh')
