#60PrepareNordpoolData.R

setwd('C:/Subversion/PRO/MCT/25Forecasting/99RawData/Nordpool')
DF2013=read.csv('elspot-prices_2013_hourly_eur.csv',header = T,sep = ';',dec = ',',stringsAsFactors = F)
ncol(DF2013)
DF2013$ELE=NULL
DF2014=read.csv('elspot-prices_2014_hourly_eur.csv',header = T,sep = ';',dec = ',',stringsAsFactors = F)
ncol(DF2014)
DF2015=read.csv('elspot-prices_2015_hourly_eur.csv',header = T,sep = ';',dec = ',',stringsAsFactors = F)
ncol(DF2015)
DF2015$ELE=NULL
DF2015$FRE=NULL
DF2016=read.csv('elspot-prices_2016_hourly_eur.csv',header = T,sep = ';',dec = ',',stringsAsFactors = F)
ncol(DF2016)
DF2016$ELE=NULL
DF2016$FRE=NULL
DF2017=read.csv('elspot-prices_2017_hourly_eur.csv',header = T,sep = ';',dec = ',',stringsAsFactors = F)

ncol(DF2017)
DF2018=read.csv('elspot-prices_2018_hourly_eur.csv',header = T,sep = ';',dec = ',',stringsAsFactors = F)
ncol(DF2018)
DF2019=read.csv('elspot-prices_2019_hourly_eur.csv',header = T,sep = ';',dec = ',',stringsAsFactors = F)
ncol(DF2019)
DF2019$AT=NULL
DF2019$BE=NULL
DF2019$DE.LU=NULL
DF2019$NL=NULL
DF2019$FR=NULL
DF=rbind(DF2013,DF2014,DF2015,DF2016,DF2017,DF2018,DF2019)

DF$Date=as.Date(strptime(x = DF$Date,'%d.%m.%Y',tz = 'UTC'))
DF$Hours=substr(DF$Hours,1,2)
FullTime=strptime(paste(as.character(DF$Date),DF$Hours),'%Y-%m-%d %H',tz = 'UTC')
Comments='sys - system price it disregards physical capacity limitations between nordic bitting areas while the capacity limitations in/oput nordic are considered. 
the system price for each hour is based on the intersection of the aggregated supply and demand curves representing
all bids and offers in the enitre nordic market.
it is the main reference for traded long term financial contracts
NO: OSLO, KR.Sand,Bergen, TR.Heim, MOLDE, TROMSO
SE: 1-4
DFK 1-2;
Hour is starting HOUR;
https://www.nordpoolgroup.com/historical-market-data/
'
DF$Time=DF$Date
DF$Date=NULL
setwd('C:/Subversion/PRO/MCT/25Forecasting/09Originale')
WriteDates('ScandinavianElectricityPrices',TSdata = DF,Comments = Comments)
# FullTimeSecs=as.POSIXct(FullTime)
#lubridate::wday(FullTime)

setwd('C:/Subversion/PRO/MCT/25Forecasting/09Originale')
V=ReadDates('ScandinavianElectricityPrices')
