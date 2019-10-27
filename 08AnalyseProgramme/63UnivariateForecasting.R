#62UnivariateForecasting.R
setwd('C:/Subversion/PRO/MCT/25Forecasting/01Transformierte')
load(file='PreparedDataAndWeeklySaisonality.rda')
length(FullTime)==nrow(Data)

plot(DF_AverageWeek$FullHours,DF_AverageWeek$AverageWeekPrice,type='l')

Univariate=Data[,'SYS']
plot(FullTime,Univariate,type='l')
points(FullTime,DecompositionModel_simple,type='l',col='red')

year=lubridate::year(FullTime)
start=min(which(year==2018))
ende=max(which(year==2018))

forecasthorizon=24 #in hours
# Naiv Model, and Simple Model

Naiv=list()
Simple=list()
EnsembleBaseline=list()
rollingForecastInd=1
TimeOfForcast=c()
for(i in start:ende){
  #TrainingData=Data[(i-(4*168)):(i-1),]
  RelvantRangeInd=(i):(i+forecasthorizon-1)
  TestTime=FullTime[RelvantRangeInd]
  TestData=Univariate[RelvantRangeInd]
  NaivForecastTmp=rep(Univariate[i-1],forecasthorizon)
  NaivForecast=data.frame(Time=TestTime,TestData=TestData,Forecast=NaivForecastTmp)
  Naiv[[rollingForecastInd]]=NaivForecast
  
  #Baseline
  simpleModel=data.frame(Time=TestTime,TestData=TestData,Forecast=DecompositionModel_simple[RelvantRangeInd])
  
  ens=data.frame(Time=TestTime,TestData=TestData,Forecast=0.6*DecompositionModel_simple[RelvantRangeInd]+0.4*NaivForecastTmp)
  
  Simple[[rollingForecastInd]]=simpleModel
  EnsembleBaseline[[rollingForecastInd]]=ens
  #naming of list
  TimeOfForcast[rollingForecastInd]=as.numeric(
      as.POSIXct(FullTime[min(RelvantRangeInd)-1],
                 origin='1970-01-01 00:00.00 UTC')
      )
  rollingForecastInd=rollingForecastInd+1
}
names(Simple)=TimeOfForcast
names(EnsembleBaseline)=TimeOfForcast
names(Naiv)=TimeOfForcast
## Evaluation
MAE=function(TestData,Forecast,InterestingRange){
  #beware: could be other mean definition too!!
  #(e.g. median,...)
  if(missing(InterestingRange)) InterestingRange=1:length(TestData)
  if(length(TestData)!=length(Forecast)) stop('Unequal length!')
  return(mean(abs(TestData-Forecast)[InterestingRange],na.rm = T))
}

NaivMAE=sapply(Naiv, function(x) return(MAE(x$TestData,x$Forecast,1:24)))
SimpleMAE=sapply(Simple, function(x) return(MAE(x$TestData,x$Forecast,1:24)))
EnsembleMAE=sapply(EnsembleBaseline, function(x) return(MAE(x$TestData,x$Forecast,1:24)))
MAE_all=cbind(NaivMAE,BaselineMAE,EnsembleMAE)
apply(MAE_all,2,mean)

MDplot(MAE_all,Scaling = 'Log',Ordering = 'Columnwise')

plot(as.numeric(names(Naiv)),NaivMAE,type='l',col='blue')
points(as.numeric(names(Simple)),SimpleMAE,type='l',main='Error per Time',ylab='MAE')
points(as.numeric(names(EnsembleBaseline)),EnsembleMAE,type='l',col='red')

##Multivariate Forecasting with Neural Networks ----

rollingForecastInd=1
TimeOfForcast=c()
ELM_ann=list()
MLP_ann=list()
for(i in start:ende){
  #Due to Ressource Limitations only a sample of the
  # nearest past data to the present was used:
  # last for weeks before present (i-1)
  TrainingData=as.data.frame(Data[(i-(4*168)):(i-1),])
  RelvantRangeInd=(i):(i+forecasthorizon-1)
  TestTime=FullTime[RelvantRangeInd]
  #Target Variable
  TestData=Univariate[RelvantRangeInd]
  ## Initialization of data frames
  ELM=data.frame(Time=TestTime,TestData=TestData,Forecast= rep(NaN,length(TestData)))
  MLP=data.frame(Time=TestTime,TestData=TestData,Forecast= rep(NaN,length(TestData)))
  
  
  #Predictors with assumption that they are
  #available PRIOR to the Target
  PredictorsTest=as.data.frame(Data[RelvantRangeInd,-1])
  # Model MLP
  try({#error handling
  model=neuralnet::neuralnet(data=TrainingData,SYS~.,hidden = 4,rep = 1)
  #Forecast MLP
  forecast=neuralnet::compute(model,PredictorsTest)
  MLP$Forecast=forecast$net.result
  })
  #Data Wrangling
  TrainingTime=as.numeric(FullTime[(i-(4*168)):(i-1)])
  PredictorTS = ts(data = TrainingData[,-1], start = TrainingTime[1])
  ResponseTS = ts(data = TrainingData[,1], start = TrainingTime[1])
  #Model extreme learning machines (ELM)
  try({#error handling
  modelScaled = nnfor::elm(ResponseTS, outplot = F, 
                           reps = 5, xreg = PredictorTS)
  #Forecast ELM
  PredictorTS_all = ts(data = rbind(PredictorTS,PredictorsTest), start = TrainingTime[1])
  predicted=forecast::forecast(modelScaled,h=forecasthorizon,xreg=PredictorTS_all)
  ELM$Forecast=predicted$mean
  })
  #Storing the Forecast systematically
  ELM_ann[[rollingForecastInd]]=ELM
  MLP_ann[[rollingForecastInd]]=MLP
  #naming of list
  TimeOfForcast[rollingForecastInd]=as.numeric(
    as.POSIXct(FullTime[min(RelvantRangeInd)-1],
               origin='1970-01-01 00:00.00 UTC')
  )
  rollingForecastInd=rollingForecastInd+1
}
names(ELM_ann)=TimeOfForcast
names(MLP_ann)=TimeOfForcast
setwd(outpath)
save(file='ElectricityPrice_ANN_Forecasts.rda',ELM_ann,MLP_ann)

elmMAE=sapply(ELM_ann, function(x) return(MAE(x$TestData,x$Forecast,1:24)))
mlpMAE=sapply(MLP_ann, function(x) return(MAE(x$TestData,x$Forecast,1:24)))
MAE_all=cbind(NaivMAE,BaselineMAE,EnsembleMAE,elmMAE,mlpMAE)

apply(MAE_all,2,mean)
MDplot(MAE_all,Scaling = 'Log',Ordering = 'Columnwise')

setwd('C:/Subversion/PRO/MCT/25Forecasting/02Forecasts')

Testdaten=lapply(Naiv, function(x) return(x$TestData))
AverageHourlyFluctuation=sapply(Testdaten, function(x) mean(abs(diff(x))))

save(file='ElectricityPrice_Benchmarking.rda',MAE_all,mlpMAE,elmMAE,NaivMAE,SimpleMAE,EnsembleBaseline,AverageHourlyFluctuation)

MDplot(cbind(MAE_all,AverageHourlyFluctuation),Scaling = 'Log',Ordering = 'Columnwise')

#Halde
setwd('C:/Subversion/PRO/MCT/25Forecasting/01Transformierte')
V=ReadDates('Daily_ScandinavianElectricityPrices')#,DailyData,Comments = 'Averaged per day, units: EUR/MWh')
DF=as.data.frame(V)
Time=DF$Time
# disregards physical capacity limitations between nordic bitting areas
# while the capacity limitations in/oput nordic are considered. 
# main reference for traded long term financial contracts
SystemPrice=as.numeric(DF$SYS)

TSAT::PlotTimeSeries(Time,SystemPrice,'Dates','EUR/MWh',main = 'System Price')

year=lubridate::year(Time)
ind=which(year<2018)
max(ind)

DF$standSys=RobustNormalization(DF$SYS,Capped = T)
FF=TSAT::DecompositionModelByRegression(DF,FeatureName = 'standSys',SplitDataAt = 1826,ForecastPeriods = 365,PlotIt = T)
