#65MultivariateForecasting.R
# performed in microsoft azure VM:
# F72s_v2, Compute optimized , 72 cores	 144 GB RAM, 32 DataDists, 80000 Max IOPS
# takes around 1 hour to compute
inpath='C:/Subversion/PRO/MCT/25Forecasting/01Transformierte'
outpath='C:/Subversion/PRO/MCT/25Forecasting/02Forecasts'
setwd(inpath)
load(file = 'PreparedDataAndWeeklySaisonality.rda')
length(FullTime) == nrow(Data)
Univariate = Data[, 'SYS']
plot(FullTime, Univariate, type = 'l')
points(FullTime,
       DecompositionModel_simple,
       type = 'l',
       col = 'red')
forecasthorizon = 24 #in hours
rollingForecastInd = 1
TimeOfForcast = c()
ELM_ann = list()
MLP_ann = list()
library(parallel)
Workers=parallel::detectCores(-1)
cl=parallel::makeCluster(Workers)

# for (i in start:ende) {
Liste=parLapply(cl,start:ende,fun = function(i,Data,forecasthorizon,FullTime){
  library(neuralnet)
  library(forecast)
  library(nnfor)
  Univariate=Data[,'SYS']
  #Due to Ressource Limitations only a sample of the
  # nearest past data to the present was used:
  # last for weeks before present (i-1)
  TrainingData = as.data.frame(Data[(i - (4 * 168)):(i - 1), ])
  RelvantRangeInd = (i):(i + forecasthorizon - 1)
  TestTime = FullTime[RelvantRangeInd]
  #Target Variable
  TestData = Univariate[RelvantRangeInd]
  ## Initialization of data frames
  ELM = data.frame(
    Time = TestTime,
    TestData = TestData,
    Forecast = rep(NaN, length(TestData))
  )
  MLP = data.frame(
    Time = TestTime,
    TestData = TestData,
    Forecast = rep(NaN, length(TestData))
  )
  
  
  #Predictors with assumption that they are
  #available PRIOR to the Target
  PredictorsTest = as.data.frame(Data[RelvantRangeInd, -1])
  # Model MLP
  try({
    #error handling
    model = neuralnet::neuralnet(data = TrainingData,
                                 SYS ~ .,
                                 hidden = 4,#usually more than one layer required!
                                 rep = 3)
    #Forecast MLP
    forecast = neuralnet::compute(model, PredictorsTest)
    MLP$Forecast = forecast$net.result
  })
  #Data Wrangling
  TrainingTime = as.numeric(FullTime[(i - (4 * 168)):(i - 1)])
  PredictorTS = ts(data = TrainingData[, -1], start = TrainingTime[1])
  ResponseTS = ts(data = TrainingData[, 1], start = TrainingTime[1])
  #Model extreme learning machines (ELM)
  try({
    #error handling
    modelScaled = nnfor::elm(ResponseTS,
                             outplot = F,
                             reps = 5,
                             xreg = PredictorTS)
    #Forecast ELM
    PredictorTS_all = ts(data = rbind(PredictorTS, PredictorsTest),
                         start = TrainingTime[1])
    predicted = forecast::forecast(modelScaled, h = forecasthorizon, xreg =
                                     PredictorTS_all)
    ELM$Forecast = predicted$mean
  })
  #Storing the Forecast systematically
  # ELM_ann[[rollingForecastInd]] = ELM
  # MLP_ann[[rollingForecastInd]] = MLP
  #naming of list
  TimeOfForcast = as.numeric(as.POSIXct(FullTime[i -1],origin = '1970-01-01 00:00.00 UTC'))
  # TimeOfForcast[rollingForecastInd] = as.numeric(as.POSIXct(FullTime[i -
  #                                                                      1],
  #                                                           origin = '1970-01-01 00:00.00 UTC'))
  # rollingForecastInd = rollingForecastInd + 1
  return(list(ELM_ann=ELM,MLP_ann=MLP,TimeOfForcast=TimeOfForcast))
},Data,forecasthorizon,FullTime)
setwd(outpath)
save(file = 'ElectricityPrice_ANN_Forecasts.rda', Liste)

ELM_ann=lapply(Liste, '[[',1)
MLP_ann=lapply(Liste, '[[',2)
TimeOfForcast=unlist(lapply(Liste, '[[',3))
names(ELM_ann) = TimeOfForcast
names(MLP_ann) = TimeOfForcast
setwd(outpath)
save(file = 'ElectricityPrice_ANN_Forecasts.rda', ELM_ann, MLP_ann,Liste)
