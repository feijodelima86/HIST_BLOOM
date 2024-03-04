library(gbm)
library(dismo)
library(MASS)
library(usdm)
library(dplyr)
library(cowplot)
library(randomForest)

ucfr.data <- read.csv("2_Incremental/CODE_3.0/Full_Dataset_manual.csv")

names(ucfr.data)

ucfr.data<-ucfr.data[,c(12,11,17,9,16,21,19)]  


#fit random forest model
model <- randomForest(
  formula = CHLa ~ ., 
  na.action=na.omit,
  data = ucfr.data,
  mtry=2,
  ntree=500,
  nodesize=12,
  replace=TRUE
  )

model

plot(model)


varImpPlot(model) 

model_tuned <- tuneRF(
  x=ucfr.data[,-7],
  y=ucfr.data$CHLa,
  ntreeTry=10000,
  mtryStart=3,
  stepFactor=2,
  improve=0.01,
  trace=T #don't show real-time progress
)



model2 <- randomForest(
  x=ucfr.data[,-7],
  y=ucfr.data$CHLa,
  ntreeTry=10000,
  mtryStart=1,
  stepFactor=5,
  improve=0.01,
  trace=T #don't show real-time progress
)

model2

partialPlot(model_tuned, ucfr.data[,1:6], x.var = "TP_mg_L")
partialPlot(model_tuned, ucfr.data[,1:6], x.var = "TN_mg_L")
partialPlot(model_tuned, ucfr.data[,1:6], x.var = "Q.Anomaly")
partialPlot(model_tuned, ucfr.data[,1:6], x.var = "Temp_oC")
partialPlot(model_tuned, ucfr.data[,1:6], x.var = "Days.Since.Freshet")
partialPlot(model_tuned, ucfr.data[,1:6], x.var = "NP")


#replace NAs with column medians
for(i in 1:ncol(ucfr.data)) {
  ucfr.data[ , i][is.na(ucfr.data[ , i])] <- median(ucfr.data[ , i], na.rm=TRUE)
}

