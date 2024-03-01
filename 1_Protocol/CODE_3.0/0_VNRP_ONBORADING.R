library(readr)
library(tidyr)
library(zoo)
library(dplyr)

ALLDATA <- data.frame(read_csv("0_Data/VNRP_DATA/resultphyschem.csv"))

names(ALLDATA)

VNRP <- ALLDATA[,c("ActivityStartDate",	
                "ActivityStartTime.Time",
                "MonitoringLocationIdentifier",	
                "MonitoringLocationName",	
                "ActivityCommentText",	
                "ResultIdentifier",	
                "ResultDetectionConditionText",	
                "MethodSpeciationName",	
                "CharacteristicName",	
                "ResultSampleFractionText",	
                "ResultMeasureValue",	
                "MeasureQualifierCode")]


#VNRP$ResultMeasureValue[is.na(VNRP$ResultMeasureValue)] <- 0

test <- VNRP %>% pivot_wider(names_from = CharacteristicName, values_from = ResultMeasureValue)

test$ActivityStartDate <-as.Date(test$ActivityStartDate, "%m/%d/%Y")

test[['date_yearmon']] <- as.yearmon(test[['ActivityStartDate']])

#test$ActivityStartDate <-as.numeric(test$ActivityStartDate)

names(test)

df2<-data.frame(test[,c(1,27,4,11:26,45)])

df2$date_yearmon <- as.factor(df2$date_yearmon)

df2$MonitoringLocationName <- as.factor(df2$MonitoringLocationName)

test3 <-aggregate(x = df2[ ,colnames(df2) != c("date_yearmon","MonitoringLocationName")],             # Sum by group
          by = list(df2$date_yearmon,df2$MonitoringLocationName),
          FUN = mean,
          na.rm = TRUE)
warnings()

write.csv(test3, "2_Incremental/CODE_3.0/VNRP_ONBOARDED.csv")

