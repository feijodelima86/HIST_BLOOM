library(readr)
library(tidyr)
library(zoo)
library(dplyr)
library(lubridate) 

VNRP <- data.frame(read_csv("0_Data/VNRP_NUTRIENT_GIMMICK_3.csv"))

VNRP$ActivityStartDate <-as.Date(VNRP$ActivityStartDate, "%m/%d/%Y")

VNRP[['date_yearmon']] <- as.yearmon(VNRP[['ActivityStartDate']])

VNRP <-aggregate(x = VNRP[ ,colnames(VNRP) != c("date_yearmon","MonitoringLocationName","CharacteristicName")],             # Sum by group
                  by = list(VNRP$date_yearmon,VNRP$MonitoringLocationName, VNRP$CharacteristicName),
                  FUN = mean,
                  na.rm = TRUE)
head(VNRP)

DF<-VNRP[,c(1:5)]

names(DF)<-c("date_yearmon", "Site", "Variable", "ActivityStartDate", "ResultMeasureValue")

test <- DF %>% pivot_wider(names_from = Variable, values_from = ResultMeasureValue)

write.csv(test, "2_Incremental/testnut.csv")


### PUll out chl data

names(test)

df2<-data.frame(test[,c(3,1,2,5,16)])

df2<-df2[!is.na(df2$"Chlorophyll.a..corrected.for.pheophytin"),]

df2$year <- year(df2$ActivityStartDate)

df3<-data.frame(test[,c(3,1,2,7,8,9,11,13,14)])

df3<-df3[!is.na(df3$"Orthophosphate"),]

df3<-df3[!is.na(df3$"Nitrate...Nitrite"),]

df3$month<-month(df3$ActivityStartDate)

df3$year <- year(df3$ActivityStartDate)

df3.1<- subset(df3, month == 6)[,c(3,4:9,11)]

df3.2<- subset(df3, month == 7)[,c(3,4:9,11)]

df3.3<- subset(df3, month == 8)[,c(3,4:9,11)]

df3.4<- subset(df3, month == 9)[,c(3,4:9,11)]

test_merge<-merge(x = df2, y = df3.1, by = c("year","Site"), all = TRUE)
test_merge<-merge(x = test_merge, y = df3.2, by = c("year","Site"), all = TRUE)
test_merge<-merge(x = test_merge, y = df3.3, by = c("year","Site"), all = TRUE)
test_merge<-merge(x = test_merge, y = df3.4, by = c("year","Site"), all = TRUE)

write.csv(test_merge, "2_Incremental/testlag.csv")

df4<-data.frame(test[,c(3,1,2,8)])

df4<-df4[!is.na(df4$"Orthophosphate"),]

df4$month<-month(df4$ActivityStartDate)

df4.1<- subset(df4, month == 6)

df4.2<- subset(df4, month == 7)

write.csv(df4.2, "2_Incremental/SRP.csv")

library(lubridate)
d <- ymd("2012-01-31")
d %m+% months(1)

df2[['date_yearmon']] <- as.yearmon(df2[['ActivityStartDate']])

df2$date_yearmon <- as.factor(df2$date_yearmon)

df2$MonitoringLocationName <- as.factor(df2$MonitoringLocationName)

df2$month<-month(df2$ActivityStartDate)
df2$day <- format(df2$ActivityStartDate, "%d")
df2$year <- year(df2$ActivityStartDate)

test3 <-aggregate(x = df2[ ,colnames(df2) != c("date_yearmon","MonitoringLocationName")],             # Sum by group
                  by = list(df2$date_yearmon,df2$MonitoringLocationName),
                  FUN = mean,
                  na.rm = TRUE)
