library(readr)
library(tidyr)
library(zoo)
library(dplyr)
library(lubridate)

### Loading data ###

ALLDATA <- data.frame(read_csv("0_Data/VNRP_DATA/resultphyschem_CORR.csv"))

names(ALLDATA)

VNRP <- ALLDATA[,c("ActivityStartDate",	"MonitoringLocationName",	"CharacteristicName",	"ResultMeasureValue", "ResultMeasure.MeasureUnitCode")]

# Multiply ResultMeasureValue by 1000 where MeasureUnitCode is "ug/L" and Value is not NA

VNRP$ResultMeasureValue[VNRP$ResultMeasure.MeasureUnitCode == "ug/L" & !is.na(VNRP$ResultMeasureValue)] <-   VNRP$ResultMeasureValue[VNRP$ResultMeasure.MeasureUnitCode == "ug/L" & !is.na(VNRP$ResultMeasureValue)] / 1000

VNRP <- VNRP[,c("ActivityStartDate",	"MonitoringLocationName",	"CharacteristicName",	"ResultMeasureValue")]

VNRP$ActivityStartDate <-as.Date(VNRP$ActivityStartDate, "%m/%d/%Y")

VNRP[['date_yearmon']] <- as.yearmon(VNRP[['ActivityStartDate']])

### Averaging all data within year and month of collection ?

VNRP <-aggregate(x = VNRP[ ,colnames(VNRP) != c("date_yearmon","MonitoringLocationName","CharacteristicName")],             # Sum by group
                 by = list(VNRP$date_yearmon,VNRP$MonitoringLocationName, VNRP$CharacteristicName),
                 FUN = mean,
                 na.rm = TRUE)

### 

DF<-VNRP[,c(1:5)]

names(DF)<-c("date_yearmon", "Site", "Variable","ActivityStartDate", "ResultMeasureValue")

DF_WIDE <- DF %>% pivot_wider(names_from = Variable, values_from = ResultMeasureValue)

names(DF_WIDE)

DF_WIDE <- DF_WIDE[,c("date_yearmon", 
                      "Site", 
                      "Ammonia", 
                      "Nitrate + Nitrite", 
                      "Orthophosphate",
                      "pH", 
                      "Specific conductance", 
                      "Temperature, water", 
                      "Total dissolved solids", 
                      "Total Phosphorus, mixed forms", 
                      "Total Nitrogen, mixed forms", 
                      "Turbidity",
                      "Weight",
                      "Chlorophyll a, corrected for pheophytin",
                      "ActivityStartDate"
)]

DF_WIDE_AGG <-aggregate(x = DF_WIDE[ ,colnames(DF_WIDE) != c("date_yearmon","Site")],             # Sum by group
                        by = list(DF_WIDE$date_yearmon,DF_WIDE$Site),
                        FUN = mean,
                        na.rm = TRUE)


names(DF_WIDE_AGG)[1]<-names(DF)[1]
names(DF_WIDE_AGG)[2]<-names(DF)[2]

write.csv(DF_WIDE_AGG, "2_Incremental/DF_WIDE_NUT_AGG.csv")

names(DF_WIDE_AGG)

df2<-data.frame(DF_WIDE_AGG[,c(1,2,13,14,15)])

df2<-df2[!is.na(df2$"Chlorophyll.a..corrected.for.pheophytin"),]



df2$year <- year(as.Date(df2$date_yearmon))

df2$month <- month(as.Date(df2$date_yearmon))

df2$day <- day(df2$date_yearmon)

df3<-data.frame(DF_WIDE_AGG[,c(1,2,3:12)])

df3$month<-month(as.Date(df3$date_yearmon))

df3$year <- year(as.Date(df3$date_yearmon))

### Month of June ###

df3.1<- subset(df3, month == 6)[,c(2,3:12,14)]

df3.1 <-aggregate(x = df3.1[ ,colnames(df3.1) != c("year","Site")],             # Sum by group
                  by = list(df3.1$year,df3.1$Site),
                  FUN = mean,
                  na.rm = TRUE)

df3.1 <- df3.1[,-c(3,14)] 
names(df3.1)[1]<-"year"
names(df3.1)[2]<-"Site"

DF_WIDE_merge.1<-merge(x = df2, y = df3.1, by = c("year","Site"), all = TRUE)

DF_WIDE_merge.1<-DF_WIDE_merge.1[!is.na(DF_WIDE_merge.1$"Chlorophyll.a..corrected.for.pheophytin"),]

### Month of July ###

df3.2<- subset(df3, month == 7)[,c(2,3:12,14)]

df3.2 <-aggregate(x = df3.2[ ,colnames(df3.2) != c("year","Site")],             # Sum by group
                  by = list(df3.2$year,df3.2$Site),
                  FUN = mean,
                  na.rm = TRUE)

df3.2 <- df3.2[,-c(3,14)]
names(df3.2)[1]<-"year"
names(df3.2)[2]<-"Site"

DF_WIDE_merge.2<-merge(x = DF_WIDE_merge.1, y = df3.2, by = c("year","Site"), all = TRUE)

DF_WIDE_merge.2<-DF_WIDE_merge.2[!is.na(DF_WIDE_merge.2$"Chlorophyll.a..corrected.for.pheophytin"),]

### Month of August ###

df3.3<- subset(df3, month == 8)[,c(2,3:12,14)]

df3.3 <-aggregate(x = df3.3[ ,colnames(df3.3) != c("year","Site")],             # Sum by group
                  by = list(df3.3$year,df3.3$Site),
                  FUN = mean,
                  na.rm = TRUE)

df3.3 <- df3.3[,-c(3,14)]
names(df3.3)[1]<-"year"
names(df3.3)[2]<-"Site"

DF_WIDE_merge.3<-merge(x = DF_WIDE_merge.2, y = df3.3, by = c("year","Site"), all = TRUE)

DF_WIDE_merge.3<-DF_WIDE_merge.3[!is.na(DF_WIDE_merge.3$"Chlorophyll.a..corrected.for.pheophytin"),]

write.csv(DF_WIDE_merge.3, "2_Incremental/DF_WIDElag.csv")

### Month of Sep ###

df3.4<- subset(df3, month == 9)[,c(2,3:12,14)]

df3.4 <-aggregate(x = df3.4[ ,colnames(df3.4) != c("year","Site")],             # Sum by group
                  by = list(df3.4$year,df3.4$Site),
                  FUN = mean,
                  na.rm = TRUE)

df3.4 <- df3.4[,-c(3,14)] 
names(df3.4)[1]<-"year"
names(df3.4)[2]<-"Site"

DF_WIDE_merge.4<-merge(x = DF_WIDE_merge.3, y = df3.4, by = c("year","Site"), all = TRUE)

DF_WIDE_merge.4<-DF_WIDE_merge.4[!is.na(DF_WIDE_merge.4$"Chlorophyll.a..corrected.for.pheophytin"),]

write.csv(DF_WIDE_merge.4, "2_Incremental/DF_WIDElag.csv")

### Feature Engineering  ###

names(DF_WIDE_merge.4)

df.ENV<-DF_WIDE_merge.4[,c("Site", 
                      "day", 
                      "month", 
                      "year", 
                      "ActivityStartDate"
                      )]

df.ENV$NH4_mg_L <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
                                      rowMeans(DF_WIDE_merge.4[, c(9, 19)], na.rm = TRUE),
                                      ifelse(DF_WIDE_merge.4$month == 8, 
                                             rowMeans(DF_WIDE_merge.4[, c(19, 29)], na.rm = TRUE),
                                             ifelse(DF_WIDE_merge.4$month == 9, 
                                                    rowMeans(DF_WIDE_merge.4[, c(29, 39)], na.rm = TRUE),
                                                    NA)))

df.ENV$DIN_mg_L <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
                                 rowMeans(DF_WIDE_merge.4[, c(10, 20)], na.rm = TRUE),
                                 ifelse(DF_WIDE_merge.4$month == 8, 
                                        rowMeans(DF_WIDE_merge.4[, c(20, 30)], na.rm = TRUE),
                                        ifelse(DF_WIDE_merge.4$month == 9, 
                                               rowMeans(DF_WIDE_merge.4[, c(30, 40)], na.rm = TRUE),
                                               NA)))

df.ENV$SRP__mg_L <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
                                 rowMeans(DF_WIDE_merge.4[, c(11, 21)], na.rm = TRUE),
                                 ifelse(DF_WIDE_merge.4$month == 8, 
                                        rowMeans(DF_WIDE_merge.4[, c(21, 31)], na.rm = TRUE),
                                        ifelse(DF_WIDE_merge.4$month == 9, 
                                               rowMeans(DF_WIDE_merge.4[, c(31, 41)], na.rm = TRUE),
                                               NA)))

df.ENV$pH<-DF_WIDE_merge.4[,22]

#df.ENV$pH        <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
#                           rowMeans(DF_WIDE_merge.4[, c(12, 22)], na.rm = TRUE),
#                           ifelse(DF_WIDE_merge.4$month == 8, 
#                                  rowMeans(DF_WIDE_merge.4[, c(22, 32)], na.rm = TRUE),
#                                  ifelse(DF_WIDE_merge.4$month == 9, 
#                                         rowMeans(DF_WIDE_merge.4[, c(32, 42)], na.rm = TRUE),
#                                         NA)))

df.ENV$SPC        <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
                           rowMeans(DF_WIDE_merge.4[, c(13, 23)], na.rm = TRUE),
                           ifelse(DF_WIDE_merge.4$month == 8, 
                                  rowMeans(DF_WIDE_merge.4[, c(23, 33)], na.rm = TRUE),
                                  ifelse(DF_WIDE_merge.4$month == 9, 
                                         rowMeans(DF_WIDE_merge.4[, c(33, 43)], na.rm = TRUE),
                                         NA)))

df.ENV$Temp_oC    <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
                            rowMeans(DF_WIDE_merge.4[, c(14, 24)], na.rm = TRUE),
                            ifelse(DF_WIDE_merge.4$month == 8, 
                                   rowMeans(DF_WIDE_merge.4[, c(24, 34)], na.rm = TRUE),
                                   ifelse(DF_WIDE_merge.4$month == 9, 
                                          rowMeans(DF_WIDE_merge.4[, c(34, 44)], na.rm = TRUE),
                                          NA)))

df.ENV$TDS    <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
                            rowMeans(DF_WIDE_merge.4[, c(15, 25)], na.rm = TRUE),
                            ifelse(DF_WIDE_merge.4$month == 8, 
                                   rowMeans(DF_WIDE_merge.4[, c(25, 35)], na.rm = TRUE),
                                   ifelse(DF_WIDE_merge.4$month == 9, 
                                          rowMeans(DF_WIDE_merge.4[, c(35, 45)], na.rm = TRUE),
                                          NA)))

df.ENV$TP_mg_L    <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
                        rowMeans(DF_WIDE_merge.4[, c(16, 26)], na.rm = TRUE),
                        ifelse(DF_WIDE_merge.4$month == 8, 
                               rowMeans(DF_WIDE_merge.4[, c(26, 36)], na.rm = TRUE),
                               ifelse(DF_WIDE_merge.4$month == 9, 
                                      rowMeans(DF_WIDE_merge.4[, c(36, 46)], na.rm = TRUE),
                                      NA)))

df.ENV$TN_mg_L    <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
                            rowMeans(DF_WIDE_merge.4[, c(17, 27)], na.rm = TRUE),
                            ifelse(DF_WIDE_merge.4$month == 8, 
                                   rowMeans(DF_WIDE_merge.4[, c(27, 37)], na.rm = TRUE),
                                   ifelse(DF_WIDE_merge.4$month == 9, 
                                          rowMeans(DF_WIDE_merge.4[, c(37, 47)], na.rm = TRUE),
                                          NA)))

df.ENV$TN_mg_L<-DF_WIDE_merge.4[,27]

df.ENV$TP_mg_L<-DF_WIDE_merge.4[,36]


df.ENV$TURBIDITY    <- ifelse(DF_WIDE_merge.4$month %in% c(6, 7), 
                            rowMeans(DF_WIDE_merge.4[, c(18, 28)], na.rm = TRUE),
                            ifelse(DF_WIDE_merge.4$month == 8, 
                                   rowMeans(DF_WIDE_merge.4[, c(28, 38)], na.rm = TRUE),
                                   ifelse(DF_WIDE_merge.4$month == 9, 
                                          rowMeans(DF_WIDE_merge.4[, c(38, 48)], na.rm = TRUE),
                                          NA)))

df.ENV$AFDM<-DF_WIDE_merge.4$Weight

df.ENV$CHLa<-DF_WIDE_merge.4$Chlorophyll.a..corrected.for.pheophytin


#Merging Anomaly and Q data

USGS_ALL_YEAR_Q <- read_csv("2_Incremental/OLD_DATA/ANOMALY_Q.csv")

df.ENV$Site<-as.factor(df.ENV$Site)

df.ENV$Site<-as.factor(df.ENV$Site)

levels(df.ENV$Site)<-c("FH","GR","MS","RC1","BN","DL","HU","CG","BM","RC2")

names(df.ENV)[names(df.ENV) == "year"] <- "Year"

test_merge<-merge(x = df.ENV, y = USGS_ALL_YEAR_Q, by = c("Year","Site"), all = TRUE)

test_merge<-test_merge[!is.na(test_merge$"CHLa"),]

names(test_merge)

test_merge$Days_Since_Freshet<-as.Date(test_merge$ActivityStartDate, "%m/%d/%Y")-as.Date(test_merge$datetime, "%m/%d/%Y")

write.csv(test_merge,"2_Incremental/CODE_3.0/TESTMERGE_ANOMALY.csv")

names(test_merge)

Full_Dataset<-test_merge[,c("Site", "ActivityStartDate","NH4_mg_L","DIN_mg_L","SRP__mg_L","pH","SPC","Temp_oC","TDS","TN_mg_L","TP_mg_L","TURBIDITY","Q_CFS.x","Q_CFS.y","Days_Since_Freshet","anomaly","AFDM","CHLa")]

write.csv(Full_Dataset,"2_Incremental/CODE_3.0/Full_Dataset.csv")



