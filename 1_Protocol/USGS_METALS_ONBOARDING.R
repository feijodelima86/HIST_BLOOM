library(readr)
library(data.table)
library(tidyr)
library(zoo)
library(dplyr)

DL <- read_csv("0_Data/USGS_CHEMISTRY/DL.csv")

names(DL)

DL <- data.table(DL)

DL <- DL[DL$Month %in% c('6','7','8','9'), ]

DL$"Begin date" <-as.Date(DL$"Begin date", "%m/%d/%Y")

DL[['date_yearmon']] <- as.yearmon(DL[['Begin date']])

#test$ActivityStartDate <-as.numeric(test$ActivityStartDate)

names(DL)

DL$date_yearmon <- as.factor(DL$date_yearmon)

DL$Site <- as.factor(DL$Site)

DL <- data.frame(DL[,c(3,109,5,6,7,45:60)])

DL$Month <- DL$Month+1 # Remember this

write.csv(DL, "2_Incremental/DL_Chemistry_INCR.csv")

DL <- data.frame(read_csv("2_Incremental/DL_Chemistry_INCR.csv"))

DL <-aggregate(x = DL[ ,colnames(DL) != c("date_yearmon","Site")],             # Sum by group
                  by = list(DL$date_yearmon,DL$Site),
                  FUN = mean,
                  na.rm = TRUE)

write.csv(DL, "2_Incremental/DL_Chemistry.csv")


