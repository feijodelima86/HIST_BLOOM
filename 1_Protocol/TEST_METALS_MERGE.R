library(readr)
library(data.table)

ucfr.data <- read.csv("2_Incremental/TESTMERGE_ANOMALY.csv")
METAL <- read.csv("0_Data/USGS_METALS_ALL_TREAT.csv")

names(ucfr.data)
names(METAL)

METAL$Month <- METAL$Month+1 # Remember this

METAL <-aggregate(x = METAL[ ,colnames(METAL) != c("Year","Month","Site")],             # Sum by group
               by = list(METAL$Year,METAL$Month,METAL$Site),
               FUN = mean,
               na.rm = TRUE)
names(METAL)

METAL<-data.frame(METAL[,c(1,2,3,5:22)])

#write.csv(METAL, "2_Incremental/ALL_METAL.csv")

METAL<-read.csv("2_Incremental/ALL_METAL.csv")

test_merge<-merge(x = ucfr.data, y = METAL, by = c("Year","Month","Site"), all = TRUE)

write.csv(test_merge,"2_Incremental/TESTMERGE_METALS.csv")


