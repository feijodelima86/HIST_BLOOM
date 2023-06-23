library(readr)

VNRP_DATA<- read_csv("2_Incremental/vnrp_data_redux_2.csv")
USGS_ALL_YEAR_Q <- read_csv("2_Incremental/ANOMALY_Q.csv")

test_merge<-merge(x = VNRP_DATA, y = USGS_ALL_YEAR_Q, by = c("Year","Site"), all = TRUE)

names(test_merge)

test_merge$Days_Since_Freshet<-as.Date(test_merge$ActivityStartDate, "%m/%d/%Y")-as.Date(test_merge$datetime, "%m/%d/%Y")

write.csv(test_merge,"2_Incremental/TESTMERGE_ANOMALY.csv")
