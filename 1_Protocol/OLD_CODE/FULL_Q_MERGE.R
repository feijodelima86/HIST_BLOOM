library(readr)

VNRP_DATA<- read_csv("2_Incremental/vnrp_data_redux_2.csv")
USGS_ALL_YEAR_Q <- read_csv("2_Incremental/USGS_ALL_YEAR_Q.csv")

test_merge<-merge(x = VNRP_DATA, y = USGS_ALL_YEAR_Q, by = c("Year","Month","Site"), all = TRUE)

names(test_merge)

write.csv(test_merge,"2_Incremental/TESTMERGE.csv")

