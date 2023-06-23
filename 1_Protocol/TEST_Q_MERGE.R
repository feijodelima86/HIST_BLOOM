library(readr)

DL_REDUX_Q_TEST <- read_csv("2_Incremental/DL_REDUX_Q_TEST.csv")
USGS_DL_year_Q <- read_csv("2_Incremental/USGS_DL_year_Q.csv")

test_merge<-merge(x = DL_REDUX_Q_TEST, y = USGS_DL_year_Q, by = "Year", all = TRUE)

names(test_merge)
test_merge$ActivityStartDate-test_merge$datetime

test_merge$Days_Since_Freshet<-as.Date(test_merge$ActivityStartDate, "%m/%d/%Y")-as.Date(test_merge$datetime, "%m/%d/%Y")

write.csv(test_merge,"2_Incremental/TESTMERGE.csv")

