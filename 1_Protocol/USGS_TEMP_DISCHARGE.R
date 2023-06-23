library(readr)
library(data.table)

USGS_ALL_Q <- na.omit(read_csv("0_Data/USGS_ALL_Q.csv"))

names(USGS_ALL_Q)

DL <- data.table(USGS_ALL_Q)
DL[ , list(Q_CFS = max(Q_CFS)), by = Year]

DL_1<-DL[ , .SD[which.max(Q_CFS)], c("Year","site_no")]

write.csv(DL_1,"2_Incremental/USGS_ALL_YEAR_Q.csv")
write.csv(DL_2,"2_Incremental/USGS_DL_YEAR_Q.csv")

