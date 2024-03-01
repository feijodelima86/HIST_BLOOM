library(readr)
library(data.table)

USGS_ALL_Q <- na.omit(read_csv("0_Data/USGS_ALL_Q.csv"))

names(USGS_ALL_Q)

DL <- data.table(USGS_ALL_Q)
DL[ , list(Q_CFS = mean(Q_CFS)), by = site_no]

DL_1<-DL[ , .SD[which.max(Q_CFS)], c("Year","site_no")]
DL_2<-DL[ , .SD[mean(Q_CFS)], c("site_no")]

merged<-merge(x = DL_1, y = DL_2, by = c("site_no"), all = TRUE)

anomaly<-merged[,c(1,2,4,5,6,7,14)]

anomaly$anomaly<-abs(anomaly$Q_CFS.x/anomaly$Q_CFS.y)^(1/3)

write.csv(DL_1,"2_Incremental/CODE_3.0/USGS_ALL_YEAR_Q.csv")
write.csv(DL_2,"2_Incremental/CODE_3.0/USGS_DL_YEAR_Q.csv")
write.csv(anomaly,"2_Incremental/CODE_3.0/ANOMALY_Q.csv")
