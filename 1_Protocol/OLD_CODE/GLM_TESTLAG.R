library("stats")
library("ggplot2")
library("sjPlot")

ucfr.data <- read.csv("2_Incremental/LAGDATA_CURRENT.csv")

names(ucfr.data)

#Nitrate  = 8, 14,  20, 26   = 
#SRP      = 9, 15,  21, 27   = 
#pH       = 10,16,  22, 28   = 22
#Temp     = 11, 17, 23, 29   = 17
#TN       = 12, 18, 24, 30   = 24+18/2
#TP       = 13, 19, 25, 31   = 25+19


#26,11,19,27,12,34,29,30### 29,27, 25, 23

DATA<-ucfr.data[,c(3,6,16,16)]

DATA[,5]<-(DATA[,3]+DATA[,4])/2

GLM.DATA<-DATA[complete.cases(DATA[ , c(1,2,5)]),][,c(1,2,5)]

#GLM.DATA <- droplevels(GLM.DATA[!GLM.DATA$Site == c('HU','MS','BM','FH')])

#GLM.DATA <- GLM.DATA[GLM.DATA$Site %in% c('DL','GR','BN','HU','MS','BM'), ]

names(GLM.DATA)<-c("Site","Chl","Anomaly")

GLM.DATA$Chl<-log10(GLM.DATA$Chl+1)

#GLM.DATA$Anomaly<-log10(GLM.DATA$Anomaly+1)

glm1<-glm(Chl ~ Site * Anomaly, family = gaussian, GLM.DATA)

summary(glm1)

#dev.new()

plotGLM <- ggplot(glm1, aes(Anomaly, Chl, col = as.factor(Site)))+  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed")

plotGLM

