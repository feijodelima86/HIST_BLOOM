library("stats")
library("ggplot2")
library("sjPlot")

ucfr.data <- read.csv("2_Incremental/TESTMERGE_ANOMALY.csv")

names(ucfr.data)

#26,11,19,27,12,34,29,30

GLM.DATA<-ucfr.data[complete.cases(ucfr.data[ , c(3,9,26)]),][,c(3,9,26)]

#GLM.DATA <- droplevels(GLM.DATA[!GLM.DATA$Site == c('HU','MS','BM','FH')])

#GLM.DATA <- GLM.DATA[GLM.DATA$Site %in% c('DL','GR','BN','HU','MS','BM'), ]

names(GLM.DATA)<-c("Site","Chl","Anomaly")

glm1<-glm(Chl ~ Site * Anomaly, family = gaussian, GLM.DATA)

summary(glm1)

#dev.new()

plotGLM <- ggplot(glm1, aes(Anomaly, Chl, col = as.factor(Site)))+  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed")

plotGLM

