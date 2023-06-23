library(gbm)
library(dismo)
library(MASS)
library(usdm)

ucfr.data <- read.csv("2_Incremental/testlagdata_3.csv")

ucfr.data[1:3,]  

ucfr.data$Site <-as.factor(ucfr.data$Site)

#ucfr.data$bloom <- as.integer(ifelse(ucfr.data$Chlorophyll.a..corrected.for.pheophytin > 100, 1, 0))


names(ucfr.data)

ucfr.data$bloom <- as.integer(ifelse(ucfr.data$Chlorophyll.a > 100, 1, 0))

ucfr.data$Chlorophyll.a <- log10(ucfr.data$Chlorophyll.a+1)
ucfr.data$Weight <- log10(ucfr.data$Weight)

#ucfr.data <- ucfr.data[ucfr.data$Site %in% c('DL','GR','BN','MS','BM','HU','FH'),]

names(ucfr.data)


ucfr.data$Temperature.oC  <- log10(ucfr.data$Temperature.oC+1)
ucfr.data$TN.ug.l  <- log10(ucfr.data$TN.ug.l+1)
ucfr.data$TP.ug.l  <- log10(ucfr.data$TP.ug.l+1)


max.VIF<-10

names(ucfr.data)

RED.DATA<-ucfr.data[complete.cases(ucfr.data[ , c(8:13)]),][,c(8:13)]

VIF.RESULTS<-vif(RED.DATA)

repeat {   
  if (max(VIF.RESULTS[,2])>max.VIF) {
    VIF.RESULTS<-vif(RED.DATA)
    RED.DATA <- within(RED.DATA, rm(list=VIF.RESULTS[which.max(VIF.RESULTS[,2]),1]))
  } else {
    break
  }
}

VIF.RESULTS

names(ucfr.data)

UCFR.tc5.lr005 <- gbm.step(data=ucfr.data, 
                           gbm.x = c(11,10,12,13,9,8),
                           gbm.y = 14,
                           family = "bernoulli",
                           #                           step = 2,
                           tree.complexity = 4,
                           learning.rate = 0.004,
                           bag.fraction = 0.75)

names(UCFR.tc5.lr005)

summary(UCFR.tc5.lr005)

BIN_DF<-data.frame(ucfr.data$bloom,UCFR.tc5.lr005$fitted)

names(BIN_DF)<-c("bloom","fitted")

fit2 <- glm(bloom~fitted, BIN_DF, family=binomial)

summary(fit2)

with(BIN_DF, plot(fitted, bloom))

minmax <- range(BIN_DF$fitted)

curve(predict(fit2, data.frame(fitted=x), type="resp"), minmax[1], minmax[2], add=TRUE)

#UCFR.simp <- gbm.simplify(UCFR.tc5.lr005, n.drops = 4)

UCFR.simp$pred.list[[1]]

gbm.plot(UCFR.tc5.lr005, write.title = F, nplots = 6, plot.layout= c(2,3), lwd=2, box=2, cex.lab=1.5, cex.axis=1.25)

gbm.plot.fits(UCFR.tc5.lr005)

find.int <- gbm.interactions(UCFR.tc5.lr005)

find.int$interactions

find.int$rank.list

####Normality tests####

gbm.perspec(UCFR.tc5.lr005,3,2, z.range=c(0,1), 
            theta = 45,
            phi=45, 
            perspective = T)

gbm.perspec(UCFR.tc5.lr005,4,1, z.range=c(0,1), 
            theta = 315,
            phi=45, 
            perspective = T)






