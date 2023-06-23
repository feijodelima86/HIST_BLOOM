library(gbm)
library(dismo)
library(MASS)
library(usdm)

ucfr.data <- read.csv("2_Incremental/TESTMERGE_ANOMALY.csv")

ucfr.data[1:3,]  

ucfr.data$Site <-as.factor(ucfr.data$Site)

ucfr.data$Chlorophyll.a..corrected.for.pheophytin <- log10(ucfr.data$Chlorophyll.a..corrected.for.pheophytin)
ucfr.data$Weight <- log10(ucfr.data$Weight)

#ucfr.data <- ucfr.data[ucfr.data$Site %in% c('DL','GR','BN','MS','BM','HU','FH'),]

names(ucfr.data)

ucfr.data$Ammonia  <- log10(ucfr.data$Ammonia+1)
ucfr.data$Nitrate...Nitrite <- log10(ucfr.data$Nitrate...Nitrite+1)
ucfr.data$Orthophosphate  <- log10(ucfr.data$Orthophosphate+1)
ucfr.data$Total.Nitrogen..mixed.forms  <- log10(ucfr.data$Total.Nitrogen..mixed.forms+1)
ucfr.data$Total.Phosphorus..mixed.forms  <- log10(ucfr.data$Total.Phosphorus..mixed.forms+1)
ucfr.data$Q_CFS.x   <- log10(ucfr.data$Q_CFS.x +1)
ucfr.data$TN_DISS   <- log10(ucfr.data$TN_DISS +1)
ucfr.data$TP_DISS   <- log10(ucfr.data$TP_DISS +1)
#ucfr.data$X.Copper..water..filtered..micrograms.per.liter<- log10(ucfr.data$X.Copper..water..filtered..micrograms.per.liter +1)
max.VIF<-10

names(ucfr.data)

RED.DATA<-ucfr.data[complete.cases(ucfr.data[ , c(26,11,19,27,12)]),][,c(26,11,19,27,12)]

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
                           gbm.x = c(26,11,12,22,23,27),
                           gbm.y = 9,
                           family = "gaussian",
                           step = 50,
                           tree.complexity = 6,
                           learning.rate = 0.005,
                           bag.fraction = 0.75)



predfit<-lm(UCFR.tc5.lr005$fitted ~ ucfr.data$Chlorophyll.a..corrected.for.pheophytin)

UCFR.tc5.lr005$contributions

summary(predfit)

plot(ucfr.data$Chlorophyll.a..corrected.for.pheophytin, UCFR.tc5.lr005$fitted, xlim=c(0.5,3), ylim=c(0.5,3), col=ucfr.data$Site)

abline(a=0, b=1)

abline(v=2)

abline(h=2)

names(UCFR.tc5.lr005)

UCFR.tc5.lr005$self.statistics

#UCFR.simp <- gbm.simplify(UCFR.tc5.lr005, n.drops = 4)

#UCFR.simp$pred.list[[1]]

#dev.new()

gbm.plot(UCFR.tc5.lr005, write.title = F)

gbm.plot.fits(UCFR.tc5.lr005)

find.int <- gbm.interactions(UCFR.tc5.lr005)

find.int$interactions

find.int$rank.list

gbm.perspec(UCFR.tc5.lr005,2,1, z.range=c(1.5,2.2),
            theta = 135,
            phi=45, 
            perspective = T,
            smooth=T)

gbm.perspec(UCFR.tc5.lr005,2,4, z.range=c(1.5,2.2),
            theta = 45,
            phi=45, 
            perspective = T,
            smooth=T)



