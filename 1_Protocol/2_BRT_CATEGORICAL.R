library(gbm)
library(dismo)
library(MASS)
library(usdm)

ucfr.data <- read.csv("2_Incremental/vnrp_data_redux_2.csv")

ucfr.data[1:3,]  

ucfr.data$Group.1 <-as.factor(ucfr.data$Group.1)

ucfr.data$Group.2 <-as.factor(ucfr.data$Group.2)

#ucfr.data$bloom <- as.integer(ifelse(ucfr.data$Chlmax > 300, 1, 0))

ucfr.data$bloom <- as.integer(ifelse(ucfr.data$Chlorophyll.a..corrected.for.pheophytin > 100, 1, 0))

#ucfr.data$Chlorophyll.a..corrected.for.pheophytin <- log(ucfr.data$Chlorophyll.a..corrected.for.pheophytin)

names(ucfr.data)

ucfr.data$Ammonia  <- log(ucfr.data$Ammonia+1)
ucfr.data$Nitrate...Nitrite <- log(ucfr.data$Nitrate...Nitrite+1)
ucfr.data$Orthophosphate  <- log(ucfr.data$Orthophosphate+1)
ucfr.data$Total.Nitrogen..mixed.forms  <- log(ucfr.data$Total.Nitrogen..mixed.forms+1)
ucfr.data$Total.Phosphorus..mixed.forms  <- log(ucfr.data$Total.Phosphorus..mixed.forms+1)


max.VIF<-10

RED.DATA<-ucfr.data[complete.cases(ucfr.data[ , c(6,7,13,14,15,17,18,19)]),][,c(6,7,13,14,15,17,18,19)]

VIF.RESULTS<-vif(RED.DATA)

repeat {   
  if (max(VIF.RESULTS[,2])>max.VIF) {
    VIF.RESULTS<-vif(RED.DATA)
    RED.DATA <- within(RED.DATA, rm(list=VIF.RESULTS[which.max(VIF.RESULTS[,2]),1]))
  } else {
    break
  }
}



UCFR.tc5.lr005 <- gbm.step(data=ucfr.data, 
                           gbm.x = c(6,17,18),
                           gbm.y = 20,
                           family = "bernoulli",
                           #                           step = 2,
                           tree.complexity = 4,
                           learning.rate = 0.0002,
                           bag.fraction = 0.75)

names(UCFR.tc5.lr005)

UCFR.tc5.lr005$fitted.vars

UCFR.simp <- gbm.simplify(UCFR.tc5.lr005, n.drops = 4)

UCFR.simp$pred.list[[1]]

gbm.plot(UCFR.tc5.lr005, write.title = F)

gbm.plot.fits(UCFR.tc5.lr005)


####Normality tests####


find.int <- gbm.interactions(UCFR.tc5.lr005)

gbm.perspec(UCFR.tc5.lr005,3,2, z.range=c(0,0.65), 
            theta = 30,
            phi=45, 
            perspective = F)







