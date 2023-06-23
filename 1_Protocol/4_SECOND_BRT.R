library(gbm)
library(dismo)
library(MASS)
library(usdm)

ucfr.data <- read.csv("2_Incremental/testlagdata_3.csv")

ucfr.data[1:3,]  

ucfr.data$Site.1 <-as.factor(ucfr.data$Site.1)

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
ucfr.data$Days.Since.Freshet  <- log10(ucfr.data$Days.Since.Freshet+1)

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
                           gbm.x = c(3,11,10,12,13,9,8),
                           gbm.y = 6,
                           family = "gaussian",
                           step = 50,
                           tree.complexity = 6,
                           learning.rate = 0.002,
                           bag.fraction = 0.75)

current.mod<-UCFR.tc5.lr005

save(current.mod, file="2_Incremental/SCD")

UCFR.tc5.lr005$contributions

#11,10,12,13,9,8

names(UCFR.tc5.lr005)

UCFR.tc5.lr005$self.statistics

UCFR.simp <- gbm.simplify(UCFR.tc5.lr005, n.drops = 4)

UCFR.simp$pred.list[[1]]

#dev.new()

gbm.plot(UCFR.tc5.lr005, write.title = F, nplots = 6, plot.layout= c(3,3), lwd=2, box=2, cex.lab=1.5, cex.axis=1.25)

gbm.plot.fits(UCFR.tc5.lr005)

find.int <- gbm.interactions(UCFR.tc5.lr005)

find.int$interactions

find.int$rank.list

gbm.perspec(UCFR.tc5.lr005,3,2, z.range=c(1.4,2.5),
            theta = 45,
            phi=45, 
            perspective = T,
            smooth=T)

gbm.perspec(UCFR.tc5.lr005,4,3, z.range=c(1.4,2.2),
            theta = 225,
            phi=45, 
            perspective = T,
            smooth=T)




