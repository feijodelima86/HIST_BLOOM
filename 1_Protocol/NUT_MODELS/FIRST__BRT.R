library(gbm)
library(dismo)
library(MASS)
library(usdm)
library(dplyr)
library(cowplot)



##### TN Dataset #####

ucfr.data <- read.csv("2_Incremental/NUT_MODELS/NUT_MODELS_N.csv")

names(ucfr.data)

N.BRT <- ucfr.data

N.BRT$Site<-as.factor(N.BRT$Site)

# Log Transform for dependent variables (CHLa and Weight)

N.BRT$DIN <- log10(N.BRT$DIN+1)
N.BRT$SN <- log10(N.BRT$SN+1)
N.BRT$Total.Nitrogen..mixed.forms <- log10(N.BRT$Total.Nitrogen..mixed.forms+1)

# Log Transforms for independent variables

#ucfr.data$Temperature.oC  <- log10(ucfr.data$Temperature.oC+1)
#ucfr.data$TN.ug.l  <- log10(ucfr.data$TN.ug.l+1)
#N.BRT$TNP  <- N.BRT$TNP/16
N.BRT$Days_Since_Freshet  <- sqrt(N.BRT$Days_Since_Freshet+1)

colnames(N.BRT)  
  
UCFR.TN <- gbm.step(data=N.BRT, 
                              gbm.x = c(5,3,8,20,11),
                              gbm.y = 24,
                              family = "gaussian",
                              tree.complexity = 4,
                              learning.rate = 0.002,
                              bag.fraction = 0.75,
                              interaction.depth = 4
)

#,11,7

N_DF<-data.frame(UCFR.TN$fitted, N.BRT$Total.Nitrogen..mixed.forms)

predfit.N<-lm(UCFR.TN$fitted ~ N.BRT$Total.Nitrogen..mixed.forms)

summary(predfit.N)

par(mfrow=c(1,1))

plot(N.BRT$Total.Nitrogen..mixed.forms, UCFR.TN$fitted, 
#     xlim=c(2,2.7), 
#     ylim=c(2,2.7), 
     col=N.BRT$Site, 
     xlab="Obs", 
     ylab="Fitted Values", 
     abline(a=0, b=1),
     lwd=1.5,        
     las=1,
     font.lab=2,
     font.axis = 2,
     cex.lab=1.5,
     cex.axis=1)

box(lwd=2)

# Plotting partial dependencies

gbm.plot(UCFR.TN, write.title = F, nplots = 8, plot.layout= c(2,3),
                  las=1,
                  lwd=2,
                  cex.lab=2,
                  cex.axis=1.5,
                  smooth =F,
                  rug=F,
                  y.label = NA
)


warnings()

gbm.plot.fits(UCFR.TN)


TN.SIMP<-gbm.simplify(UCFR.TN)

TN.SIMP$final.drops

find.int.TN <- gbm.interactions(UCFR.TN)

find.int.TN$interactions

find.int.TN$rank.list


##### TP Dataset #####

ucfr.data <- read.csv("2_Incremental/NUT_MODELS/NUT_MODELS_SRP.csv")

names(ucfr.data)

P.BRT <- ucfr.data

P.BRT$Site<-as.factor(P.BRT$Site)

# Log Transform for dependent variables (CHLa and Weight)

P.BRT$SRP <- log10(P.BRT$SRP+1)
P.BRT$SP <- log10(P.BRT$SP+1)
P.BRT$Total.Phosphorus..mixed.forms <- log10(P.BRT$Total.Phosphorus..mixed.forms+1)

# Log Transforms for independent variables

#ucfr.data$Temperature.oC  <- log10(ucfr.data$Temperature.oC+1)
#ucfr.data$TN.ug.l  <- log10(ucfr.data$TN.ug.l+1)
#ucfr.data$TP.ug.l  <- log10(ucfr.data$TP.ug.l+1)
P.BRT$Days_Since_Freshet  <- log10(P.BRT$Days_Since_Freshet+1)
P.BRT$Q_CFS.x <- P.BRT$Q_CFS.x^(1/9) 


colnames(P.BRT)  

UCFR.TP <- gbm.step(data=P.BRT, 
                    gbm.x = c(3,4,7,8),
                    gbm.y = 24,
                    family = "gaussian",
                    tree.complexity = 4,
                    learning.rate = 0.002,
                    bag.fraction = 0.75
)



P_DF<-data.frame(UCFR.TP$fitted, P.BRT$Total.Phosphorus..mixed.forms)

predfit.P<-lm(UCFR.TP$fitted ~ P.BRT$Total.Phosphorus..mixed.forms)

summary(predfit.P)

par(mfrow=c(1,1))

plot(P.BRT$Total.Phosphorus..mixed.forms, UCFR.TP$fitted, 
     #     xlim=c(2,2.7), 
     #     ylim=c(2,2.7), 
     col=P.BRT$Site, 
     xlab="Obs", 
     ylab="Fitted Values", 
     abline(a=0, b=1),
     lwd=1.5,        
     las=1,
     font.lab=2,
     font.axis = 2,
     cex.lab=1.5,
     cex.axis=1)

box(lwd=2)

# Plotting partial dependencies

gbm.plot(UCFR.TP, write.title = F, nplots = 8, plot.layout= c(2,3),
         las=1,
         lwd=2,
         cex.lab=2,
         cex.axis=1.5,
         smooth =F,
         rug=F,
         y.label = NA
)

gbm.plot.fits(UCFR.TP)

TP.SIMP<-gbm.simplify(UCFR.TP)

TP.SIMP$final.drops

find.int.TP <- gbm.interactions(UCFR.TP)

find.int.TP$interactions

find.int.TP$rank.list

