library(gbm)
library(dismo)
library(MASS)
library(usdm)
library(dplyr)

ucfr.data <- read.csv("2_Incremental/LAGDATA_CURRENT.csv")

#Cheacking to see if dataset was properly imported.

ucfr.data[1:3,]  

# Converting variable Site.1 (Site acronyms) into factor. 

ucfr.data$Site.1 <-as.factor(ucfr.data$Site.1)

#Creating binomial variable 

ucfr.data$bloom <- as.integer(ifelse(ucfr.data$Chlorophyll.a > 100, 1, 0))

#Subsetting by site, if desired

#ucfr.data <- ucfr.data[ucfr.data$Site %in% c('DL','GR','BN','MS','BM','HU','FH'),]

# Log Transform for dependent variables (CHLa and Weight)

ucfr.data$Chlorophyll.a <- log10(ucfr.data$Chlorophyll.a+1)
ucfr.data$Weight <- log10(ucfr.data$Weight)

# Log Transforms for independent variables

ucfr.data$Temperature.oC  <- log10(ucfr.data$Temperature.oC+1)
ucfr.data$TN.ug.l  <- log10(ucfr.data$TN.ug.l+1)
ucfr.data$TP.ug.l  <- log10(ucfr.data$TP.ug.l+1)
ucfr.data$Days.Since.Freshet  <- log10(ucfr.data$Days.Since.Freshet+1)

#Testing selected variables for variance inflation factors. 

max.VIF<-10

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

write.csv(VIF.RESULTS, "3_Products/Manuscript_files/TABLES/test1.csv")



# In this case, all VIFs < 2, so we good. 

# Performing Boosted regression trees.  

# First one is for standing stocks (log10 CHLa). Tree complexity is set to 6, learning rate was adjusted to 0.002 achieve 10^3 trees. 
#Bag fraction of 0.75 means that every tree is generated based on 25% of the data and 75% of the data is used as training set. 

UCFR.SS.tc5.lr002 <- gbm.step(data=ucfr.data, 
                           gbm.x = c(11,10,12,13,9,8),
                           gbm.y = 6,
                           family = "gaussian",
                           tree.complexity = 6,
                           learning.rate = 0.002,
                           bag.fraction = 0.75)

# Second one is for binomial bloom/no bloom variable (0 = CHl<100 mg/m2). Tree complexity is set to 6, learning rate was adjusted to 0.001 achieve 10^3 trees. 
#Bag fraction of 0.75 means that every tree is generated based on 25% of the data and 75% of the data is used as training set. 
#Results seem best with lower bag fraction. Keeping them as are for now.


UCFR.BNB.tc4.lr004 <- gbm.step(data=ucfr.data, 
                           gbm.x = c(11,10,12,13,9,8),
                           gbm.y = 14,
                           family = "bernoulli",
                           tree.complexity = 4,
                           learning.rate = 0.001,
                           bag.fraction = 0.75)

# Predicred vs observed linear regression for standing stocks. 


CHL_DF<-data.frame(UCFR.SS.tc5.lr002$fitted, ucfr.data$Chlorophyll.a)

predfit<-lm(UCFR.SS.tc5.lr002$fitted ~ ucfr.data$Chlorophyll.a)

summary(predfit)

plot(ucfr.data$Chlorophyll.a, UCFR.SS.tc5.lr002$fitted, xlim=c(0.5,3), ylim=c(0.5,3), col=ucfr.data$Site.1, xlab="Chla mg/m2", ylab="Fitted Values", abline(a=0, b=1))

# Adding vertical and horizontal lines at 2 (100 mg/m2)

abline(v=2)
abline(h=2)

plot_1a <- recordPlot()   

plot_1a

# Predicred vs observed binomial model for bloom/no bloom. 

BIN_DF<-data.frame(ucfr.data$bloom,UCFR.BNB.tc4.lr004$fitted)

names(BIN_DF)<-c("bloom","fitted")

fit2 <- glm(bloom~fitted, BIN_DF, family=binomial)

summary(fit2)

with(BIN_DF, plot(fitted, bloom))

minmax <- range(BIN_DF$fitted)

curve(predict(fit2, data.frame(fitted=x), type="resp"), minmax[1], minmax[2], add=TRUE)

plot_1b <- recordPlot()   

plot_1b

# Estimating type 1 (No bloom, model thinks it is) and type 2 (Bloom, model thinks it is not) error. 

CHL_DF<-data.frame(UCFR.SS.tc5.lr002$fitted, ucfr.data$Chlorophyll.a)

CHL_DF<-CHL_DF %>%  mutate(name = case_when(UCFR.SS.tc5.lr002.fitted >= 2 & ucfr.data.Chlorophyll.a >= 2 ~ 'BLOOM',
                                UCFR.SS.tc5.lr002.fitted <= 2 & ucfr.data.Chlorophyll.a <= 2 ~ 'NO BLOOM',
                                UCFR.SS.tc5.lr002.fitted >= 2 & ucfr.data.Chlorophyll.a <= 2 ~ 'Type 1 ERR',
                                UCFR.SS.tc5.lr002.fitted <= 2 & ucfr.data.Chlorophyll.a >= 2 ~ 'Type 2 ERR',
                                ))
table(CHL_DF$name)


# Plotting variable importance, find command

# Plotting partial dependencies


plot_3a<-gbm.plot(UCFR.SS.tc5.lr002, write.title = F, nplots = 6, plot.layout= c(2,3), lwd=2, box=2, cex.lab=1.5, cex.axis=1.25, smooth =F, rug=F)

plot_3b<-gbm.plot(UCFR.BNB.tc4.lr004, write.title = F, nplots = 6, plot.layout= c(2,3), lwd=2, box=2, cex.lab=1.5, cex.axis=1.25, smooth =F, rug=F)

# Finding interactions between variables

find.int.SS <- gbm.interactions(UCFR.SS.tc5.lr002)

find.int.SS$interactions

find.int.SS$rank.list

find.int.BNB <- gbm.interactions(UCFR.BNB.tc4.lr004)

find.int.BNB$interactions

find.int.BNB$rank.list

# Perspec plots for interactions

gbm.perspec(UCFR.SS.tc5.lr002,3,2, z.range=c(1.4,2.5),
            theta = 45,
            phi=45, 
            perspective = T,
            smooth=T)

gbm.perspec(UCFR.SS.tc5.lr002,4,3, z.range=c(1.4,2.2),
            theta = 225,
            phi=45, 
            perspective = T,
            smooth=T)

gbm.perspec(UCFR.BNB.tc4.lr004,3,2, z.range=c(0,1), 
            theta = 45,
            phi=45, 
            perspective = T)

gbm.perspec(UCFR.BNB.tc4.lr004,4,1, z.range=c(0,1), 
            theta = 315,
            phi=45, 
            perspective = T)


