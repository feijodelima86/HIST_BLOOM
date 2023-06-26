library(gbm)
library(dismo)
library(MASS)
library(usdm)
library(dplyr)
library(cowplot)

ucfr.data <- read.csv("2_Incremental/LAGDATA_CURRENT.csv")

#Cheacking to see if dataset was properly imported.

ucfr.data[1:3,]  

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

#### Boxplot of chlorophyll by site ####

names(ucfr.data)

as.factor(ucfr.data$Site.1)

ucfr.data$Site.1  = factor(ucfr.data$Site.1, levels=c("DL", "GR", "BN", "MS", "BM", "HU", "FH"))

# In this case, all VIFs < 2, so we good. 

# Performing Boosted regression trees.  

# First one is for standing stocks (log10 CHLa). Tree complexity is set to 6, learning rate was adjusted to 0.002 achieve 10^3 trees. 
#Bag fraction of 0.75 means that every tree is generated based on 25% of the data and 75% of the data is used as training set. 

names(ucfr.data)

UCFR.SS.tc5.lr002 <- gbm.step(data=ucfr.data, 
                              gbm.x = c(11,10,12,13,9,8),
                              gbm.y = 6,
                              family = "gaussian",
                              tree.complexity = 6,
                              learning.rate = 0.002,
                              bag.fraction = 0.75
)

# Second one is for binomial bloom/no bloom variable (0 = CHl<100 mg/m2). Tree complexity is set to 6, learning rate was adjusted to 0.001 achieve 10^3 trees. 
#Bag fraction of 0.75 means that every tree is generated based on 25% of the data and 75% of the data is used as training set. 
#Results seem best with lower bag fraction. Keeping them as are for now.


UCFR.BNB.tc4.lr004 <- gbm.step(data=ucfr.data, 
                               gbm.x = c(11,10,12,13,9,8),
                               gbm.y = 19,
                               family = "bernoulli",
                               tree.complexity = 4,
                               learning.rate = 0.001,
                               bag.fraction = 0.75)




