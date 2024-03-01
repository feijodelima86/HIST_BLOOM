library(gbm)
library(dismo)
library(MASS)
library(usdm)
library(dplyr)
library(cowplot)

ucfr.data <- read.csv("2_Incremental/CODE_3.0/Full_Dataset_manual.csv")

#Cheacking to see if dataset was properly imported.

ucfr.data[1:3,]  

#Creating binomial variable 

ucfr.data$bloom <- as.integer(ifelse(ucfr.data$CHLa  > 100, 1, 0))

#Subsetting by site, if desired

#ucfr.data <- ucfr.data[ucfr.data$Site %in% c('DL','GR','BN','MS','BM','HU','FH'),]

#Creating N/P ratio

ucfr.data$NP<-ifelse(ucfr.data$TP_mg_L==0, NA, ucfr.data$TN_mg_L/ucfr.data$TP_mg_L)

# Log Transform for dependent variables (CHLa and Weight)

ucfr.data$CHLa  <- log10(ucfr.data$CHLa +1)
ucfr.data$AFDM <- log10(ucfr.data$AFDM)

# Log Transforms for independent variables

names(ucfr.data)

ucfr.data$Temp_oC  <- log10(ucfr.data$Temp_oC+1)
ucfr.data$TN_mg_L  <- log10(ucfr.data$TN_mg_L*1000+1)
ucfr.data$TP_mg_L  <- log10(ucfr.data$TP_mg_L*1000+1)
#ucfr.data$Days_Since_Freshet  <- log10(ucfr.data$Days_Since_Freshet+1)



#### Boxplot of chlorophyll by site ####

names(ucfr.data)

as.factor(ucfr.data$Site)

ucfr.data<-ucfr.data[ucfr.data$Site %in% c("DL", "GR", "BN", "MS", "BM", "HU", "FH"), ]   

ucfr.data$Site  = factor(ucfr.data$Site, levels=c("DL", "GR", "BN", "MS", "BM", "HU", "FH"))

y_expression <- expression(log[10] ~ "CHLa" ~ (mg/m^2))

#png("3_Products/Manuscript_files/FIGURES/Chla_Boxplots.png", width = 4.5, height = 4.25, units = "in", res=1200)
#par(mfrow=c(1,1),mar=c(2,4,1,1)+.1, mgp=c(2.2,1,0))
boxplot(CHLa ~ Site, ucfr.data,                              
        ylab = y_expression,
        text.font=2,
        font.axis = 2,
        xlab=NULL,
        las=1,
        cex.lab=1,
        cex.axis=0.75
)
box(lwd=2)


# Performing Boosted regression trees.  

# First one is for standing stocks (log10 CHLa). Tree complexity is set to 6, learning rate was adjusted to 0.002 achieve 10^3 trees. 
#Bag fraction of 0.75 means that every tree is generated based on 25% of the data and 75% of the data is used as training set. 

names(ucfr.data)

UCFR.SS.tc5.lr002 <- gbm.step(data=ucfr.data, 
                           gbm.x = c(12,11,17,9,16),
                           gbm.y = 19,
                           family = "gaussian",
                           tree.complexity = 5,
                           learning.rate = 0.002,
                           bag.fraction = 0.9
                           )


### Nicer plots

y.bar <- min(UCFR.SS.tc5.lr002$cv.values) 
y.min <- min(UCFR.SS.tc5.lr002$cv.values - UCFR.SS.tc5.lr002$cv.loss.ses)
y.max <- max(UCFR.SS.tc5.lr002$cv.values + UCFR.SS.tc5.lr002$cv.loss.ses) 

#dev.new()
plot(UCFR.SS.tc5.lr002$trees.fitted, UCFR.SS.tc5.lr002$cv.values, 
     type = 'l', 
     ylab = "Holdout deviance",
     xlab = "Number of trees", 
     ylim = c(y.min,y.max), 
     lwd=1.5,        
     las=1,
     font.lab= 2,
     font.axis = 2,
     cex.lab=1.5,
     cex.axis=1)

abline(h = y.bar, col = 2)

lines(UCFR.SS.tc5.lr002$trees.fitted, UCFR.SS.tc5.lr002$cv.values + UCFR.SS.tc5.lr002$cv.loss.ses, lty=2)  
lines(UCFR.SS.tc5.lr002$trees.fitted, UCFR.SS.tc5.lr002$cv.values - UCFR.SS.tc5.lr002$cv.loss.ses, lty=2)  

target.trees <- UCFR.SS.tc5.lr002$trees.fitted[match(TRUE,UCFR.SS.tc5.lr002$cv.values == y.bar)]
abline(v = target.trees, col=3, lwd=2)
box(lwd=2)


# Predicred vs observed linear regression for standing stocks. 

CHL_DF<-data.frame(UCFR.SS.tc5.lr002$fitted, ucfr.data$CHLa)

predfit<-lm(UCFR.SS.tc5.lr002$fitted ~ ucfr.data$CHLa)

summary(predfit)

plot(ucfr.data$CHLa, UCFR.SS.tc5.lr002$fitted, 
      xlim=c(0.5,3), 
      ylim=c(0.5,3), 
     col=ucfr.data$Site, xlab="Chla mg/m2", ylab="Fitted Values", 
     abline(a=0, b=1),
     lwd=1.5,        
     las=1,
     font.lab=2,
     font.axis = 2,
     cex.lab=1.5,
     cex.axis=1)

box(lwd=2)

# Adding vertical and horizontal lines at 2 (100 mg/m2)

abline(v=2)
abline(h=2)

# Plotting variable importance, find command

# Plotting partial dependencies

plot_4a<-gbm.plot(UCFR.SS.tc5.lr002, write.title = F, nplots = 8, plot.layout= c(3,2),
                  las=1,
                  lwd=2,
                  cex.lab=2,
                  cex.axis=1.5,
                  smooth =F,
                  rug=F,
                  y.label = NA
                  )


gbm.plot.fits(UCFR.SS.tc5.lr002)

# Finding interactions between variables

find.int.SS <- gbm.interactions(UCFR.SS.tc5.lr002)

find.int.SS$interactions

find.int.SS$rank.list



# Perspec plots for interactions

dev.new()
gbm.perspec(UCFR.SS.tc5.lr002,4,2, 
            z.range=c(1.4,2.5),
            theta = 45,
            phi=45, 
            cex.lab = 1.2, font.lab = 2, cex.axis = 1, font.axis= 1,
            perspective = T,
            smooth=F)


