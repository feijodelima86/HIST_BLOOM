library(readr)
library(ggplot2)
library(lubridate)

ucfr.data <- read.csv("2_Incremental/LAGDATA_CURRENT.csv")

#### Boxplot of chlorophyll by site ####

names(ucfr.data)

as.factor(ucfr.data$Site.1)

ucfr.data$Site.1  = factor(ucfr.data$Site.1, levels=c("DL", "GR", "BN", "MS", "BM", "HU", "FH"))

ucfr.data <- ucfr.data[which(ucfr.data$Site.1 == "DL" | ucfr.data$Site.1 == "GR" | ucfr.data$Site.1 == "BN" | ucfr.data$Site.1 == "MS"),]

ucfr.site <- ucfr.data[which(ucfr.data$Site.1 == "MS"),]

y_expression <- expression(log[10] ~ "CHLa" ~ (mg/m^2))

plotdata <- data.frame()

plotdata<- ucfr.site[,c("year", "Chlorophyll.a")]

plotdata <-aggregate(x = plotdata[,colnames(plotdata) != c("year")],          
                                 by = list(plotdata$year),
                                 FUN = mean,
                                 na.rm = T)

names(plotdata)<- c("year", "Chlorophyll.a")

plotdata$Chlorophyll.a = log10(plotdata$Chlorophyll.a)

par(mar=c(5,5,5,1)+.1)

plot(plotdata$year , plotdata$Chlorophyll.a,
     pch=21,
     cex=1.8,
     cex.axis=1.25,
     cex.lab = 1.5,
     las=1,
     ylim=c(min(log10(ucfr.data$Chlorophyll.a)),max(log10(ucfr.data$Chlorophyll.a))),
     ylab=expression(bold(log[10] ~ "CHLa" ~ (mg/m^2))),
     col="black",
     bg="black",
#     xaxt='n',
     lwd=2,
     xlab=NA)

box(lwd=2)
abline(h=2, lty=2, lwd=2)

