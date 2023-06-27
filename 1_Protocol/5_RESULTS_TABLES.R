library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(psych)


ucfr.data <- read.csv("2_Incremental/LAGDATA_CURRENT.csv")

names(ucfr.data)

as.factor(ucfr.data$Site.1)

ucfr.data$Site.1  = factor(ucfr.data$Site.1, levels=c("DL", "GR", "BN", "MS", "BM", "HU", "FH"))

### Data Summary Table

summary.data<-ucfr.data[,c(3,8,9,10,11,12,13,6,7)]

names(summary.data)

desc.list<-describeBy(summary.data, group=ucfr.data$Site.1, fast=TRUE)
a<-lapply(desc.list, FUN = function(x){x[c(2:9),c(2:4)]})
a<-lapply(a, FUN = function(x){paste(round(x$mean, digits = 1), " (", round(x$sd, digits = 2), ") ", sep="")})
varsum.csv <- data.frame(matrix(unlist(a), nrow=length(a), byrow=TRUE))
colnames(varsum.csv)<-c("Temp (oC)", "pH", "TN (mg/l)" , "TP (mg/l)", "pSize", "dP", "CHLa (mg/m2)", "AFDM (g/m2)")
rownames(varsum.csv)<-c("DL", "GR", "BN", "MS", "BM", "HU", "FH")

write.csv(varsum.csv, "3_Products/Manuscript_files/TABLES/varsum.csv")

### Minus algae

summary.data<-ucfr.data[,c(3,8,9,10,11,12,13)]

names(summary.data)

desc.list<-describeBy(summary.data, group=ucfr.data$Site.1, fast=TRUE)
a<-lapply(desc.list, FUN = function(x){x[c(2:7),c(2:4)]})
a<-lapply(a, FUN = function(x){paste(round(x$mean, digits = 1), " (", round(x$sd, digits = 2), ") ", sep="")})
varsum.csv <- data.frame(matrix(unlist(a), nrow=length(a), byrow=TRUE))
colnames(varsum.csv)<-c("Temp (oC)", "pH", "TN (mg/l)" , "TP (mg/l)", "pSize", "dP")
rownames(varsum.csv)<-c("DL", "GR", "BN", "MS", "BM", "HU", "FH")

write.csv(varsum.csv, "3_Products/Manuscript_files/TABLES/varsum.csv")



### VIF Table:

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

#####


library(table1)
table1::label(gapminder$lifeExp) <- "Life Expectancy"
table1::label(gapminder$pop) <- "Population"
table1::label(gapminder$gdpPercap) <- "Gdp Per Capita"

mytable<-table1::table1(~Temperature.oC + pH + Q.Anomaly + Days.Since.Freshet + TN.ug.l + TP.ug.l + Chlorophyll.a | Site.1, data = summary.data,
                        transpose = F,
                        render.missing = NULL,
                        overall=F,
                        render.continuous = function(x){
                          with(stats.default(x),
                               sprintf("%s (%s)",
                                       round_pad(MEAN, 2),
                                       round_pad(SD, 2))
                          )
                        })

asdf<-t1kable(mytable)

write.csv(asdf, "3_Products/Manuscript_files/TABLES/test2.csv")
