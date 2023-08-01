library(readr)
library(ggplot2)

ucfr.data <- read.csv("2_Incremental/LAGDATA_CURRENT.csv")

#### Boxplot of chlorophyll by site ####

names(ucfr.data)

as.factor(ucfr.data$Site.1)

ucfr.data$Site.1  = factor(ucfr.data$Site.1, levels=c("DL", "GR", "BN", "MS", "BM", "HU", "FH"))

y_expression <- expression(log[10] ~ "CHLa" ~ (mg/m^2))

png("3_Products/Manuscript_files/FIGURES/test.png")
par(mar=c(5,5,5,1)+.1)
boxplot(Chlorophyll.a ~ Site.1, ucfr.data,                              
        ylab = y_expression,
        text.font=2,
        xlab=NULL,
        las=1,
        cex.lab=1
)
box(lwd=2)
dev.off()

#### Variance inflation test results for predictor variables ####

Table_1<-knitr::kable(read.csv("3_Products/Manuscript_files/TABLES/test1.csv"), "simple")

