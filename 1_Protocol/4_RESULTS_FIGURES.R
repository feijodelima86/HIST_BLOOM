library(readr)
library(ggplot2)

ucfr.data <- read.csv("2_Incremental/LAGDATA_CURRENT.csv")

# Boxplot of chlorophyll by site

ucfr.data$Chlorophyll.a <- log10(ucfr.data$Chlorophyll.a+1)

names(ucfr.data)

as.factor(ucfr.data$Site.1)

ucfr.data$Site.1  = factor(ucfr.data$Site.1, levels=c("DL", "GR", "BN", "MS", "BM", "HU", "FH"))

y_expression <- bold(expression(log[10] ~ "CHLa" ~ (mg/m^2)))


plot_1 <- recordPlot()   

png("3_Products/Manuscript_files/FIGURES/test.png")
par(mar=c(5,6,4,1)+.1)
boxplot(Chlorophyll.a ~ Site.1, ucfr.data,                              
        ylab = y_expression,
        text.font=2,
        xlab=NULL,
        las=1,
        cex.lab=1
)
box(lwd=2)
dev.off()

theme_set(theme_bw())

p <- ggplot(ucfr.data, aes(x=Site.1, y=Chlorophyll.a)) + 
  geom_violin() 
  
p + stat_summary(fun.y=median, geom="point", size=2, color="black") 
  + labs=(y=expression(log[10] ~ "CHLa" ~ (mg/m^2))) 


Table_1<-knitr::kable(read.csv("Manuscript_files/TABLES/test1.csv"), "simple")
