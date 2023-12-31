library("stats")
library("ggplot2")
library("sjPlot")

ucfr.data <- read.csv("2_Incremental/LAGDATA_CURRENT.csv")

names(ucfr.data)

ucfr.data$ActivityStartDate<-as.Date(ucfr.data$ActivityStartDate, "%m/%d/%Y")

ucfr.data$Time <- as.integer(ucfr.data$ActivityStartDate - as.Date("2008-01-01"))

# Log Transform for dependent variables (CHLa and Weight)

ucfr.data$Chlorophyll.a <- log10(ucfr.data$Chlorophyll.a+1)
ucfr.data$Weight <- log10(ucfr.data$Weight)

# Log Transforms for independent variables

ucfr.data$Temperature.oC  <- log10(ucfr.data$Temperature.oC+1)
ucfr.data$TN.ug.l  <- log10(ucfr.data$TN.ug.l+1)
ucfr.data$TP.ug.l  <- log10(ucfr.data$TP.ug.l+1)
ucfr.data$Days.Since.Freshet  <- log10(ucfr.data$Days.Since.Freshet+1)

#26,11,19,27,12,34,29,30


names(ucfr.data)

GLM.DATA<-ucfr.data[complete.cases(ucfr.data[ , c(3,12,10)]),][,c(3,12,10)]

#GLM.DATA <- droplevels(GLM.DATA[!GLM.DATA$Site == c('HU','MS','BM','FH')])

#GLM.DATA <- GLM.DATA[GLM.DATA$Site %in% c('DL','GR','BN','HU','MS','BM'), ]

names(GLM.DATA)<-c("Site","Time","Var")

glm1<-glm(Var ~ Site * Time, family = gaussian, GLM.DATA)

summary(glm1)

#dev.new()

plotGLM <- ggplot(glm1, aes(Time, Var, col = as.factor(Site)))+  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed")

plotGLM

