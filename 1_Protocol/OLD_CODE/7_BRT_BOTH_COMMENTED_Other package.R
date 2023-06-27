library(gbm)
library(dismo)
library(MASS)
library(usdm)
library(dplyr)
library(viridis)

ucfr.data <- read.csv("2_Incremental/LAGDATA_CURRENT.csv")

ucfr.data$bloom <- as.integer(ifelse(ucfr.data$Chlorophyll.a > 100, 1, 0))
ucfr.data$Chlorophyll.a <- log10(ucfr.data$Chlorophyll.a+1)
ucfr.data$Weight <- log10(ucfr.data$Weight)

ucfr.data$Temperature.oC  <- log10(ucfr.data$Temperature.oC+1)
ucfr.data$TN.ug.l  <- log10(ucfr.data$TN.ug.l+1)
ucfr.data$TP.ug.l  <- log10(ucfr.data$TP.ug.l+1)
#ucfr.data$Days.Since.Freshet  <- log10(ucfr.data$Days.Since.Freshet+1)


names(ucfr.data)
 
data=ucfr.data[,c(6,8,9,10,11,12,13)]

ucfr.data$Site.1 <-as.factor(ucfr.data$Site.1)


gbm1 <- gbm(Chlorophyll.a ~ ., data = data, 
#            var.monotone = c(0, 0, 0, 0, 0, 0),
            distribution = "gaussian", 
            n.trees = 10000, 
            shrinkage = 0.002,             
            interaction.depth = 6, 
            bag.fraction = 0.75, 
#            train.fraction = 0.25,  
            n.minobsinnode = 10, 
            cv.folds = 5, 
            keep.data = TRUE, 
            verbose = T, 
            n.cores = 1)  

best.iter <- gbm.perf(gbm1, method = "OOB")
print(best.iter)

# Check performance using the 50% heldout test set
best.iter <- gbm.perf(gbm1, method = "test")
print(best.iter)


# Check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1, method = "cv")
print(best.iter)


# Plot relative influence of each variable
par(mfrow = c(1, 2))
summary(gbm1, n.trees = 1)          # using first tree
summary(gbm1, n.trees = best.iter)  # using estimated best number of trees

# Compactly print the first and last trees for curiosity
print(pretty.gbm.tree(gbm1, i.tree = 1))
print(pretty.gbm.tree(gbm1, i.tree = gbm1$n.trees))


# Construct univariate partial dependence plots
plot(gbm1, i.var = 1, n.trees = best.iter)
plot(gbm1, i.var = 2, n.trees = best.iter)
plot(gbm1, i.var = 3, n.trees = best.iter)
plot(gbm1, i.var = 4, n.trees = best.iter)
plot(gbm1, i.var = 5, n.trees = best.iter)
plot(gbm1, i.var = 6, n.trees = best.iter)










