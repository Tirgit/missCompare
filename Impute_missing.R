#source("https://bioconductor.org/biocLite.R")
#biocLite("pcaMethods")
#biocLite("missMDA")
#install.packages("FactoMineR")
#install.packages("missForest")
#install.packages("mice")
#install.packages("Amelia")
#install.packages("mi")
library(pcaMethods)
library(missMDA)
library(FactoMineR)
library(missForest)
library(Hmisc)
library(mice)
library(Amelia)
library(mi)

#set working directory
setwd("/Users/med-tv_/Documents/Projects/missingdata/")
data <- readRDS("simulated_dataframe.rds")

#scale data
data_scaled <- as.data.frame(scale(data))
#means are 0, SD=1
summary(data_scaled)
#create matrix from data frame
real_matrix <- as.matrix(data_scaled)

#set number of simulations
numsim <- 3

###Simulation, MEDIAN imputation
outputvalues_median <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    for(i in 1:ncol(real_matrix_miss)){
      real_matrix_miss[is.na(real_matrix_miss[,i]), i] <- median(real_matrix_miss[,i], na.rm = TRUE)
    }
    imp_matrix <- real_matrix_miss
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_median[nrow(outputvalues_median) + 1, ] <- values
}
#saveRDS(outputvalues_median, file="/mnt/lustre/Home/tibor_v/missing_median.rds")


###PCA USING MISSMDA
###Simulation, REGULARIZED method
outputvalues_regularized <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
ns <- NULL
count <- 1
repeat {
  percentage <- cats
  real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
  ncomp <- estim_ncpPCA(real_matrix_miss)
  res.imp <- imputePCA(real_matrix_miss, ncp= ncomp$ncp, method = "Regularized")
  res.pca.miss <- PCA(res.imp$completeObs, graph = F)
  imp_matrix <- res.imp$completeObs
  n <- cor(c(real_matrix), c(imp_matrix))
  ns <- c(ns, n) 
  count <- count+1
if(count >= numsim) {
  break
}
}
meanvalue <- mean(ns)
sevalue <- sd(ns)/sqrt(length(ns))
values <- c(percentage,meanvalue,sevalue)
outputvalues_regularized[nrow(outputvalues_regularized) + 1, ] <- values
}
#saveRDS(outputvalues_regularized, file="/mnt/lustre/Home/tibor_v/missing_regularized.rds")


###Simulation, EM method
outputvalues_em <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    ncomp <- estim_ncpPCA(real_matrix_miss)
    res.imp <- imputePCA(real_matrix_miss, ncp= ncomp$ncp, method = "EM")
    res.pca.miss <- PCA(res.imp$completeObs, graph = F)
    imp_matrix <- res.imp$completeObs
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_em[nrow(outputvalues_em) + 1, ] <- values
}
#saveRDS(outputvalues_em, file="/mnt/lustre/Home/tibor_v/missing_em.rds")


###PCA USING PCAMETHODS
###Simulation, PPCA
outputvalues_ppca <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    ncomp <- estim_ncpPCA(real_matrix_miss)
    resPPCA <- pca(real_matrix_miss, method="ppca", center=FALSE, nPcs=ncomp$ncp)
    imp_matrix <- resPPCA@completeObs
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_ppca[nrow(outputvalues_ppca) + 1, ] <- values
}
#saveRDS(outputvalues_ppca, file="/mnt/lustre/Home/tibor_v/missing_ppca.rds")


###Simulation, SVDIMPUTE
outputvalues_svdimpute <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    ncomp <- estim_ncpPCA(real_matrix_miss)
    resSVDI <- pca(real_matrix_miss, method="svdImpute", center=FALSE, nPcs=ncomp$ncp)
    imp_matrix <- resSVDI@completeObs
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_svdimpute[nrow(outputvalues_svdimpute) + 1, ] <- values
}
#saveRDS(outputvalues_svdimpute, file="/mnt/lustre/Home/tibor_v/missing_svdimpute.rds")


###Simulation, BPCA
outputvalues_bpca <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    ncomp <- estim_ncpPCA(real_matrix_miss)
    resBPCA <- pca(real_matrix_miss, method="bpca", center=FALSE, nPcs=ncomp$ncp)
    imp_matrix <- resBPCA@completeObs
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_bpca[nrow(outputvalues_bpca) + 1, ] <- values
}
#saveRDS(outputvalues_bpca, file="/mnt/lustre/Home/tibor_v/missing_bpca.rds")


###Simulation, NIPALS
outputvalues_nipals <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    ncomp <- estim_ncpPCA(real_matrix_miss)
    resNipals <- pca(real_matrix_miss, method="nipals", center=FALSE, nPcs=ncomp$ncp)
    imp_matrix <- resNipals@completeObs
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_nipals[nrow(outputvalues_nipals) + 1, ] <- values
}
#saveRDS(outputvalues_nipals, file="/mnt/lustre/Home/tibor_v/missing_nipals.rds")

###Simulation, NLPCA
outputvalues_nlpca <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    ncomp <- estim_ncpPCA(real_matrix_miss)
    resNLPCA <- pca(real_matrix_miss, method="nlpca", center=FALSE, nPcs=ncomp$ncp, maxSteps=100)
    imp_matrix <- resNLPCA@completeObs
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_nlpca[nrow(outputvalues_nlpca) + 1, ] <- values
}
#saveRDS(outputvalues_nlpca, file="/mnt/lustre/Home/tibor_v/missing_nlpca.rds")


###Simulation, MICE
outputvalues_mice <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    imputed_Data <- mice(real_matrix_miss, m=1, maxit = 100, method = 'pmm')
    imp_matrix <- as.matrix(complete(imputed_Data,1))
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_mice[nrow(outputvalues_mice) + 1, ] <- values
}
#saveRDS(outputvalues_mice, file="/mnt/lustre/Home/tibor_v/missing_mice.rds")


###Simulation, AMELIA
outputvalues_amelia <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.3, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    amelia_fit <- amelia(real_matrix_miss, m=1)
    imp_matrix <- amelia_fit$imputations[[1]]
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_amelia[nrow(outputvalues_amelia) + 1, ] <- values
}
outputvalues_amelia[nrow(outputvalues_amelia) + 1, ] <- c(0.35,NA,NA)
outputvalues_amelia[nrow(outputvalues_amelia) + 1, ] <- c(0.4,NA,NA)
outputvalues_amelia[nrow(outputvalues_amelia) + 1, ] <- c(0.45,NA,NA)
outputvalues_amelia[nrow(outputvalues_amelia) + 1, ] <- c(0.5,NA,NA)
#saveRDS(outputvalues_amelia, file="/mnt/lustre/Home/tibor_v/missing_amelia.rds")


###Simulation, missForest
outputvalues_forest <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.5, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    results <- missForest(real_matrix_miss)
    imp_matrix <- results$ximp
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_forest[nrow(outputvalues_forest) + 1, ] <- values
}
#saveRDS(outputvalues_forest, file="/mnt/lustre/Home/tibor_v/missing_forest.rds")


###Simulation, mi
outputvalues_mi <- data.frame("percentage" = numeric() ,"mean" = numeric(), "se" = numeric())
percentage_cat <- c(seq(0.05, 0.3, 0.05))
for(cats in percentage_cat){
  ns <- NULL
  count <- 1
  repeat {
    percentage <- cats
    real_matrix_miss <- prodNA(real_matrix, noNA = percentage)
    mi_data <- mi(as.data.frame(real_matrix_miss), n.chain = 1)
    imputed <- complete(mi_data,1)
    imp_matrix <- as.matrix(imputed[,1:ncol(real_matrix_miss)])
    n <- cor(c(real_matrix), c(imp_matrix))
    ns <- c(ns, n) 
    count <- count+1
    if(count >= numsim) {
      break
    }
  }
  meanvalue <- mean(ns)
  sevalue <- sd(ns)/sqrt(length(ns))
  values <- c(percentage,meanvalue,sevalue)
  outputvalues_mi[nrow(outputvalues_mi) + 1, ] <- values
}
outputvalues_mi[nrow(outputvalues_mi) + 1, ] <- c(0.35,NA,NA)
outputvalues_mi[nrow(outputvalues_mi) + 1, ] <- c(0.4,NA,NA)
outputvalues_mi[nrow(outputvalues_mi) + 1, ] <- c(0.45,NA,NA)
outputvalues_mi[nrow(outputvalues_mi) + 1, ] <- c(0.5,NA,NA)
#saveRDS(outputvalues_mi, file="/mnt/lustre/Home/tibor_v/missing_mi.rds")


#PLOT RESULTS FROM SIMULATIONS
# Iliinsky and Steele color name vector
IScolors <- c("red", "green", "yellow", "blue",
              "black", "white", "pink", "cyan",
              "gray", "orange", "brown", "purple")

library(Hmisc)
pdf("simulations.pdf")
plot(outputvalues_regularized$percentage, outputvalues_regularized$mean,
     minor.tick(nx=2),
     ylim=c(ymin=0.7, ymax=1),
     pch=19, xlab="Percentage missing", ylab="Correlation, 95% CI",
     main="Comparison of PCA imputation methods",
     col = "blue"
)
arrows(outputvalues_regularized$percentage, outputvalues_regularized$mean-(1.96*outputvalues_regularized$se), outputvalues_regularized$percentage, outputvalues_regularized$mean+(1.96*outputvalues_regularized$se), length=0.05, angle=90, code=3, col = "blue")
points(outputvalues_regularized$percentage, outputvalues_em$mean, col = "red", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_em$mean-(1.96*outputvalues_em$se), outputvalues_regularized$percentage, outputvalues_em$mean+(1.96*outputvalues_em$se), length=0.05, angle=90, code=3, col = "red")
points(outputvalues_regularized$percentage, outputvalues_svdimpute$mean, col = "yellow", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_svdimpute$mean-(1.96*outputvalues_svdimpute$se), outputvalues_regularized$percentage, outputvalues_svdimpute$mean+(1.96*outputvalues_svdimpute$se), length=0.05, angle=90, code=3, col = "yellow")
points(outputvalues_regularized$percentage, outputvalues_bpca$mean, col = "green", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_bpca$mean-(1.96*outputvalues_bpca$se), outputvalues_regularized$percentage, outputvalues_bpca$mean+(1.96*outputvalues_bpca$se), length=0.05, angle=90, code=3, col = "green")
points(outputvalues_regularized$percentage, outputvalues_ppca$mean, col = "orange", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_ppca$mean-(1.96*outputvalues_ppca$se), outputvalues_regularized$percentage, outputvalues_ppca$mean+(1.96*outputvalues_ppca$se), length=0.05, angle=90, code=3, col = "orange")
points(outputvalues_regularized$percentage, outputvalues_nipals$mean, col = "deepskyblue2", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_nipals$mean-(1.96*outputvalues_nipals$se), outputvalues_regularized$percentage, outputvalues_nipals$mean+(1.96*outputvalues_nipals$se), length=0.05, angle=90, code=3, col = "deepskyblue2")
points(outputvalues_regularized$percentage, outputvalues_nlpca$mean, col = "dimgrey", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_nlpca$mean-(1.96*outputvalues_nlpca$se), outputvalues_regularized$percentage, outputvalues_nlpca$mean+(1.96*outputvalues_nlpca$se), length=0.05, angle=90, code=3, col = "dimgrey")
points(outputvalues_regularized$percentage, outputvalues_median$mean, col = "black", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_median$mean-(1.96*outputvalues_median$se), outputvalues_regularized$percentage, outputvalues_median$mean+(1.96*outputvalues_median$se), length=0.05, angle=90, code=3, col = "black")
points(outputvalues_regularized$percentage, outputvalues_mice$mean, col = "lightcoral", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_mice$mean-(1.96*outputvalues_mice$se), outputvalues_regularized$percentage, outputvalues_mice$mean+(1.96*outputvalues_mice$se), length=0.05, angle=90, code=3, col = "lightcoral")
points(outputvalues_regularized$percentage, outputvalues_amelia$mean, col = "antiquewhite", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_amelia$mean-(1.96*outputvalues_amelia$se), outputvalues_regularized$percentage, outputvalues_amelia$mean+(1.96*outputvalues_amelia$se), length=0.05, angle=90, code=3, col = "antiquewhite")
points(outputvalues_regularized$percentage, outputvalues_forest$mean, col = "aquamarine", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_forest$mean-(1.96*outputvalues_forest$se), outputvalues_regularized$percentage, outputvalues_forest$mean+(1.96*outputvalues_forest$se), length=0.05, angle=90, code=3, col = "aquamarine")
points(outputvalues_regularized$percentage, outputvalues_mi$mean, col = "darkorchid", pch = 19)
arrows(outputvalues_regularized$percentage, outputvalues_mi$mean-(1.96*outputvalues_mi$se), outputvalues_regularized$percentage, outputvalues_mi$mean+(1.96*outputvalues_mi$se), length=0.05, angle=90, code=3, col = "darkorchid")
legend(0.35,1, c("missMDA Regularized","missMDA EM","pcaMethods SVDimpute","pcaMethods BPCA","pcaMethods PPCA","pcaMethods Nipals","pcaMethods NLPCA","Median imputation","MICE pmm","AMELIA EMB algorithm", "missForest non-parametric", "mi"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue","red","yellow","green","orange","deepskyblue2","dimgrey","black","lightcoral","antiquewhite","aquamarine","darkorchid"), cex=0.7 ,pt.cex=1);
dev.off()
