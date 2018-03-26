#LOAD PACKAGES
library(MASS)
library(Matrix)
library(missForest)
library(magrittr)
library(reshape2)
library(pcaMethods)
library(missMDA)
library(FactoMineR)
library(missForest)
library(Hmisc)
library(mice)
library(Amelia)
library(mi)



#IMPUTATIONS
outputvalues <- data.frame("mean" = numeric(), "se" = numeric())


  
###Simulation, MICE
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  imputed_Data <- mice(real_matrix_miss, m=1, maxit = 100, method = 'pmm')
  imp_matrix <- as.matrix(complete(imputed_Data,1))
  n <- cor(c(X_hat), c(imp_matrix))
  ns <- c(ns, n) 
  count <- count+1
  if(count >= numsim) {
    break
  }
}
meanvalue <- mean(ns)
sevalue <- sd(ns)/sqrt(length(ns))
values <- c(meanvalue,sevalue)
outputvalues[nrow(outputvalues) + 1, ] <- values

###Simulation, AMELIA
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  amelia_fit <- amelia(real_matrix_miss, m=1)
  imp_matrix <- amelia_fit$imputations[[1]]
  n <- cor(c(X_hat), c(imp_matrix))
  ns <- c(ns, n) 
  count <- count+1
  if(count >= numsim) {
    break
  }
}
meanvalue <- mean(ns)
sevalue <- sd(ns)/sqrt(length(ns))
values <- c(meanvalue,sevalue)
outputvalues[nrow(outputvalues) + 1, ] <- values
  
###Simulation, missForest
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  results <- missForest(real_matrix_miss)
  imp_matrix <- results$ximp
  n <- cor(c(X_hat), c(imp_matrix))
  ns <- c(ns, n) 
  count <- count+1
  if(count >= numsim) {
    break
  }
}
meanvalue <- mean(ns)
sevalue <- sd(ns)/sqrt(length(ns))
values <- c(meanvalue,sevalue)
outputvalues[nrow(outputvalues) + 1, ] <- values
  
###Simulation, mi
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  mi_data <- mi(as.data.frame(real_matrix_miss), n.chain = 1)
  imputed <- complete(mi_data,1)
  imp_matrix <- as.matrix(imputed[,1:ncol(real_matrix_miss)])
  n <- cor(c(X_hat), c(imp_matrix))
  ns <- c(ns, n) 
  count <- count+1
  if(count >= numsim) {
    break
  }
}
meanvalue <- mean(ns)
sevalue <- sd(ns)/sqrt(length(ns))
values <- c(meanvalue,sevalue)
outputvalues[nrow(outputvalues) + 1, ] <- values
  


methods <- c("Median imputation", "Mean imputation", "missMDA Regularized", "missMDA EM", "pcaMethods PPCA", 
  "pcaMethods svdImpute", "pcaMethods BPCA", "pcaMethods NIPALS", "pcaMethods NLPCA", "mice",
  "Amelia", "missForest", "mi")
  


