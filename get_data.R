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



#define in function
filename <- "/Users/med-tv_/Documents/Projects/missingdata/pheno.txt"
NA_gen <- 0
missplot <- "/Users/med-tv_/Documents/Projects/missingdata/missingness_plot.pdf"
numsim <- 10

#STEP 1 - GET AND CLEAN DATASET
df <- read.delim(filename, stringsAsFactors = F)
N <- nrow(df)
#remove character variables
df <- df[,!sapply(df, is.character)]
#remove monomorphic variables (variables that take the same value for all observations)
df <- df[,sapply(df, sd, na.rm = T) != 0]
miss <- sum(is.na(df))/(ncol(df)*nrow(df))
df <- prodNA(df, noNA = NA_gen)
#create native correlation matrix
x <- cor(df, use = "pairwise.complete.obs")
corr_matrix <- Matrix(as.matrix(x))
#find nearest positive definitive correlation matrix
pd_corr_matrix <- nearPD(corr_matrix, keepDiag=T, conv.tol = 1e-7, corr=T)
M <- ncol(pd_corr_matrix$mat)
mu <- rep(0,M)
stddev <- rep(1,M)
covMat <- stddev %*% t(stddev) * pd_corr_matrix$mat
X_hat <- mvrnorm(n=N, mu=mu, Sigma=covMat, empirical=TRUE) # Simulated values
#compare correlation coefficients (defaul all variables):
corr_matrix[1:5,1:5]
cor(X_hat)[1:5,1:5]

###MISSING DATA OBSERVATION, PLOTTING
ggplot_missing <- function(x) {
 x %>% 
 is.na %>%
 melt %>%
 ggplot(data = ., aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill = value)) +
  scale_fill_grey(name = "", labels = c("Present","Missing")) +
  theme_minimal() + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
  labs(x = "Variables in Dataset", y = "Rows / observations")
}
if (!is.na(missplot)) {
  pdf(missplot)
  p <- ggplot_missing(df)
  print(p)
  dev.off()
}

#IMPUTATIONS
outputvalues <- data.frame("mean" = numeric(), "se" = numeric())

###Simulation, MEDIAN imputation
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  for(i in 1:ncol(real_matrix_miss)){
    real_matrix_miss[is.na(real_matrix_miss[,i]), i] <- median(real_matrix_miss[,i], na.rm = TRUE)
  }
  imp_matrix <- real_matrix_miss
  n <- cor(c(X_hat), c(imp_matrix))
  ns <- c(ns, n) 
  count <- count+1
  if(count > numsim) {
    break
  }
}
meanvalue <- mean(ns)
sevalue <- sd(ns)/sqrt(length(ns))
values <- c(meanvalue,sevalue)
outputvalues[nrow(outputvalues) + 1, ] <- values

###Simulation, MEAN imputation
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  for(i in 1:ncol(real_matrix_miss)){
    real_matrix_miss[is.na(real_matrix_miss[,i]), i] <- mean(real_matrix_miss[,i], na.rm = TRUE)
  }
  imp_matrix <- real_matrix_miss
  n <- cor(c(X_hat), c(imp_matrix))
  ns <- c(ns, n) 
  count <- count+1
  if(count > numsim) {
    break
  }
}
meanvalue <- mean(ns)
sevalue <- sd(ns)/sqrt(length(ns))
values <- c(meanvalue,sevalue)
outputvalues[nrow(outputvalues) + 1, ] <- values
  
###Simulation, missMDA REGULARIZED method
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  ncomp <- estim_ncpPCA(real_matrix_miss)
  res.imp <- imputePCA(real_matrix_miss, ncp= ncomp$ncp, method = "Regularized")
  res.pca.miss <- PCA(res.imp$completeObs, graph = F)
  imp_matrix <- res.imp$completeObs
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

###Simulation, missMDA EM method
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  ncomp <- estim_ncpPCA(real_matrix_miss)
  res.imp <- imputePCA(real_matrix_miss, ncp= ncomp$ncp, method = "EM")
  res.pca.miss <- PCA(res.imp$completeObs, graph = F)
  imp_matrix <- res.imp$completeObs
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

###Simulation, pcaMethods PPCA
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  ncomp <- estim_ncpPCA(real_matrix_miss)
  resPPCA <- pca(real_matrix_miss, method="ppca", center=FALSE, nPcs=ncomp$ncp)
  imp_matrix <- resPPCA@completeObs
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
  
###Simulation, pcaMethods SVDIMPUTE
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  ncomp <- estim_ncpPCA(real_matrix_miss)
  resSVDI <- pca(real_matrix_miss, method="svdImpute", center=FALSE, nPcs=ncomp$ncp)
  imp_matrix <- resSVDI@completeObs
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

###Simulation, pcaMethods BPCA
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  ncomp <- estim_ncpPCA(real_matrix_miss)
  resBPCA <- pca(real_matrix_miss, method="bpca", center=FALSE, nPcs=ncomp$ncp)
  imp_matrix <- resBPCA@completeObs
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

###Simulation, pcaMethods NIPALS
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  ncomp <- estim_ncpPCA(real_matrix_miss)
  resNipals <- pca(real_matrix_miss, method="nipals", center=FALSE, nPcs=ncomp$ncp)
  imp_matrix <- resNipals@completeObs
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
  
###Simulation, pcaMethods NLPCA
ns <- NULL
count <- 1
repeat {
  real_matrix_miss <- prodNA(X_hat, noNA = miss)
  ncomp <- estim_ncpPCA(real_matrix_miss)
  resNLPCA <- pca(real_matrix_miss, method="nlpca", center=FALSE, nPcs=ncomp$ncp, maxSteps=100)
  imp_matrix <- resNLPCA@completeObs
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
  


