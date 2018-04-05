###PACKAGES
library(mice)
library(VIM)
library(ggplot2)
library(magrittr)
library(dplyr)
library(MASS)
library(Matrix)
library(missForest)
library(missMDA)
library(pcaMethods)
library(mi)
library(Amelia)
library(tidyr)
library(pROC)
library(ROCR)  


###FUNCTIONS
dimple_clean <- function(x, var_removal_threshold = 0.5, ind_removal_threshold = 0.9, missingness_coding = NA) {
  
  x[x == missingness_coding] <- NA
  
  missfrac_per_var <- colMeans(is.na(x))
  vars_above_thres <- colnames(x)[missfrac_per_var >= var_removal_threshold]
  if (length(vars_above_thres) != 0) new_df <- x[, -which(missfrac_per_var >= var_removal_threshold)] else new_df <- x
  
  if (length(vars_above_thres) != 0) message(paste("Variable(s) ",
                                                   (paste(vars_above_thres,collapse=", ") ),
                                                   " removed due to exceeding the pre-defined removal threshold (>",
                                                   var_removal_threshold*100,
                                                   "%) for missingness.", sep= ""))
  
  missfrac_per_ind <- rowMeans(is.na(new_df))
  inds_above_thres <- rownames(x)[missfrac_per_ind >= ind_removal_threshold]
  if (length(inds_above_thres) != 0) clean_df <- new_df[-which(missfrac_per_ind >= ind_removal_threshold), ] else clean_df <- new_df
  
  if (length(inds_above_thres) != 0) message(paste(length(inds_above_thres),
                                                   "individual(s) removed due to exceeding the pre-defined removal threshold (>",
                                                   ind_removal_threshold*100,
                                                   "%) for missingness.", sep= ""))
  
  list(Dataframe_clean = clean_df)
  
}
dimple_get_data <- function(X, matrixplot_sort = F ,missplot = F) {
  comp <- sum(complete.cases(X))
  rows <- nrow(X)
  cols <- ncol(X)
  mat <- cor(X, use="pairwise.complete.obs", method="pearson") 
  missfrac_per_df <- sum(is.na(X))/(nrow(X)*ncol(X))
  missfrac_per_var <- colMeans(is.na(X))
  na_per_df <-  sum(is.na(X))
  na_per_var <- sapply(X, function(x) sum(length(which(is.na(x))))) 
  mdpat <- md.pattern(X)
  
  nm1 <- names(X)[colSums(is.na(X)) >0]
  arr_X <- X %>% 
    arrange_at(vars(nm1), funs(desc(is.na(.))))
  
  vars_above_half <- colnames(X)[missfrac_per_var>=0.5]
  if (length(vars_above_half) != 0) message(paste("Warning! Missingness exceeds 50% for variable(s) ",
                                                  (paste(vars_above_half,collapse=", ") ),
                                                  ". Consider excluding these variables using dimple_clean() and repeating function until no warnings are shown.", sep= ""))
  
  #matrix plot
  if (matrixplot_sort == F) matrixplot(X, interactive = F, col= "red") else matrixplot(arr_X, interactive = F, col= "red")
  
  #output
  list(Complete_cases = comp, Rows = rows, Columns = cols, Corr_matrix = mat, 
       Fraction_missingness = missfrac_per_df, Fraction_missingness_per_variable = missfrac_per_var, 
       Total_NA = na_per_df, NA_per_variable = na_per_var, MD_Pattern = mdpat,
       Vars_above_half = vars_above_half)
  
}
dimple_sim <- function(rownum, colnum, cormat, meanval = 0, sdval = 1) {
  pd_corr_matrix <- nearPD(cormat, keepDiag=T, conv.tol = 1e-7, corr=T)
  mu <- rep(meanval,colnum)
  stddev <- rep(sdval,colnum)
  covMat <- stddev %*% t(stddev) * pd_corr_matrix$mat
  X_hat <- mvrnorm(n=rownum, mu=mu, Sigma=covMat, empirical=TRUE) # Simulated values
  original_sample <- cormat[1:5,1:5]
  nearPD_sample <- cor(X_hat)[1:5,1:5]
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  list(Simulated_matrix = X_hat, Original_correlation_sample = original_sample, NearPD_correlation_sample = nearPD_sample)
}
dimple_MCAR <- function(X_hat, missfrac_per_var) {
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  for (i in 1:length(missfrac_per_var)) {
    X_hat[,i] <- prodNA(as.matrix(X_hat[,i]), noNA = missfrac_per_var[i])
  }
  
  matrix_summary <- summary(X_hat)
  
  list(MCAR_matrix = X_hat, Summary = matrix_summary)
  
}
dimple_MNAR <- function(X_hat, missfrac_per_var) {
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  logi <- sample(0:2, length(missfrac_per_var), replace = T)
  
  for (i in 1:length(missfrac_per_var)) {
    
    Q1 <- quantile(X_hat[,i])[2]
    Q2 <- quantile(X_hat[,i])[3]
    Q3 <- quantile(X_hat[,i])[4]
    
    low_ind <- X_hat[,i] <= Q2
    mid_ind <- X_hat[,i] <= Q3 & X_hat[,i] >= Q1
    high_ind <- X_hat[,i] >= Q2
    
    if (logi[i]==0) to_NA <- sample(rownames(X_hat)[low_ind], missfrac_per_var[i]*nrow(X_hat)) else if (logi[i]==1) to_NA <- sample(rownames(X_hat)[mid_ind], missfrac_per_var[i]*nrow(X_hat)) else to_NA <- sample(rownames(X_hat)[high_ind], missfrac_per_var[i]*nrow(X_hat))
    
    X_hat[,i][to_NA] <- NA    
  }
  
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]
  
  matrix_summary <- summary(X_hat)
  
  list(MNAR_matrix = X_hat, Summary = matrix_summary)
  
}
dimple_MAR <- function(X_hat, missfrac_per_var) {
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  logi <- sample(0:2, length(missfrac_per_var)-1, replace = T)
  
  for (i in 1:(length(missfrac_per_var)-1)) {
    
    Q1 <- quantile(X_hat[,i+1])[2]
    Q2 <- quantile(X_hat[,i+1])[3]
    Q3 <- quantile(X_hat[,i+1])[4]
    
    low_ind <- X_hat[,i+1] <= Q2
    mid_ind <- X_hat[,i+1] <= Q3 & X_hat[,i+1] >= Q1
    high_ind <- X_hat[,i+1] >= Q2
    
    if (logi[i]==0) to_NA <- sample(rownames(X_hat)[low_ind], missfrac_per_var[i]*nrow(X_hat)) else if (logi[i]==1) to_NA <- sample(rownames(X_hat)[mid_ind], missfrac_per_var[i]*nrow(X_hat)) else to_NA <- sample(rownames(X_hat)[high_ind], missfrac_per_var[i]*nrow(X_hat))
    
    X_hat[,i][to_NA] <- NA    
  }
  
  logi2 <- sample(0:2, 1)
  
  Q1 <- quantile(X_hat[,1], na.rm = T)[2]
  Q2 <- quantile(X_hat[,1], na.rm = T)[3]
  Q3 <- quantile(X_hat[,1], na.rm = T)[4]
  
  low_ind <- X_hat[,1] <= Q2
  mid_ind <- X_hat[,1] <= Q3 & X_hat[,1] >= Q1
  high_ind <- X_hat[,1] >= Q2
  NAs <- is.na(X_hat[,1])
  
  if (logi2==0) to_NA <- sample(rownames(X_hat)[low_ind | NAs], missfrac_per_var[length(missfrac_per_var)]*nrow(X_hat)) else if (logi2==1) to_NA <- sample(rownames(X_hat)[mid_ind | NAs], missfrac_per_var[length(missfrac_per_var)]*nrow(X_hat)) else to_NA <- sample(rownames(X_hat)[high_ind | NAs], missfrac_per_var[length(missfrac_per_var)]*nrow(X_hat))
  
  X_hat[,10][to_NA] <- NA      
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]
  
  matrix_summary <- summary(X_hat)
  
  list(MAR_matrix = X_hat, Summary = matrix_summary)
  
}
dimple_all_patterns <- function(X_hat, missfrac_per_var) {
  
  MCAR <- dimple_MCAR(X_hat, missfrac_per_var)
  MAR <- dimple_MAR(X_hat, missfrac_per_var)
  MNAR <- dimple_MNAR(X_hat, missfrac_per_var)
  
  list(MCAR_matrix = MCAR$MCAR_matrix , MAR_matrix = MAR$MAR_matrix, MNAR_matrix = MNAR$MNAR_matrix)
  
}

dimple_random_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  random_imp <- function(X) {
    for(i in 1:ncol(X)) {
      X[,i][is.na(X[,i])] <- sample(X[,i][!is.na(X[,i])], size=sum(is.na(X[,i])), replace=T)
    }
    
    list(Imputed = X)
  }
  
  results <- lapply(list, random_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_median_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  median_imp <- function(X) {
    for(i in 1:ncol(X)) {
      X[is.na(X[,i]), i] <- median(X[,i], na.rm = TRUE)
    }
    
    list(Imputed = X)
  }
  
  results <- lapply(list, median_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_mean_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  mean_imp <- function(X) {
    for(i in 1:ncol(X)) {
      X[is.na(X[,i]), i] <- mean(X[,i], na.rm = TRUE)
    }
    
    list(Imputed = X)
  }
  
  results <- lapply(list, mean_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_missMDA_regularized_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  missMDA_regularized_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    res.imp <- imputePCA(X, ncp= ncomp$ncp, method = "Regularized")
    imp_matrix <- res.imp$completeObs
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, missMDA_regularized_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_missMDA_EM_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  missMDA_EM_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    res.imp <- imputePCA(X, ncp= ncomp$ncp, method = "EM")
    imp_matrix <- res.imp$completeObs
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, missMDA_EM_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_pcaMethods_BPCA_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  pcaMethods_BPCA_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    if (ncomp$ncp>0) resBPCA <- pca(X, method="bpca", center=FALSE, nPcs=ncomp$ncp) else resBPCA <- pca(X, method="bpca", center=FALSE, nPcs=2)
    imp_matrix <- resBPCA@completeObs
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, pcaMethods_BPCA_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_pcaMethods_Nipals_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  pcaMethods_Nipals_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    if (ncomp$ncp>0) resNipals <- pca(X, method="nipals", center=FALSE, nPcs=ncomp$ncp) else resNipals <- pca(X, method="nipals", center=FALSE, nPcs=2)
    imp_matrix <- resNipals@completeObs
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, pcaMethods_Nipals_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_pcaMethods_svdImpute_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  pcaMethods_svdImpute_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    if (ncomp$ncp>0) ressvdImpute <- pca(X, method="svdImpute", center=FALSE, nPcs=ncomp$ncp) else ressvdImpute <- pca(X, method="svdImpute", center=FALSE, nPcs=2)
    imp_matrix <- ressvdImpute@completeObs
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, pcaMethods_svdImpute_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_pcaMethods_PPCA_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  pcaMethods_PPCA_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    if (ncomp$ncp>0) resPPCA <- pca(X, method="ppca", center=FALSE, nPcs=ncomp$ncp) else resPPCA <- pca(X, method="ppca", center=FALSE, nPcs=2)
    imp_matrix <- resPPCA@completeObs
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, pcaMethods_PPCA_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_pcaMethods_NLPCA_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  pcaMethods_NLPCA_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    if (ncomp$ncp>0) resNLPCA <- pca(X, method="nlpca", center=FALSE, nPcs=ncomp$ncp, maxSteps=100) else resNLPCA <- pca(X, method="nlpca", center=FALSE, nPcs=2, maxSteps=100)
    imp_matrix <- resNLPCA@completeObs
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, pcaMethods_NLPCA_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_mice_mixed_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  mice_mixed_imp <- function(X) {
    imputed_Data <- mice(X, m=1, maxit = 100)
    imp_matrix <- as.matrix(mice::complete(imputed_Data,1))
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, mice_mixed_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_mi_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  mi_imp <- function(X) {
    mi_data <- mi(as.data.frame(X), n.chain = 1, n.iter = 100)
    imputed <- mi::complete(mi_data,1)
    imp_matrix <- as.matrix(imputed[,1:ncol(X)])
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, mi_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_AmeliaII_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  AmeliaII_imp <- function(X) {
    amelia_fit <- amelia(X, m=1)
    imp_matrix <- amelia_fit$imputations[[1]]
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, AmeliaII_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}
dimple_missForest_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  missForest_imp <- function(X) {
    results <- missForest(X, maxiter = 10, ntree = 100, replace = T)
    imp_matrix <- results$ximp
    
    
    list(Imputed = imp_matrix)
  }
  
  results <- lapply(list, missForest_imp)
  
  #using NA index to identify the original values (later set to missing)
  orig_MCAR <- X_hat[index[[1]]]
  orig_MAR <- X_hat[index[[2]]]
  orig_MNAR <- X_hat[index[[3]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  
  list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
}

dimple_imp_wrapper <- function(rownum, colnum, cormat, missfrac_per_var, n.iter = 10) {
  
  collect_res <- data.frame(matrix(NA, nrow = 14*n.iter, ncol = 4))
  colnames(collect_res) <- c("Method", "MCAR_RMSE", "MAR_RMSE", "MNAR_RMSE")
  
  for (i in 1:n.iter) {
    
    collect_res[((14*(i-1))+1):((14*(i-1))+14),1] <- c("Random replacement", "Median imputation", "Mean imputation", "missMDA Regularized", 
                                                       "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                       "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                       "mi Bayesian", "Amelia II", "missForest")
    
    sim <- dimple_sim(rownum, colnum, cormat)
    res <- dimple_all_patterns(sim$Simulated_matrix, missfrac_per_var)
    
    collect_res[((14*(i-1))+1),2:4] <- as.data.frame(dimple_random_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+2),2:4] <- as.data.frame(dimple_median_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+3),2:4] <- as.data.frame(dimple_mean_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+4),2:4] <- as.data.frame(dimple_missMDA_regularized_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+5),2:4] <- as.data.frame(dimple_missMDA_EM_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+6),2:4] <- as.data.frame(dimple_pcaMethods_PPCA_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+7),2:4] <- as.data.frame(dimple_pcaMethods_svdImpute_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+8),2:4] <- as.data.frame(dimple_pcaMethods_BPCA_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+9),2:4] <- as.data.frame(dimple_pcaMethods_Nipals_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+10),2:4] <- as.data.frame(dimple_pcaMethods_NLPCA_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+11),2:4] <- as.data.frame(dimple_mice_mixed_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+12),2:4] <- as.data.frame(dimple_mi_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+13),2:4] <- as.data.frame(dimple_AmeliaII_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+14),2:4] <- as.data.frame(dimple_missForest_imp(sim$Simulated_matrix, list = res))
    
  }
  
  RMSE_stats <- collect_res %>%
    group_by(Method) %>%
    summarise(mean_RMSE_MCAR = mean(MCAR_RMSE, na.rm=T),
              mean_RMSE_MAR = mean(MAR_RMSE, na.rm=T),
              mean_RMSE_MNAR = mean(MNAR_RMSE, na.rm=T),
              SD_RMSE_MCAR = sd(MCAR_RMSE, na.rm=T),
              SD_RMSE_MAR = sd(MAR_RMSE, na.rm=T),
              SD_RMSE_MNAR = sd(MNAR_RMSE, na.rm=T),
              n_RMSE_MCAR = sum(!is.na(MCAR_RMSE)),
              n_RMSE_MAR = sum(!is.na(MAR_RMSE)),
              n_RMSE_MNAR = sum(!is.na(MNAR_RMSE))) %>%
    mutate(SE_RMSE_MCAR = SD_RMSE_MCAR / sqrt(n_RMSE_MCAR),
           lower.ci_RMSE_MCAR = mean_RMSE_MCAR - qt(1 - (0.05 / 2), n_RMSE_MCAR - 1) * SE_RMSE_MCAR,
           upper.ci_RMSE_MCAR = mean_RMSE_MCAR + qt(1 - (0.05 / 2), n_RMSE_MCAR - 1) * SE_RMSE_MCAR,
           SE_RMSE_MAR = SD_RMSE_MAR / sqrt(n_RMSE_MAR),
           lower.ci_RMSE_MAR = mean_RMSE_MAR - qt(1 - (0.05 / 2), n_RMSE_MAR - 1) * SE_RMSE_MAR,
           upper.ci_RMSE_MAR = mean_RMSE_MAR + qt(1 - (0.05 / 2), n_RMSE_MAR - 1) * SE_RMSE_MAR,
           SE_RMSE_MNAR = SD_RMSE_MNAR / sqrt(n_RMSE_MNAR),
           lower.ci_RMSE_MNAR = mean_RMSE_MNAR - qt(1 - (0.05 / 2), n_RMSE_MNAR - 1) * SE_RMSE_MNAR,
           upper.ci_RMSE_MNAR = mean_RMSE_MNAR + qt(1 - (0.05 / 2), n_RMSE_MNAR - 1) * SE_RMSE_MNAR) %>% 
    dplyr::select(Method, mean_RMSE_MCAR, mean_RMSE_MAR, mean_RMSE_MNAR, lower.ci_RMSE_MCAR, upper.ci_RMSE_MCAR, 
                  lower.ci_RMSE_MAR, upper.ci_RMSE_MAR, lower.ci_RMSE_MNAR, upper.ci_RMSE_MNAR)
  
  
  #Best methods for the three missingness types
  Best_method_MCAR <- RMSE_stats %>%
    filter(mean_RMSE_MCAR == min(mean_RMSE_MCAR)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  Best_method_MAR <- RMSE_stats %>%
    filter(mean_RMSE_MAR == min(mean_RMSE_MAR)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  Best_method_MNAR <- RMSE_stats %>%
    filter(mean_RMSE_MNAR == min(mean_RMSE_MNAR)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  forgraph <- gather(collect_res, Pattern, RMSE, MCAR_RMSE:MNAR_RMSE, factor_key=TRUE)
  forgraph$Method <- factor(forgraph$Method, levels = c("Random replacement", "Median imputation", "Mean imputation", "missMDA Regularized", 
                                                        "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                        "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                        "mi Bayesian", "Amelia II", "missForest"))
  levels(forgraph$Pattern) <- c("MCAR", "MAR", "MNAR")
  p <- ggplot(forgraph, aes(x=Method, y=RMSE, fill=Method)) + 
    geom_boxplot() +
    facet_grid(~Pattern, scale="free") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #output list
  list(Imputation_RMSE = collect_res, Imputation_RMSE_means = RMSE_stats, Best_method_MCAR = Best_method_MCAR,
       Best_method_MAR = Best_method_MAR, Best_method_MNAR = Best_method_MNAR, Plot = p)
  
  
}
dimple_summary <- function(wrapper_output) {
  
  print(paste("In case you assume a missing completely at random (MCAR) missingness pattern, dimple suggests you to use the ",
              wrapper_output$Best_method_MCAR,
              " algorithm for imputation", sep= ""))
  print(paste("In case you assume a missing at random (MAR) missingness pattern, dimple suggests you to use the ",
              wrapper_output$Best_method_MAR,
              " algorithm for imputation", sep= ""))
  print(paste("In case you assume a missing not at random (MNAR) missingness pattern, dimple suggests you to use the ",
              wrapper_output$Best_method_MNAR,
              " algorithm for imputation", sep= ""))
  
  wrapper_output$Plot
  
}



###LAB
df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
df_miss <- prodNA(df, 0.3)

cleaned <- dimple_clean(df_miss)
y <- dimple_get_data(cleaned$Dataframe_clean, matrixplot_sort = T)

yy <- dimple_sim(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)

res <- dimple_MCAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
res <- dimple_MNAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
res <- dimple_MAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)

res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
matrixplot(res$MCAR_matrix, interactive = F, col= "red") 
matrixplot(res$MAR_matrix, interactive = F, col= "red") 
matrixplot(res$MNAR_matrix, interactive = F, col= "red") 

dimple_median_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_mean_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_missMDA_regularized_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_missMDA_EM_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_PPCA_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_svdImpute_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_BPCA_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_Nipals_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_NLPCA_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_mice_mixed_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_mi_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_AmeliaII_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_missForest_imp(X_hat = yy$Simulated_matrix, list = res)

wrap2 <- dimple_imp_wrapper(rownum = y$Rows, 
                           colnum = y$Columns, 
                           cormat = y$Corr_matrix, 
                           missfrac_per_var =  y$Fraction_missingness_per_variable, 
                           n.iter = 6)

dimple_summary(wrap)
dimple_summary(wrap2)

saveRDS(wrap, file = "/Users/med-tv_/Documents/Projects/missingdata/wrap.rds")
saveRDS(wrap2, file = "/Users/med-tv_/Documents/Projects/missingdata/wrap2.rds")

wrap0 <- readRDS("/Users/med-tv_/Documents/Projects/missingdata/wrap.rds")
wrap1 <- readRDS("/Users/med-tv_/Documents/Projects/missingdata/wrap2.rds")





pdf("/Users/med-tv_/Documents/Projects/missingdata/RMSE_plot.pdf")
wrap$Plot
dev.off()






#REAL DATA
library(foreign)
mydata <- read.dta("/Volumes/External/LOCUS/pheno/glacier_corr_160127.dta")
vars <- colnames(mydata)
to_exclude <- c(grep("da", names(mydata), value=TRUE) ,
                grep("gram", names(mydata), value=TRUE),
                grep("sum", names(mydata), value=TRUE),
                grep("upps", names(mydata), value=TRUE),
                grep("delp", names(mydata), value=TRUE)
                )
mydata <- mydata[, !(names(mydata) %in% to_exclude )] 
mydata <- mydata[mydata$besok == 1,]
mydata <- mydata[!is.na(mydata$id),]
mydata <- mydata[1:2000, 2:16]



y <- dimple_get_data(mydata, matrixplot_sort = T)
clean <- dimple_clean(mydata)
y <- dimple_get_data(clean$Dataframe_clean, matrixplot_sort = T)

yy <- dimple_sim(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)

wrap <- dimple_imp_wrapper(rownum = y$Rows, 
                           colnum = y$Columns, 
                           cormat = y$Corr_matrix, 
                           missfrac_per_var =  y$Fraction_missingness_per_variable, 
                           n.iter = 5)












