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


###FUNCTIONS
dimple_get_data <- function(X, matrixplot_sort = F ,missplot = F) {
  comp <- sum(complete.cases(X))
  rows <- nrow(X)
  cols <- ncol(X)
  mat <- cor(X, use="complete.obs", method="pearson") 
  missfrac_per_df <- sum(is.na(X))/(nrow(X)*ncol(X))
  missfrac_per_var <- colMeans(is.na(df_miss))
  na_per_df <-  sum(is.na(X))
  na_per_var <- sapply(X, function(x) sum(length(which(is.na(x))))) 
  mdpat <- md.pattern(X)
  
  nm1 <- names(X)[colSums(is.na(X)) >0]
  arr_X <- X %>% 
    arrange_at(vars(nm1), funs(desc(is.na(.))))
  
  if (matrixplot_sort == F) matrixplot(X, interactive = F, col= "red") else matrixplot(arr_X, interactive = F, col= "red")
  list(Complete_cases = comp, Rows = rows, Columns = cols, Corr_matrix = mat, Fraction_missingness = missfrac_per_df, Fraction_missingness_per_variable = missfrac_per_var, Total_NA = na_per_df, NA_per_variable = na_per_var, MD_Pattern = mdpat)
}
dimple_sim_df <- function(rownum, colnum, cormat, meanval = 0, sdval = 1) {
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
  
  logi <- sample(0:1, length(missfrac_per_var), replace = T)
  
  for (i in 1:length(missfrac_per_var)) {
    if (logi[i]==1) X_hat <- X_hat[order(X_hat[,i], decreasing = T),] else X_hat <- X_hat[order(X_hat[,i], decreasing = F),]
    threshold_for_excl <- X_hat[,i][ceiling(missfrac_per_var[i]*nrow(X_hat))]
    if (logi[i]==1) X_hat[,i] <- ifelse(X_hat[,i]>threshold_for_excl, NA, X_hat[,i]) else X_hat[,i] <- ifelse(X_hat[,i]<threshold_for_excl, NA, X_hat[,i]) 
  }
  
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]
  
  matrix_summary <- summary(X_hat)
  
  list(MNAR_matrix = X_hat, Summary = matrix_summary)
  
}
dimple_MAR <- function(X_hat, missfrac_per_var) {
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  logi <- sample(0:1, length(missfrac_per_var)-1, replace = T)
  
  for (i in 1:(length(missfrac_per_var)-1)) {
    if (logi[i]==1) X_hat <- X_hat[order(X_hat[,i+1], decreasing = T),] else X_hat <- X_hat[order(X_hat[,i+1], decreasing = F),]
    threshold_for_excl <- X_hat[,i+1][ceiling(missfrac_per_var[i]*nrow(X_hat))]
    if (logi[i]==1) X_hat[,i] <- ifelse(X_hat[,i+1]>threshold_for_excl, NA, X_hat[,i]) else X_hat[,i] <- ifelse(X_hat[,i+1]<threshold_for_excl, NA, X_hat[,i]) 
  }
  
  logi2 <- sample(0:1, 1)
  
  if (logi2==1) X_hat <- X_hat[order(X_hat[,1], decreasing = T),] else X_hat <- X_hat[order(X_hat[,1], decreasing = F),]
  X_hat[1:(nrow(X_hat)*(missfrac_per_var[length(missfrac_per_var)])),length(missfrac_per_var)] <- NA
  
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
dimple_pcaMethods_PPCA_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  pcaMethods_PPCA_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    if (ncomp$ncp>0) nresPPCA <- pca(X, method="ppca", center=FALSE, nPcs=ncomp$ncp) else nresPPCA <- pca(X, method="ppca", center=FALSE, nPcs=2)
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
dimple_pcaMethods_svdImpute_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  pcaMethods_svdImpute_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    if (ncomp$ncp>0) nresPPCA <- pca(X, method="svdImpute", center=FALSE, nPcs=ncomp$ncp) else nresPPCA <- pca(X, method="svdImpute", center=FALSE, nPcs=2)
    imp_matrix <- resPPCA@completeObs
    
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
dimple_pcaMethods_BPCA_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  pcaMethods_BPCA_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    if (ncomp$ncp>0) nresPPCA <- pca(X, method="bpca", center=FALSE, nPcs=ncomp$ncp) else nresPPCA <- pca(X, method="bpca", center=FALSE, nPcs=2)
    imp_matrix <- resPPCA@completeObs
    
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
    if (ncomp$ncp>0) nresPPCA <- pca(X, method="nipals", center=FALSE, nPcs=ncomp$ncp) else nresPPCA <- pca(X, method="nipals", center=FALSE, nPcs=2)
    imp_matrix <- resPPCA@completeObs
    
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
dimple_pcaMethods_NLPCA_imp <- function(X_hat, list) {
  
  index <- lapply(list, is.na)  
  
  pcaMethods_NLPCA_imp <- function(X) {
    ncomp <- estim_ncpPCA(X)
    if (ncomp$ncp>0) nresPPCA <- pca(X, method="nlpca", center=FALSE, nPcs=ncomp$ncp, maxSteps=100) else nresPPCA <- pca(X, method="nlpca", center=FALSE, nPcs=2, maxSteps=100)
    imp_matrix <- resPPCA@completeObs
    
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



###LAB
df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
library(missForest)
df_miss <- prodNA(df, 0.3)

y <- dimple_get_data(df_miss, matrixplot_sort = T)

yy <- dimple_sim_df(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)

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




