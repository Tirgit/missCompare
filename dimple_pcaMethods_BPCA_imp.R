




###PACKAGES
library(pcaMethods)


#FUNCTION

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

#LAB
res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
dimple_pcaMethods_BPCA_imp(X_hat = yy$Simulated_matrix, list = res)





