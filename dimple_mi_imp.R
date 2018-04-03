#################################################################################################################################################
#
#The purpose of this function is impute missing datapoints using mi imputation
#The process approximates a Bayesian framework; multiple chains are run and convergence is assessed after a pre-specified number of iterations within each chain. 

#The function takes two arguments:
#
#The simulated matrix with no missingness
#List of matrices with MCAR, MAR, and MNAR patterns.
#
#The function outputs a list of RMSE values between the full matrix and the matrices with missingess.
#RMSE is calculated based on those datapoints that are set to missing.
#
#
#################################################################################################################################################


###PACKAGES
library(mi)

#FUNCTION

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

#LAB
res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
dimple_mi_imp(X_hat = yy$Simulated_matrix, list = res)





