


###PACKAGES
library(mice)

#FUNCTION

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

#LAB
res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
dimple_mice_mixed_imp(X_hat = yy$Simulated_matrix, list = res)




