#################################################################################################################################################
#
#The purpose of this function is impute missing datapoints using missMDA EM imputation
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
library(missMDA)


#FUNCTION

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
  if (length(index)==4) orig_MAP <- X_hat[index[[4]]]
  
  #using NA index to identify the imputed values
  imp_MCAR <- results$MCAR_matrix$Imputed[index[[1]]]
  imp_MAR <- results$MAR_matrix$Imputed[index[[2]]]
  imp_MNAR <- results$MNAR_matrix$Imputed[index[[3]]]
  if (length(index)==4) imp_MAP <- results$MAP_matrix$Imputed[index[[4]]]
  
  #RMSE
  rmse_MCAR <- sqrt(mean((orig_MCAR-imp_MCAR)^2))
  rmse_MAR <- sqrt(mean((orig_MAR-imp_MAR)^2))
  rmse_MNAR <- sqrt(mean((orig_MNAR-imp_MNAR)^2))
  if (length(index)==4) rmse_MAP <- sqrt(mean((orig_MAP-imp_MAP)^2))
  
  if (length(index)==4) list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR, MAP_RMSE = rmse_MAP) else list(MCAR_RMSE = rmse_MCAR, MAR_RMSE = rmse_MAR, MNAR_RMSE = rmse_MNAR)
  
  
}



#LAB
#res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
#dimple_missMDA_EM_imp(X_hat = yy$Simulated_matrix, list = res)



