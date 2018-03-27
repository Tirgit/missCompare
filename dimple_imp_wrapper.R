

###PACKAGES


#FUNCTION
dimple_imp_wrapper <- function(X_hat, missfrac_per_var, n.iter) {
  
  collect_res <- data.frame(matrix(NA, nrow = 13*n.iter, ncol = 4))
  colnames(collect_res) <- c("Method", "MCAR_RMSE", "MAR_RMSE", "MNAR_RMSE")
  
  for (i in 1:n.iter) {
    
    collect_res[((13*(n.iter-1))+1):((13*(n.iter-1))+13),1] <- c("Median imputation", "Mean imputation", "missMDA Regularized", 
                                                                 "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                                 "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                                 "mi Bayesian", "Amelia II", "missForest")
    
    res <- dimple_all_patterns(X_hat, missfrac_per_var)
    
    collect_res[1*n.iter,2:4] <- as.data.frame(dimple_median_imp(X_hat, list = res))
    collect_res[2*n.iter,2:4] <- as.data.frame(dimple_mean_imp(X_hat, list = res))
    collect_res[3*n.iter,2:4] <- as.data.frame(dimple_missMDA_regularized_imp(X_hat, list = res))
    collect_res[4*n.iter,2:4] <- as.data.frame(dimple_missMDA_EM_imp(X_hat, list = res))
    collect_res[5*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_PPCA_imp(X_hat, list = res))
    collect_res[6*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_svdImpute_imp(X_hat, list = res))
    collect_res[7*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_BPCA_imp(X_hat, list = res))
    collect_res[8*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_Nipals_imp(X_hat, list = res))
    collect_res[9*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_NLPCA_imp(X_hat, list = res))
    collect_res[10*n.iter,2:4] <- as.data.frame(dimple_mice_mixed_imp(X_hat, list = res))
    collect_res[11*n.iter,2:4] <- as.data.frame(dimple_mi_imp(X_hat, list = res))
    collect_res[12*n.iter,2:4] <- as.data.frame(dimple_AmeliaII_imp(X_hat, list = res))
    collect_res[13*n.iter,2:4] <- as.data.frame(dimple_missForest_imp(X_hat, list = res))
    
  }
  
  list(Imputation_Error = collect_res)
  
}

#LAB
errorres <- dimple_imp_wrapper(X_hat = yy$Simulated_matrix, missfrac_per_var =  y$Fraction_missingness_per_variable, n.iter = 2)
errorres$Imputation_Error




collect_res <- data.frame(matrix(NA, nrow = 0, ncol = 4))
colnames(collect_res) <- c("Method", "MCAR_RMSE", "MAR_RMSE", "MNAR_RMSE")

for (i in 1:n.iter) {
  
  collect_res[((13*(n.iter-1))+1):((13*(n.iter-1))+13),1] <- c("Median imputation", "Mean imputation", "missMDA Regularized", "missMDA EM", "pcaMethods PPCA", 
                             "pcaMethods svdImpute", "pcaMethods BPCA", "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                             "mi Bayesian", "Amelia II", "missForest")
  
  res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable)

  collect_res[1*n.iter,2:4] <- as.data.frame(dimple_median_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[2*n.iter,2:4] <- as.data.frame(dimple_mean_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[3*n.iter,2:4] <- as.data.frame(dimple_missMDA_regularized_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[4*n.iter,2:4] <- as.data.frame(dimple_missMDA_EM_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[5*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_PPCA_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[6*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_svdImpute_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[7*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_BPCA_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[8*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_Nipals_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[9*n.iter,2:4] <- as.data.frame(dimple_pcaMethods_NLPCA_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[10*n.iter,2:4] <- as.data.frame(dimple_mice_mixed_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[11*n.iter,2:4] <- as.data.frame(dimple_mi_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[12*n.iter,2:4] <- as.data.frame(dimple_AmeliaII_imp(X_hat = yy$Simulated_matrix, list = res))
  collect_res[13*n.iter,2:4] <- as.data.frame(dimple_missForest_imp(X_hat = yy$Simulated_matrix, list = res))
  
}

n.iter <- 2

