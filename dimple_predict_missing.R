

#given available data predicts missing data (diganostics for MAR)
#first convert missing vs available data to 0/1
#set up logistic regression model and random forest to predict 0/1 outcome

dimple_predict_missing <- function(rownum, colnum, cormat, missfrac_per_var, bootstrap) {
  
  sim <- dimple_sim_df(rownum, colnum, cormat)
  res <- dimple_all_patterns(sim$Simulated_matrix, missfrac_per_var)
  
  
  
  
}


#LAB
dimple_predict_missing(rownum = y$Rows, 
                       colnum = y$Columns, 
                       cormat = y$Corr_matrix, 
                       missfrac_per_var =  y$Fraction_missingness_per_variable, 
                       bootstrap = 100)


#loop around to predict all variables and create mean AUC and 95% CIs




# 0 if available data
outcome <- rep(0, 1000)
# 1 if missing
outcome[is.na(res$MCAR_matrix[,1])] <- 1
#predict using logistic regression using the rest of the variables, obtain AUC ROC

#predict using random forest using the rest of the variables, obtain AUC ROC
