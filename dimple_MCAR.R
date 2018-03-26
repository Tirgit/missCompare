#################################################################################################################################################
#
#This function uses the generated simulated matrix and generates missing datapoints in MCAR (missing completely at random) pattern per variable.
#
#Inputs are:
#
#simulated matrix
#missing fraction per variable
#
#The function outputs a matrix with missing values and a summary of the matrix.
#
#################################################################################################################################################

###PACKAGES
library(missForest)


###FUNCTION
dimple_MCAR <- function(X_hat, missfrac_per_var) {
  
  for (i in 1:length(missfrac_per_var)) {
    X_hat[,i] <- prodNA(as.matrix(X_hat[,i]), noNA = missfrac_per_var[i])
  }
  
  matrix_summary <- summary(X_hat)
  
  list(MCAR_matrix = X_hat, Summary = matrix_summary)
  
}


###LAB
res <- dimple_MCAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
matrixplot(res$MCAR_matrix, interactive = F, col= "red") 



