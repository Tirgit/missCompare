#################################################################################################################################################
#
#This function uses the generated simulated matrix and generates missing datapoints in MAR, MCAR and MNAR pattern per variable.
#
#Inputs are:
#
#simulated matrix
#missing fraction per variable
#
#The function outputs a list of three matrices with missing values.
#
#################################################################################################################################################

###PACKAGES
library(missForest)


###FUNCTION
dimple_all_patterns <- function(X_hat, missfrac_per_var) {
  
  MCAR <- dimple_MCAR(X_hat, missfrac_per_var)
  MAR <- dimple_MAR(X_hat, missfrac_per_var)
  MNAR <- dimple_MNAR(X_hat, missfrac_per_var)
  
  list(MCAR_matrix = MCAR$MCAR_matrix , MAR_matrix = MAR$MAR_matrix, MNAR_matrix = MNAR$MNAR_matrix)
  
}


###LAB
res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable)


