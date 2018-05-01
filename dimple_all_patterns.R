#################################################################################################################################################
#
#This function uses the generated simulated matrix and generates missing datapoints in MAR, MCAR and MNAR pattern per variable.
#
#Inputs are:
#
#simulated matrix
#missing fraction per variable
#assumed pattern (optional) - vector of missingess types (must be same length as missingness fraction per variable)
#window with default 0.5. This regulates the "extremity of missingness spike in (larger windows result in more sparse missing data placement
#whereas smaller windows result in more dense missing data per value - stronger patterns of missingness)
#
#The function outputs a list of three matrices with missing values.
#
#################################################################################################################################################

###PACKAGES
library(missForest)


###FUNCTION
dimple_all_patterns <- function(X_hat, missfrac_per_var, assumed_pattern = NA, window = 0.5) {
  
  MCAR <- dimple_MCAR(X_hat, missfrac_per_var)
  MAR <- dimple_MAR(X_hat, missfrac_per_var, window = window)
  MNAR <- dimple_MNAR(X_hat, missfrac_per_var, window = window)
  if (length(assumed_pattern) == length(missfrac_per_var)) MAP <- dimple_MAP(X_hat, missfrac_per_var, assumed_pattern, window = window)
  
  if (length(assumed_pattern) == length(missfrac_per_var)) list(MCAR_matrix = MCAR$MCAR_matrix , MAR_matrix = MAR$MAR_matrix, MNAR_matrix = MNAR$MNAR_matrix, MAP_matrix = MAP$MAP_matrix) else list(MCAR_matrix = MCAR$MCAR_matrix , MAR_matrix = MAR$MAR_matrix, MNAR_matrix = MNAR$MNAR_matrix)   
  
}


###LAB
#res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable, window=0.1)

#res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable, 
#                           assumed_pattern = c("MAR", "MCAR", "MCAR", "MAR", "MNAR", "MCAR", "MAR", "MAR", "MNAR", "MNAR", 
#                                               "MAR", "MAR", "MNAR", "MNAR"))


#assumed_pattern <- c("MAR", "MCAR", "MCAR", "MAR", "MNAR", "MCAR", "MAR", "MAR", "MNAR", "MNAR")
