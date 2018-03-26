#################################################################################################################################################
#
#The purpose of this function is to simulate a dataset that resembles the original dataset based on real information.
#The function will estimate the nearest possible positive definitive matrix for correlation structure with normally distributed variables with given mean and SD.
#At this stage, the simulated dataset will not contain missing values.
#These pieces of information are:
#
#data size
#data dimensions
#correlation matrix
#
#
#The function outputs the simulated matrix, the original correlation structure and the correlation structure of the simulated matrix
#The two correlation matrices should be very similar.
#
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



