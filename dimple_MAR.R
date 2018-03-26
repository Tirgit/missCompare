#################################################################################################################################################
#
#This function uses the generated simulated matrix and generates missing datapoints in MAR (missing at random) pattern per variable.
#This is achieved by sorting each column and removing the top/bottom of the column (according to the original missingness).
#Selection from top or bottom is random.
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
dimple_MAR <- function(X_hat, missfrac_per_var) {
  
  logi <- sample(0:1, length(missfrac_per_var)-1, replace = T)
  
  for (i in 1:(length(missfrac_per_var)-1)) {
    if (logi[i]==1) X_hat <- X_hat[order(X_hat[,i+1], decreasing = T),] else X_hat <- X_hat[order(X_hat[,i+1], decreasing = F),]
    threshold_for_excl <- X_hat[,i+1][ceiling(missfrac_per_var[i]*nrow(X_hat))]
    if (logi[i]==1) X_hat[,i] <- ifelse(X_hat[,i+1]>threshold_for_excl, NA, X_hat[,i]) else X_hat[,i] <- ifelse(X_hat[,i+1]<threshold_for_excl, NA, X_hat[,i]) 
  }
  
  logi2 <- sample(0:1, 1)
  
  if (logi2==1) X_hat <- X_hat[order(X_hat[,1], decreasing = T),] else X_hat <- X_hat[order(X_hat[,1], decreasing = F),]
  X_hat[1:(nrow(X_hat)*(missfrac_per_var[length(missfrac_per_var)])),length(missfrac_per_var)] <- NA
  
  matrix_summary <- summary(X_hat)
  
  list(MAR_matrix = X_hat, Summary = matrix_summary)
  
}



###LAB
res <- dimple_MAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
matrixplot(res$MAR_matrix, interactive = F, col= "red") 


