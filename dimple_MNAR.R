#################################################################################################################################################
#
#This function uses the generated simulated matrix and generates missing datapoints in MNAR (missing non at random) pattern per variable.
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
dimple_MNAR <- function(X_hat, missfrac_per_var) {
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  logi <- sample(0:1, length(missfrac_per_var), replace = T)
  
  for (i in 1:length(missfrac_per_var)) {
    if (logi[i]==1) X_hat <- X_hat[order(X_hat[,i], decreasing = T),] else X_hat <- X_hat[order(X_hat[,i], decreasing = F),]
    threshold_for_excl <- X_hat[,i][ceiling(missfrac_per_var[i]*nrow(X_hat))]
    if (logi[i]==1) X_hat[,i] <- ifelse(X_hat[,i]>threshold_for_excl, NA, X_hat[,i]) else X_hat[,i] <- ifelse(X_hat[,i]<threshold_for_excl, NA, X_hat[,i]) 
      }
  
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]
  
  matrix_summary <- summary(X_hat)
  
  list(MNAR_matrix = X_hat, Summary = matrix_summary)
  
}



###LAB
res <- dimple_MNAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
matrixplot(res$MNAR_matrix, interactive = F, col= "red") 



