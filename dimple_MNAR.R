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
  
  logi <- sample(0:2, length(missfrac_per_var), replace = T)
  
  for (i in 1:length(missfrac_per_var)) {
    
    Q1 <- quantile(X_hat[,i])[2]
    Q2 <- quantile(X_hat[,i])[3]
    Q3 <- quantile(X_hat[,i])[4]
    
    low_ind <- X_hat[,i] <= Q2
    mid_ind <- X_hat[,i] <= Q3 & X_hat[,i] >= Q1
    high_ind <- X_hat[,i] >= Q2
    
    if (logi[i]==0) to_NA <- sample(rownames(X_hat)[low_ind], missfrac_per_var[i]*nrow(X_hat)) else if (logi[i]==1) to_NA <- sample(rownames(X_hat)[mid_ind], missfrac_per_var[i]*nrow(X_hat)) else to_NA <- sample(rownames(X_hat)[high_ind], missfrac_per_var[i]*nrow(X_hat))
    
    X_hat[,i][to_NA] <- NA    
  }
  
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]
  
  matrix_summary <- summary(X_hat)
  
  list(MNAR_matrix = X_hat, Summary = matrix_summary)
  
}



###LAB
res <- dimple_MNAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
matrixplot(res$MNAR_matrix, interactive = F, col= "red") 
hist(res$MNAR_matrix[,4])
