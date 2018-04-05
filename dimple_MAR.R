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
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  logi <- sample(0:2, length(missfrac_per_var)-1, replace = T)
  
  for (i in 1:(length(missfrac_per_var)-1)) {
    
    Q1 <- quantile(X_hat[,i+1])[2]
    Q2 <- quantile(X_hat[,i+1])[3]
    Q3 <- quantile(X_hat[,i+1])[4]
    
    low_ind <- X_hat[,i+1] <= Q2
    mid_ind <- X_hat[,i+1] <= Q3 & X_hat[,i+1] >= Q1
    high_ind <- X_hat[,i+1] >= Q2
    
    if (logi[i]==0) to_NA <- sample(rownames(X_hat)[low_ind], missfrac_per_var[i]*nrow(X_hat)) else if (logi[i]==1) to_NA <- sample(rownames(X_hat)[mid_ind], missfrac_per_var[i]*nrow(X_hat)) else to_NA <- sample(rownames(X_hat)[high_ind], missfrac_per_var[i]*nrow(X_hat))
    
    X_hat[,i][to_NA] <- NA    
  }
  
  logi2 <- sample(0:2, 1)
  
  Q1 <- quantile(X_hat[,1], na.rm = T)[2]
  Q2 <- quantile(X_hat[,1], na.rm = T)[3]
  Q3 <- quantile(X_hat[,1], na.rm = T)[4]
  
  low_ind <- X_hat[,1] <= Q2
  mid_ind <- X_hat[,1] <= Q3 & X_hat[,1] >= Q1
  high_ind <- X_hat[,1] >= Q2
  NAs <- is.na(X_hat[,1])
  
  if (logi2==0) to_NA <- sample(rownames(X_hat)[low_ind | NAs], missfrac_per_var[length(missfrac_per_var)]*nrow(X_hat)) else if (logi2==1) to_NA <- sample(rownames(X_hat)[mid_ind | NAs], missfrac_per_var[length(missfrac_per_var)]*nrow(X_hat)) else to_NA <- sample(rownames(X_hat)[high_ind | NAs], missfrac_per_var[length(missfrac_per_var)]*nrow(X_hat))
  
  X_hat[,10][to_NA] <- NA      
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]
  
  missfrac_per_ind <- rowMeans(is.na(X_hat))
  inds_above_thres <- rownames(X_hat)[missfrac_per_ind == 1]
  if (length(inds_above_thres) != 0) X_hat <- X_hat[-which(missfrac_per_ind == 1), ]
  
  matrix_summary <- summary(X_hat)
  
  list(MAR_matrix = X_hat, Summary = matrix_summary)
  
}



###LAB
res <- dimple_MAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
matrixplot(res$MAR_matrix, interactive = F, col= "red") 




