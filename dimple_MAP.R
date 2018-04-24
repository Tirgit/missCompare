#################################################################################################################################################
#
#This function uses the generated simulated matrix and generates missing datapoints in an assumed pattern per variable.
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
dimple_MAP <- function(X_hat, missfrac_per_var, assumed_pattern) {
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  logi <- sample(0:2, length(missfrac_per_var), replace = T)
  
  if (length(assumed_pattern) != length(missfrac_per_var)) message(paste("The length of argument missfrac_per_var (",
                                                                                            length(missfrac_per_var),
                                                                                            ") and argument assumed_pattern (",
                                                                                            length(assumed_pattern),
                                                                                            ") do not match. Please double-check the arguments of the function.", sep= ""))
  
  MCAR_vars <- which(assumed_pattern %in% "MCAR")
  MAR_vars <- which(assumed_pattern %in% "MAR")
  MNAR_vars <- which(assumed_pattern %in% "MNAR")
  
  logi <- sample(0:2, length(missfrac_per_var), replace = T)
  
  #MCAR
  for (i in MCAR_vars) {
    X_hat[,i] <- prodNA(as.matrix(X_hat[,i]), noNA = missfrac_per_var[i])
  }
  
  #MNAR
  for (i in MNAR_vars) {
    
    Q1 <- quantile(X_hat[,i])[2]
    Q2 <- quantile(X_hat[,i])[3]
    Q3 <- quantile(X_hat[,i])[4]
    
    low_ind <- X_hat[,i] <= Q2
    mid_ind <- X_hat[,i] <= Q3 & X_hat[,i] >= Q1
    high_ind <- X_hat[,i] >= Q2
    
    if (logi[i]==0) to_NA <- sample(rownames(X_hat)[low_ind], missfrac_per_var[i]*nrow(X_hat)) else if (logi[i]==1) to_NA <- sample(rownames(X_hat)[mid_ind], missfrac_per_var[i]*nrow(X_hat)) else to_NA <- sample(rownames(X_hat)[high_ind], missfrac_per_var[i]*nrow(X_hat))
    
    X_hat[,i][to_NA] <- NA    
  }
  
  #MAR
  for (i in 1:(length(MAR_vars)-1)) {
    
    Q1 <- quantile(X_hat[,MAR_vars[i+1]])[2]
    Q2 <- quantile(X_hat[,MAR_vars[i+1]])[3]
    Q3 <- quantile(X_hat[,MAR_vars[i+1]])[4]
    
    low_ind <- X_hat[,MAR_vars[i+1]] <= Q2
    mid_ind <- X_hat[,MAR_vars[i+1]] <= Q3 & X_hat[,MAR_vars[i+1]] >= Q1
    high_ind <- X_hat[,MAR_vars[i+1]] >= Q2
    
    if (logi[MAR_vars[i]]==0) to_NA <- sample(rownames(X_hat)[low_ind], missfrac_per_var[MAR_vars[i]]*nrow(X_hat)) else if (logi[MAR_vars[i]]==1) to_NA <- sample(rownames(X_hat)[mid_ind], missfrac_per_var[MAR_vars[i]]*nrow(X_hat)) else to_NA <- sample(rownames(X_hat)[high_ind], missfrac_per_var[MAR_vars[i]]*nrow(X_hat))
    
    X_hat[,MAR_vars[i]][to_NA] <- NA    
  }
  
  logi2 <- sample(0:2, 1)
  
  Q1 <- quantile(X_hat[,MAR_vars[1]], na.rm = T)[2]
  Q2 <- quantile(X_hat[,MAR_vars[1]], na.rm = T)[3]
  Q3 <- quantile(X_hat[,MAR_vars[1]], na.rm = T)[4]
  
  low_ind <- X_hat[,MAR_vars[1]] <= Q2
  mid_ind <- X_hat[,MAR_vars[1]] <= Q3 & X_hat[,MAR_vars[1]] >= Q1
  high_ind <- X_hat[,MAR_vars[1]] >= Q2
  NAs <- is.na(X_hat[,MAR_vars[1]])
  
  if (logi2==0) to_NA <- sample(rownames(X_hat)[low_ind | NAs], missfrac_per_var[MAR_vars[length(MAR_vars)]]*nrow(X_hat)) else if (logi2==1) to_NA <- sample(rownames(X_hat)[mid_ind | NAs], missfrac_per_var[MAR_vars[length(MAR_vars)]]*nrow(X_hat)) else to_NA <- sample(rownames(X_hat)[high_ind | NAs], missfrac_per_var[MAR_vars[length(MAR_vars)]]*nrow(X_hat))
  
  X_hat[,MAR_vars[length(MAR_vars)]][to_NA] <- NA     
  
  #reorder and remove rows with full missingness
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]
  
  missfrac_per_ind <- rowMeans(is.na(X_hat))
  inds_above_thres <- rownames(X_hat)[missfrac_per_ind == 1]
  if (length(inds_above_thres) != 0) X_hat <- X_hat[-which(missfrac_per_ind == 1), ]
  
  matrix_summary <- summary(X_hat)
  
  #output
  list(MAP_matrix = X_hat, Summary = matrix_summary)
  
}







###LAB
res <- dimple_MAP(yy$Simulated_matrix, y$Fraction_missingness_per_variable, 
                   assumed_pattern = c("MAR", "MCAR", "MCAR", "MAR", "MNAR", "MCAR", "MAR", "MAR", "MNAR", "MNAR"))
matrixplot(res$MAP_matrix, interactive = F, col= "red") 
hist(res$MAP_matrix[,10])



