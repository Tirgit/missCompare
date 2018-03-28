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
    
    if (logi[i]==0) X_hat[,i] <- prodNA(as.matrix(X_hat[,i][low_ind]), noNA = 2*missfrac_per_var[i]) else if (logi[i]==1) X_hat[,i] <- prodNA(as.matrix(X_hat[,i][mid_ind]), noNA = 2*missfrac_per_var[i]) else X_hat[,i] <- prodNA(as.matrix(X_hat[,i][high_ind]), noNA = 2*missfrac_per_var[i])
    
  }
  
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]
  
  matrix_summary <- summary(X_hat)
  
  list(MNAR_matrix = X_hat, Summary = matrix_summary)
  
}



###LAB
res <- dimple_MNAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
matrixplot(res$MNAR_matrix, interactive = F, col= "red") 

hist(res$MNAR_matrix[,10])

plot(x = yy$Simulated_matrix[,1], y = res$MNAR_matrix[,1])

y <- dimple_get_data(df_miss, matrixplot_sort = T)
yy <- dimple_sim_df(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)



logi <- sample(0:2, length(y$Fraction_missingness_per_variable), replace = T)

rownames(yy$Simulated_matrix) <- 1:nrow(yy$Simulated_matrix)

Q1 <- quantile(yy$Simulated_matrix[,1])[2]
Q2 <- quantile(yy$Simulated_matrix[,1])[3]
Q3 <- quantile(yy$Simulated_matrix[,1])[4]

low_ind <- yy$Simulated_matrix[,1] <= Q2
mid_ind <- yy$Simulated_matrix[,1] <= Q3 & yy$Simulated_matrix[,1] >= Q1
high_ind <- yy$Simulated_matrix[,1] >= Q2

low_NA <- sample(rownames(yy$Simulated_matrix)[low_ind], y$Fraction_missingness_per_variable[1]*nrow(yy$Simulated_matrix))
mid_NA <- sample(rownames(yy$Simulated_matrix)[mid_ind], y$Fraction_missingness_per_variable[1]*nrow(yy$Simulated_matrix))
high_NA <- sample(rownames(yy$Simulated_matrix)[high_ind], y$Fraction_missingness_per_variable[1]*nrow(yy$Simulated_matrix))

yy$Simulated_matrix[,1][low_NA] <- NA

sum(is.na(yy$Simulated_matrix[,1]))
