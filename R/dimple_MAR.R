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
#window with default 0.5. This regulates the "extremity of missingness spike in (larger windows result in more sparse missing data placement
#whereas smaller windows result in more dense missing data per value - stronger patterns of missingness)
#
#The function outputs a matrix with missing values and a summary of the matrix.
#
#################################################################################################################################################

###PACKAGES
library(missForest)


###FUNCTION
dimple_MAR <- function(X_hat, missfrac_per_var, window = 0.5) {
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  for (i in 1:(length(missfrac_per_var)-1)) {
    window_start <- runif(1, min=0, max=1-window-missfrac_per_var[i])
    window_end <- window_start+missfrac_per_var[i]+window
    quants <- quantile(X_hat[,i+1], c(window_start, window_end))
    ind <- X_hat[,i+1] <= quants[2] & X_hat[,i+1] >= quants[1]
    to_NA <- sample(rownames(X_hat)[ind], missfrac_per_var[i]*nrow(X_hat)) 
    X_hat[,i][to_NA] <- NA
  }
  
  window_start <- runif(1, min=0, max=1-window-missfrac_per_var[length(missfrac_per_var)])
  window_end <- window_start+missfrac_per_var[length(missfrac_per_var)]+window
  quants <- quantile(X_hat[,1], c(window_start, window_end), na.rm = T)
  ind <- X_hat[,1] <= quants[2] & X_hat[,1] >= quants[1]
  NAs <- is.na(X_hat[,1])
  to_NA <- sample(rownames(X_hat)[ind | NAs], missfrac_per_var[length(missfrac_per_var)]*nrow(X_hat))
  X_hat[,length(missfrac_per_var)][to_NA] <- NA
  
  #reorder and remove rows with full missingness
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]
  
  missfrac_per_ind <- rowMeans(is.na(X_hat))
  inds_above_thres <- rownames(X_hat)[missfrac_per_ind == 1]
  if (length(inds_above_thres) != 0) X_hat <- X_hat[-which(missfrac_per_ind == 1), ]
  
  matrix_summary <- summary(X_hat)
  
  list(MAR_matrix = X_hat, Summary = matrix_summary)
  
}



###LAB
#res <- dimple_MAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
#matrixplot(res$MAR_matrix, interactive = F, col= "red") 




