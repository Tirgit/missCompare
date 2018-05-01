#################################################################################################################################################
#
#This function uses the generated simulated matrix and generates missing datapoints in an assumed pattern per variable.
#This is achieved by sorting each column and removing the top/bottom of the column (according to the original missingness).
#Selection from top or bottom is random.
#
#Inputs are:
#
#simulated matrix
#missingness fraction per variable
#assumed pattern - vector of missingess types (must be same length as missingness fraction per variable)
#window with default 0.5. This regulates the "extremity of missingness spike in (larger windows result in more sparse missing data placement
#whereas smaller windows result in more dense missing data per value - stronger patterns of missingness)
#
#The function outputs a matrix with missing values and a summary of the matrix.
#
#################################################################################################################################################

###PACKAGES
library(missForest)


###FUNCTION
dimple_MAP <- function(X_hat, missfrac_per_var, assumed_pattern, window = 0.5) {
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  if (length(assumed_pattern) != length(missfrac_per_var)) stop(paste("The length of argument missfrac_per_var (",
                                                                                            length(missfrac_per_var),
                                                                                            ") and argument assumed_pattern (",
                                                                                            length(assumed_pattern),
                                                                                            ") do not match. Please double-check the arguments of the function.", sep= ""))
  
  MCAR_vars <- which(assumed_pattern %in% "MCAR")
  MAR_vars <- which(assumed_pattern %in% "MAR")
  MNAR_vars <- which(assumed_pattern %in% "MNAR")
  
  #MCAR
  for (i in MCAR_vars) {
    X_hat[,i] <- prodNA(as.matrix(X_hat[,i]), noNA = missfrac_per_var[i])
  }
  
  #MNAR
  for (i in MNAR_vars) {
    window_start <- runif(1, min=0, max=1-window-missfrac_per_var[i])
    window_end <- window_start+missfrac_per_var[i]+window
    quants <- quantile(X_hat[,i],c(window_start, window_end))
    ind <- X_hat[,i] <= quants[2] & X_hat[,i] >= quants[1]
    to_NA <- sample(rownames(X_hat)[ind], missfrac_per_var[i]*nrow(X_hat)) 
    X_hat[,i][to_NA] <- NA
  }
  
  #MAR
  for (i in 1:(length(MAR_vars)-1)) {
    window_start <- runif(1, min=0, max=1-window-missfrac_per_var[MAR_vars[i]])
    window_end <- window_start+missfrac_per_var[MAR_vars[i]]+window
    quants <- quantile(X_hat[,MAR_vars[i+1]], c(window_start, window_end))
    ind <- X_hat[,MAR_vars[i+1]] <= quants[2] & X_hat[,MAR_vars[i+1]] >= quants[1]
    to_NA <- sample(rownames(X_hat)[ind], missfrac_per_var[MAR_vars[i]]*nrow(X_hat)) 
    X_hat[,MAR_vars[i]][to_NA] <- NA
  }
  
  window_start <- runif(1, min=0, max=1-window-missfrac_per_var[MAR_vars[length(MAR_vars)]])
  window_end <- window_start+missfrac_per_var[MAR_vars[length(MAR_vars)]]+window
  quants <- quantile(X_hat[,MAR_vars[1]], c(window_start, window_end), na.rm = T)
  ind <- X_hat[,MAR_vars[1]] <= quants[2] & X_hat[,MAR_vars[1]] >= quants[1]
  NAs <- is.na(X_hat[,MAR_vars[1]])
  to_NA <- sample(rownames(X_hat)[ind | NAs], missfrac_per_var[MAR_vars[length(MAR_vars)]]*nrow(X_hat)) 
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
#res <- dimple_MAP(yy$Simulated_matrix, y$Fraction_missingness_per_variable, 
#                   assumed_pattern = c("MAR", "MCAR", "MCAR", "MAR", "MNAR", "MCAR", "MAR", "MAR", "MNAR", "MNAR",
#                                       "MCAR", "MAR", "MNAR", "MAR"))
#matrixplot(res$MAP_matrix, interactive = F, col= "red") 
#hist(res$MAP_matrix[,10])

