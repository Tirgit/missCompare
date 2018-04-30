#################################################################################################################################################
#
#The purpose of this function is to simulate a dataset that resembles the original dataset based on real information.
#The function will estimate the nearest possible positive definitive matrix for correlation structure with normally distributed variables with given mean and SD.
#At this stage, the simulated dataset will not contain any missing values.
#The pieces of information used for the simulation are:
#
#data dimensions (number of rows and number of columns)
#correlation matrix
#
#
#The function outputs the simulated matrix, and samples from the original correlation structure and the correlation structure of the simulated matrix.
#The two correlation matrices should be very similar.
#
#
#################################################################################################################################################

###PACKAGES
library(MASS)
library(Matrix)


###FUNCTION
dimple_sim <- function(rownum, colnum, cormat, meanval = 0, sdval = 1) {
  pd_corr_matrix <- nearPD(cormat, keepDiag=T, conv.tol = 1e-7, corr=T)
  mu <- rep(meanval,colnum)
  stddev <- rep(sdval,colnum)
  covMat <- stddev %*% t(stddev) * pd_corr_matrix$mat
  X_hat <- mvrnorm(n=rownum, mu=mu, Sigma=covMat, empirical=TRUE) # Simulated values
  if (colnum >5) original_sample <- cormat[1:5,1:5] else original_sample <- cormat[1:colnum,1:colnum]
  if (colnum >5) nearPD_sample <- cor(X_hat)[1:5,1:5] else nearPD_sample <- cor(X_hat)[1:colnum,1:colnum]
  
  rownames(X_hat) <- 1:nrow(X_hat)
  
  list(Simulated_matrix = X_hat, Original_correlation_sample = original_sample, NearPD_correlation_sample = nearPD_sample)
}



###LAB
#yy <- dimple_sim(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)
