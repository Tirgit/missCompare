#################################################################################################################################################
#
#The purpose of this function is to simulate a dataset that resembles the original dataset based on real information
#These pieces of information are:
#
#data size
#data dimensions
#correlation matrix
#fraction of missingness
#missing data pattern
#
#
#################################################################################################################################################

###PACKAGES
library(MASS)
library(Matrix)


###FUNCTION

dimple_sim_df <- function(rownum, colnum, cormat, pdmat = T, meanval = 0, sdval = 1) {
  pd_corr_matrix <- nearPD(cormat, keepDiag=T, conv.tol = 1e-7, corr=T)
  mu <- rep(meanval,colnum)
  stddev <- rep(sdval,colnum)
  covMat <- stddev %*% t(stddev) * pd_corr_matrix$mat
  X_hat <- mvrnorm(n=rownum, mu=mu, Sigma=covMat, empirical=TRUE) # Simulated values
  original_sample <- cormat[1:5,1:5]
  nearPD_sample <- cor(X_hat)[1:5,1:5]
  
  list(Simulated_matrix = X_hat, Original_correlation_sample = original_sample, NearPD_correlation_sample = nearPD_sample)
}






###LAB
cormat = y$Corr_matrix
rownum = y$Rows
colnum = y$Columns
meanval = 0
sdval = 1


pd_corr_matrix <- nearPD(cormat, keepDiag=T, conv.tol = 1e-7, corr=T)
mu <- rep(meanval,colnum)
stddev <- rep(sdval,colnum)
covMat <- stddev %*% t(stddev) * pd_corr_matrix$mat
X_hat <- mvrnorm(n=rownum, mu=mu, Sigma=covMat, empirical=TRUE) # Simulated values
original_sample <- cormat[1:5,1:5]
nearPD_sample <- cor(X_hat)[1:5,1:5]


dimple_sim_df(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)








