#' @title Simulation of matrix with no missingess
#'
#' @description
#' simulate() simulates a clean matrix with no missingness based on the original data structure
#' where all variables have the same mean and standard deviation and are normally distributed.
#'
#' @details
#' This function requires the metadata from the original dataframe and simulates a matrix with no missingness
#' with the same number of rows and columns and with the same or very similar correlation matrix as observed
#' in the original dataframe. When the correlation matrix is a non positive definitive matrix, the nearPD function
#' estimates the closest positive definitive matrix. Outputs from the function makes it easy to compare the original
#' correlation matrix with the nearPD correlation matrix. In the simulated matrix all variables have normal
#' distribution and fixed mean and standard deviation. This matrix will subsequently used for spiking in missing
#' values and test various missing data imputation algorithms.
#'
#' @param rownum Number of rows (samples) in the original dataframe (Rows output from the get_data() function)
#' @param colnum Number of rows (variables) in the original dataframe (Columns output from the get_data() function)
#' @param cormat Correlation matrix of the original dataframe (Corr_matrix output from the get_data() function)
#' @param meanval Desired mean value for the simulated variables, default = 0
#' @param sdval Desired standard deviation value for the simulated variables, default = 1
#'
#' @name simulate
#'
#' @return
#' \item{Simulated_matrix}{Simulated matrix with no missingness. The simulated matrix resembles the original dataframe in size and correlation structure, but has normally distributed variables with fixed means and SDs}
#' \item{Original_correlation_sample}{Sample of the original correlation structure (for comparison)}
#' \item{NearPD_correlation_sample}{Sample of the nearPD (nearest positive definitive matrix) correlation structure of the simulated matrix (for comparison)}
#'
#' @examples
#' simulated <- simulate(rownum = metadata$Rows, colnum = metadata$Columns, cormat = metadata$Corr_matrix)
#'
#' @export


###PACKAGES
library(MASS)
library(Matrix)


###FUNCTION
simulate <- function(rownum, colnum, cormat, meanval = 0, sdval = 1) {
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
#yy <- simulate(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)
