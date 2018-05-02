#' @title Missing data spike-in in MNAR pattern
#'
#' @description
#' MNAR() spikes in missingness using missing-not-at-random (MNAR) pattern
#'
#' @details
#' This function uses the generated simulated matrix and generates missing datapoints in an assumed pattern per variable.
#' This is achieved by sorting each column and removing the top/bottom of the column (according to the original missingness).
#' Selection from top or bottom is random.
#'
#' @param X_hat Simulated matrix with no missingess (Simulated_matrix output from the simulate() function)
#' @param missfrac_per_var Fraction of missingness per variable (Fraction_missingness_per_variable output from the get_data() function)
#' @param window Window (with default 0.5). This regulates the "extremity" of missingness spike in (larger windows result in more sparse missing data placement whereas smaller windows result in more dense missing data per value - stronger patterns of missingness)
#'
#' @name MNAR
#'
#' @return
#' \item{MNAR_matrix}{Matrix with MNAR pred-defined missingess pattern}
#' \item{Summary}{Summary of MNAR_matrix including number of missing values per variable}
#'
#' @examples
#' MNAR(simulated$Simulated_matrix, metadata$Fraction_missingness_per_variable)
#' MNAR(simulated$Simulated_matrix, metadata$Fraction_missingness_per_variable, window = 0.2)
#'
#' @export


###PACKAGES
library(missForest)


###FUNCTION
MNAR <- function(X_hat, missfrac_per_var, window = 0.5) {

  rownames(X_hat) <- 1:nrow(X_hat)

  for (i in 1:length(missfrac_per_var)) {
    window_start <- runif(1, min=0, max=1-window-missfrac_per_var[i])
    window_end <- window_start+missfrac_per_var[i]+window
    quants <- quantile(X_hat[,i], c(window_start, window_end))
    ind <- X_hat[,i] <= quants[2] & X_hat[,i] >= quants[1]
    to_NA <- sample(rownames(X_hat)[ind], missfrac_per_var[i]*nrow(X_hat))
    X_hat[,i][to_NA] <- NA
  }

  #reorder and remove rows with full missingness
  X_hat <- X_hat[ order(as.numeric(row.names(X_hat))),]

  missfrac_per_ind <- rowMeans(is.na(X_hat))
  inds_above_thres <- rownames(X_hat)[missfrac_per_ind == 1]
  if (length(inds_above_thres) != 0) X_hat <- X_hat[-which(missfrac_per_ind == 1), ]

  matrix_summary <- summary(X_hat)

  list(MNAR_matrix = X_hat, Summary = matrix_summary)

}



###LAB
#res <- MNAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
#matrixplot(res$MNAR_matrix, interactive = F, col= "red")
#hist(res$MNAR_matrix[,4])

