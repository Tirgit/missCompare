#' @title Missing data spike-in in MNAR pattern
#'
#' @description
#' MNAR() spikes in missingness using missing-not-at-random (MNAR) pattern
#'
#' @details
#' This function uses the generated simulated matrix and generates missing datapoints in a missing-not-at-random
#' pattern for each variable, considering the fraction of missingness for each variable, so potential missing data fraction
#' imbalances between variables in the original data will be retained. The characteristic of the MNAR pattern is that
#' the missingness in a variable is dependent on its own distribution. Therefore, this function sorts each column (variable)
#' and based on percentile ranks, randomly selects a window in which missing datapoints will be spiked in. The size of the
#' window is the fraction of missingness + 0.5 by default. This means that e.g. given 10\% missingness, a window of
#' 10\% + 50\% = 60\% of the data (60 percentiles) will be randomly selected from which 10\% of the variable data
#' will be randomly removed. In case the user defines a narrower window, the density of missing data removal will be more extreme.
#' E.g. in an extreme case where the window is defined as 0, 10\% of the variable data will be removed from 10 adjacent
#' percentiles (for example, the 10\% lowest observations or data between the 72nd and 82nd percentiles). The default size
#' of the window is 0.5, which means that with this setting the fuction will only run with maximum 50\% of missing data for
#' any variable. Consequently, with a window setting of 0.25, the function will allow maximum 75\%  missing data a variable.
#' The user should note that in case the fraction of missingness + the window gets closer to 100\% (e.g. a window of 0.4 and
#' a variable missingess of 55\%), the more the algorithm will spike in missingness that will resemble a random pattern
#' (as the random "window" will represent (almost) all the data in a variable). Hence, it is suggested that the user carefully
#' examines the missing data fractions, excludes variables with high missingess using the clean() function and
#' sets a sensible window for the analysis.
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

