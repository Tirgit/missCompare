#' @title Missing data spike-in in various missing data patterns
#'
#' @description
#' all_patterns() spikes in missingness using MCAR, MAR, MNAR (default) and MAP (optional) patterns
#'
#' @details
#' This function uses the generated simulated matrix and generates missing datapoints in an assumed pattern per variable.
#' This is achieved by sorting each column and removing the top/bottom of the column (according to the original missingness).
#' Selection from top or bottom is random.
#'
#' @param X_hat Simulated matrix with no missingess (Simulated_matrix output from the simulate() function)
#' @param missfrac_per_var Fraction of missingness per variable (Fraction_missingness_per_variable output from the get_data() function)
#' @param assumed_pattern Vector of missingess types (must be same length as missingness fraction per variable). If this input is specified, the function will spike in missing datapoints in a MAP pattern as well.
#' @param window Window (with default 0.5). This regulates the "extremity" of missingness spike in (larger windows result in more sparse missing data placement whereas smaller windows result in more dense missing data per value - stronger patterns of missingness)
#'
#' @name all_patterns
#'
#' @return
#' \item{MCAR_matrix}{Matrix with MCAR pred-defined missingess pattern (default output)}
#' \item{MAR_matrix}{Matrix with MAR pred-defined missingess pattern (default output)}
#' \item{MNAR_matrix}{Matrix with MNAR pred-defined missingess pattern (default output)}
#' \item{MAP_matrix}{Matrix with MAP pred-defined missingess pattern (optional output)}
#'
#' @examples
#' miss_list <- all_patterns(simulated$Simulated_matrix, metadata$Fraction_missingness_per_variable, assumed_pattern = c("MAR", "MCAR", "MCAR", "MAR", "MNAR", "MCAR"))
#' miss_list <- all_patterns(simulated$Simulated_matrix, metadata$Fraction_missingness_per_variable, window = 0.2)
#'
#' @export



###PACKAGES
library(missForest)


###FUNCTION
all_patterns <- function(X_hat, missfrac_per_var, assumed_pattern = NA, window = 0.5) {

  MCAR <- dimple_MCAR(X_hat, missfrac_per_var)
  MAR <- dimple_MAR(X_hat, missfrac_per_var, window = window)
  MNAR <- dimple_MNAR(X_hat, missfrac_per_var, window = window)

  if (!is.na(assumed_pattern[1]) & (length(assumed_pattern) != length(missfrac_per_var))) stop(paste("The length of argument missfrac_per_var (",
                                                                      length(missfrac_per_var),
                                                                      ") and argument assumed_pattern (",
                                                                      length(assumed_pattern),
                                                                      ") do not match. Please double-check the arguments of the function.", sep= ""))


  if (!is.na(assumed_pattern[1])) MAP <- dimple_MAP(X_hat, missfrac_per_var, assumed_pattern, window = window)

  if (!is.na(assumed_pattern[1])) list(MCAR_matrix = MCAR$MCAR_matrix , MAR_matrix = MAR$MAR_matrix, MNAR_matrix = MNAR$MNAR_matrix, MAP_matrix = MAP$MAP_matrix) else list(MCAR_matrix = MCAR$MCAR_matrix , MAR_matrix = MAR$MAR_matrix, MNAR_matrix = MNAR$MNAR_matrix)

}


###LAB
#res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable, window=0.1)

#res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable,
#                           assumed_pattern = c("MAR", "MCAR", "MCAR", "MAR", "MNAR", "MCAR", "MAR", "MAR", "MNAR", "MNAR",
#                                               "MAR", "MAR", "MNAR", "MNAR"))

