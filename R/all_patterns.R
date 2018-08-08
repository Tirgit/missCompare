#' @title Missing data spike-in in various missing data patterns
#'
#' @description
#' \code{\link{all_patterns}} spikes in missingness using MCAR, MAR, MNAR (default) and MAP (optional) patterns
#'
#' @details
#' This function uses the generated simulated matrix and generates missing datapoints in MCAR, MAR and MNAR patterns.
#' Optionally, in case the user defines an assumed pattern, the \code{\link{all_patterns}} function will also generate
#' a MAP missingness pattern. The size of the window is the fraction of missingness + 0.5 by default. This means that e.g. given 10\% missingness, a window of
#' 10\% + 50\% = 60\% of the data (60 percentiles) will be randomly selected from which 10\% of the variable data
#' will be randomly removed. In case the user defines a narrower window, the density of missing data removal will be more extreme.
#' E.g. in an extreme case where the window is defined as 0, 10\% of the variable data will be removed from 10 adjacent
#' percentiles (for example, the 10\% lowest observations or data between the 72nd and 82nd percentiles). The default size
#' of the window is 0.5, which means that with this setting the fuction will only run with maximum 50\% of missing data for
#' any variable. Consequently, with a window setting of 0.25, the function will allow maximum 75\%  missing data a variable.
#' The user should note that in case the fraction of missingness + the window gets closer to 100\% (e.g. a window of 0.4 and
#' a variable missingess of 55\%), the more the algorithm will spike in missingness that will resemble a random pattern
#' (as the random 'window' will represent (almost) all the data in a variable). Hence, it is suggested that the user carefully
#' examines the missing data fractions, excludes variables with high missingess using the \code{\link{clean}} function and
#' sets a sensible window for the analysis. For more information on the functions that spike in missing data in MCAR, MAR,
#' MNAR and MAP patterns, please see the functions \code{\link{MCAR}}, \code{\link{MAR}}, \code{\link{MNAR}} and \code{\link{MAP}}.
#'
#'
#' @param X_hat Simulated matrix with no missingess (Simulated_matrix output from the \code{\link{simulate}} function)
#' @param missfrac_per_var Fraction of missingness per variable (Fraction_missingness_per_variable output from the \code{\link{get_data}} function)
#' @param assumed_pattern Vector of missingess types (must be same length as missingness fraction per variable). If this input is specified, the function will spike in missing datapoints in a MAP pattern as well.
#' @param window Window (with default 0.5). This regulates the 'extremity' of missingness spike in (larger windows result in more sparse missing data placement whereas smaller windows result in more dense missing data per value - stronger patterns of missingness)
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
#' \dontrun{
#' miss_list <- all_patterns(simulated$Simulated_matrix, metadata$Fraction_missingness_per_variable,
#' assumed_pattern = c('MAR', 'MCAR', 'MCAR', 'MAR', 'MNAR', 'MCAR'))
#' miss_list <- all_patterns(simulated$Simulated_matrix, metadata$Fraction_missingness_per_variable,
#' window = 0.2)
#' }
#'
#' @export


### FUNCTION
all_patterns <- function(X_hat, missfrac_per_var, assumed_pattern = NA, window = 0.5) {
    
    MCAR <- MCAR(X_hat, missfrac_per_var)
    MAR <- MAR(X_hat, missfrac_per_var, window = window)
    MNAR <- MNAR(X_hat, missfrac_per_var, window = window)
    
    if (!is.na(assumed_pattern[1]) & (length(assumed_pattern) != length(missfrac_per_var))) 
        stop(paste("The length of argument missfrac_per_var (", length(missfrac_per_var), ") and argument assumed_pattern (", 
            length(assumed_pattern), ") do not match. Please double-check the arguments of the function.", 
            sep = ""))
    
    
    if (!is.na(assumed_pattern[1])) 
        MAP <- MAP(X_hat, missfrac_per_var, assumed_pattern, window = window)
    
    if (!is.na(assumed_pattern[1])) 
        list(MCAR_matrix = MCAR$MCAR_matrix, MAR_matrix = MAR$MAR_matrix, MNAR_matrix = MNAR$MNAR_matrix, 
            MAP_matrix = MAP$MAP_matrix) else list(MCAR_matrix = MCAR$MCAR_matrix, MAR_matrix = MAR$MAR_matrix, MNAR_matrix = MNAR$MNAR_matrix)
    
}


### LAB res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable,
### window=0.1)

# res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable,
# assumed_pattern = c('MAR', 'MCAR', 'MCAR', 'MAR', 'MNAR', 'MCAR', 'MAR', 'MAR', 'MNAR',
# 'MNAR', 'MAR', 'MAR', 'MNAR', 'MNAR'))

