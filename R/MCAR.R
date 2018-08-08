#' @title Missing data spike-in in MCAR pattern
#'
#' @description
#' \code{\link{MCAR}} spikes in missingness using missing-completely-at-random (MCAR) pattern
#'
#' @details
#' This function uses the generated simulated matrix and generates missing datapoints in a missing-completely-at-random
#' pattern for each variable, considering the fraction of missingness for each variable, so potential missing data fraction
#' imbalances between variables in the original data will be retained. The missing data spike-in is completely at random.
#'
#' @param X_hat Simulated matrix with no missingess (Simulated_matrix output from the \code{\link{simulate}} function)
#' @param missfrac_per_var Fraction of missingness per variable (Fraction_missingness_per_variable output from the \code{\link{get_data}} function)
#'
#' @name MCAR
#'
#' @return
#' \item{MCAR_matrix}{Matrix with MCAR pred-defined missingess pattern}
#' \item{Summary}{Summary of MCAR_matrix including number of missing values per variable}
#'
#' @examples
#' \dontrun{
#' MCAR(simulated$Simulated_matrix, metadata$Fraction_missingness_per_variable)
#' MCAR(simulated$Simulated_matrix, metadata$Fraction_missingness_per_variable, window = 0.2)
#' }
#'
#' @export


### FUNCTION
MCAR <- function(X_hat, missfrac_per_var) {
    
    rownames(X_hat) <- 1:nrow(X_hat)
    
    for (i in 1:length(missfrac_per_var)) {
        X_hat[, i] <- prodNA(as.matrix(X_hat[, i]), noNA = missfrac_per_var[i])
    }
    
    # remove rows with full missingness
    missfrac_per_ind <- rowMeans(is.na(X_hat))
    inds_above_thres <- rownames(X_hat)[missfrac_per_ind == 1]
    if (length(inds_above_thres) != 0) 
        X_hat <- X_hat[-which(missfrac_per_ind == 1), ]
    
    matrix_summary <- summary(X_hat)
    
    list(MCAR_matrix = X_hat, Summary = matrix_summary)
    
}


### LAB res <- MCAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
### matrixplot(res$MCAR_matrix, interactive = F, col= 'red')



