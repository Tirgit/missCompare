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
#' @param MD_pattern Missing data pattern in the original dataset (MD_Pattern output from the \code{\link{get_data}} function)
#' @param NA_fraction Fraction of missingness in the original dataset (Fraction_missingness output from the \code{\link{get_data}} function)
#' @param min_PDM All patterns with number of observations less than this number will be removed from the missing data generation. This argument is necessary to be carefully set, as the function will fail or generate erroneous missing data patterns with very complicated missing data patterns. The default is 10, but for large datasets this number needs to be set higher to avoid errors.
#'
#' @name MCAR
#'
#' @return
#' \item{MCAR_matrix}{Matrix with MCAR pred-defined missingess pattern}
#' \item{Summary}{Summary of MCAR_matrix including number of missing values per variable}
#'
#' @examples
#' \dontrun{
#' MCAR(simulated$Simulated_matrix,
#'     MD_pattern = metadata$MD_Pattern,
#'     NA_fraction = metadata$Fraction_missingness,
#'     min_PDM = 10)
#' }
#'
#' @export


### FUNCTION
MCAR <- function(X_hat, MD_pattern, NA_fraction, min_PDM = 10) {
    
    rownames(X_hat) <- 1:nrow(X_hat)
    data_names <- colnames(MD_pattern)
    colnames(X_hat) <- data_names
    
    # removing row 1 - the row representing complete cases from MD_pattern removing
    # last row - the row with summary stats from MD_pattern
    MD_pattern <- MD_pattern[-c(1, nrow(MD_pattern)), ]
    
    # trying to remove all patterns with less than min_PDM obs
    index <- as.numeric(rownames(MD_pattern)) >= min_PDM
    MD_pattern_simple <- MD_pattern[index, ]
    message(paste0(round(sum(100 * as.numeric(rownames(MD_pattern_simple)))/sum(as.numeric(rownames(MD_pattern))), 
        2), "% of observations covered by setting min_PDM to ", min_PDM), sep = "")
    
    # creating frequency vector
    totrows <- as.numeric(rownames(MD_pattern_simple))
    myfreq <- totrows/sum(totrows)
    
    # removing row names, but keeping column names
    rownames(MD_pattern_simple) <- NULL
    
    # amputation
    amputed <- mice::ampute(X_hat, prop = NA_fraction, patterns = MD_pattern_simple, 
        freq = myfreq, bycases = F, mech = "MCAR")
    
    X_hat <- amputed$amp
    
    # remove rows with full missingness
    missfrac_per_ind <- rowMeans(is.na(X_hat))
    inds_above_thres <- rownames(X_hat)[missfrac_per_ind == 1]
    if (length(inds_above_thres) != 0) 
        X_hat <- X_hat[-which(missfrac_per_ind == 1), ]
    
    matrix_summary <- summary(X_hat)
    
    list(MCAR_matrix = X_hat, Summary = matrix_summary)
    
}

