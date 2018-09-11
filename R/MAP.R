#' @title Missing data spike-in in MAP pattern
#'
#' @description
#' \code{\link{MAP}} spikes in missingness using missing-at-assumed (MAP) pattern
#'
#' @details
#' This function uses the generated simulated matrix and generates missing datapoints in a missing-at-assumed
#' pattern for each variable using the \code{\link[mice]{ampute}} function, considering the fraction of missingness in
#' the original dataset and the original missingness pattern. It is suggested that the user carefully
#' examines the missing data fractions, excludes variables with high missingess using the \code{\link{clean}} function before proceeding to
#' missing data generation. In the \code{\link{MAP}} function, the user needs to define a character vector
#' (of length the same as the fraction the number of columns in the dataset) that specifies which missingness pattern corresponds
#' to the variables. In case the first four columns are assumed missing at random, the next one missing completely at random and
#' the last two column not at random, the input vector will be:
#' \code{c(rep('MAR', 4), 'MCAR', rep('MNAR',2))}
#' The algorithm will spike in missing values according to the specified pattern.
#'
#'
#' @param X_hat Simulated matrix with no missingess (Simulated_matrix output from the \code{\link{simulate}} function)
#' @param MD_pattern Missing data pattern in the original dataset (MD_Pattern output from the \code{\link{get_data}} function)
#' @param NA_fraction Fraction of missingness in the original dataset (Fraction_missingness output from the \code{\link{get_data}} function)
#' @param min_PDM All patterns with number of observations less than this number will be removed from the missing data generation. This argument is necessary to be carefully set, as the function will fail or generate erroneous missing data patterns with very complicated missing data patterns. The default is 10, but for large datasets this number needs to be set higher to avoid errors.
#' @param assumed_pattern Vector of missingess types (must be same length as missingness fraction per variable)
#'
#' @name MAP
#'
#' @return
#' \item{MAP_matrix}{Matrix with MAP pred-defined missingess pattern}
#' \item{Summary}{Summary of MAP_matrix including number of missing values per variable}
#'
#' @examples
#' \dontrun{
#' MAP(simulated$Simulated_matrix,
#'     MD_pattern = metadata$MD_Pattern,
#'     NA_fraction = metadata$Fraction_missingness,
#'     min_PDM = 10,
#'     assumed_pattern = c('MAR', 'MCAR', 'MCAR', 'MAR', 'MNAR', 'MCAR'))
#' }
#'
#' @export

### FUNCTION
MAP <- function(X_hat, MD_pattern, NA_fraction, min_PDM = 10, assumed_pattern = c("MAR",
    "MCAR", "MCAR", "MAR", "MNAR", "MCAR", "MCAR", "MAR", "MNAR", "MCAR", "MCAR")) {

    if (length(assumed_pattern) != ncol(X_hat))
        stop(paste("The number of columns in X_hat (", ncol(X_hat), ") and argument assumed_pattern (",
            length(assumed_pattern), ") do not match. Please double-check the arguments of the function.",
            sep = ""))

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
        2), "% of observations (with at least one missing datapoint) covered by setting min_PDM to ", min_PDM), sep = "")

    # creating frequency vector
    totrows <- as.numeric(rownames(MD_pattern_simple))
    myfreq <- totrows/sum(totrows)

    # removing row names, but keeping column names
    rownames(MD_pattern_simple) <- NULL

    # amputation in 3 patterns
    amputed <- mice::ampute(X_hat, prop = NA_fraction, patterns = MD_pattern_simple,
        freq = myfreq, bycases = F, mech = "MCAR")
    X_hat_MCAR <- amputed$amp

    amputed <- mice::ampute(X_hat, prop = NA_fraction, patterns = MD_pattern_simple,
        freq = myfreq, bycases = F, mech = "MAR")
    X_hat_MAR <- amputed$amp

    amputed <- mice::ampute(X_hat, prop = NA_fraction, patterns = MD_pattern_simple,
        freq = myfreq, bycases = F, mech = "MNAR")
    X_hat_MNAR <- amputed$amp

    X_hat <- cbind(X_hat_MCAR[, assumed_pattern == "MCAR"], X_hat_MAR[, assumed_pattern ==
        "MAR"], X_hat_MNAR[, assumed_pattern == "MNAR"])

    X_hat <- X_hat[, data_names]

    # remove rows with full missingness
    missfrac_per_ind <- rowMeans(is.na(X_hat))
    inds_above_thres <- rownames(X_hat)[missfrac_per_ind == 1]
    if (length(inds_above_thres) != 0)
        X_hat <- X_hat[-which(missfrac_per_ind == 1), ]

    matrix_summary <- summary(X_hat)

    list(MAP_matrix = X_hat, Summary = matrix_summary)

}

