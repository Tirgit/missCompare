#' @title Dataframe cleaning for missing data handling
#'
#' @description
#' \code{\link{clean}} helps in the conversion of missing values, variable types and removes rows and columns above
#' pre-specified missingess
#'
#' @details
#' For better imputation performance, a good quality dataframe is needed. Variables and samples with very high
#' missingness rates might negatively impact missing data imputation algorithms. This function cleans the original
#' dataframe by removing rows (samples) and columns (variables) above pre-specified thresholds. The function
#' will also convert any strangely coded missing data to NAs. Note that all non numeric variables will
#' be converted or coerced to numeric variables - you will receive a message when factors are converted,
#' but a warning when character variables are coerced to numeric.
#'
#' @param X Original dataframe with samples in rows and variables as columns
#' @param var_remove Variables to remove (e.g. ID). Define by character vector, e.g. c('ID', 'character_variable')
#' @param var_removal_threshold Variable removal threshold with default 0.5 (range between 0 and 1). Variables (columns) above this missingness fraction will be removed during the cleaning process
#' @param ind_removal_threshold Individual removal threshold with default 1 (range between 0 and 1). Individuals (rows) above this missingness fraction will be removed during the cleaning process
#' @param missingness_coding Non NA coding in original dataframe that should be changed to NA (e.g. -9). Can take a single value (define by: missingness_coding = -9) or multiple values (define by: missingness_coding = c(-9, -99, -999))
#'
#' @name clean
#'
#' @return
#' Clean dataset with NAs as missing values and rows/columns above the pre-specified missingness thresholds removed
#'
#' @examples
#' \dontrun{
#' # basic settings
#' cleaned <- clean(clindata_miss, missingness_coding = -9)
#' # factors (sex, education) converted to numeric
#' # variables exceeding the pre-defined removal threshold (PPG) removed
#'
#' # setting very conservative removal thresholds
#' cleaned <- clean(clindata_miss,
#'                  var_removal_threshold = 0.10,
#'                  ind_removal_threshold = 0.9,
#'                  missingness_coding = -9)
#' # factors (sex, education) converted to numeric
#' # variables exceeding the custom set conservative removal threshold (PPG, HDL) removed
#' }
#'
#' @export


# FUNCTION
clean <- function(X, var_remove = NULL, var_removal_threshold = 0.5, ind_removal_threshold = 1,
    missingness_coding = NA) {

    # remove undesired variables
    X[var_remove] <- NULL

    strings_present <- sum(sapply(X, is.character)) > 0

    if (strings_present == TRUE) {
      stop("Warning! Your data contains string variables. Please inspect your data and either remove these variables using the
var_remove argument or convert them into type factor/numeric where applicable.") }

    # convert all variables to numeric
    vars_non_num <- names(X)[!sapply(X, is.numeric)]
    if (length(vars_non_num) != 0)
        X <- as.data.frame(sapply(X, as.numeric))
    if (length(vars_non_num) != 0)
        message(paste("Variable(s) ", (paste(vars_non_num, collapse = ", ")), " converted to numeric.",
            sep = ""))

    # convert to NA
    X <- as.data.frame(lapply(X, function(x) replace(x, x %in% missingness_coding,
        NA)))

    # remove variables above missingness threshold
    missfrac_per_var <- colMeans(is.na(X))
    vars_above_thres <- colnames(X)[missfrac_per_var >= var_removal_threshold]
    if (length(vars_above_thres) != 0)
        new_df <- X[, -which(missfrac_per_var >= var_removal_threshold)] else new_df <- X

    if (length(vars_above_thres) != 0)
        message(paste("Variable(s) ", (paste(vars_above_thres, collapse = ", ")), " removed due to exceeding the pre-defined removal threshold (>",
            var_removal_threshold * 100, "%) for missingness.", sep = ""))

    # remove individuals above missingness threshold
    missfrac_per_ind <- rowMeans(is.na(new_df))
    inds_above_thres <- rownames(X)[missfrac_per_ind >= ind_removal_threshold]
    if (length(inds_above_thres) != 0)
        clean_df <- new_df[-which(missfrac_per_ind >= ind_removal_threshold), ] else clean_df <- new_df

    if (length(inds_above_thres) != 0)
        message(paste(length(inds_above_thres), " individual(s) removed due to exceeding the pre-defined removal threshold (>",
            ind_removal_threshold * 100, "%) for missingness.", sep = ""))

    return(clean_df)

}

