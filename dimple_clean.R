#################################################################################################################################################
#
#The purpose of this function is to pre-clean the dataset before starting the imputation pipeline
#Specifically, the function removes columns (variables) above pre-defined missingness thresholds (by default, above 50%).
#The function can also convert badly coded missing values (e.g. -9).
#The function removes rows with more than x data missing (predefined is 90%, but can be changed).
#The function's arguments are:
#
#the original data frame
#removal threshold (by default, 0.5 (50%))
#missingness coding 
#
#The function outputs the a clean dataframe with variables above the removal threshold excluded.
#
#################################################################################################################################################

#converts any missing data conding system to NAs in R
#removes variables with missingness above 50%


#FUNCTION
dimple_clean <- function(x, var_removal_threshold = 0.5, ind_removal_threshold = 1, missingness_coding = NA) {
  
  x[x == missingness_coding] <- NA
  
  missfrac_per_var <- colMeans(is.na(x))
  vars_above_thres <- colnames(x)[missfrac_per_var >= var_removal_threshold]
  if (length(vars_above_thres) != 0) new_df <- x[, -which(missfrac_per_var >= var_removal_threshold)] else new_df <- x
  
  if (length(vars_above_thres) != 0) message(paste("Variable(s) ",
                                                  (paste(vars_above_thres,collapse=", ") ),
                                                  " removed due to exceeding the pre-defined removal threshold (>",
                                                  var_removal_threshold*100,
                                                  "%) for missingness.", sep= ""))
  
  missfrac_per_ind <- rowMeans(is.na(new_df))
  inds_above_thres <- rownames(x)[missfrac_per_ind >= ind_removal_threshold]
  if (length(inds_above_thres) != 0) clean_df <- new_df[-which(missfrac_per_ind >= ind_removal_threshold), ] else clean_df <- new_df
  
  if (length(inds_above_thres) != 0) message(paste(length(inds_above_thres),
                                                   "individual(s) removed due to exceeding the pre-defined removal threshold (>",
                                                   ind_removal_threshold*100,
                                                   "%) for missingness.", sep= ""))
  
  list(Dataframe_clean = clean_df)
  
}

