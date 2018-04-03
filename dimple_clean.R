#################################################################################################################################################
#
#The purpose of this function is to pre-clean the dataset before starting the imputation pipeline
#Specifically, the function removes columns (variables) above pre-defined missingness thresholds (by default, above 50%).
#The function can also convert badly coded missing values (e.g. -9).
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
dimple_clean <- function(x, removal_threshold = 0.5, missingness_coding = NA) {
  
  x[x == missingness_coding] <- NA
  
  missfrac_per_var <- colMeans(is.na(x))
  vars_above_half <- colnames(x)[missfrac_per_var>= removal_threshold]
  
  if (length(vars_above_half) != 0) message(paste("Variable(s) ",
                                                  (paste(vars_above_half,collapse=", ") ),
                                                  " removed due to exceeding the pre-defined removal threshold (>",
                                                  removal_threshold*100,
                                                  "%) for missingness.", sep= ""))
  
  new_df <- x[, -which(missfrac_per_var >= removal_threshold)]
  list(Dataframe_clean = new_df)
  
}










###LAB
df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
df_miss <- prodNA(df, 0.48)
df_miss[is.na(df_miss)] <- -9


y <- dimple_get_data(df_miss, matrixplot_sort = T)
y$Fraction_missingness_per_variable

try <- dimple_clean(df_miss, removal_threshold=0.5, missingness_coding = -9)

y <- dimple_get_data(try$Dataframe_clean, matrixplot_sort = T)



df_miss[df_miss == -9] <- NA


new_df <- df_miss[, -which(colMeans(is.na(df_miss)) >= 0.5)]
new