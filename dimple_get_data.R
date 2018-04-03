#################################################################################################################################################
#
#The purpose of this function is to obtain information about the user's dataset.
#These pieces of information are:
#
#data size
#data dimensions
#correlation matrix
#fraction of missingness
#missing data pattern
#
#
#The function also provides statistics and visualization for missing data observation
#
#################################################################################################################################################


###PACKAGES
library(mice)
library(VIM)
library(ggplot2)
library(magrittr)
library(dplyr)


###FUNCTION
dimple_get_data <- function(X, matrixplot_sort = F ,missplot = F) {
  comp <- sum(complete.cases(X))
  rows <- nrow(X)
  cols <- ncol(X)
  mat <- cor(X, use="pairwise.complete.obs", method="pearson") 
  missfrac_per_df <- sum(is.na(X))/(nrow(X)*ncol(X))
  missfrac_per_var <- colMeans(is.na(X))
  na_per_df <-  sum(is.na(X))
  na_per_var <- sapply(X, function(x) sum(length(which(is.na(x))))) 
  mdpat <- md.pattern(X)
  
  nm1 <- names(X)[colSums(is.na(X)) >0]
  arr_X <- X %>% 
    arrange_at(vars(nm1), funs(desc(is.na(.))))
  
  vars_above_half <- colnames(X)[missfrac_per_var>=0.5]
  if (length(vars_above_half) != 0) message(paste("Warning! Missingness exceeds 50% for variable(s) ",
                (paste(vars_above_half,collapse=", ") ),
                ". Consider excluding these variables using dimple_clean() and repeating function until no warnings are shown.", sep= ""))
  
  #matrix plot
  if (matrixplot_sort == F) matrixplot(X, interactive = F, col= "red") else matrixplot(arr_X, interactive = F, col= "red")
  
  #output
  list(Complete_cases = comp, Rows = rows, Columns = cols, Corr_matrix = mat, 
       Fraction_missingness = missfrac_per_df, Fraction_missingness_per_variable = missfrac_per_var, 
       Total_NA = na_per_df, NA_per_variable = na_per_var, MD_Pattern = mdpat,
       Vars_above_half = vars_above_half)
  
}


###LAB
df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
library(missForest)
df_miss <- prodNA(df, 0.2)


y <- dimple_get_data(df_miss, matrixplot_sort = T)

 


