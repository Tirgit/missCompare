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
#
#The function also provides statistics and visualization for missing data observation
#
#
#Necessary packages: ggplot2, VIM, mice, ...
#
#
#################################################################################################################################################

library(mice)
library(VIM)
library(Amelia)

df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
library(missForest)
df_miss <- prodNA(df, 0.3)

dimple_get_data <- function(X, matrixplot_sort = F ,missplot = NA) {
  rows <- nrow(X)
  cols <- ncol(X)
  mat <- cor(X, use="complete.obs", method="pearson") 
  missfrac <- sum(is.na(X))/(nrow(X)*ncol(X))
  mdpat <- md.pattern(df_miss)
  comp <- sum(complete.cases(df_miss))
  list(Complete_cases = comp, Rows = rows, Columns = cols, Matrix = mat, Fraction_missingness = missfrac, MD_Pattern = mdpat)
  
  matrixplot(X, interactive = F)
  
}

dimple_get_data(df_miss)


matrixplot(df_miss, interactive = F, sortby = "X1")
missmap(df_miss, rank.order = F)




