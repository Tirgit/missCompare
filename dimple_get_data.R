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
library(Hmisc)
library(ggdendro)


###FUNCTION
dimple_get_data <- function(X, matrixplot_sort = F, plot_transform = T) {
  comp <- sum(complete.cases(X))
  rows <- nrow(X)
  cols <- ncol(X)
  mat <- cor(X, use="pairwise.complete.obs", method="pearson") 
  missfrac_per_df <- sum(is.na(X))/(nrow(X)*ncol(X))
  missfrac_per_var <- colMeans(is.na(X))
  na_per_df <-  sum(is.na(X))
  na_per_var <- sapply(X, function(x) sum(length(which(is.na(x))))) 
  mdpat <- md.pattern(X)
  
  if (plot_transform == T) X_update <- as.data.frame(scale(X)) else X_update <- X
  
  nm1 <- names(X_update)[colSums(is.na(X_update)) >0]
  arr_X <- X_update %>% 
    arrange_at(vars(nm1), funs(desc(is.na(.))))
  
  vars_above_half <- colnames(X_update)[missfrac_per_var>=0.5]
  if (length(vars_above_half) != 0) message(paste("Warning! Missingness exceeds 50% for variable(s) ",
                (paste(vars_above_half,collapse=", ") ),
                ". Consider excluding these variables using dimple_clean() and repeating function until no warnings are shown.", sep= ""))

  #matrix plot
  df_miss_id <- cbind(c(1:rows), arr_X)
  colnames(df_miss_id) <- c("Observations", colnames(arr_X))
  df_melt <- melt(df_miss_id, id=c("Observations"))
  matrixplot_sorted <- ggplot(df_melt, aes(x=variable, y=Observations)) +
    geom_tile(aes(fill=value)) +
    scale_fill_gradient(low = "white", high = "lightblue") + 
    theme(panel.background = element_blank()) + 
    ggtitle("Matrix plot of missing data") +
    theme(plot.title = element_text(hjust = 0.5))
  
  df_miss_id <- cbind(c(1:rows), X_update)
  colnames(df_miss_id) <- c("Observations", colnames(X_update))
  df_melt <- melt(df_miss_id, id=c("Observations"))
  matrixplot_unsorted <- ggplot(df_melt, aes(x=variable, y=Observations)) +
    geom_tile(aes(fill=value)) +
    scale_fill_gradient(low = "white", high = "lightblue") + 
    theme(panel.background = element_blank()) + 
    ggtitle("Matrix plot of missing data") +
    theme(plot.title = element_text(hjust = 0.5))
    
  if (matrixplot_sort == F) matrix_plot <- matrixplot_unsorted else matrix_plot <- matrixplot_sorted
  
  #cluster plot
  
  any_miss <- X_update[,which(!colSums(is.na(X_update))==0)]
  
  yesno <- any_miss %>% 
    is.na 
  d <- dist(t(yesno), method = "binary")
  hc <- hclust(d, method= "ward.D")
  hcdata <- dendro_data(hc)
  cluster_plot <- ggdendrogram(hcdata, theme_dendro = FALSE) + 
    ggtitle("Cluster plot of missing data") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(x = "variable", y= "Height")
  
  #output
  list(Complete_cases = comp, Rows = rows, Columns = cols, Corr_matrix = mat, 
       Fraction_missingness = missfrac_per_df, Fraction_missingness_per_variable = missfrac_per_var, 
       Total_NA = na_per_df, NA_per_variable = na_per_var, MD_Pattern = mdpat,
       Vars_above_half = vars_above_half, Matrix_plot = matrix_plot, Cluster_plot = cluster_plot)
  
}



