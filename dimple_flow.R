###PACKAGES
library(mice)
library(VIM)
library(ggplot2)
library(magrittr)
library(dplyr)
library(MASS)
library(Matrix)
library(missForest)
library(missMDA)
library(pcaMethods)
library(mi)
library(Amelia)
library(tidyr)
library(pROC)
library(ROCR)  
library(Hmisc)
library(ggdendro)
library(mlbench)

###FUNCTIONS
setwd("/Users/med-tv_/Documents/GitHub/missingdata")
source("dimple_clean.R")
source("dimple_get_data.R")
source("dimple_sim.R")
source("dimple_MCAR.R")
source("dimple_MAR.R")
source("dimple_MNAR.R")
source("dimple_MAP.R")
source("dimple_all_patterns.R")
#source("dimple_predict_missing.R")

source("dimple_random_imp.R")
source("dimple_median_imp.R")
source("dimple_mean_imp.R")
source("dimple_missMDA_EM_imp.R")
source("dimple_missMDA_regularized_imp.R")
source("dimple_pcaMethods_svdImpute_imp.R")
source("dimple_pcaMethods_PPCA_imp.R")
source("dimple_pcaMethods_NLPCA_imp.R")
source("dimple_pcaMethods_Nipals_imp.R")
source("dimple_pcaMethods_BPCA_imp.R")
source("dimple_mi_imp.R")
source("dimple_mice_mixed_imp.R")
source("dimple_missForest_imp.R")
source("dimple_AmeliaII_imp.R")

source("dimple_imp_wrapper.R")
source("dimple_summary.R")
source("dimple_get_data.R")


###LAB
data("BostonHousing")
df <- BostonHousing
df_miss <- prodNA(df, 0.2)

cleaned <- dimple_clean(df_miss)
y <- dimple_get_data(cleaned$Dataframe_clean, matrixplot_sort = T)

yy <- dimple_sim(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)

res <- dimple_MCAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
res <- dimple_MNAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
res <- dimple_MAR(yy$Simulated_matrix, y$Fraction_missingness_per_variable)

res <- dimple_all_patterns(yy$Simulated_matrix, y$Fraction_missingness_per_variable)
matrixplot(res$MCAR_matrix, interactive = F, col= "red") 
matrixplot(res$MAR_matrix, interactive = F, col= "red") 
matrixplot(res$MNAR_matrix, interactive = F, col= "red") 

dimple_median_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_mean_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_missMDA_regularized_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_missMDA_EM_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_PPCA_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_svdImpute_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_BPCA_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_Nipals_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_pcaMethods_NLPCA_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_mice_mixed_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_mi_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_AmeliaII_imp(X_hat = yy$Simulated_matrix, list = res)
dimple_missForest_imp(X_hat = yy$Simulated_matrix, list = res)



wrap <- dimple_imp_wrapper(rownum = y$Rows, 
                           colnum = y$Columns, 
                           cormat = y$Corr_matrix, 
                           missfrac_per_var =  y$Fraction_missingness_per_variable, 
                           n.iter = 3)



wraps <- readRDS(file = "/Users/med-tv_/Documents/Projects/missingdata/wraps.rds")

Imputation_RMSE <- rbind(wraps, wrap$Imputation_RMSE)

saveRDS(Imputation_RMSE, file = "/Users/med-tv_/Documents/Projects/missingdata/wraps.rds")


forgraph <- gather(Imputation_RMSE, Pattern, RMSE, MCAR_RMSE:MNAR_RMSE, factor_key=TRUE)
forgraph$Method <- factor(forgraph$Method, levels = c("Random replacement", "Median imputation", "Mean imputation", "missMDA Regularized", 
                                                      "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                      "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                      "mi Bayesian", "Amelia II", "missForest"))
levels(forgraph$Pattern) <- c("MCAR", "MAR", "MNAR")
p <- ggplot(forgraph, aes(x=Method, y=RMSE, fill=Method)) + 
  geom_boxplot() +
  facet_grid(~Pattern, scale="free") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Root-mean-square error (RMSE) of various missing data imputation methods") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="") 




pdf("/Users/med-tv_/Documents/Projects/missingdata/RMSE_plot.pdf", width=10, height=6)
p
dev.off()


dimple_summary(wrap)









#REAL DATA
library(foreign)
mydata <- read.dta("/Volumes/External/LOCUS/pheno/glacier_corr_160127.dta")
vars <- colnames(mydata)
to_exclude <- c(grep("da", names(mydata), value=TRUE) ,
                grep("gram", names(mydata), value=TRUE),
                grep("sum", names(mydata), value=TRUE),
                grep("upps", names(mydata), value=TRUE),
                grep("delp", names(mydata), value=TRUE)
                )
mydata <- mydata[, !(names(mydata) %in% to_exclude )] 
mydata <- mydata[mydata$besok == 1,]
mydata <- mydata[!is.na(mydata$id),]
mydata <- mydata[1:2000, 2:16]



y <- dimple_get_data(mydata, matrixplot_sort = T, plot_transform = T)
y$Matrix_plot
clean <- dimple_clean(mydata, var_removal_threshold = 0.4)
y <- dimple_get_data(clean$Dataframe_clean, matrixplot_sort = T)
y$Matrix_plot
y$Cluster_plot

yy <- dimple_sim(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)


n <- naclus(clean$Dataframe_clean)
plot(n)

wrap <- dimple_imp_wrapper(rownum = y$Rows, 
                           colnum = y$Columns, 
                           cormat = y$Corr_matrix, 
                           missfrac_per_var =  y$Fraction_missingness_per_variable, 
                           n.iter = 5)
dimple_summary(wrap)





as.data.frame(scale(mydata))


#visualizing MAR/MNAR on matrixplot
MNAR_spike <- res$MNAR_matrix
colnames(MNAR_spike) <- c("X1", "X2","X3","X4","X5","X6","X7","X8","X9","X10")
nm1 <- colnames(MNAR_spike)[colSums(is.na(MNAR_spike)) >0]
arr_MNAR_spike <- MNAR_spike %>% 
  as.data.frame() %>% 
  arrange_at(vars(nm1), funs(desc(is.na(.))))
matrixplot(arr_MNAR_spike, interactive = F, col= "red")




