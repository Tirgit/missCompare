

###PACKAGES
library(tidyr)

#FUNCTION
dimple_imp_wrapper <- function(X_hat, missfrac_per_var, n.iter = 10) {
  
  collect_res <- data.frame(matrix(NA, nrow = 13*n.iter, ncol = 4))
  colnames(collect_res) <- c("Method", "MCAR_RMSE", "MAR_RMSE", "MNAR_RMSE")
  
  for (i in 1:n.iter) {
    
    collect_res[((13*(i-1))+1):((13*(i-1))+13),1] <- c("Median imputation", "Mean imputation", "missMDA Regularized", 
                                                                 "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                                 "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                                 "mi Bayesian", "Amelia II", "missForest")
    
    res <- dimple_all_patterns(X_hat, missfrac_per_var)
    
    collect_res[((13*(i-1))+1),2:4] <- as.data.frame(dimple_median_imp(X_hat, list = res))
    collect_res[((13*(i-1))+2),2:4] <- as.data.frame(dimple_mean_imp(X_hat, list = res))
    collect_res[((13*(i-1))+3),2:4] <- as.data.frame(dimple_missMDA_regularized_imp(X_hat, list = res))
    collect_res[((13*(i-1))+4),2:4] <- as.data.frame(dimple_missMDA_EM_imp(X_hat, list = res))
    collect_res[((13*(i-1))+5),2:4] <- as.data.frame(dimple_pcaMethods_PPCA_imp(X_hat, list = res))
    collect_res[((13*(i-1))+6),2:4] <- as.data.frame(dimple_pcaMethods_svdImpute_imp(X_hat, list = res))
    collect_res[((13*(i-1))+7),2:4] <- as.data.frame(dimple_pcaMethods_BPCA_imp(X_hat, list = res))
    collect_res[((13*(i-1))+8),2:4] <- as.data.frame(dimple_pcaMethods_Nipals_imp(X_hat, list = res))
    collect_res[((13*(i-1))+9),2:4] <- as.data.frame(dimple_pcaMethods_NLPCA_imp(X_hat, list = res))
    collect_res[((13*(i-1))+10),2:4] <- as.data.frame(dimple_mice_mixed_imp(X_hat, list = res))
    collect_res[((13*(i-1))+11),2:4] <- as.data.frame(dimple_mi_imp(X_hat, list = res))
    collect_res[((13*(i-1))+12),2:4] <- as.data.frame(dimple_AmeliaII_imp(X_hat, list = res))
    collect_res[((13*(i-1))+13),2:4] <- as.data.frame(dimple_missForest_imp(X_hat, list = res))
    
  }
  
  #means + 95% CIs of RMSE output
  
  #plotting
  
  #best method (lowest mean?)
  
  
  #output list
  #collect_res dataset, means dataset, plot, best method
  list(Imputation_Error = collect_res)
  
  
}

#LAB
errorres <- dimple_imp_wrapper(X_hat = yy$Simulated_matrix, missfrac_per_var =  y$Fraction_missingness_per_variable, n.iter = 5)


dimple_pcaMethods_PPCA_imp(X_hat = yy$Simulated_matrix, list = res)



errorres$Imputation_Error %>%
  group_by(Method) %>%
  summarise(mean_RMSE_MCAR = mean(MCAR_RMSE),
            mean_RMSE_MAR = mean(MAR_RMSE),
            mean_RMSE_MNAR = mean(MNAR_RMSE),
            SD_RMSE_MCAR = sd(MCAR_RMSE),
            SD_RMSE_MAR = sd(MAR_RMSE),
            SD_RMSE_MNAR = sd(MNAR_RMSE),
            n_RMSE_MCAR = n(),
            n_RMSE_MAR = n(),
            n_RMSE_MNAR = n()) %>%
  mutate(SE_RMSE_MCAR = SD_RMSE_MCAR / sqrt(n_RMSE_MCAR),
         lower.ci_RMSE_MCAR = mean_RMSE_MCAR - qt(1 - (0.05 / 2), n_RMSE_MCAR - 1) * SE_RMSE_MCAR,
         upper.ci_RMSE_MCAR = mean_RMSE_MCAR + qt(1 - (0.05 / 2), n_RMSE_MCAR - 1) * SE_RMSE_MCAR,
         SE_RMSE_MAR = SD_RMSE_MAR / sqrt(n_RMSE_MAR),
         lower.ci_RMSE_MAR = mean_RMSE_MAR - qt(1 - (0.05 / 2), n_RMSE_MAR - 1) * SE_RMSE_MAR,
         upper.ci_RMSE_MAR = mean_RMSE_MAR + qt(1 - (0.05 / 2), n_RMSE_MAR - 1) * SE_RMSE_MAR,
         SE_RMSE_MNAR = SD_RMSE_MNAR / sqrt(n_RMSE_MNAR),
         lower.ci_RMSE_MNAR = mean_RMSE_MNAR - qt(1 - (0.05 / 2), n_RMSE_MNAR - 1) * SE_RMSE_MNAR,
         upper.ci_RMSE_MNAR = mean_RMSE_MNAR + qt(1 - (0.05 / 2), n_RMSE_MNAR - 1) * SE_RMSE_MNAR) %>% 
  select(Method, mean_RMSE_MCAR, mean_RMSE_MAR, mean_RMSE_MNAR, lower.ci_RMSE_MCAR, upper.ci_RMSE_MCAR, 
           lower.ci_RMSE_MAR, upper.ci_RMSE_MAR, lower.ci_RMSE_MNAR, upper.ci_RMSE_MNAR)
         
         
         


forgraph <- gather(errorres$Imputation_Error, Pattern, RMSE, MCAR_RMSE:MNAR_RMSE, factor_key=TRUE)
forgraph$Method <- factor(forgraph$Method, levels = c("Median imputation", "Mean imputation", "missMDA Regularized", 
                                                      "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                      "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                      "mi Bayesian", "Amelia II", "missForest"))
forgraph$Pattern <- factor(forgraph$Pattern, levels = c("MCAR", "MAR", "MNAR"))
ggplot(forgraph, aes(x=Method, y=RMSE, fill=Method)) + 
  geom_boxplot() +
  facet_grid(~Pattern, scale="free") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~variety, scale="free")


variety=rep(LETTERS[1:7], each=40)
treatment=rep(c("high","low"),each=20)
note=seq(1:280)+sample(1:150, 280, replace=T)
data=data.frame(variety, treatment ,  note)



















