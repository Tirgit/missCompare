

###PACKAGES
library(tidyr)
library(dplyr)
library(ggplot2)

#FUNCTION
dimple_imp_wrapper <- function(rownum, colnum, cormat, missfrac_per_var, n.iter = 10) {
  
  collect_res <- data.frame(matrix(NA, nrow = 13*n.iter, ncol = 4))
  colnames(collect_res) <- c("Method", "MCAR_RMSE", "MAR_RMSE", "MNAR_RMSE")
  
  for (i in 1:n.iter) {
    
    collect_res[((13*(i-1))+1):((13*(i-1))+13),1] <- c("Median imputation", "Mean imputation", "missMDA Regularized", 
                                                                 "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                                 "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                                 "mi Bayesian", "Amelia II", "missForest")
    
    sim <- dimple_sim_df(rownum, colnum, cormat)
    res <- dimple_all_patterns(sim$Simulated_matrix, missfrac_per_var)
    
    collect_res[((13*(i-1))+1),2:4] <- as.data.frame(dimple_median_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+2),2:4] <- as.data.frame(dimple_mean_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+3),2:4] <- as.data.frame(dimple_missMDA_regularized_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+4),2:4] <- as.data.frame(dimple_missMDA_EM_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+5),2:4] <- as.data.frame(dimple_pcaMethods_PPCA_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+6),2:4] <- as.data.frame(dimple_pcaMethods_svdImpute_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+7),2:4] <- as.data.frame(dimple_pcaMethods_BPCA_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+8),2:4] <- as.data.frame(dimple_pcaMethods_Nipals_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+9),2:4] <- as.data.frame(dimple_pcaMethods_NLPCA_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+10),2:4] <- as.data.frame(dimple_mice_mixed_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+11),2:4] <- as.data.frame(dimple_mi_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+12),2:4] <- as.data.frame(dimple_AmeliaII_imp(sim$Simulated_matrix, list = res))
    collect_res[((13*(i-1))+13),2:4] <- as.data.frame(dimple_missForest_imp(sim$Simulated_matrix, list = res))
    
  }
  
  RMSE_stats <- collect_res %>%
    group_by(Method) %>%
    summarise(mean_RMSE_MCAR = mean(MCAR_RMSE, na.rm=T),
              mean_RMSE_MAR = mean(MAR_RMSE, na.rm=T),
              mean_RMSE_MNAR = mean(MNAR_RMSE, na.rm=T),
              SD_RMSE_MCAR = sd(MCAR_RMSE, na.rm=T),
              SD_RMSE_MAR = sd(MAR_RMSE, na.rm=T),
              SD_RMSE_MNAR = sd(MNAR_RMSE, na.rm=T),
              n_RMSE_MCAR = sum(!is.na(MCAR_RMSE)),
              n_RMSE_MAR = sum(!is.na(MAR_RMSE)),
              n_RMSE_MNAR = sum(!is.na(MNAR_RMSE))) %>%
    mutate(SE_RMSE_MCAR = SD_RMSE_MCAR / sqrt(n_RMSE_MCAR),
           lower.ci_RMSE_MCAR = mean_RMSE_MCAR - qt(1 - (0.05 / 2), n_RMSE_MCAR - 1) * SE_RMSE_MCAR,
           upper.ci_RMSE_MCAR = mean_RMSE_MCAR + qt(1 - (0.05 / 2), n_RMSE_MCAR - 1) * SE_RMSE_MCAR,
           SE_RMSE_MAR = SD_RMSE_MAR / sqrt(n_RMSE_MAR),
           lower.ci_RMSE_MAR = mean_RMSE_MAR - qt(1 - (0.05 / 2), n_RMSE_MAR - 1) * SE_RMSE_MAR,
           upper.ci_RMSE_MAR = mean_RMSE_MAR + qt(1 - (0.05 / 2), n_RMSE_MAR - 1) * SE_RMSE_MAR,
           SE_RMSE_MNAR = SD_RMSE_MNAR / sqrt(n_RMSE_MNAR),
           lower.ci_RMSE_MNAR = mean_RMSE_MNAR - qt(1 - (0.05 / 2), n_RMSE_MNAR - 1) * SE_RMSE_MNAR,
           upper.ci_RMSE_MNAR = mean_RMSE_MNAR + qt(1 - (0.05 / 2), n_RMSE_MNAR - 1) * SE_RMSE_MNAR) %>% 
    dplyr::select(Method, mean_RMSE_MCAR, mean_RMSE_MAR, mean_RMSE_MNAR, lower.ci_RMSE_MCAR, upper.ci_RMSE_MCAR, 
           lower.ci_RMSE_MAR, upper.ci_RMSE_MAR, lower.ci_RMSE_MNAR, upper.ci_RMSE_MNAR)

  
  #Best methods for the three missingness types
  Best_method_MCAR <- RMSE_stats %>%
    filter(mean_RMSE_MCAR == min(mean_RMSE_MCAR)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  Best_method_MAR <- RMSE_stats %>%
    filter(mean_RMSE_MAR == min(mean_RMSE_MAR)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  Best_method_MNAR <- RMSE_stats %>%
    filter(mean_RMSE_MNAR == min(mean_RMSE_MNAR)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  forgraph <- gather(collect_res, Pattern, RMSE, MCAR_RMSE:MNAR_RMSE, factor_key=TRUE)
  forgraph$Method <- factor(forgraph$Method, levels = c("Median imputation", "Mean imputation", "missMDA Regularized", 
                                                        "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                        "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                        "mi Bayesian", "Amelia II", "missForest"))
  levels(forgraph$Pattern) <- c("MCAR", "MAR", "MNAR")
  p <- ggplot(forgraph, aes(x=Method, y=RMSE, fill=Method)) + 
    geom_boxplot() +
    facet_grid(~Pattern, scale="free") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #output list
  list(Imputation_RMSE = collect_res, Imputation_RMSE_means = RMSE_stats, Best_method_MCAR = Best_method_MCAR,
       Best_method_MAR = Best_method_MAR, Best_method_MNAR = Best_method_MNAR, Plot = p)
  
  
}






#LAB
wrap <- dimple_imp_wrapper(rownum = y$Rows, 
                               colnum = y$Columns, 
                               cormat = y$Corr_matrix, 
                               missfrac_per_var =  y$Fraction_missingness_per_variable, 
                               n.iter = 3)










collect_res <- data.frame(matrix(NA, nrow = 13*2, ncol = 4))
colnames(collect_res) <- c("Method", "MCAR_RMSE", "MAR_RMSE", "MNAR_RMSE")

for (i in 1:2) {
  
  collect_res[((13*(i-1))+1):((13*(i-1))+13),1] <- c("Median imputation", "Mean imputation", "missMDA Regularized", 
                                                     "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                     "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                     "mi Bayesian", "Amelia II", "missForest")
  
  sim <- dimple_sim_df(rownum = y$Rows, colnum = y$Columns, cormat = y$Corr_matrix)
  res <- dimple_all_patterns(sim$Simulated_matrix, missfrac_per_var = y$Fraction_missingness_per_variable)
  
  collect_res[((13*(i-1))+1),2:4] <- as.data.frame(dimple_median_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+2),2:4] <- as.data.frame(dimple_mean_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+3),2:4] <- as.data.frame(dimple_missMDA_regularized_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+4),2:4] <- as.data.frame(dimple_missMDA_EM_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+5),2:4] <- as.data.frame(dimple_pcaMethods_PPCA_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+6),2:4] <- as.data.frame(dimple_pcaMethods_svdImpute_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+7),2:4] <- as.data.frame(dimple_pcaMethods_BPCA_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+8),2:4] <- as.data.frame(dimple_pcaMethods_Nipals_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+9),2:4] <- as.data.frame(dimple_pcaMethods_NLPCA_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+10),2:4] <- as.data.frame(dimple_mice_mixed_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+11),2:4] <- as.data.frame(dimple_mi_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+12),2:4] <- as.data.frame(dimple_AmeliaII_imp(sim$Simulated_matrix, list = res))
  collect_res[((13*(i-1))+13),2:4] <- as.data.frame(dimple_missForest_imp(sim$Simulated_matrix, list = res))
  
}

RMSE_stats <- collect_res %>%
  group_by(Method) %>%
  summarise(mean_RMSE_MCAR = mean(MCAR_RMSE, na.rm=T),
            mean_RMSE_MAR = mean(MAR_RMSE, na.rm=T),
            mean_RMSE_MNAR = mean(MNAR_RMSE, na.rm=T),
            SD_RMSE_MCAR = sd(MCAR_RMSE, na.rm=T),
            SD_RMSE_MAR = sd(MAR_RMSE, na.rm=T),
            SD_RMSE_MNAR = sd(MNAR_RMSE, na.rm=T),
            n_RMSE_MCAR = sum(!is.na(MCAR_RMSE)),
            n_RMSE_MAR = sum(!is.na(MAR_RMSE)),
            n_RMSE_MNAR = sum(!is.na(MNAR_RMSE))) %>%
  mutate(SE_RMSE_MCAR = SD_RMSE_MCAR / sqrt(n_RMSE_MCAR),
         lower.ci_RMSE_MCAR = mean_RMSE_MCAR - qt(1 - (0.05 / 2), n_RMSE_MCAR - 1) * SE_RMSE_MCAR,
         upper.ci_RMSE_MCAR = mean_RMSE_MCAR + qt(1 - (0.05 / 2), n_RMSE_MCAR - 1) * SE_RMSE_MCAR,
         SE_RMSE_MAR = SD_RMSE_MAR / sqrt(n_RMSE_MAR),
         lower.ci_RMSE_MAR = mean_RMSE_MAR - qt(1 - (0.05 / 2), n_RMSE_MAR - 1) * SE_RMSE_MAR,
         upper.ci_RMSE_MAR = mean_RMSE_MAR + qt(1 - (0.05 / 2), n_RMSE_MAR - 1) * SE_RMSE_MAR,
         SE_RMSE_MNAR = SD_RMSE_MNAR / sqrt(n_RMSE_MNAR),
         lower.ci_RMSE_MNAR = mean_RMSE_MNAR - qt(1 - (0.05 / 2), n_RMSE_MNAR - 1) * SE_RMSE_MNAR,
         upper.ci_RMSE_MNAR = mean_RMSE_MNAR + qt(1 - (0.05 / 2), n_RMSE_MNAR - 1) * SE_RMSE_MNAR) %>% 
  dplyr::select(Method, mean_RMSE_MCAR, mean_RMSE_MAR, mean_RMSE_MNAR, lower.ci_RMSE_MCAR, upper.ci_RMSE_MCAR, 
         lower.ci_RMSE_MAR, upper.ci_RMSE_MAR, lower.ci_RMSE_MNAR, upper.ci_RMSE_MNAR)


#Best methods for the three missingness types
Best_method_MCAR <- RMSE_stats %>%
  filter(mean_RMSE_MCAR == min(mean_RMSE_MCAR)) %>%
  dplyr::select(Method) %>%
  as.character()

Best_method_MAR <- RMSE_stats %>%
  filter(mean_RMSE_MAR == min(mean_RMSE_MAR)) %>%
  dplyr::select(Method) %>%
  as.character()

Best_method_MNAR <- RMSE_stats %>%
  filter(mean_RMSE_MNAR == min(mean_RMSE_MNAR)) %>%
  dplyr::select(Method) %>%
  as.character()

forgraph <- gather(collect_res, Pattern, RMSE, MCAR_RMSE:MNAR_RMSE, factor_key=TRUE)
forgraph$Method <- factor(forgraph$Method, levels = c("Median imputation", "Mean imputation", "missMDA Regularized", 
                                                      "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                      "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                      "mi Bayesian", "Amelia II", "missForest"))
levels(forgraph$Pattern) <- c("MCAR", "MAR", "MNAR")
p <- ggplot(forgraph, aes(x=Method, y=RMSE, fill=Method)) + 
  geom_boxplot() +
  facet_grid(~Pattern, scale="free") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#output list
list(Imputation_RMSE = collect_res, Imputation_RMSE_means = RMSE_stats, Best_method_MCAR = Best_method_MCAR,
     Best_method_MAR = Best_method_MAR, Best_method_MNAR = Best_method_MNAR, Plot = p)




