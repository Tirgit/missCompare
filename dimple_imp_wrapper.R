#################################################################################################################################################
#
#The purpose of this function is impute missing datapoints using all missing data imputation algoritms implemented in dimple

#The function takes arguments from the beginning of the pipeline:
#
#Dimensions of the dataset
#Correlation matrix
#Fraction of missingess per variable
#
#The function outputs a list of RMSE values between the full matrix and the matrices with missingess.
#Also, there is an output summarizing the RMSE values (mean, 95 CIs).
#A plot is drawn to compare the missingness patterns.
#The best methods for each missingess patterns are also output.
#
#################################################################################################################################################


###PACKAGES
library(tidyr)
library(dplyr)
library(ggplot2)

#FUNCTION
dimple_imp_wrapper <- function(rownum, colnum, cormat, missfrac_per_var, n.iter = 10, assumed_pattern = NA) {
  
  if (!is.na(assumed_pattern)) collect_res <- data.frame(matrix(NA, nrow = 14*n.iter, ncol = 5)) else collect_res <- data.frame(matrix(NA, nrow = 14*n.iter, ncol = 4))
  if (!is.na(assumed_pattern)) colnames(collect_res) <- c("Method", "MCAR_RMSE", "MAR_RMSE", "MNAR_RMSE", "MAP_RMSE") else colnames(collect_res) <- c("Method", "MCAR_RMSE", "MAR_RMSE", "MNAR_RMSE")
  
  for (i in 1:n.iter) {
    
    collect_res[((14*(i-1))+1):((14*(i-1))+14),1] <- c("Random replacement", "Median imputation", "Mean imputation", "missMDA Regularized", 
                                                                 "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                                 "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                                 "mi Bayesian", "Amelia II", "missForest")
    
    sim <- dimple_sim(rownum, colnum, cormat)
    res <- dimple_all_patterns(sim$Simulated_matrix, missfrac_per_var, assumed_pattern)
    
    collect_res[((14*(i-1))+1),2:ncol(collect_res)] <- as.data.frame(dimple_random_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+2),2:ncol(collect_res)] <- as.data.frame(dimple_median_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+3),2:ncol(collect_res)] <- as.data.frame(dimple_mean_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+4),2:ncol(collect_res)] <- as.data.frame(dimple_missMDA_regularized_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+5),2:ncol(collect_res)] <- as.data.frame(dimple_missMDA_EM_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+6),2:ncol(collect_res)] <- as.data.frame(dimple_pcaMethods_PPCA_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+7),2:ncol(collect_res)] <- as.data.frame(dimple_pcaMethods_svdImpute_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+8),2:ncol(collect_res)] <- as.data.frame(dimple_pcaMethods_BPCA_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+9),2:ncol(collect_res)] <- as.data.frame(dimple_pcaMethods_Nipals_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+10),2:ncol(collect_res)] <- as.data.frame(dimple_pcaMethods_NLPCA_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+11),2:ncol(collect_res)] <- as.data.frame(dimple_mice_mixed_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+12),2:ncol(collect_res)] <- as.data.frame(dimple_mi_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+13),2:ncol(collect_res)] <- as.data.frame(dimple_AmeliaII_imp(sim$Simulated_matrix, list = res))
    collect_res[((14*(i-1))+14),2:ncol(collect_res)] <- as.data.frame(dimple_missForest_imp(sim$Simulated_matrix, list = res))
    
  }
  
  notmiss <- function(x) { sum(!is.na(x)) }
  RMSE_statmaker <- function (x) {
    if (ncol(x) == 5) {
      RMSE_stats <- x %>%
        group_by(Method) %>%
        summarise_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), notmiss)) %>%
        mutate(SE_RMSE_MCAR = MCAR_RMSE_sd / sqrt(MCAR_RMSE_notmiss),
               lower.ci_RMSE_MCAR = MCAR_RMSE_mean - qt(1 - (0.05 / 2), MCAR_RMSE_notmiss - 1) * SE_RMSE_MCAR,
               upper.ci_RMSE_MCAR = MCAR_RMSE_mean + qt(1 - (0.05 / 2), MCAR_RMSE_notmiss - 1) * SE_RMSE_MCAR,
               SE_RMSE_MAR = MAR_RMSE_sd / sqrt(MAR_RMSE_notmiss),
               lower.ci_RMSE_MAR = MAR_RMSE_mean - qt(1 - (0.05 / 2), MAR_RMSE_notmiss - 1) * SE_RMSE_MAR,
               upper.ci_RMSE_MAR = MAR_RMSE_mean + qt(1 - (0.05 / 2), MAR_RMSE_notmiss - 1) * SE_RMSE_MAR,
               SE_RMSE_MNAR = MNAR_RMSE_sd / sqrt(MNAR_RMSE_notmiss),
               lower.ci_RMSE_MNAR = MNAR_RMSE_mean - qt(1 - (0.05 / 2), MNAR_RMSE_notmiss - 1) * SE_RMSE_MNAR,
               upper.ci_RMSE_MNAR = MNAR_RMSE_mean + qt(1 - (0.05 / 2), MNAR_RMSE_notmiss - 1) * SE_RMSE_MNAR,
               SE_RMSE_MAP = MAP_RMSE_sd / sqrt(MAP_RMSE_notmiss),
               lower.ci_RMSE_MAP = MAP_RMSE_mean - qt(1 - (0.05 / 2), MAP_RMSE_notmiss - 1) * SE_RMSE_MAP,
               upper.ci_RMSE_MAP = MAP_RMSE_mean + qt(1 - (0.05 / 2), MAP_RMSE_notmiss - 1) * SE_RMSE_MAP) %>% 
        dplyr::select(Method, MCAR_RMSE_mean, MAR_RMSE_mean, MNAR_RMSE_mean, MAP_RMSE_mean, lower.ci_RMSE_MCAR, upper.ci_RMSE_MCAR, 
                      lower.ci_RMSE_MAR, upper.ci_RMSE_MAR, lower.ci_RMSE_MNAR, upper.ci_RMSE_MNAR, lower.ci_RMSE_MAP, upper.ci_RMSE_MAP) } else {
                        RMSE_stats <- x %>%
                          group_by(Method) %>%
                          summarise_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), notmiss)) %>%
                          mutate(SE_RMSE_MCAR = MCAR_RMSE_sd / sqrt(MCAR_RMSE_notmiss),
                                 lower.ci_RMSE_MCAR = MCAR_RMSE_mean - qt(1 - (0.05 / 2), MCAR_RMSE_notmiss - 1) * SE_RMSE_MCAR,
                                 upper.ci_RMSE_MCAR = MCAR_RMSE_mean + qt(1 - (0.05 / 2), MCAR_RMSE_notmiss - 1) * SE_RMSE_MCAR,
                                 SE_RMSE_MAR = MAR_RMSE_sd / sqrt(MAR_RMSE_notmiss),
                                 lower.ci_RMSE_MAR = MAR_RMSE_mean - qt(1 - (0.05 / 2), MAR_RMSE_notmiss - 1) * SE_RMSE_MAR,
                                 upper.ci_RMSE_MAR = MAR_RMSE_mean + qt(1 - (0.05 / 2), MAR_RMSE_notmiss - 1) * SE_RMSE_MAR,
                                 SE_RMSE_MNAR = MNAR_RMSE_sd / sqrt(MNAR_RMSE_notmiss),
                                 lower.ci_RMSE_MNAR = MNAR_RMSE_mean - qt(1 - (0.05 / 2), MNAR_RMSE_notmiss - 1) * SE_RMSE_MNAR,
                                 upper.ci_RMSE_MNAR = MNAR_RMSE_mean + qt(1 - (0.05 / 2), MNAR_RMSE_notmiss - 1) * SE_RMSE_MNAR) %>% 
                          dplyr::select(Method, MCAR_RMSE_mean, MAR_RMSE_mean, MNAR_RMSE_mean, lower.ci_RMSE_MCAR, upper.ci_RMSE_MCAR, 
                                        lower.ci_RMSE_MAR, upper.ci_RMSE_MAR, lower.ci_RMSE_MNAR, upper.ci_RMSE_MNAR)
                      }
    list(RMSE_stats = RMSE_stats)
  }
  
  summary <- RMSE_statmaker(collect_res)
  
  #Best methods for the three missingness types
  Best_method_MCAR <- summary$RMSE_stats %>%
    filter(MCAR_RMSE_mean == min(MCAR_RMSE_mean)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  Best_method_MAR <- summary$RMSE_stats %>%
    filter(MAR_RMSE_mean == min(MAR_RMSE_mean)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  Best_method_MNAR <- summary$RMSE_stats %>%
    filter(MNAR_RMSE_mean == min(MNAR_RMSE_mean)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  if (!is.na(assumed_pattern)) Best_method_MAP <- summary$RMSE_stats %>%
    filter(MAP_RMSE_mean == min(MAP_RMSE_mean)) %>%
    dplyr::select(Method) %>%
    as.character()
  
  if (!is.na(assumed_pattern)) forgraph <- gather(collect_res, Pattern, RMSE, MCAR_RMSE:MAP_RMSE, factor_key=TRUE) else forgraph <- gather(collect_res, Pattern, RMSE, MCAR_RMSE:MNAR_RMSE, factor_key=TRUE)
  forgraph$Method <- factor(forgraph$Method, levels = c("Random replacement", "Median imputation", "Mean imputation", "missMDA Regularized", 
                                                        "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA", 
                                                        "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed",
                                                        "mi Bayesian", "Amelia II", "missForest"))
  if (!is.na(assumed_pattern)) levels(forgraph$Pattern) <- c("MCAR", "MAR", "MNAR", "MAP") else levels(forgraph$Pattern) <- c("MCAR", "MAR", "MNAR")
  p <- ggplot(forgraph, aes(x=Method, y=RMSE, fill=Method)) + 
    geom_boxplot() +
    facet_grid(~Pattern, scale="free") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ggtitle("Root-mean-square error (RMSE) of various missing data imputation methods") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="") 
  
  #output list
  if (!is.na(assumed_pattern)) list(Imputation_RMSE = collect_res, Imputation_RMSE_means = summary$RMSE_stats, Best_method_MCAR = Best_method_MCAR,
                                    Best_method_MAR = Best_method_MAR, Best_method_MNAR = Best_method_MNAR, Best_method_MAP = Best_method_MAP, Plot = p) else list(Imputation_RMSE = collect_res, Imputation_RMSE_means = summary$RMSE_stats, Best_method_MCAR = Best_method_MCAR,
                                    Best_method_MAR = Best_method_MAR, Best_method_MNAR = Best_method_MNAR, Plot = p)
  

}






#LAB
#wrap <- dimple_imp_wrapper(rownum = y$Rows, 
#                               colnum = y$Columns, 
#                               cormat = y$Corr_matrix, 
#                               missfrac_per_var =  y$Fraction_missingness_per_variable, 
#                               n.iter = 3, assumed_pattern = NA)


#collect_res <- wraps
#collect_res$MAP_RMSE <- collect_res$MCAR_RMSE

