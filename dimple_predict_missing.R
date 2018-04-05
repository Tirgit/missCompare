

#given available data predicts missing data (diganostics for MAR)
#first convert missing vs available data to 0/1
#set up logistic regression model and random forest to predict 0/1 outcome


#LIBRARIES
library(pROC)
library(ROCR)    


#FUNCTION
dimple_predict_missing <- function(rownum, colnum, cormat, missfrac_per_var, bootstrap) {
  
  MCAR_AUC <- vector('numeric')
  MAR_AUC <- vector('numeric')
  MNAR_AUC <- vector('numeric')
  
  for (number in 1:bootstrap) {
  
  sim <- dimple_sim(rownum, colnum, cormat)
  res <- dimple_all_patterns(sim$Simulated_matrix, missfrac_per_var)
  
  for (i in 1:ncol(res$MCAR_matrix)) {
    complete <- res$MCAR_matrix[complete.cases(res$MCAR_matrix[,-i]),]
    
    # 0 if available data
    outcome <- rep(0, nrow(complete))
    # 1 if missing
    outcome[is.na(complete[,i])] <- 1
    #predict using logistic regression using the rest of the variables, obtain AUC ROC
    mylogit <- glm(outcome ~ complete[,-i], family = "binomial")
    prob <- predict(mylogit,type=c("response")) 
    AUC <- auc(roc(outcome ~ prob))  
    MCAR_AUC <- c(MCAR_AUC,AUC)
  }
  
  for (i in 1:ncol(res$MAR_matrix)) {
    complete <- res$MAR_matrix[complete.cases(res$MAR_matrix[,-i]),]
    
    # 0 if available data
    outcome <- rep(0, nrow(complete))
    # 1 if missing
    outcome[is.na(complete[,i])] <- 1
    #predict using logistic regression using the rest of the variables, obtain AUC ROC
    mylogit <- glm(outcome ~ complete[,-i], family = "binomial")
    prob <- predict(mylogit,type=c("response")) 
    AUC <- auc(roc(outcome ~ prob))  
    MAR_AUC <- c(MAR_AUC,AUC)
  }
  
  for (i in 1:ncol(res$MNAR_matrix)) {
    complete <- res$MNAR_matrix[complete.cases(res$MNAR_matrix[,-i]),]
    
    # 0 if available data
    outcome <- rep(0, nrow(complete))
    # 1 if missing
    outcome[is.na(complete[,i])] <- 1
    #predict using logistic regression using the rest of the variables, obtain AUC ROC
    mylogit <- glm(outcome ~ complete[,-i], family = "binomial")
    prob <- predict(mylogit,type=c("response")) 
    AUC <- auc(roc(outcome ~ prob))  
    MNAR_AUC <- c(MNAR_AUC,AUC)
  }
  
  }
  
  rocs <- as.data.frame(cbind(MCAR_AUC, MAR_AUC, MNAR_AUC))
  names(rocs) <- c("MCAR", "MAR", "MNAR")
  rocs_forgraph <- gather(rocs, Pattern, AUC, factor_key=TRUE)
  levels(rocs_forgraph$Pattern) <- c("MCAR", "MAR", "MNAR")
  
  AUCplot <- ggplot(rocs_forgraph, aes(x=Pattern, y=AUC, fill=Pattern)) + 
    geom_violin() + 
    ggtitle("ROC AUC statistics for predicting whether data is available or missing") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="") 
  
  #output list
  list(MCAR_AUC = MCAR_AUC, MAR_AUC = MAR_AUC, MNAR_AUC = MNAR_AUC, Plot = AUCplot)
  
}
















#LAB
AUC_values <- dimple_predict_missing(rownum = y$Rows, 
                         colnum = y$Columns, 
                         cormat = y$Corr_matrix, 
                         missfrac_per_var =  y$Fraction_missingness_per_variable, 
                         bootstrap = 100)

pdf("/Users/med-tv_/Documents/Projects/missingdata/ROC_AUC_plot.pdf")
AUC_values$Plot
dev.off()






#PLOT output for various percentages
df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
df_miss <- prodNA(df, 0.01)
cleaned <- dimple_clean(df_miss)
y <- dimple_get_data(cleaned$Dataframe_clean, matrixplot_sort = F)

AUC_values_1 <- dimple_predict_missing(rownum = y$Rows, 
                                        colnum = y$Columns, 
                                        cormat = y$Corr_matrix, 
                                        missfrac_per_var =  y$Fraction_missingness_per_variable, 
                                        bootstrap = 100)

df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
df_miss <- prodNA(df, 0.05)
cleaned <- dimple_clean(df_miss)
y <- dimple_get_data(cleaned$Dataframe_clean, matrixplot_sort = F)

AUC_values_5 <- dimple_predict_missing(rownum = y$Rows, 
                                        colnum = y$Columns, 
                                        cormat = y$Corr_matrix, 
                                        missfrac_per_var =  y$Fraction_missingness_per_variable, 
                                        bootstrap = 100)

df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
df_miss <- prodNA(df, 0.1)
cleaned <- dimple_clean(df_miss)
y <- dimple_get_data(cleaned$Dataframe_clean, matrixplot_sort = F)

AUC_values_10 <- dimple_predict_missing(rownum = y$Rows, 
                                     colnum = y$Columns, 
                                     cormat = y$Corr_matrix, 
                                     missfrac_per_var =  y$Fraction_missingness_per_variable, 
                                     bootstrap = 100)

df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
df_miss <- prodNA(df, 0.2)
cleaned <- dimple_clean(df_miss)
y <- dimple_get_data(cleaned$Dataframe_clean, matrixplot_sort = F)

AUC_values_20 <- dimple_predict_missing(rownum = y$Rows, 
                                        colnum = y$Columns, 
                                        cormat = y$Corr_matrix, 
                                        missfrac_per_var =  y$Fraction_missingness_per_variable, 
                                        bootstrap = 100)

df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
df_miss <- prodNA(df, 0.3)
cleaned <- dimple_clean(df_miss)
y <- dimple_get_data(cleaned$Dataframe_clean, matrixplot_sort = F)

AUC_values_30 <- dimple_predict_missing(rownum = y$Rows, 
                                        colnum = y$Columns, 
                                        cormat = y$Corr_matrix, 
                                        missfrac_per_var =  y$Fraction_missingness_per_variable, 
                                        bootstrap = 100)

df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
df_miss <- prodNA(df, 0.4)
cleaned <- dimple_clean(df_miss)
y <- dimple_get_data(cleaned$Dataframe_clean, matrixplot_sort = F)

AUC_values_40 <- dimple_predict_missing(rownum = y$Rows, 
                                        colnum = y$Columns, 
                                        cormat = y$Corr_matrix, 
                                        missfrac_per_var =  y$Fraction_missingness_per_variable, 
                                        bootstrap = 100)


rocs1 <- as.data.frame(cbind(rep("1%", 1000), AUC_values_1$MCAR_AUC, AUC_values_1$MAR_AUC, AUC_values_1$MNAR_AUC))
names(rocs1) <- c("Percentage", "MCAR", "MAR", "MNAR")
rocs5 <- as.data.frame(cbind(rep("5%", 1000), AUC_values_5$MCAR_AUC, AUC_values_5$MAR_AUC, AUC_values_5$MNAR_AUC))
names(rocs5) <- c("Percentage", "MCAR", "MAR", "MNAR")
rocs10 <- as.data.frame(cbind(rep("10%", 1000), AUC_values_10$MCAR_AUC, AUC_values_10$MAR_AUC, AUC_values_10$MNAR_AUC))
names(rocs10) <- c("Percentage", "MCAR", "MAR", "MNAR")
rocs20 <- as.data.frame(cbind(rep("20%", 1000), AUC_values_20$MCAR_AUC, AUC_values_20$MAR_AUC, AUC_values_20$MNAR_AUC))
names(rocs20) <- c("Percentage", "MCAR", "MAR", "MNAR")
rocs30 <- as.data.frame(cbind(rep("30%", 1000), AUC_values_30$MCAR_AUC, AUC_values_30$MAR_AUC, AUC_values_30$MNAR_AUC))
names(rocs30) <- c("Percentage", "MCAR", "MAR", "MNAR")
rocs40 <- as.data.frame(cbind(rep("40%", 1000), AUC_values_40$MCAR_AUC, AUC_values_40$MAR_AUC, AUC_values_40$MNAR_AUC))
names(rocs40) <- c("Percentage", "MCAR", "MAR", "MNAR")

allrocs <- rbind(rocs1, rocs5, rocs10, rocs20, rocs30, rocs40)
rocs_forgraph <- gather(allrocs, Pattern, AUC, MCAR:MNAR, factor_key=TRUE)
levels(rocs_forgraph$Pattern) <- c("MCAR", "MAR", "MNAR")
rocs_forgraph$AUC <- as.numeric(rocs_forgraph$AUC)

AUCplot <- ggplot(rocs_forgraph, aes(x=Percentage, y=AUC, fill=Percentage)) + 
  geom_boxplot()+
  facet_grid(~Pattern, scale="free") + 
  ggtitle("ROC AUC statistics for predicting whether data is available or missing, by fraction of missingness") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="") 

pdf("/Users/med-tv_/Documents/Projects/missingdata/ROC_AUC_plot_perc.pdf")
AUCplot
dev.off()


