

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
  
  #output list
  list(MCAR_AUC = MCAR_AUC, MAR_AUC = MAR_AUC, MNAR_AUC = MNAR_AUC)
  
}






#LAB
AUC_values <- dimple_predict_missing(rownum = y$Rows, 
                         colnum = y$Columns, 
                         cormat = y$Corr_matrix, 
                         missfrac_per_var =  y$Fraction_missingness_per_variable, 
                         bootstrap = 30)

AUC_values$MCAR_AUC



