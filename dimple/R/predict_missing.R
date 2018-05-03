

#given available data predicts missing data (diganostics for MAR)
#first convert missing vs available data to 0/1
#set up logistic regression model and random forest to predict 0/1 outcome


#LIBRARIES
library(pROC)
library(ROCR)


#FUNCTION
predict_missing <- function(rownum, colnum, cormat, missfrac_per_var, bootstrap) {

  MCAR_AUC <- vector('numeric')
  MAR_AUC <- vector('numeric')
  MNAR_AUC <- vector('numeric')

  for (number in 1:bootstrap) {

  sim <- simulate(rownum, colnum, cormat)
  res <- all_patterns(sim$Simulated_matrix, missfrac_per_var)

  MCAR_cols_with_miss <- which(colSums(is.na(res$MCAR_matrix))>0)

  for (i in MCAR_cols_with_miss) {
    complete <- res$MCAR_matrix[stats::complete.cases(res$MCAR_matrix[,-i]),]

    # 0 if available data
    outcome <- rep(0, nrow(complete))
    # 1 if missing
    outcome[is.na(complete[,i])] <- 1
    #predict using logistic regression using the rest of the variables, obtain AUC ROC
    mylogit <- stats::glm(outcome ~ complete[,-i], family = "binomial")
    prob <- stats::predict(mylogit,type=c("response"))
    AUC <- auc(roc(outcome ~ prob))
    MCAR_AUC <- c(MCAR_AUC,AUC)
  }

  MAR_cols_with_miss <- which(colSums(is.na(res$MAR_matrix))>0)

  for (i in MAR_cols_with_miss) {
    complete <- res$MAR_matrix[stats::complete.cases(res$MAR_matrix[,-i]),]

    # 0 if available data
    outcome <- rep(0, nrow(complete))
    # 1 if missing
    outcome[is.na(complete[,i])] <- 1
    #predict using logistic regression using the rest of the variables, obtain AUC ROC
    mylogit <- stats::glm(outcome ~ complete[,-i], family = "binomial")
    prob <- stats::predict(mylogit,type=c("response"))
    AUC <- auc(roc(outcome ~ prob))
    MAR_AUC <- c(MAR_AUC,AUC)
  }

  MNAR_cols_with_miss <- which(colSums(is.na(res$MNAR_matrix))>0)

  for (i in MNAR_cols_with_miss) {
    complete <- res$MNAR_matrix[stats::complete.cases(res$MNAR_matrix[,-i]),]

    # 0 if available data
    outcome <- rep(0, nrow(complete))
    # 1 if missing
    outcome[is.na(complete[,i])] <- 1
    #predict using logistic regression using the rest of the variables, obtain AUC ROC
    mylogit <- stats::glm(outcome ~ complete[,-i], family = "binomial")
    prob <- stats::predict(mylogit,type=c("response"))
    AUC <- auc(roc(outcome ~ prob))
    MNAR_AUC <- c(MNAR_AUC,AUC)
  }

  }

  rocs <- as.data.frame(cbind(MCAR_AUC, MAR_AUC, MNAR_AUC))
  names(rocs) <- c("MCAR", "MAR", "MNAR")
  rocs_forgraph <- gather(rocs, Pattern, AUC, factor_key=TRUE)
  levels(rocs_forgraph$Pattern) <- c("MCAR", "MAR", "MNAR")

  AUCplot <- ggplot(rocs_forgraph, aes(x=Pattern, y=AUC, fill=Pattern)) +
    geom_boxplot() +
    ggtitle("ROC AUC statistics for predicting whether data is available or missing") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="")

  #output list
  list(MCAR_AUC = MCAR_AUC, MAR_AUC = MAR_AUC, MNAR_AUC = MNAR_AUC, Plot = AUCplot)

}




#LAB
