#' @title Post imputation diagnostics
#'
#' @description
#' \code{\link{post_imp_diag}} serves as post imputation diagnostics. The function compares the original dataset (with missing data) with the imputed dataset. The function outputs visualizations that will help the user compare the distributions of the original values and the imputed values.
#'
#' @details
#' This function uses the original dataframe and extracts descriptive meta-data.
#'
#' @param X_orig Dataframe - the original data that contains missing values.
#' @param X_imp Dataframe - the imputed data with no missing values.
#' @param scale Boolean with default TRUE. Scaling will scale and center all variables to mean=0 and standard deviation=1 in the \strong{original dataframe with missingness}. The user should select TRUE or FALSE here depending on whether the imputed dataframe has scaled or unscaled values.
#'
#' @name predict_missing
#'
#' @return
#' \item{Densityplots}{List of density plots of all variables. The density plots show the original values and the imputed values overlaid for each variables in the dataframe}
#' \item{Boxplot}{List of boxplots of all variables. The boxplots show the original values and the imputed values for each variables in the dataframe. As normally, the boxplots show the median values, the IQR and the range of values}
#'
#' @examples
#' \dontrun{
#' plots <- post_imp_diag(X_orig = df_miss, X_imp = df_imputed, scale=T)
#' plots$Densityplots$variable_X
#' plots$Boxplots$variable_Z
#' }
#'
#' @export


#given available data predicts missing data (diganostics for MAR)
#first convert missing vs available data to 0/1
#set up logistic regression model and random forest to predict 0/1 outcome


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
