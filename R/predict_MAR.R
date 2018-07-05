#' @title Prediction of missing data based on available data
#'
#' @description
#' \code{\link{predict_MAR}} predict missingness status (missing vs. non-missing) based on other variables.
#'
#' @details
#' This function xxxx
#'
#' @param rownum Number of rows (samples) in the original dataframe (Rows output from the \code{\link{get_data}} function)
#'
#' @name predict_MAR
#'
#' @return
#' \item{MCAR_AUC}{xxxxx}
#' \item{MAR_AUC}{xxxxx}
#' \item{MNAR_AUC}{xxxxx}
#' \item{Plot}{xxxxxx}
#'
#' @examples
#' \dontrun{
#' xxxx
#' }
#'
#' @export


#FUNCTION
predict_MAR <- function(X) {

  X <- as.data.frame(X)

  cols_with_miss <- which(colSums(is.na(X))>0)
  MAR_AUC <- data.frame()

  for (i in cols_with_miss) {
    complete <- X[stats::complete.cases(X[,-i]),]
    print(paste(nrow(complete), "complete sets of observations to predict MAR pattern in variable", colnames(X)[i], sep=" "))

    # 0 if available data
    outcome <- rep(0, nrow(complete))
    # 1 if missing
    outcome[is.na(complete[,i])] <- 1
    complete[,i] <- as.factor(outcome)
    #predict using logistic regression using the rest of the variables, obtain AUC ROC
    formula <- as.formula(paste(colnames(X)[i], "~ .", sep=" "))
    mylogit <- stats::glm(formula, data = complete, family = "binomial")
    prob <- stats::predict(mylogit,type=c("response"))
    AUC <- pROC::roc(outcome ~ prob, auc=TRUE, ci=TRUE)
    MAR_AUC <- rbind(MAR_AUC, c(AUC$ci[2], AUC$ci[1], AUC$ci[3]))

  }

  MAR_AUC <- cbind(names(cols_with_miss),MAR_AUC)
  colnames(MAR_AUC) <- c("Variable","AUC","AUC_loci","AUC_hici")

  #plot of AUC values and 95% CIs per variable
  AUCplot <- ggplot(data=MAR_AUC, aes(x=Variable, y=AUC)) +
    geom_boxplot() +
    geom_errorbar(aes(ymin=AUC_loci, ymax=AUC_hici)) +
    ggtitle("ROC AUC statistics for predicting MAR pattern") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim = c(0, 1))

  #output list
  list(MAR_stats = MAR_AUC, Plot = AUCplot)

}
