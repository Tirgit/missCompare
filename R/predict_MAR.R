#' @title Prediction of MAR missing data pattern based on available data
#'
#' @description
#' \code{\link{predict_MAR}} predict missingness status (missing vs. non-missing) based on other variables.
#'
#' @details
#' This function aims to predict whether a feature approximates a missing-at-random (MAR)
#' missingness pattern. The function iterates over all variables by converting it to a binary
#' variable by missing/non-missing status. Following this transformation, all other variables
#' are used to establish a prediction model (using GLM). ROC AUC statistics are extracted
#' to establish the collective predictive accuracy of all remaining variables in relation to
#' the transformed binary feature in question.
#'
#' @param X Dataframe - the original data that contains missing values.
#' @param scale Boolean with default TRUE. Scaling will scale and center all variables to mean=0 and standard deviation=1
#'
#' @name predict_MAR
#'
#' @return
#' \item{MAR_stats}{ROC AUC values and 95\% CIs for each variable in the dataset. The ROC AUC statistics are established using all other variables.}
#' \item{Plot}{Plot visualizing ROC AUC values and 95\% CIs for each variable in the dataset.}
#'
#' @examples
#' \dontrun{
#' predicted <- predict_MAR(X=df)
#' }
#'
#' @export


# FUNCTION
predict_MAR <- function(X, scale = T) {
    
    # optional scaling
    if (scale == T) {
        df_miss <- as.data.frame(scale(df_miss))
    } else {
        df_miss <- as.data.frame(df_miss)
    }
    
    cols_with_miss <- which(colSums(is.na(X)) > 0)
    MAR_AUC <- data.frame()
    
    for (i in cols_with_miss) {
        complete <- X[stats::complete.cases(X[, -i]), ]
        print(paste(nrow(complete), "complete sets of observations to predict MAR pattern in variable", 
            colnames(X)[i], sep = " "))
        
        # 0 if available data
        outcome <- rep(0, nrow(complete))
        # 1 if missing
        outcome[is.na(complete[, i])] <- 1
        complete[, i] <- as.factor(outcome)
        # predict using logistic regression using the rest of the variables, obtain AUC ROC
        formula <- as.formula(paste(colnames(X)[i], "~ .", sep = " "))
        mylogit <- stats::glm(formula, data = complete, family = "binomial")
        prob <- stats::predict(mylogit, type = c("response"))
        # default 2000 bootstraps
        AUC <- pROC::roc(outcome ~ prob, auc = TRUE, ci = TRUE)
        MAR_AUC <- rbind(MAR_AUC, c(AUC$ci[2], AUC$ci[1], AUC$ci[3]))
        
    }
    
    MAR_AUC <- cbind(names(cols_with_miss), MAR_AUC)
    colnames(MAR_AUC) <- c("Variable", "AUC", "AUC_loci", "AUC_hici")
    
    # plot of AUC values and 95% CIs per variable
    AUC_loci <- AUC_hici <- Variable <- NULL
    AUCplot <- ggplot(data = MAR_AUC, aes(x = Variable, y = AUC)) + geom_boxplot() + 
        geom_errorbar(aes(ymin = AUC_loci, ymax = AUC_hici)) + ggtitle("ROC AUC statistics for predicting MAR pattern") + 
        theme(plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(0, 
        1))
    
    # output list
    list(MAR_stats = MAR_AUC, Plot = AUCplot)
    
}
