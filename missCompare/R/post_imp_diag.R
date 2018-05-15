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
#' @name post_imp_diag
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


post_imp_diag <- function(X_orig, X_imp, scale=T) {

  #warning if dimensions are unequal
  if (!identical(dim(X_orig), dim(X_imp))) stop(paste("Warning! The dimensions of the original and imputed dataframes are unequal!", sep= ""))

  X_orig <- as.data.frame(X_orig)
  X_imp <- as.data.frame(X_imp)

  #optional scaling
  if (scale == T) {
    X_orig <- as.data.frame(scale(X_orig))
  }

  histograms <- list()
  boxplots <- list()

  for (i in 1:ncol(X_orig)) {

    col_index <- is.na(X_orig[,i])

    orig_values <- X_orig[,i][!col_index]
    imp_values <- X_imp[,i][col_index]

    origvec <- rep("Original values", length(orig_values))
    impvec <- rep("Imputed values", length(imp_values))
    vals <- data.frame(cbind(c(orig_values, imp_values), c(origvec, impvec)))
    vals$X1 <- as.numeric(as.character(vals$X1))
    colnames(vals) <- c(colnames(X_orig)[i], "Data")

    p <- ggplot(data = data.frame(x = vals[,1], y = vals[,2])
                , aes(x, fill=y)) +
      geom_density(alpha=0.5) +
      ggtitle("Overlaid density plot of original values and imputed values") +
      labs(x=colnames(X_orig)[i]) +
      guides(fill=guide_legend(title=""))

    q <- ggplot(data = data.frame(x = vals[,2], y = vals[,1])
                , aes(x = x, y = y)) +
      geom_boxplot() +
      stat_summary(fun.y=mean, geom="point", shape=3, size=5) +
      ggtitle("Boxplots of original values and imputed values") +
      labs(x=colnames(X_orig)[i], y="")

    pltName <- colnames(X_orig)[i]
    histograms[[pltName]] <- p
    boxplots[[pltName]] <- q

  }

  #output
  list(Densityplots = histograms, Boxplots = boxplots)

}



