#' @title Post imputation diagnostics
#'
#' @description
#' \code{\link{post_imp_diag}} serves as post imputation diagnostics. The function compares the original dataset (with missing data) with the imputed dataset. The function outputs statistics and visualizations that will help the user compare the distributions of the original values and the imputed values and potential differences between correlation statistics before and after imputation.
#'
#' @details
#' This function uses the original dataframe and produces plots that allows the user
#' to compare the distributions of the original values and the imputed values for each
#' numeric variables. If there are factors present in the dataframes, the function will
#' recognize this and create barcharts for these categorical variables. In addition, the
#' function will calculate bootstrapped pairwise Pearson correlation coefficients between
#' numeric variables in the original dataframe (with missingness) and the imputed dataframe
#' and plot these for the user to assess whether the imputation distorted the original
#' data structure or not. The function will also visualize clusters of the variables from
#' the original dataframe and the imputed one. Should the imputation algorithm perform well,
#' the variable distributions and the variable clusters should be similar.
#'
#'
#' @param X_orig Dataframe - the original data that contains missing values.
#' @param X_imp Dataframe - the imputed data with no missing values.
#' @param scale Boolean with default TRUE. Scaling will scale and center all variables to mean=0 and standard deviation=1 in the \strong{original dataframe with missingness}. The user should select TRUE or FALSE here depending on whether the imputed dataframe has scaled or unscaled values.
#' @param n.boot Number of bootstrap iterations to generate mean pairwise Pearson correlation coefficients and 95\% confidence intervals for variable pairs from the original and the imputed dataframes.
#'
#' @name post_imp_diag
#'
#' @return
#' \item{Histograms}{List of density plots of all numeric variables. The density plots show the original values and the imputed values overlaid for each variables in the dataframe}
#' \item{Boxplots}{List of boxplots of all numeric variables. The boxplots show the original values and the imputed values for each variables in the dataframe. As normally, the boxplots show the median values, the IQR and the range of values}
#' \item{Barcharts}{List of bar charts of all categorical (factor) variables. The bar charts show the original categories and the imputed categories for each categorical variables in the dataframe. Barcharts will only be output if scale is set to FALSE and both the original and imputed data contain the same factor variables}
#' \item{Statistics}{List of output statistics for all variables. A named vector containing means and standard deviations of the original and imputed values, P value from Welch's t test and D test statistic from a Kolmogorov–Smirnov test}
#' \item{Variable_clusters_orig}{Variable clusters based on the original dataframe (with missingness). Regardless of the argument scale being set to TRUE or FALSE, the clusters are assessed based on normalized data}
#' \item{Variable_clusters_imp}{Variable clusters based on the imputed dataframe. Regardless of the argument scale being set to TRUE or FALSE, the clusters are assessed based on normalized data}
#' \item{Correlation_stats}{Mean pairwise Pearson's correlation coefficients and 95\% confidence intervals from the original dataframe (with missingness) and the imputed dataframe}
#' \item{Correlation_plot}{Scatter plot of mean pairwise Pearson's correlation coefficients from the original dataframe (with missingness) and the imputed dataframe. The blue line represents a line with slope 1 and intercept 0. The red line is a fitted line of the correlation coefficient pairs. The error bars around the points represent the individual 95\% confidence intervals drawn from bootstrapping the correlation coefficients}
#'
#' @examples
#' \dontrun{
#' diagnostics <- post_imp_diag(X_orig = df_miss, X_imp = df_imputed, scale=T)
#' diagnostics$Histograms$variable_X
#' diagnostics$Boxplots$variable_Z
#' diagnostics$Statistics$variable_Y
#' }
#'
#' @export

post_imp_diag <- function(X_orig, X_imp, scale = T, n.boot = 100) {

  # warning if dimensions are unequal
  if (!identical(dim(X_orig), dim(X_imp)))
    stop(paste("Warning! The dimensions of the original and imputed dataframes are unequal!",
               sep = ""))

  X_orig <- as.data.frame(X_orig)
  X_imp <- as.data.frame(X_imp)

  # optional scaling
  if (scale == T) {
    X_orig <- as.data.frame(scale(X_orig))
  }

  histograms <- list()
  boxplots <- list()
  statistics <- list()
  barcharts <- list()

  factors_present <- sum(!sapply(X_orig, is.numeric)) > 0

  X_orig_num <- X_orig[sapply(X_orig, is.numeric)]
  if (factors_present == T) {
    X_orig_factor <- X_orig[!sapply(X_orig, is.numeric)]
  }
  X_imp_num <- X_imp[sapply(X_imp, is.numeric)]
  if (factors_present == T) {
    X_imp_factor <- X_imp[!sapply(X_imp, is.numeric)]
  }

  x <- NULL
  # barcharts for categorical (factor) variables
  if (factors_present == T) {
    for (i in 1:ncol(X_orig_factor)) {

      pltName <- colnames(X_orig_factor)[i]

      col_index <- is.na(X_orig_factor[, i])

      orig_values <- X_orig_factor[, i][!col_index]
      imp_values <- X_imp_factor[, i][col_index]

      origvec <- rep("Original values", length(orig_values))
      impvec <- rep("Imputed values", length(imp_values))
      vals <- data.frame(cbind(c(orig_values, imp_values), c(origvec, impvec)))
      vals$X1 <- as.numeric(as.character(vals$X1))
      colnames(vals) <- c(colnames(X_orig_factor)[i], "Data")

      q <- ggplot(data = data.frame(x = vals[, 1], y = vals[, 2]), aes(x = factor(y),
                                                                       fill = factor(x))) + geom_bar(position = "fill") + ggtitle("Bar chart of original values and imputed values") +
        labs(y = "Proportion", x = colnames(X_orig_factor)[i]) + guides(fill = guide_legend(title = ""))

      barcharts[[pltName]] <- q

    }
  }

  # cluster plot comparison - always scaled, irrespective of scale being TRUE or FALSE
  X_orig_mat <- as.matrix(as.data.frame(scale(X_orig_num)))
  X_imp_mat <- as.matrix(as.data.frame(scale(X_imp_num)))
  colnames(X_orig_mat) <- colnames(X_orig_num)
  colnames(X_imp_mat) <- colnames(X_imp_num)
  X_orig_dendro <- stats::as.dendrogram(stats::hclust(stats::dist(t(X_orig_mat))))
  X_imp_dendro <- stats::as.dendrogram(stats::hclust(stats::dist(t(X_imp_mat))))

  X_orig_dendro_plot <- ggdendro::ggdendrogram(data = X_orig_dendro, rotate = TRUE) +
    labs(title = "Original dataframe - variable clusters") + theme(plot.title = element_text(hjust = 0.5))

  X_imp_dendro_plot <- ggdendro::ggdendrogram(data = X_imp_dendro, rotate = TRUE) +
    labs(title = "Imputed dataframe - variable clusters") + theme(plot.title = element_text(hjust = 0.5))

  # histograms and boxplots for numeric variables
  for (i in 1:ncol(X_orig_num)) {

    pltName <- colnames(X_orig_num)[i]

    col_index <- is.na(X_orig_num[, i])

    orig_values <- X_orig_num[, i][!col_index]
    imp_values <- X_imp_num[, i][col_index]

    if (length(imp_values) != 0) {
      tstats <- stats::t.test(orig_values, imp_values, alternative = "two.sided",
                              var.equal = FALSE)
      ksstats <- stats::ks.test(orig_values, imp_values, exact=TRUE)$statistic
      statistics[[pltName]] <- c(Mean_original = mean(orig_values), SD_original = stats::sd(orig_values),
                                 Mean_imputed = mean(imp_values), SD_imputed = stats::sd(imp_values),
                                 Welch_ttest_P = tstats$p.value, KS_test = ksstats)

      origvec <- rep("Original values", length(orig_values))
      impvec <- rep("Imputed values", length(imp_values))
      vals <- data.frame(cbind(c(orig_values, imp_values), c(origvec, impvec)))
      vals$X1 <- as.numeric(as.character(vals$X1))
      colnames(vals) <- c(colnames(X_orig_num)[i], "Data")

      p <- ggplot(data = data.frame(x = vals[, 1], y = vals[, 2]), aes(x, fill=y)) +
        geom_histogram(alpha = 0.5, binwidth = 0.5, position="identity") + ggtitle("Overlaid density plot of original values and imputed values") +
        labs(x = colnames(X_orig_num)[i]) + guides(fill = guide_legend(title = "")) +
        theme(plot.title = element_text(hjust = 0.5))

      q <- ggplot(data = data.frame(x = vals[, 2], y = vals[, 1]), aes(x = x, y = y)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point", shape = 3,
                                      size = 5) + ggtitle("Boxplots of original values and imputed values") +
        labs(x = colnames(X_orig_num)[i], y = "") + theme(plot.title = element_text(hjust = 0.5))

      histograms[[pltName]] <- p
      boxplots[[pltName]] <- q

    } else { # in case there are no missing values in a variable

      statistics[[pltName]] <- c(Mean_original = mean(orig_values), SD_original = stats::sd(orig_values),
                                 Mean_imputed = NA, SD_imputed = NA, Welch_ttest_P = NA, KS_test = NA)

      origvec <- rep("Original values", length(orig_values))
      impvec <- rep("Imputed values", length(imp_values))
      vals <- data.frame(cbind(c(orig_values, imp_values), c(origvec, impvec)))
      vals$X1 <- as.numeric(as.character(vals$X1))
      colnames(vals) <- c(colnames(X_orig_num)[i], "Data")

      p <- ggplot(data = data.frame(x = vals[, 1], y = vals[, 2]), aes(x, fill=y)) +
        geom_histogram(alpha = 0.5, binwidth = 0.5, position="identity") + ggtitle("Histogram of original values") +
        labs(x = colnames(X_orig_num)[i]) + guides(fill = guide_legend(title = "")) +
        theme(plot.title = element_text(hjust = 0.5))

      q <- ggplot(data = data.frame(x = vals[, 2], y = vals[, 1]), aes(x = x, y = y)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point", shape = 3,
                                      size = 5) + ggtitle("Boxplot of original values") +
        labs(x = colnames(X_orig_num)[i], y = "") + theme(plot.title = element_text(hjust = 0.5))

      histograms[[pltName]] <- p
      boxplots[[pltName]] <- q

    }

  }

  corrs <- NULL
  meanmat <- matrix(nrow = ncol(X_orig_num), ncol = ncol(X_orig_num))
  locimat <- matrix(nrow = ncol(X_orig_num), ncol = ncol(X_orig_num))
  hicimat <- matrix(nrow = ncol(X_orig_num), ncol = ncol(X_orig_num))


  for (i in 1:ncol(X_orig_num)) {
    for (j in 1:ncol(X_orig_num)) {
      for (b in 1:n.boot) {
        idx <- sample.int(nrow(X_orig_num), nrow(X_orig_num), replace = TRUE)
        corrs[b] <- cor(X_orig_num[idx, ], use = "pairwise.complete.obs", method = "pearson")[i,
                                                                                              j]
      }
      meanmat[i, j] <- mean(corrs)
      locimat[i, j] <- quantile(corrs, c(0.025))
      hicimat[i, j] <- quantile(corrs, c(0.975))
      corrs <- NULL
    }
  }

  var1 <- NULL
  meanmat[lower.tri(meanmat, diag = T)] <- NA
  y <- as.data.frame(meanmat)
  colnames(y) <- colnames(X_orig_num)
  rownames(y) <- colnames(X_orig_num)
  y$var1 <- row.names(y)
  z <- gather(data = y, key = "var2", value = "value", -var1)
  correlation_results <- z[!is.na(z$value), ]

  locimat[lower.tri(locimat, diag = T)] <- NA
  y <- as.data.frame(locimat)
  colnames(y) <- colnames(X_orig_num)
  rownames(y) <- colnames(X_orig_num)
  y$var1 <- row.names(y)
  z <- gather(data = y, key = "var2", value = "value", -var1)
  z <- z[!is.na(z$value), ]
  correlation_results <- cbind(correlation_results, z$value)

  hicimat[lower.tri(hicimat, diag = T)] <- NA
  y <- as.data.frame(hicimat)
  colnames(y) <- colnames(X_orig_num)
  rownames(y) <- colnames(X_orig_num)
  y$var1 <- row.names(y)
  z <- gather(data = y, key = "var2", value = "value", -var1)
  z <- z[!is.na(z$value), ]
  correlation_results <- cbind(correlation_results, z$value)

  meanmat <- matrix(nrow = ncol(X_imp_num), ncol = ncol(X_imp_num))
  locimat <- matrix(nrow = ncol(X_imp_num), ncol = ncol(X_imp_num))
  hicimat <- matrix(nrow = ncol(X_imp_num), ncol = ncol(X_imp_num))

  for (i in 1:ncol(X_imp_num)) {
    for (j in 1:ncol(X_imp_num)) {
      for (b in 1:n.boot) {
        idx <- sample.int(nrow(X_imp_num), nrow(X_imp_num), replace = TRUE)
        corrs[b] <- cor(X_imp_num[idx, ], use = "pairwise.complete.obs", method = "pearson")[i,
                                                                                             j]
      }
      meanmat[i, j] <- mean(corrs)
      locimat[i, j] <- quantile(corrs, c(0.025))
      hicimat[i, j] <- quantile(corrs, c(0.975))
    }
  }

  meanmat[lower.tri(meanmat, diag = T)] <- NA
  y <- as.data.frame(meanmat)
  colnames(y) <- colnames(X_imp_num)
  rownames(y) <- colnames(X_imp_num)
  y$var1 <- row.names(y)
  z <- gather(data = y, key = "var2", value = "value", -var1)
  z <- z[!is.na(z$value), ]
  correlation_results <- cbind(correlation_results, z$value)

  locimat[lower.tri(locimat, diag = T)] <- NA
  y <- as.data.frame(locimat)
  colnames(y) <- colnames(X_imp_num)
  rownames(y) <- colnames(X_imp_num)
  y$var1 <- row.names(y)
  z <- gather(data = y, key = "var2", value = "value", -var1)
  z <- z[!is.na(z$value), ]
  correlation_results <- cbind(correlation_results, z$value)

  hicimat[lower.tri(hicimat, diag = T)] <- NA
  y <- as.data.frame(hicimat)
  colnames(y) <- colnames(X_imp_num)
  rownames(y) <- colnames(X_imp_num)
  y$var1 <- row.names(y)
  z <- gather(data = y, key = "var2", value = "value", -var1)
  z <- z[!is.na(z$value), ]
  correlation_results <- cbind(correlation_results, z$value)

  colnames(correlation_results) <- c("var1", "var2", "orig.cor", "orig.lo.ci", "orig.hi.ci",
                                     "imp.cor", "imp.lo.ci", "imp.hi.ci")

  imp.lo.ci <- imp.hi.ci <- orig.lo.ci <- orig.hi.ci <- orig.cor <- imp.cor <- NULL
  p <- ggplot(data = correlation_results, aes(x = orig.cor, y = imp.cor)) + geom_point(alpha = 0.2) +
    geom_errorbar(aes(ymin = imp.lo.ci, ymax = imp.hi.ci), alpha = 0.1) + geom_errorbarh(aes(xmin = orig.lo.ci,
                                                                                             xmax = orig.hi.ci), alpha = 0.1) + geom_abline(slope = 1, intercept = 0, col = "blue",
                                                                                                                                            alpha = 0.5) + geom_line(stat = "smooth", method = "lm", col = "red", alpha = 0.5) +
    ggtitle("Scatter plot of correlation coefficients") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = "Correlation coefficient (original)", y = "Correlation coefficient (imputed)")

  # output
  list(Histograms = histograms, Boxplots = boxplots, Barcharts = barcharts, Statistics = statistics,
       Variable_clusters_orig = X_orig_dendro_plot, Variable_clusters_imp = X_imp_dendro_plot,
       Correlation_stats = correlation_results, Correlation_plot = p)

}
