#' @title Missing data imputation with various methods
#'
#' @description
#' \code{\link{impute_data}} imputes a dataframe with missing values with selected algorithm(s)
#'
#' @details
#' This function assumes that the user has performed simulations using the \code{\link{impute_simulated}} function and arrived to
#' some conclusions regarding which functions would be the best performing on their datasets. This function offers a convenient
#' way to impute datasets with a curated list of functions. Some of the functions allow for a multiple imputation framework
#' (they operate with probabilistic models, hence there is uncertainty in the imputed values), so this function allows to generate
#' multiple imputed datasets. The user can decide to impute their dataframe with a selected method or with multiple methods.
#'
#'
#' @param X Dataframe - the original data that contains missing values.
#' @param scale Boolean with default TRUE. Scaling will scale and center all numeric variables to mean = 0 and standard deviation = 1. This is strongly suggested for all PCA-based methods, and for the sake of comparison (and in case all methods are run), for the other methods too. Please note, however, that some methods (e.g. pcaMethods NLPCA, missForest, etc.) are equipped to handle non-linear data. In these cases scaling is up to the user. Factor variables will not be scaled.
#' @param n.iter Number of iterations to perform with default 10. This will only affect the probabilistic methods that allow for a multiple imputation framework. The rest of the methods (if specified to run) will only generate 1 imputed dataframe.
#' @param sel_method Numeric vector that specifies which methods to run. Default is all methods (1-16), but any combinations, including selecting a single method, are allowed. \tabular{ll}{
#' 1 \tab random replacement\cr
#' 2 \tab median imputation\cr
#' 3 \tab mean imputation\cr
#' 4 \tab missMDA Regularized\cr
#' 5 \tab missMDA EM\cr
#' 6 \tab pcaMethods PPCA\cr
#' 7 \tab pcaMethods svdImpute\cr
#' 8 \tab pcaMethods BPCA\cr
#' 9 \tab pcaMethods NIPALS\cr
#' 10 \tab pcaMethods NLPCA\cr
#' 11 \tab mice mixed\cr
#' 12 \tab mi Bayesian\cr
#' 13 \tab Amelia II\cr
#' 14 \tab missForest\cr
#' 15 \tab Hmisc aregImpute\cr
#' 16 \tab VIM kNN\cr
#' }
#'
#'
#' @return
#' A nested list of imputed datasets. In case only a subset of methods was selected the non-selected list elements will be empty.
#' \item{random_replacement}{Imputed dataset using random replacement}
#' \item{mean_imputation}{Imputed dataset using mean imputation}
#' \item{median_imputation}{Imputed dataset using median imputation}
#' \item{missMDA_reg_imputation}{Imputed dataset using the missMDA regularized imputation algorithm}
#' \item{missMDA_EM_imputation}{Imputed dataset using the missMDA EM imputation algorithm}
#' \item{pcaMethods_PPCA_imputation}{Imputed dataset using the pcaMethods PPCA imputation algorithm}
#' \item{pcaMethods_svdImpute_imputation}{Imputed dataset using the pcaMethods svdImpute imputation algorithm}
#' \item{pcaMethods_BPCA_imputation}{Imputed dataset using the pcaMethods BPCA imputation algorithm}
#' \item{pcaMethods_Nipals_imputation}{Imputed dataset using the pcaMethods NIPALS imputation algorithm}
#' \item{pcaMethods_NLPCA_imputation}{Imputed dataset using the pcaMethods NLPCA imputation algorithm}
#' \item{mice_mixed_imputation}{Imputed dataset using the mice mixed imputation algorithm}
#' \item{mi_Bayesian_imputation}{Imputed dataset using the mi Bayesian imputation algorithm}
#' \item{ameliaII_imputation}{Imputed dataset using the Amelia2 imputation algorithm replacement}
#' \item{missForest_imputation}{Imputed dataset using the missForest imputation algorithm replacement}
#' \item{Hmisc_aregImpute_imputation}{Imputed dataset using the Hmisc aregImpute imputation algorithm}
#' \item{VIM_kNN_imputation}{Imputed dataset using the VIM kNN imputation algorithm replacement}
#'
#'
#' @name impute_data
#'
#' @examples
#' \dontrun{
#' #running 10 iterations of all algoritms (that allow for multiple imputation) and
#' #one copy of those that do not allow for multiple imputations
#' impute_data(df, scale = T, n.iter = 10,
#'             sel_method = c(1:16))
#' #running 20 iterations of missForest (e.g. this was the best performing algorithm
#' #in simulations) on a non-scaled dataframe
#' impute_data(df, scale = F, n.iter = 20,
#'             sel_method = c(14))
#' #running 1 iterations of four selected non-probabilistic algorithms on a scaled dataframe
#' impute_data(df, scale = T, n.iter = 1,
#'             sel_method = c(2:3, 5, 7))
#' }
#'
#' @export

## FUNCTION
impute_data <- function(X, scale = T, n.iter = 10, sel_method = c(1:16)) {

  strings_present <- (sum(sapply(X, is.character)) > 0)

  if (strings_present == TRUE) {
    stop("Your data contains string variables. Please inspect your data and either remove these variables or convert them
         into type factor/numeric where applicable.") }

  factors_present <- (sum(sapply(X, is.factor)) > 0)

  if (any(((c(2:10,13) %in% sel_method) & factors_present))) {
    stop("One or more of your selected methods does not allow factors - these methods are mean imputation, median imputation,
          all missMDA and pcaMethods methods and AmeliaII. In case you aim to use any of these, please convert your dataframe to all
          numeric and attempt re-running this.") }

  if (any(((c(4:9) %in% sel_method) & (scale == F)))) {
    warning("One or more of your selected methods is based on PCA - Although your command will run, scaling is strongly recommended.") }


  # optional scaling
  if (factors_present & (scale == T)) {
    ind <- sapply(X, is.numeric)
    X[ind] <- as.data.frame(lapply(X[ind], scale)) } else if ((factors_present == F) & (scale == T)) {
    X <- as.data.frame(scale(X)) }


    # creating empty lists for output
    random_imp_list <- list()
    median_imp_list <- list()
    mean_imp_list <- list()
    missMDA_reg_list <- list()
    missMDA_EM_list <- list()
    pcaMethods_PPCA_list <- list()
    pcaMethods_svdImpute_list <- list()
    pcaMethods_BPCA_list <- list()
    pcaMethods_Nipals_list <- list()
    pcaMethods_NLPCA_list <- list()
    mice_mixed_list <- list()
    mi_Bayesian_list <- list()
    amelia_list <- list()
    missForest_list <- list()
    aregImpute_list <- list()
    kNN_list <- list()

    # 1 = random replacement (run if selected)
    if (1 %in% sel_method) {
        print("random replacement imputation - in progress")
        for (n in 1:n.iter) {
            Y <- X
            for (i in 1:ncol(Y)) {
                Y[, i][is.na(Y[, i])] <- sample(Y[, i][!is.na(Y[, i])], size = sum(is.na(Y[,
                  i])), replace = T)
            }
            random_imp_list[[n]] <- as.data.frame(Y)
        }
    }

    # 2 = median imputation (run if selected)
    if (2 %in% sel_method) {
        print("Median imputation - in progress")
        Y <- X
        for (i in 1:ncol(Y)) {
            Y[is.na(Y[, i]), i] <- stats::median(Y[, i], na.rm = TRUE)
        }
        median_imp_list[[1]] <- as.data.frame(Y)
    }

    # 3 = mean imputation (run if selected)
    if (3 %in% sel_method) {
        print("Mean imputation - in progress")
        Y <- X
        for (i in 1:ncol(Y)) {
            Y[is.na(Y[, i]), i] <- mean(Y[, i], na.rm = TRUE)
        }
        mean_imp_list[[1]] <- as.data.frame(Y)
    }

    # 4 = missMDA Regularized (run if selected)
    if (4 %in% sel_method) {
        print("missMDA regularized imputation - in progress")
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X) - 2)
        log_output <- utils::capture.output(res <- missMDA::imputePCA(X, ncp = ncomp$ncp,
            method = "Regularized"))
        imp_matrix <- res$completeObs
        missMDA_reg_list[[1]] <- as.data.frame(imp_matrix)
    }

    # 5 = missMDA EM (run if selected)
    if (5 %in% sel_method) {
        print("missMDA EM imputation - in progress")
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X) - 2)
        log_output <- utils::capture.output(res <- missMDA::imputePCA(X, ncp = ncomp$ncp,
            method = "EM"))
        imp_matrix <- res$completeObs
        missMDA_EM_list[[1]] <- as.data.frame(imp_matrix)
    }

    # 6 = pcaMethods PPCA (run if selected)
    if (6 %in% sel_method) {
        print("pcaMethods PPCA imputation - in progress")
        for (n in 1:n.iter) {
            ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X) - 2)
            if (ncomp$ncp > 0) {
                log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "ppca",
                  center = FALSE, nPcs = ncomp$ncp))
            } else {
                log_output <- utils::capture.output(res <- pca(X, method = "ppca",
                  center = FALSE, nPcs = 2))
            }
            imp_matrix <- res@completeObs
            pcaMethods_PPCA_list[[n]] <- as.data.frame(imp_matrix)
        }
    }

    # 7 = pcaMethods svdImpute (run if selected)
    if (7 %in% sel_method) {
        print("pcaMethods svdImpute imputation - in progress")
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X) - 2)
        if (ncomp$ncp > 0) {
            log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "svdImpute",
                center = FALSE, nPcs = ncomp$ncp))
        } else {
            log_output <- utils::capture.output(res <- pca(X, method = "svdImpute",
                center = FALSE, nPcs = 2))
        }
        imp_matrix <- res@completeObs
        pcaMethods_svdImpute_list[[1]] <- as.data.frame(imp_matrix)
    }

    # 8 = pcaMethods BPCA (run if selected)
    if (8 %in% sel_method) {
        print("pcaMethods BPCA imputation - in progress")
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X) - 2)
        if (ncomp$ncp > 0) {
            log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "bpca",
                center = FALSE, nPcs = ncomp$ncp))
        } else {
            log_output <- utils::capture.output(res <- pca(X, method = "bpca", center = FALSE,
                nPcs = 2))
        }
        imp_matrix <- res@completeObs
        pcaMethods_BPCA_list[[1]] <- as.data.frame(imp_matrix)
    }

    # 9 = pcaMethods NIPALS (run if selected)
    if (9 %in% sel_method) {
        print("pcaMethods NIPALS imputation - in progress")
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X) - 2)
        if (ncomp$ncp > 0) {
            log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "nipals",
                center = FALSE, nPcs = ncomp$ncp))
        } else {
            log_output <- utils::capture.output(res <- pca(X, method = "nipals", center = FALSE,
                nPcs = 2))
        }
        imp_matrix <- res@completeObs
        pcaMethods_Nipals_list[[1]] <- as.data.frame(imp_matrix)
    }

    # 10 = pcaMethods NLPCA (run if selected)
    if (10 %in% sel_method) {
        print("pcaMethods NLPCA imputation - in progress")
        for (n in 1:n.iter) {
            ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X) - 2)
            if (ncomp$ncp > 0) {
                log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "nlpca",
                  center = FALSE, nPcs = ncomp$ncp, maxSteps = 100))
            } else {
                log_output <- utils::capture.output(res <- pca(X, method = "nlpca",
                  center = FALSE, nPcs = 2, maxSteps = 100))
            }
            imp_matrix <- res@completeObs
            pcaMethods_NLPCA_list[[n]] <- as.data.frame(imp_matrix)
        }
    }

    # 11 = mice mixed (run if selected)
    if (11 %in% sel_method) {
        print("mice mixed imputation - in progress")
        for (n in 1:n.iter) {
            log_output <- utils::capture.output(imputed_Data <- mice::mice(X, m = 1,
                maxit = 100))
            mice_mixed_list[[n]] <- mice::complete(imputed_Data, 1)
        }
    }

    # 12 = mi Bayesian (run if selected)
    if (12 %in% sel_method) {
        print("mi imputation - in progress")
        for (n in 1:n.iter) {
            log_output <- utils::capture.output(mi_data <- mi::mi(as.data.frame(X),
                n.chain = 1, n.iter = 100, verbose = FALSE))
            imputed <- mi::complete(mi_data, 1)
            mi_Bayesian_list[[n]] <- as.data.frame(imputed[, 1:ncol(X)])
        }
    }

    # 13 = Amelia II (run if selected)
    if (13 %in% sel_method) {
        print("Amelia II imputation - in progress")
        for (n in 1:n.iter) {
            log_output <- utils::capture.output(amelia_fit <- Amelia::amelia(X, m = 1))
            imp_matrix <- amelia_fit$imputations[[1]]
            amelia_list[[n]] <- as.data.frame(imp_matrix)
        }
    }

    # 14 = missForest (run if selected)
    if (14 %in% sel_method) {
        print("missForest imputation - in progress")
        for (n in 1:n.iter) {
            log_output <- utils::capture.output(results <- missForest::missForest(X,
                maxiter = 10, ntree = 100, replace = T))
            imp_matrix <- results$ximp
            missForest_list[[n]] <- as.data.frame(imp_matrix)
        }
    }

    # 15 = Hmisc aregImpute (run if selected)
    if (15 %in% sel_method) {
        print("Hmisc aregImpute imputation - in progress")
        for (n in 1:n.iter) {
          Xcolnames <- colnames(X)
          Xformula <- stats::as.formula(paste("~", paste(Xcolnames, collapse = "+")))
          log_output <- utils::capture.output(hmisc_algo <- Hmisc::aregImpute(formula = Xformula,
                                                                              data = X, n.impute = 1, burnin = 5, nk = 0, type = "pmm", pmmtype = 2))
          completeData <- as.data.frame(Hmisc::impute.transcan(hmisc_algo, imputation = 1,
                                                               data = X, list.out = TRUE, pr = FALSE, check = FALSE))
          aregImpute_list[[n]] <- as.data.frame(completeData)
        }
    }

    # 16 = VIM kNN (run if selected)
    if (16 %in% sel_method) {
        print("VIM kNN imputation - in progress")
        Xcolnames <- colnames(X)
        log_output <- utils::capture.output(completeData <- VIM::kNN(data = X, variable = Xcolnames,
            k = 10, trace = F, imp_var = F))
        kNN_list[[1]] <- as.data.frame(completeData)
    }

    list(random_replacement = random_imp_list, mean_imputation = mean_imp_list, median_imputation = median_imp_list,
        missMDA_reg_imputation = missMDA_reg_list, missMDA_EM_imputation = missMDA_EM_list,
        pcaMethods_PPCA_imputation = pcaMethods_PPCA_list, pcaMethods_svdImpute_imputation = pcaMethods_svdImpute_list,
        pcaMethods_BPCA_imputation = pcaMethods_BPCA_list, pcaMethods_Nipals_imputation = pcaMethods_Nipals_list,
        pcaMethods_NLPCA_imputation = pcaMethods_NLPCA_list, mice_mixed_imputation = mice_mixed_list,
        mi_Bayesian_imputation = mi_Bayesian_list, ameliaII_imputation = amelia_list,
        missForest_imputation = missForest_list, Hmisc_aregImpute_imputation = aregImpute_list,
        VIM_kNN_imputation = kNN_list)

}


