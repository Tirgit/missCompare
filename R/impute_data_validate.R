#' @title Missing data imputation with various methods
#'
#' @description
#' \code{\link{impute_data_validate}} imputes a dataframe with missing values with selected algorithm(s) using a validation framwork
#'
#' @details
#' This function assumes that the user has performed simulations using the \code{\link{impute_simulated}} function and arrived to
#' some conclusions regarding which functions would be the best performing on their datasets. This function offers a convenient
#' way to validate this on the original dataset. This function differs from the \code{\link{impute_data}} function as here a very small
#' amount of extra missing data is spike in and the imputation performance is tested on this small fraction, whereas the \code{\link{impute_data}}
#' function does not allow for this validation step. Therefore, it is suggested that the \code{\link{impute_data_validate}} is used before undertaking
#' the data imputation using \code{\link{impute_data}}. The additional missing values (1\% as default) will be indexed and RMSE will be calculated
#' using the original datapoints and the indexed imputed values. The spike.in parameter spikes in additional missing values in a random fashion
#' (the assumption here is that a very small amount of extra missing values will not distort the missingness pattern). The user can specify whether
#' all methods or only a subset of methods will be included in this validation step.
#'
#' @param X Dataframe - the original data that contains missing values.
#' @param scale Boolean with default TRUE. Scaling will scale and center all variables to mean=0 and standard deviation=1. This is strongly suggested for all PCA-based methods, and for the sake of comparison (and in the case when all methods are run), for the other methods too. Please note, however, that some methods (e.g. pcaMethods NLPCA, missForest, etc.) are equipped to handle non-linear data. In these cases scaling is up to the user.
#' @param spike.in Numeric value with 0.01 (or 1\%) as default. This parameter controls the additional missing data spike-in.
#' @param n.iter Number of iterations to perform with default 10. This will only affect the probabilistic methods that allow for a multiple imputation framwork. The rest of the methods (if specified to run) will only generate 1 imputed dataframe.
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
#' A nested list of imputed datasets. In case only a subset of methods was selected the not-selected list elements will be empty.
#' \item{random_replacement}{Imputed dataset using random replacement}
#' \item{mean_imputation}{Imputed dataset using mean imputation}
#' \item{median_imputation}{Imputed dataset using median imputation}
#' \item{missMDA_reg_imputation}{Imputed dataset using the missMDA regularized imputation algorithm}
#' \item{missMDA_EM_imputation}{Imputed dataset using the missMDA EM imputation algorithm}
#' \item{pcaMethods_PPCA_imputation}{Imputed dataset using the pcaMethods PPCA imputation algorithm}
#' \item{pcaMethods_svdImpute_imputation}{Imputed dataset using the pcaMethods svdImpute imputation algorithm}
#' \item{pcaMethods_BPCA_imputation}{Imputed dataset using the pcaMethods BPCA imputation algorithm}
#' \item{pcaMethods_Nipals_imputation}{Imputed dataset using the pcaMethods Nipals imputation algorithm}
#' \item{pcaMethods_NLPCA_imputation}{Imputed dataset using the pcaMethods NLPCA imputation algorithm}
#' \item{mice_mixed_imputation}{Imputed dataset using the mice mixed imputation algorithm}
#' \item{mi_Bayesian_imputation}{Imputed dataset using the mi Bayesian imputation algorithm}
#' \item{ameliaII_imputation}{Imputed dataset using the Amelia2 imputation algorithm replacement}
#' \item{missForest_imputation}{Imputed dataset using the missForest imputation algorithm replacement}
#' \item{Hmisc_aregImpute_imputation}{Imputed dataset using the Hmisc aregImpute imputation algorithm}
#' \item{VIM_kNN_imputation}{Imputed dataset using the VIM kNN imputation algorithm replacement}
#'
#'
#' @name impute_data_validate
#'
#' @examples
#' \dontrun{
#' #running 10 iterations of all algoritms (that allow for multiple imputation) and
#' #one copy of those that do not allow for multiple imputations with 1% spike-in for validation
#' impute_data_validate(df, scale = T, spike.in = 0.01,
#'                      n.iter = 10, sel_method = c(1:16))
#' #running 5 iterations of pcaMethods NLPCA and missForest (e.g. these two were
#' #the best performing algorithms in simulations) on a non-scaled dataframe
#' #with 1% spike-in for validation
#' res <- impute_data_validate(df, scale = F, spike.in = 0.01,
#'                             n.iter = 5, sel_method = c(10,16))
#' }
#'
#' @export


## FUNCTION
impute_data_validate <- function(X, scale = T, spike.in = 0.01, n.iter = 10, sel_method = c(1:16)) {

    # optional scaling
    if (scale == T) {
        X <- as.data.frame(scale(X))
    }

    # spike in an additional small amount of missing values
    X_orig <- X
    X <- missForest::prodNA(X, spike.in)
    index_orig <- sapply(X_orig, is.na)
    index_extra <- sapply(X, is.na)
    index_spikein <- (index_orig != index_extra)

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
        print("Random replacement imputation - in progress")
        for (n in 1:n.iter) {
            Y <- X
            for (i in 1:ncol(Y)) {
                Y[, i][is.na(Y[, i])] <- sample(Y[, i][!is.na(Y[, i])], size = sum(is.na(Y[, i])),
                  replace = T)
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
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X)-1)
        log_output <- utils::capture.output(res <- missMDA::imputePCA(X, ncp = ncomp$ncp, method = "Regularized"))
        imp_matrix <- res$completeObs
        missMDA_reg_list[[1]] <- as.data.frame(imp_matrix)
    }

    # 5 = missMDA EM (run if selected)
    if (5 %in% sel_method) {
        print("missMDA EM imputation - in progress")
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X)-1)
        log_output <- utils::capture.output(res <- missMDA::imputePCA(X, ncp = ncomp$ncp, method = "EM"))
        imp_matrix <- res$completeObs
        missMDA_EM_list[[1]] <- as.data.frame(imp_matrix)
    }

    # 6 = pcaMethods PPCA (run if selected)
    if (6 %in% sel_method) {
        print("pcaMethods PPCA imputation - in progress")
        for (n in 1:n.iter) {
            ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X)-1)
            if (ncomp$ncp > 0) {
                log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "ppca",
                  center = FALSE, nPcs = ncomp$ncp))
            } else {
                log_output <- utils::capture.output(res <- pca(X, method = "ppca", center = FALSE,
                  nPcs = 2))
            }
            imp_matrix <- res@completeObs
            pcaMethods_PPCA_list[[n]] <- as.data.frame(imp_matrix)
        }
    }

    # 7 = pcaMethods svdImpute (run if selected)
    if (7 %in% sel_method) {
        print("pcaMethods svdImpute imputation - in progress")
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X)-1)
        if (ncomp$ncp > 0) {
            log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "svdImpute",
                center = FALSE, nPcs = ncomp$ncp))
        } else {
            log_output <- utils::capture.output(res <- pca(X, method = "svdImpute", center = FALSE,
                nPcs = 2))
        }
        imp_matrix <- res@completeObs
        pcaMethods_svdImpute_list[[1]] <- as.data.frame(imp_matrix)
    }

    # 8 = pcaMethods BPCA (run if selected)
    if (8 %in% sel_method) {
        print("pcaMethods BPCA imputation - in progress")
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X)-1)
        if (ncomp$ncp > 0) {
            log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "bpca", center = FALSE,
                nPcs = ncomp$ncp))
        } else {
            log_output <- utils::capture.output(res <- pca(X, method = "bpca", center = FALSE,
                nPcs = 2))
        }
        imp_matrix <- res@completeObs
        pcaMethods_BPCA_list[[1]] <- as.data.frame(imp_matrix)
    }

    # 9 = pcaMethods NIPALS (run if selected)
    if (9 %in% sel_method) {
        print("pcaMethods Nipals imputation - in progress")
        ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X)-1)
        if (ncomp$ncp > 0) {
            log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "nipals", center = FALSE,
                nPcs = ncomp$ncp))
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
            ncomp <- missMDA::estim_ncpPCA(X, ncp.max = ncol(X)-1)
            if (ncomp$ncp > 0) {
                log_output <- utils::capture.output(res <- pcaMethods::pca(X, method = "nlpca",
                  center = FALSE, nPcs = ncomp$ncp, maxSteps = 100))
            } else {
                log_output <- utils::capture.output(res <- pca(X, method = "nlpca", center = FALSE,
                  nPcs = 2, maxSteps = 100))
            }
            imp_matrix <- res@completeObs
            pcaMethods_NLPCA_list[[n]] <- as.data.frame(imp_matrix)
        }
    }

    # 11 = mice mixed (run if selected)
    if (11 %in% sel_method) {
        print("mice mixed imputation - in progress")
        for (n in 1:n.iter) {
            log_output <- utils::capture.output(imputed_Data <- mice::mice(X, m = 1, maxit = 100))
            imp_matrix <- as.matrix(mice::complete(imputed_Data, 1))
            mice_mixed_list[[n]] <- as.data.frame(imp_matrix)
        }
    }

    # 12 = mi Bayesian (run if selected)
    if (12 %in% sel_method) {
        print("mi imputation - in progress")
        for (n in 1:n.iter) {
            log_output <- utils::capture.output(mi_data <- mi::mi(as.data.frame(X), n.chain = 1,
                n.iter = 100, verbose = FALSE))
            imputed <- mi::complete(mi_data, 1)
            imp_matrix <- as.matrix(imputed[, 1:ncol(X)])
            mi_Bayesian_list[[n]] <- as.data.frame(imp_matrix)
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
            log_output <- utils::capture.output(results <- missForest::missForest(X, maxiter = 10,
                ntree = 100, replace = T))
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
            completeData <- as.data.frame(Hmisc::impute.transcan(hmisc_algo, imputation = 1, data = X,
                list.out = TRUE, pr = FALSE, check = FALSE))
            imp_matrix <- as.matrix(completeData)
            aregImpute_list[[n]] <- as.data.frame(imp_matrix)
        }
    }

    # 16 = VIM kNN (run if selected)
    if (16 %in% sel_method) {
        print("VIM kNN imputation - in progress")
        Xcolnames <- colnames(X)
        log_output <- utils::capture.output(completeData <- VIM::kNN(data = X, variable = Xcolnames,
            k = 10, trace = F, imp_var = F))
        imp_matrix <- as.matrix(completeData)
        kNN_list[[1]] <- as.data.frame(imp_matrix)
    }

    all_imp_dfs <- list(random_replacement = random_imp_list, mean_imputation = mean_imp_list,
        median_imputation = median_imp_list, missMDA_reg_imputation = missMDA_reg_list, missMDA_EM_imputation = missMDA_EM_list,
        pcaMethods_PPCA_imputation = pcaMethods_PPCA_list, pcaMethods_svdImpute_imputation = pcaMethods_svdImpute_list,
        pcaMethods_BPCA_imputation = pcaMethods_BPCA_list, pcaMethods_Nipals_imputation = pcaMethods_Nipals_list,
        pcaMethods_NLPCA_imputation = pcaMethods_NLPCA_list, mice_mixed_imputation = mice_mixed_list,
        mi_Bayesian_imputation = mi_Bayesian_list, ameliaII_imputation = amelia_list, missForest_imputation = missForest_list,
        Hmisc_aregImpute_imputation = aregImpute_list, VIM_kNN_imputation = kNN_list)

    imp_dat_res_unlisted <- unlist(all_imp_dfs, recursive = F)

    # calculate RMSE values
    RMSE <- data.frame(matrix(NA, nrow = length(imp_dat_res_unlisted), ncol = 2))
    colnames(RMSE) <- c("Method", "RMSE")
    RMSE[, 1] <- names(imp_dat_res_unlisted)
    RMSE[, 1] <- gsub("[0-9]+", "", RMSE[, 1])
    for (i in 1:length(imp_dat_res_unlisted)) {
        RMSE[i, 2] <- sqrt(mean((X_orig[index_spikein] - imp_dat_res_unlisted[[i]][index_spikein])^2))
    }

    RMSE$Method <- factor(RMSE$Method)
    RMSE$Method <- plyr::revalue(RMSE$Method, c(random_replacement = "Random replacement", median_imputation = "Median imputation",
        mean_imputation = "Mean imputation", missMDA_reg_imputation = "missMDA Regularized", missMDA_EM_imputation = "missMDA EM",
        pcaMethods_PPCA_imputation = "pcaMethods PPCA", pcaMethods_svdImpute_imputation = "pcaMethods svdImpute",
        pcaMethods_BPCA_imputation = "pcaMethods BPCA", pcaMethods_Nipals_imputation = "pcaMethods NIPALS",
        pcaMethods_NLPCA_imputation = "pcaMethods NLPCA", mice_mixed_imputation = "mice mixed",
        mi_Bayesian_imputation = "mi Bayesian", ameliaII_imputation = "Amelia II", missForest_imputation = "missForest",
        Hmisc_aregImpute_imputation = "Hmisc aregImpute", VIM_kNN_imputation = "VIM kNN"))
    RMSE$Method <- factor(RMSE$Method, levels = c("Random replacement", "Median imputation", "Mean imputation",
        "missMDA Regularized", "missMDA EM", "pcaMethods PPCA", "pcaMethods svdImpute", "pcaMethods BPCA",
        "pcaMethods NIPALS", "pcaMethods NLPCA", "mice mixed", "mi Bayesian", "Amelia II", "missForest",
        "Hmisc aregImpute", "VIM kNN"))

    Method <- NULL
    p <- ggplot(RMSE, aes(x = Method, y = RMSE, col = Method)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90,
        hjust = 1)) + ggtitle("Root-mean-square error (RMSE) of various missing data imputation methods") +
        theme(plot.title = element_text(hjust = 0.5)) + labs(x = "")

    list(RMSE = RMSE, Plot = p)

}

