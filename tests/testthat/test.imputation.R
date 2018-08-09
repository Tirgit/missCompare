context("Missing data imputation")
library(missCompare)
library(mlbench)
library(missForest)

data("BostonHousing")
df_miss <- prodNA(BostonHousing, 0.2)

# expecting error if non numeric variables are in the dataframe
test_that("errors in impute_data_validate()", {
  expect_error(missCompare::impute_data_validate(df_miss))
})
test_that("errors in impute_data()", {
  expect_error(missCompare::impute_data(df_miss))
})

cleaned <- clean(df_miss)
df_miss <- cleaned$Dataframe_clean
df_miss <- df_miss[1:200,1:10]
y <- get_data(df_miss, matrixplot_sort = T)
simulated <- simulate(rownum = y$Rows, colnum =y$Columns, cormat=y$Corr_matrix)
res <- all_patterns(X_hat = simulated$Simulated_matrix, missfrac_per_var = y$Fraction_missingness_per_variable, window = 0.2)

# no errors in indivisual imputation algorithms
test_that("mean imputation errors", {
  expect_error(missCompare::test_mean_imp(simulated$Simulated_matrix, res), NA)
})
test_that("median imputation errors", {
  expect_error(missCompare::test_median_imp(simulated$Simulated_matrix, res), NA)
})
test_that("Amelia imputation errors", {
  expect_error(missCompare::test_AmeliaII(simulated$Simulated_matrix, res), NA)
})
test_that("kNN imputation errors", {
  expect_error(missCompare::test_kNN(simulated$Simulated_matrix, res), NA)
})
test_that("mi imputation errors", {
  expect_error(missCompare::test_mi(simulated$Simulated_matrix, res), NA)
})
test_that("mice imputation errors", {
  expect_error(missCompare::test_mice_mixed(simulated$Simulated_matrix, res), NA)
})
test_that("mean imputation errors", {
  expect_error(missCompare::test_mean_imp(simulated$Simulated_matrix, res), NA)
})
test_that("missForest imputation errors", {
  expect_error(missCompare::test_missForest(simulated$Simulated_matrix, res), NA)
})
test_that("mean imputation errors", {
  expect_error(missCompare::test_mean_imp(simulated$Simulated_matrix, res), NA)
})
test_that("missMDA EM imputation errors", {
  expect_error(missCompare::test_missMDA_EM(simulated$Simulated_matrix, res), NA)
})
test_that("missMDA reg imputation errors", {
  expect_error(missCompare::test_missMDA_reg(simulated$Simulated_matrix, res), NA)
})
test_that("pcaMethods BPCA imputation errors", {
  expect_error(missCompare::test_pcaMethods_BPCA(simulated$Simulated_matrix, res), NA)
})
test_that("pcaMethods PPCA imputation errors", {
  expect_error(missCompare::test_pcaMethods_PPCA(simulated$Simulated_matrix, res), NA)
})
test_that("pcaMethods Nipals imputation errors", {
  expect_error(missCompare::test_pcaMethods_Nipals(simulated$Simulated_matrix, res), NA)
})
test_that("pcaMethods NLPCA imputation errors", {
  expect_error(missCompare::test_pcaMethods_NLPCA(simulated$Simulated_matrix, res), NA)
})
test_that("pcaMethods svdImpute imputation errors", {
  expect_error(missCompare::test_pcaMethods_svdImpute(simulated$Simulated_matrix, res), NA)
})
test_that("random imputation errors", {
  expect_error(missCompare::test_random_imp(simulated$Simulated_matrix, res), NA)
})

# no error if multiple iterations are defined with algorithm that are not suitable for multiple imputation

test_that("median imputation only one copy of imputation / 1", {
  expect_error(missCompare::impute_data(df_miss, scale = T, n.iter = 2, sel_method = c(2)), NA)
})

imputed <- missCompare::impute_data(df_miss, scale = T, n.iter = 2, sel_method = c(2))
imputed$median_imputation[[1]]

test_that("median imputation only one copy of imputation / 2", {
  expect_error(imputed$median_imputation[[2]])
})

# generating imputed dataframe
imputed <- missCompare::impute_data(df_miss, scale = T, n.iter = 1, sel_method = c(14))
df_imp <- imputed$missForest_imputation[[1]]

# expecting no error when running post imp diag script
test_that("no errors in post imp diag script", {
  expect_error(missCompare::post_imp_diag(df_miss, df_imp, scale = T, n.boot = 5), NA)
})

