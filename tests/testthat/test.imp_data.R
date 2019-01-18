context("Missing data imputation - data imputation")
library(missCompare)

data("clindata_miss")
small <- clindata_miss[1:60, 1:4]
small$string <- "string"

# expecting error if strings are in the dataframe
test_that("string errors in impute_data", {
  expect_error(impute_data(small))
})

# expecting error if factors are present and methods are defined that do not support factors
small <- clindata_miss[1:60, 1:4]
test_that("factors error when methods dont support it", {
  expect_error(impute_data(small, scale = T, sel_method = c(2:10,13)))
})

# expect methods that support factors run OK when factors are defined - with scaling
small <- clindata_miss[1:60, 1:4]
test_that("factors OK scaling runs OK", {
  expect_error(impute_data(small, n.iter = 1, scale = T, sel_method = c(1,11,12,14:16)), NA)
})

# expect methods that support factors run OK when factors are defined - without scaling
small <- clindata_miss[1:60, 1:4]
test_that("factors OK no scaling runs OK", {
  expect_error(impute_data(small, n.iter = 1, scale = F, sel_method = c(1,11,12,14:16)), NA)
})

# expect all methods to run OK when variables are all numeric - with scaling
small <-  clindata_miss[1:60, 3:7]
test_that("runs OK when all numeric and with scaling", {
  expect_error(impute_data(small, n.iter = 1, scale = T, sel_method = c(1:16)), NA)
})

# expect all methods to run OK when variables are all numeric - without scaling (note is output though)
small <-  clindata_miss[1:60, 3:7]
test_that("runs OK when all numeric and without scaling", {
  expect_error(suppressWarnings(impute_data(small, n.iter = 1, scale = F, sel_method = c(1:16))), NA)
})

# test that single imputation methods do not output multiple imputed sets when multiple iters are defined
small <-  clindata_miss[1:60, 3:7]
imputed <- impute_data(small, scale = T, n.iter = 2, sel_method = c(2:5, 7:9, 16))

test_that("median imputation error", {
  expect_error(imputed$median_imputation[[2]])
})
test_that("mean imputation error", {
  expect_error(imputed$mean_imputation[[2]])
})
test_that("missMDA EM imputation error", {
  expect_error(imputed$missMDA_EM_imputation[[2]])
})
test_that("missMDA reg imputation error", {
  expect_error(imputed$missMDA_reg_imputation[[2]])
})
test_that("pcaMethods svd imputation error", {
  expect_error(imputed$pcaMethods_svdImpute_imputation[[2]])
})
test_that("pcaMethods BPCA imputation error", {
  expect_error(imputed$pcaMethods_BPCA_imputation[[2]])
})
test_that("pcaMethods Nipals imputation error", {
  expect_error(imputed$pcaMethods_Nipals_imputation[[2]])
})
test_that("VIM kNN imputation error", {
  expect_error(imputed$VIM_kNN_imputation[[2]])
})


df_imp <- imputed$VIM_kNN_imputation[[1]]
# expecting no error when running post imp diag script, when scale = T and original was scaled
test_that("scaling on, post imp diag no error", {
  expect_error(suppressWarnings(post_imp_diag(small, df_imp, scale = T, n.boot = 2)), NA)
})

# expecting  error when dimensions of imputed and original are not the same
test_that("post imp diag dim error", {
  expect_error(post_imp_diag(small[,1:4], df_imp, scale = T, n.boot = 2))
})

imp_res <- suppressWarnings(post_imp_diag(small, df_imp, scale = T, n.boot = 2))
# expect no barcharts when only numeric variables are present
test_that("barcharts not present", {
  expect_true(length(imp_res$Barcharts)==0)
})

# expecting barcharts when factors are imputed
small <- clindata_miss[1:100, 1:4]
imputed <- impute_data(small, scale = F, n.iter = 1, sel_method = c(1))
df_imp <- imputed$random_replacement[[1]]
imp_res <- suppressWarnings(post_imp_diag(small, df_imp, scale = F, n.boot = 3))

test_that("barchart is present", {
  expect_false(length(imp_res$Barcharts)==0)
})

rm(list=ls())
