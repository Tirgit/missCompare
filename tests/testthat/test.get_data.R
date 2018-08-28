context("Data extraction and simulation")
library(missCompare)
library(missForest)

data("clindata_miss")

# expecting error if non numeric variables are in the dataframe
test_that("numeric errors in get_data()", {
  expect_error(missCompare::get_data(clindata_miss))
})

cleaned <- clean(clindata_miss)
df_miss <- cleaned$Dataframe_clean
y <- get_data(df_miss, matrixplot_sort = T)
simulated <- simulate(rownum = y$Rows, colnum =y$Columns, cormat=y$Corr_matrix)
res <- all_patterns(X_hat = simulated$Simulated_matrix,
                    MD_pattern = y$MD_Pattern,
                    NA_fraction = y$Fraction_missingness,
                    min_PDM = 10)

# checking no errors in functions
test_that("no errors in get_data()", {
  expect_error(missCompare::get_data(df_miss), NA)
})
test_that("no errors in simulate()", {
  expect_error(missCompare::simulate(rownum = y$Rows, colnum =y$Columns, cormat=y$Corr_matrix), NA)
})

# checking output

test_that("correlation matrix dimensions correct", {
  expect_equal(dim(y$Corr_matrix), c(ncol(clindata_miss)-1,ncol(clindata_miss)-1))
})
test_that("fraction of missingness vector length correct", {
  expect_equal(length(y$Fraction_missingness_per_variable), ncol(clindata_miss)-1)
})
test_that("equal dimensions of simulated matrix and original dataframe", {
  expect_equal(dim(simulated$Simulated_matrix), dim(df_miss))
})
test_that("equal dimensions of simulated matrix and MCAR matrix", {
  expect_equal(dim(simulated$Simulated_matrix), dim(res$MCAR_matrix))
})
test_that("equal dimensions of simulated matrix and MNAR matrix", {
  expect_equal(dim(simulated$Simulated_matrix), dim(res$MNAR_matrix))
})
test_that("equal dimensions of simulated matrix and MAR matrix", {
  expect_equal(dim(simulated$Simulated_matrix), dim(res$MAR_matrix))
})



