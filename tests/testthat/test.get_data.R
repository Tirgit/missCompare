context("Data extraction and simulation")
library(missCompare)
library(mlbench)
library(missForest)

data("BostonHousing")
df_miss <- prodNA(BostonHousing, 0.1)
cleaned <- clean(df_miss)
df_miss <- cleaned$Dataframe_clean
y <- get_data(df_miss, matrixplot_sort = T)
simulated <- simulate(rownum = y$Rows, colnum =y$Columns, cormat=y$Corr_matrix)
res <- all_patterns(X_hat = simulated$Simulated_matrix, missfrac_per_var = y$Fraction_missingness_per_variable, window = 0.2)


# checking no errors in functions
test_that("no errors in get_data()", {
  expect_error(missCompare::get_data(df_miss), NA)
})
test_that("no errors in simulate()", {
  expect_error(missCompare::simulate(rownum = y$Rows, colnum =y$Columns, cormat=y$Corr_matrix), NA)
})
test_that("no errors in all_patterns(), MCAR(), MNAR(), MAR() and MAP()", {
  expect_error(missCompare::all_patterns(X_hat = simulated$Simulated_matrix,
                                         missfrac_per_var = y$Fraction_missingness_per_variable,
                                         window = 0.2,
                                         assumed_pattern = c('MAR', 'MCAR', 'MCAR', 'MAR', 'MNAR', 'MCAR',
                                                             'MAR', 'MCAR', 'MCAR', 'MAR', 'MNAR', 'MCAR',
                                                             'MAR', 'MNAR')), NA)
})

# checking output

test_that("correlation matrix dimensions correct", {
  expect_equal(dim(y$Corr_matrix), c(ncol(BostonHousing),ncol(BostonHousing)))
})
test_that("fraction of missingness vector length correct", {
  expect_equal(length(y$Fraction_missingness_per_variable), ncol(BostonHousing))
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



