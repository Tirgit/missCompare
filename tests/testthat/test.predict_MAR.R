context("MAR Prediction")
library(missCompare)
library(mlbench)
library(missForest)

data("BostonHousing")
df_miss <- prodNA(BostonHousing, 0.1)

# expecting error if non numeric variables are in the dataframe
test_that("no errors in get_data()", {
  expect_error(missCompare::get_data(df_miss))
})

cleaned <- clean(df_miss)
df_miss <- cleaned$Dataframe_clean
y <- get_data(df_miss, matrixplot_sort = T)
simulated <- simulate(rownum = y$Rows, colnum =y$Columns, cormat=y$Corr_matrix)
res <- all_patterns(X_hat = simulated$Simulated_matrix, missfrac_per_var = y$Fraction_missingness_per_variable, window = 0.2)
