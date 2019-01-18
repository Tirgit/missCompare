context("Missing data imputation - simulation")
library(missCompare)

data("clindata_miss")
small <- clindata_miss[1:60, 1:4]
small$age[1:10] <- NA
cleaned <- clean(small)
y <- get_data(cleaned, matrixplot_sort = T)
simulated <- simulate(rownum = y$Rows, colnum =y$Columns, cormat=y$Corr_matrix)
res <- all_patterns(X_hat = simulated$Simulated_matrix,
                    MD_pattern = y$MD_Pattern,
                    NA_fraction = y$Fraction_missingness,
                    min_PDM = 2)

# simulation runs OK
test_that("simulation runs without errors", {
  suppressWarnings(expect_error(impute_simulated(rownum = y$Rows,
                           colnum = y$Columns,
                           cormat = y$Corr_matrix,
                           MD_pattern = y$MD_Pattern,
                           NA_fraction = y$Fraction_missingness,
                           min_PDM = 2,
                           n.iter = 2), NA))
})

rm(list=ls())
