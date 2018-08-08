context("Cleaning")
library(missCompare)
library(mlbench)
library(missForest)

data("BostonHousing")

# variable "chas" is a factor, expecting a message of conversion
test_that("message for numeric conversion", {
  expect_message(missCompare::clean(BostonHousing))
})

# converting "chas" to numeric, now NOT expecting message
BostonHousing$chas <- as.numeric(as.character(BostonHousing$chas))
test_that("no message for numeric conversion", {
  expect_message(missCompare::clean(BostonHousing), NA)
})

# message when dropping variables
BostonHousing_var <- BostonHousing
BostonHousing_var[,1:2] <- missForest::prodNA(BostonHousing_var[,1:2], 0.55)
test_that("message for variable removal", {
  expect_message(missCompare::clean(BostonHousing_var,
                                    var_removal_threshold = 0.5))
})

# message when dropping individuals
BostonHousing_ind <- BostonHousing
BostonHousing_ind[c(1:10),] <- NA
test_that("message for individual removal", {
  expect_message(missCompare::clean(BostonHousing_ind,
                                    ind_removal_threshold = 1))
})

# checking for no error
test_that("no errors in clean()", {
  expect_error(missCompare::clean(BostonHousing), NA)
})

# checking output
cleaned <- missCompare::clean(BostonHousing)

test_that("output dataset obs", {
  expect_output(str(cleaned$Dataframe_clean), "506 obs")
})
test_that("output dataset vars", {
  expect_output(str(cleaned$Dataframe_clean), "14 variables")
})
