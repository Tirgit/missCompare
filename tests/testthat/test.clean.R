context("Cleaning")
library(missCompare)
library(missForest)

data("clindata_miss")

# factor variables present, expecting a message of conversion
test_that("message for numeric conversion", {
  expect_message(missCompare::clean(clindata_miss))
})

# converting factors to num, now NOT expecting message
clindata_miss$education <- as.numeric(clindata_miss$education)
clindata_miss$sex <- as.numeric(clindata_miss$sex)
test_that("no message for numeric conversion", {
  expect_message(missCompare::clean(clindata_miss, var_removal_threshold = 0.7), NA)
})

# message when dropping variables
test_that("message for variable removal", {
  expect_message(missCompare::clean(clindata_miss,
                                    var_removal_threshold = 0.5))
})

# message when dropping individuals
clindata_miss_ind <- clindata_miss
clindata_miss_ind[c(1:10),] <- NA
test_that("message for individual removal", {
  expect_message(missCompare::clean(clindata_miss_ind,
                                    ind_removal_threshold = 1))
})

# checking for no error
test_that("no errors in clean()", {
  expect_error(missCompare::clean(clindata_miss), NA)
})

# checking output
cleaned <- missCompare::clean(clindata_miss)

test_that("output dataset obs", {
  expect_output(str(cleaned$Dataframe_clean), "2500 obs")
})
test_that("output dataset vars", {
  expect_output(str(cleaned$Dataframe_clean), "11 variables")
})
