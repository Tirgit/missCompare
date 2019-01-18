context("Cleaning")
library(missCompare)

data("clindata_miss")
small <- clindata_miss[1:80, 1:4]
small$string <- "string"

# string variables present, expecting a stop and warning
test_that("error if strings present", {
  testthat::expect_error(clean(small))
})

small <- clindata_miss[1:80, 1:4]

# factor variables present, expecting a message of conversion
test_that("message for numeric conversion", {
  testthat::expect_message(clean(small))
})

# converting factors to num, now NOT expecting message
small$sex <- as.numeric(small$sex)
test_that("no message for numeric conversion", {
  expect_message(clean(small, var_removal_threshold = 0.7), NA)
})

# message when dropping variables
small$age[1:60] <- NA
test_that("message for variable removal", {
  expect_message(clean(small, var_removal_threshold = 0.5))
})

# message when dropping individuals
small$age <- NULL
small[c(1:10),] <- NA
test_that("message for individual removal", {
  expect_message(clean(small, ind_removal_threshold = 1))
})


# checking output
small <- clindata_miss[1:80, 1:4]
cleaned <- clean(small)

test_that("output dataset obs", {
  expect_output(str(cleaned), "80 obs")
})
test_that("output dataset vars", {
  expect_output(str(cleaned), "4 variables")
})
test_that("equal dims", {
  expect_equal(dim(small), dim(cleaned))
})

rm(list=ls())
