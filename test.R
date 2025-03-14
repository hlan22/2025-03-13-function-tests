library(testthat)
library(dplyr)
set.seed(2024)
source("count.R")


# test input data
test_that("a dataframe with 2 unique values we want to count", {
  expect_equal(count_classes(two_classes_2_obs, class_label),  two_classes_2_obs_output)
  
  expect_s3_class(count_classes(two_classes_2_obs, class_label), "data.frame")
}) # this is a unit test!

test_that("empty data frame", {
  # "edge cases" test to be added here
})

test_that("incorrect types", {
  expect_error(count_classes(two_classes_2_obs_as_list, class_label))
})


- test driven development
- use test-that to test the big test we are making
- you can make as many as you want
- you can create a separate file to source the function temporarily
- next week: work through point blank and tests, and work up to creating package
