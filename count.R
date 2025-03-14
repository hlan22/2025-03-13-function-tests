#' Count class observations
#'
#' Creates a new data frame with two columns,
#' listing the classes present in the input data frame,
#' and the number of observations for each class.
#'
#' @param data_frame A data frame or data frame extension (e.g. a tibble).
#' @param class_col unquoted column name of column containing class labels
#'
#' @return A data frame with two columns.
#'   The first column (named class) lists the classes from the input data frame.
#'   The second column (named count) lists the number of observations
#'   for each class from the input data frame.
#'   It will have one row for each class present in input data frame.
#'
#' @export
#' @examples
#' count_classes(mtcars, am)
#' 




count_classes <- function(data_frame, class_col) {
  # returns a data frame with two columns: class and count
}



library(testthat)

test_that("`count_classes` should return a data frame, or tibble,
with the number of rows corresponding to the number of unique classes
in the `class_col` from the original dataframe. The new dataframe
will have a `class column` whose values are the unique classes,
and a `count` column, whose values will be the number of observations
for each  class", {
  # "expected use cases" tests to be added here
})

test_that("`count_classes` should return an empty data frame, or tibble,
if the input to the function is an empty data frame", {
  # "edge cases" test to be added here
})

test_that("`count_classes` should throw an error when incorrect types
are passed to the `data_frame` argument", {
  # "error" tests to be added here
})




test_that("a dataframe with 2 unique values we want to count", {
  # "expected use cases" tests to be added here
})

test_that("empty data frame", {
  # "edge cases" test to be added here
})

test_that("incorrect types", {
  # "error" tests to be added here
})





library(dplyr)
set.seed(2024)

# test input data
two_classes_2_obs <- tibble(class_labels = rep(c("class1",
                                                 "class2"), 2),
                            values = round(runif(4), 1))
two_classes_2_and_1_obs <- tibble(class_labels = c(rep("class1", 2),
                                                   "class2"),
                                  values = round(runif(3), 1))
one_class_2_obs <- tibble(class_labels = c("class1", "class1"),
                          values = round(runif(2), 1))
empty_df  <- tibble(class_labels = character(0),
                    values = double(0))
two_classes_two_obs_as_list <- list(class_labels = rep(c("class1",
                                                         "class2"), 2),
                                    values = round(runif(4), 1))

# expected test outputs
two_classes_2_obs_output <- tibble(class = c("class1", "class2"),
                                   count = c(2,2))
two_classes_2_and_1_obs_output <- tibble(class = c("class1", "class2"),
                                         count = c(2, 1))
one_class_2_obs_output <- tibble(class = "class1",
                                 count = 2)
empty_df_output <- tibble(class = character(0),
                          count = numeric(0))




count_classes <- function(data_frame, class_col) {
  if (!is.data.frame(data_frame)) {
    stop("`data_frame` should be a data frame or data frame extension (e.g. a tibble)")
  }
  return(data_frame |>
    dplyr::group_by({{ class_col }}) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::rename("class" = {{ class_col }}))
}


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


