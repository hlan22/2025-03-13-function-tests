my_add <- function(x) {
  return(x + 10)
}

my_add(10) == 20
stopifnot(my_add(100) == 110) # prevents dumping out output
stopifnot(my_add(10.5) == 20.5)

stopifnot(TRUE) # works perfectly fine
# stopifnot(FALSE) # R code will throw an error

install.packages("testthat")
library(testthat)

expect_equal(my_add(10.5), 20.5,) # tolerance = )

# ?expect_equal

# these tests are good with decimals 1/9 == 0.1111111 even tho thats what it gives

# tdd: test-driven-development

my_add <- function(x) {
  # after you define the test you implement
  return(x + 10)
}
expect_equal(my_add(10.5), 20.5,)




