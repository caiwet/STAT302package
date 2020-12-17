test_that("invalid alternative throws error", {
  x <- rnorm(10, mean = 10, sd = 1)
  expect_error(my_t.test(x, "hello", 0))
})

test_that("alternative output is correct", {
  x <- rnorm(10, mean = 10, sd = 1)
  expect_match(my_t.test(x, "two.sided", 0)$alternative,
               "two.sided")
})

test_that("degree of freedom type is correct", {
  x <- rnorm(10, mean = 0.1, sd = 1)
  expect_type(my_t.test(x, "greater", 0)$df, "double")
})

test_that("p value is correct", {
  x <- rnorm(10, mean = 0.1, sd = 1)
  actual <- my_t.test(x, "less", 0)$p_val
  expect_type(actual, "double")
})

test_that("test statistic type is correct", {
  x <- rnorm(10, mean = 0.1, sd = 1)
  actual <- my_t.test(x, "less", 0)$test_stat
  expect_type(actual, "double")
})


