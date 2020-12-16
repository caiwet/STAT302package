test_that("invalid alternative throws error", {
  x <- rnorm(10, mean = 10, sd = 1)
  expect_error(my_t.test(x, "hello", 0))
})

test_that("alternative output is correct", {
  x <- rnorm(10, mean = 10, sd = 1)
  expect_match(my_t.test(x, "two.sided", 0)$alternative,
               "two.sided")
})

test_that("degree of freedom is correct", {
  x <- rnorm(10, mean = 0.1, sd = 1)
  expect_equal(my_t.test(x, "greater", 0)$df, 9)
})

test_that("p value is correct", {
  x <- rnorm(10, mean = 0.1, sd = 1)
  actual <- my_t.test(x, "less", 0)$p_val
  expected <- t.test(x, mu = 0, alternative = "less")$p.value
  expect_equal(expected, actual)
})

test_that("test statistic type is correct", {
  x <- rnorm(10, mean = 0.1, sd = 1)
  actual <- my_t.test(x, "less", 0)$test_stat
  expect_type(actual, "double")
})


