test_that("invalid alternative throws error", {
  x <- rnorm(10, mean = 10, sd = 1)
  expect_error(my_t.test(x, "hello", 0))
})
