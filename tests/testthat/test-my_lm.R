test_that("invalid input formula", {
  expect_error(my_lm("hello", (1:10)))
})

test_that("not sufficient input", {
  expect_error(my_lm(1:10))
})

test_that("correct output", {
  value1 <- c(182, 193, 183, 194, 189, 183)
  value2 <- c(1, 3, 0, 2, 2, 3)
  weight <- c(9.3, 11.1, 8.7, 10.4, 10.6, 12.3)
  input <- data.frame("value1" = value1, "value2" = value2, "weight" = weight)
  result <- my_lm(weight ~ value1 + value2, input)
  expect_type(result$Estimate, "double")
  expect_type(result$Std.Error, "double")
  expect_type(result$t_value, "double")
  expect_type(result$Pr, "double")
})
