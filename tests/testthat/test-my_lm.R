test_that("invalid input formula", {
  expect_error(my_lm("hello", (1:10)))
})

test_that("not sufficient input", {
  expect_error(my_lm(1:10))
})
