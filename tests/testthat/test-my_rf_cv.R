test_that("invalid input", {
  expect_error(my_rf_cv("input"))
})

test_that("output type is numeric", {
  expect_type(my_rf_cv(5), "double")
})
