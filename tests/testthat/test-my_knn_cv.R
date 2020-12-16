test_that("invalid input throws error", {
  expect_error(my_knn_cv(1, 2, 3))
})

test_that("output 1 is a list", {
  penguins_df <- na.omit(my_penguins)
  penguins_cl <- penguins_df$species
  penguins_df <- penguins_df %>%
    dplyr::select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
  result <- my_knn_cv(penguins_df, penguins_cl, 1, 5)
  expect_type(result$class, "list")
})

test_that("output 2 is numeric", {
  penguins_df <- na.omit(my_penguins)
  penguins_cl <- penguins_df$species
  penguins_df <- penguins_df %>%
    dplyr::select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
  result <- my_knn_cv(penguins_df, penguins_cl, 1, 5)
  expect_type(result$cv_error, "numeric")
})
