#' k_Nearest Neighbors Cross-Validation
#'
#' This function classifies obervations and computes the misclassification error.
#'
#' @param train input data frame.
#' @param cl true class value of the training data.
#' @param k_nn integer representing the number of neighbors.
#' @param k_cv integer representing the number of folds.
#' @keywords prediction
#' @return a list contains a vector of the predicted class for all
#'   observations: \code{"class"}, and a numeric of the cross-validation
#'   misclassification error: \code{"cv_err"}.
#' @examples
#' penguins_df <- na.omit(my_penguins)
#' penguins_cl <- penguins_df$species
#' penguins_df <- penguins_df %>%
#'   dplyr::select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
#' my_knn_cv(penguins_df, penguins_cl, 1, 5)
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  n <- nrow(train)
  fold <- sample(rep(1:k_cv, length = n))
  train <- data.frame("x" = train, "split" = fold)
  cl <- as.data.frame(cl)
  cl$split <- fold
  cv_error <- 0

  # Iterate through every fold
  for (i in 1:k_cv) {
    data_train <- train %>% filter(split != i)
    data_test <- train %>% filter(split == i)
    cl_train <- cl %>% filter(split != i)
    cl_train <- as.vector(cl_train$cl)
    cl_test <- cl %>% filter(split == i)
    cl_test <- as.vector(cl_test$cl)

    result <- knn(train = data_train, test = data_test, cl = cl_train, k = k_nn)

    # Compute cv error
    num_err <- 0
    for (j in 1:length(cl_test)) {
      if (result[j] != cl_test[j]) {
        num_err <- num_err + 1
      }
    }
    cv_error <- cv_error + num_err / length(cl_test)
  }

  cv_error <- cv_error / k_cv
  cl <- as.vector(cl$cl)

  class <- knn(train = train[, 1:4], test = train[, 1:4], cl = cl, k = k_nn)
  return(list("class" = class, "cv_error" = cv_error))
}
