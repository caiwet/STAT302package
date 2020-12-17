#' Fitting Linear Models
#'
#' This function fits a linear model for input data.
#'
#' @param formula a \code{"formula"} class object (a symbolic description of
#'   the model to be fitted).
#' @param data input data frame.
#' @keywords inference prediction
#' @return a \code{"table"} with rows for each coefficient and columns for the
#'   \code{"Estimate"}, \code{"Std.Error"}, \code{"t_value"},
#'   and \code{"Pr"}.
#' @examples
#' # Create a random input for test
#' value1 <- c(182, 193, 183, 194, 189, 183)
#' value2 <- c(1, 3, 0, 2, 2, 3)
#' weight <- c(9.3, 11.1, 8.7, 10.4, 10.6, 12.3)
#' input <- data.frame("value1" = value1, "value2" = value2, "weight" = weight)
#' my_lm(weight ~ value1 + value2, input)
#' @export
my_lm <- function(formula, data) {
  X <- model.matrix(formula, data)
  frame <- model.frame(formula, data)
  Y <- model.response(frame)
  est <- solve(t(X) %*% X) %*% t(X) %*% Y
  df <- nrow(data) - ncol(data)
  n <- nrow(X)
  sigma_sq <- 0

  for (i in 1:n) {
    sigma_sq <- sigma_sq + (Y[i] - X[i,] %*% est) ^ 2
  }

  sigma_sq <- sigma_sq / df
  se <- sqrt(as.vector(sigma_sq) * diag(solve(t(X) %*% X)))

  t_value <- est / se

  Pr <- pt(abs(t_value), df, lower.tail = FALSE) * 2
  df <- data.frame("Estimate" = est, "Std.Error" = se,
                   "t_value" = t_value, "Pr" = Pr)
  return(df)
}
