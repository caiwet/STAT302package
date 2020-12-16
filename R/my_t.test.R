#' T-test Function
#'
#' This function performs one-sample t test for both two-sided or one-sided.
#'
#' @param x numeric vector of data.
#' @param alternative a character string specifying the alternative hypothesis.
#'   Only accept \code{"two.sided"}, \code{"less"} or \code{"greater"}.
#' @param mu a number indicating the null hypothesis value of the mean.
#' @keywords inference
#' @return A list with numeric test statistic \code{"test_stat"},
#'   the degrees of freedom \code{"df"},
#'   the value of the parameter alternative \code{"alternative"},
#'   and the numeric p-value \code{"p_val"}.
#' @examples
#' x <- rnorm(10, mean = 0.1, sd = 1)
#' my_t.test(x, "two.sided", 0)
#' my_t.test(x, "greater", 0)
#' @export
my_t.test <- function(x, alternative, mu) {
  n <- length(x)
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(n))
  df <- n - 1
  alt <- ""
  p_val <- 0
  if (alternative == "two.sided") {
    p_val <- abs(pt(abs(test_stat), df, lower.tail = FALSE)) * 2
  } else if (alternative == "less") {
    p_val <- pt(test_stat, df)
  } else if (alternative == "greater") {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else {
    stop("wrong alternative")
  }
  return(list("test_stat" = test_stat, "df" = df,
              "alternative" = alternative, "p_val" = p_val))
}
