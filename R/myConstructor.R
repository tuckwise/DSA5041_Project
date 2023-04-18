#' myConstructor
#'
#' @param x a vector of values
#' @param y a vector of values
#' @param alpha a significance level less than 1
#'
#' @return returns a list of the x & y vectors in a data frame, the alpha value, the p-value of the test test, and the confidence interval for the difference of the means
#' @export
#'
#' @examples \dontrun{myConstructor(x, y, 0.05)}
myConstructor <- function(x, y, alpha) {

  # Perform two-sample t-test assuming equal variances
  ttest_result <- t.test(x, y, var.equal = TRUE)

  # Calculate confidence interval
  conf_int <- ttest_result$conf.int

  # Calculate p-value
  p_value <- ttest_result$p.value

  # Create a data frame containing x and y
  max_length <- max(length(x), length(y))
  df <- data.frame("X" = rep(NA, max_length), "Y" = rep(NA, max_length))
  df[1:length(x), "X"] <- x
  df[1:length(y), "Y"] <- y

  # Create a named list with the data frame, alpha, confidence interval, and p-value
  result <- list(
    data = df,
    alpha = alpha,
    conf_int = conf_int,
    p_value = p_value
  )

  # Set the class of the list to "Rttest"
  class(result) <- "Rttest"

  return(result)
}
