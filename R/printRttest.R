#' print.Rttest
#'
#' @param x an object of class Rttest
#' @param ... anything else
#'
#' @return returns the data frame as a kable table, as well as various parameters (p-value, alpha, confidenc interval)
#' @export
#'
#' @examples \dontrun{print.Rttest(x)}
print.Rttest <- function(x, ...) {
  cat("\nData Frame:\n")
  print(kableExtra::kable(x$data, format = 'simple'))
  cat("\nAlpha: ", x$alpha, "\n")
  cat("\nConfidence Interval for mu_x - mu_y: [", x$conf_int[1], ", ", x$conf_int[2], "]\n")
  cat("\nP-value: ", x$p_value, "\n")
}
