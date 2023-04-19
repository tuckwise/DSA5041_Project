#' plot.Rttest
#'
#' @param x an Rttest object
#' @param ... anything else
#'
#' @return prints out a side-by-side boxplot of the variables in the Rttest dataframe
#' @export
#' @importFrom ggplot2 ggplot aes geom_boxplot labs theme_minimal scale_fill_manual
#' @importFrom dplyr %>%
#' @importFrom tidyr gather
#' @importFrom rlang sym
#'
#' @examples \dontrun{plot(x, ...)}
plot.Rttest <- function(x, ...) {
  variable <- rlang::sym("variable")
  value <- rlang::sym("value")
  X <- rlang::sym("X")
  Y <- rlang::sym("Y")

  df_long <- x$data %>%
    tidyr::gather(!!variable, !!value, !!X, !!Y) %>%
    dplyr::filter(!is.na(!!value))

  boxplot_gg <- ggplot2::ggplot(df_long, ggplot2::aes(x = !!variable, y = !!value, fill = !!variable)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Variable", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = c("X" = "steelblue", "Y" = "darkorange"))

  print(boxplot_gg)
}

