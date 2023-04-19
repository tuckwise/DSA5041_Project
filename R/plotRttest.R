#' plot.Rttest
#'
#' @param x an Rttest object
#' @param ... anything else
#'
#' @return prints out a side-by-side boxplot of the variables in the Rttest dataframe
#' @export
#'
#' @examples \dontrun{plot(x)}
plot.Rttest <- function(x, ...) {
  df_long <- x$data %>%
    tidyr::gather(variable, value, X, Y) %>%
    dplyr::filter(!is.na(value))

  boxplot_gg <- ggplot(df_long, aes(x = variable, y = value, fill = variable)) +
    geom_boxplot() +
    labs(x = "Variable", y = "Value") +
    theme_minimal() +
    scale_fill_manual(values = c("X" = "steelblue", "Y" = "darkorange"))

  print(boxplot_gg)
}
