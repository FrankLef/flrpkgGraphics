#' Scatter plot for added value analysis
#'
#' Scatter plot for added value analysis.
#'
#' Scatter plot for added value analysis with a custom format. Labels on the
#' axes are converted to the natural scale.
#'
#' @param data Data.frame.
#' @param x_var String with name of the x axis.
#' @param y_var String with name of the y axis.
#' @param group_var String with name of the group variable.
#' @param pal Color palette for the group.
#' @param fun Function to convert axes' labels.
#' @param titles List of titles.
#'
#' @return Object of class \code{gg}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ggp_scatter_addval <- function(data, x_var, y_var, group_var, pal, fun, titles) {
  data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = .data[[group_var]]
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_extended(n = 5),
      labels = \(x) {
        y <- fun(x)
        scales::label_currency(accuracy = 1)(y)
      }
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_extended(n = 5),
      labels = \(x) {
        y <- fun(x)
        scales::label_currency(accuracy = 1)(y)
      }
    ) +
    ggplot2::scale_color_discrete(type = pal) +
    ggplot2::annotate(
      geom = "rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
      alpha = 0.3, fill = "pink"
    ) +
    ggplot2::facet_wrap(. ~ .data[[group_var]], scales = "fixed") +
    ggplot2::labs(
      title = titles$title,
      subtitle = titles$subtitle,
      x = titles$x,
      y = titles$y
    )
}
