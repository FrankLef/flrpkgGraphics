#' Histogram for added value analysis
#'
#' Histogram for added value analysis.
#'
#' Histogram for added value analysis with a custom format.
#'
#' @param data Data.frame.
#' @param x_var String to identify the x variable.
#' @param group_var String to identify the group variable.
#' @param binwidth Number for the bin width.
#' @param accuracy Accuracy for the x axis.
#' @param pal Palette of colors used by group.
#' @param fun Function used to transform x-axis labels.
#' @param titles List of plot titles.
#' \describe{
#'  \item{title}{Main title.}
#'  \item{subtitle}{Subtitle.}
#'  \item{x, y}{Labels for x and y axis.}
#' }
#'
#' @return Object of class \code{gg}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ggp_hist_addval <- function(
    data, x_var, group_var, binwidth = 0.25, accuracy = 1, pal, fun, titles) {
  stats <- data |>
    ggp_hist_addval_stats(
      x_var = x_var, group_var = group_var
    )

  data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[x_var]], fill = ggplot2::after_stat(count))) +
    ggplot2::geom_histogram(binwidth = binwidth) +
    ggplot2::geom_vline(
      data = stats, ggplot2::aes(xintercept = med), color = "purple",
      linetype = "longdash"
    ) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_extended(n = 5),
      labels = \(x) {
        y <- fun(x)
        scales::label_currency(accuracy = accuracy)(y)
      }
    ) +
    ggplot2::scale_fill_gradientn(colors = pal) +
    ggplot2::facet_wrap(. ~ .data[[group_var]], scales = "fixed") +
    ggplot2::labs(
      title = titles$title,
      subtitle = titles$subtitle,
      x = titles$x,
      y = titles$y
    )
}

ggp_hist_addval_stats <- function(data, x_var, group_var) {
  data |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::summarize(
      med = stats::median(.data[[x_var]]),
      label = scales::label_dollar(accuracy = 1)(expm1(med))
    )
}
