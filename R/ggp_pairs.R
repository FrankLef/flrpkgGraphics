#' Matrix of plots with a given data set
#'
#' Matrix of plots with a given data set.
#'
#' Matrix of plots with density plot on the diagonal, scaterr plots on
#' lower triangle and correlations on upper triangle.
#'
#' @param data Data.frame.
#' @param columns Character() with name of columns to plot.
#' @param color_var Variable to use to color by group.
#' @param pal Color to use by \code{color_var}.
#' @param titles List of titles.
#' @param thm Function or an object of class \code{theme}.
#'
#' @return Object of class \code{gg}.
#'
#' @importFrom GGally ggpairs
#'
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ggp_pairs <- function(data, columns, color_var, pal, thm, titles) {
  # useful line when using a loop
  if (is.function(thm)) thm <- thm()

  # draw the density lines on the diagonal
  myline <- function(data, mapping) {
    ggplot2::ggplot(data = data, mapping = mapping) +
      ggplot2::geom_density(linewidth = 1)
  }

  data |> GGally::ggpairs(
    columns = columns,
    mapping = ggplot2::aes(color = .data[[color_var]]),
    diag = list(continuous = myline)
  ) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::labs(
      title = titles$title,
      subtitle = titles$subtitle
    ) +
    thm
}
