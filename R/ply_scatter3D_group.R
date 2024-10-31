#' Create a 3D plot with \pkg{plotly} showing data colored by group
#'
#' Create a 3D plot with \pkg{plotly} showing data colored by group.
#'
#' The 3D graph will be colored based on the \code{color_var} to differentiate
#' the groups.
#'
#' @param data Data frame.
#' @param x_var String with name of x variable.
#' @param y_var String with name of y variable.
#' @param z_var String with name of y variable.
#' @param color_var String with name of color variable.
#' @param colrs List with colors:
#' \describe{
#'  \item{groups}{Palette of colors for the groups.}
#'  \item{title}{Color of title.}
#'  \item{backgroung}{Color of background.}
#' }
#' @param size Size of markers.
#' @param fun Function used to transform values on the scale. Default is
#'   \code{identity()}. Often used is \code{ceiling(flrpkgTools::expm1s())}.
#' @param titles List of titles:
#' \describe{
#'  \item{title}{Main title.}
#'  \item{x, y, z}{Labels for x, y and z axis.}
#' }
#' @param show_legend Falg to indocate if the legend should appear.
#'  Default is \code{TRUE}
#'
#' @source \url{https://plotly.com/r/3d-axes/}
#'
#' @return Object of class \code{"plotly", "htmlwidget"}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ply_scatter3D_group <- function(data, x_var, y_var, z_var, color_var,
                                colrs, size, fun = identity, titles, show_legend = TRUE) {
  x_axis <- ply_scatter3D_group_axis(
    data,
    var = x_var, title = titles$x, colrs = colrs, fun = fun
  )

  y_axis <- ply_scatter3D_group_axis(
    data,
    var = y_var, title = titles$y, colrs = colrs, fun = fun
  )

  z_axis <- ply_scatter3D_group_axis(
    data,
    var = z_var, title = titles$z, colrs = colrs, fun = fun
  )

  data |>
    plotly::plot_ly(
      x = ~ .data[[x_var]], y = ~ .data[[y_var]], z = ~ .data[[z_var]],
      marker = list(size = size, symbol = "circle")
    ) |>
    plotly::add_markers(color = ~ .data[[color_var]], colors = colrs$groups) |>
    plotly::layout(
      title = list(text = titles$title, font = list(color = colrs$title)),
      scene = list(
        xaxis = x_axis,
        yaxis = y_axis,
        zaxis = z_axis,
        aspectmode = "cube"
      ),
      margin = list(t = -0.5, b = 0.5),
      showlegend = show_legend,
      paper_bgcolor = colrs$background
    )
}


#' Create the axis used by \code{ply_scatter3D_group}
#'
#' Create the axis used by \code{ply_scatter3D_group}.
#'
#' Create the axis used by \code{ply_scatter3D_group} with labels formatted
#' using \code{fun}.
#'
#' @inheritParams ply_scatter3D_group
#'
#' @param data Data frame.
#' @param var String with name of the variable used for the axis.
#' @param title String with the axis' label.
#' @param colrs List of colors with the following items:
#' \describe{
#'  \item{title}{Color of axes label.}
#'  \item{grid}{Color of grid.}
#'  \item{zeroline}{Color of zero line.}
#' }
#'
#'
#' @seealso [ply_scatter3D_group]
#'
#' @return List of arguments used by \code{plotly::layout}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
#'
ply_scatter3D_group_axis <- function(data, var, title, colrs, fun = identity) {
  the_breaks <- scales::breaks_width(width = 1)(range(data[[var]]))
  the_labels_val <- fun(the_breaks)
  the_labels <- scales::label_number_auto()(the_labels_val)
  list(
    title = list(text = title, font = list(color = colrs$title)),
    gridcolor = colrs$grid,
    zerolinecolor = colrs$zeroline,
    tickmode = "array",
    tickvals = the_breaks,
    ticktext = the_labels
  )
}
