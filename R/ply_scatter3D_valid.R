#' Create a 3D plot with \pkg{plotly} to show valid vs invalid data
#'
#' Create a 3D plot with \pkg{plotly} to show valid vs invalid data.
#'
#' The 3D graph will be colored based on the \code{color_var} to differentiate
#' the valid vs invalid data.
#'
#' @param data Data.frame.
#' @param x_var String with name of x variable.
#' @param y_var String with name of y variable.
#' @param z_var String with name of z variable.
#' @param color_var String with name of color variable.
#' @param colrs List with colors.
#' \describe{
#'  \item{prune}{Palette of prune flags.}
#'  \item{title}{color of title.}
#'  \item{backgroung}{Color of background.}
#' }
#' @param fun Function used to tranform values on the scale. default is i
#'   \code{dentity()}.
#' @param titles List of titles.
#'
#' @source \url{https://plotly.com/r/3d-axes/}.
#'
#' @return Object of class \code{"plotly", "htmlwidget"}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ply_scatter3D_valid <- function(data, x_var, y_var, z_var, color_var,
                                colrs, fun = identity, titles) {
  x_axis <- ply_scatter3D_valid_axis(
    data,
    var = x_var, title = titles$x, colrs = colrs, fun = fun
  )

  y_axis <- ply_scatter3D_valid_axis(
    data,
    var = y_var, title = titles$y, colrs = colrs, fun = fun
  )

  z_axis <- ply_scatter3D_valid_axis(
    data,
    var = z_var, title = titles$z, colrs = colrs, fun = fun
  )

  data |>
    plotly::plot_ly(
      x = ~ .data[[x_var]], y = ~ .data[[y_var]], z = ~ .data[[z_var]],
      marker = list(size = 2, symbol = "circle")
    ) |>
    plotly::add_markers(color = ~ .data[[color_var]], colors = colrs$prune) |>
    plotly::layout(
      title = list(text = titles$title, font = list(color = colrs$title)),
      scene = list(
        xaxis = x_axis,
        yaxis = y_axis,
        zaxis = z_axis,
        aspectmode = "cube"
      ),
      margin = list(t = -0.5, b = 0.5),
      showlegend = FALSE,
      paper_bgcolor = colrs$background
    )
}


#' Create the axis used by \code{ply_scatter3D_valid}
#'
#' Create the axis used by \code{ply_scatter3D_valid}.
#'
#' Create the axis used by \code{ply_scatter3D_valid} with labels formatted
#' using \code{fun}.
#'
#' @inheritParams ply_scatter3D_valid
#'
#' @param var String with name of the variable used for the axis.
#' @param title String with the axis' label.
#'
#' @seealso [ply_scatter3D_valid]
#'
#' @return List of arguments used by \code{plotly::layout}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
#'
ply_scatter3D_valid_axis <- function(data, var, title, colrs, fun = identity) {
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
