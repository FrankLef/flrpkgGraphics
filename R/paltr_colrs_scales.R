#' Create a named vector of colors
#'
#' Create a named vector of colors.
#'
#' The character vector is in the form \code{c("value" = "color")}. The colors
#' come from a palette provided by \pkg{paletter}.
#'
#' @param pal Name of palette available in \pkg{paletteer}.
#' @param vals Character vector of values.
#' @param direction Direction of palette. See \pkg{paletteer} for details.
#'
#' @return Named vector with color names.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
paltr_colrs_scales <- function(pal, vals, direction = 1L) {
  a_pal <- paletteer::paletteer_d(pal, direction = direction)
  a_pal <- unclass(a_pal)
  npal <- length(a_pal)
  nvals <- length(vals)
  colr_fn <- scales::col_factor(
    palette = a_pal, domain = vals, na.color = "#808080"
  )
  colr_fn(vals)
}
