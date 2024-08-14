#' Many density plots for hurdle analysis
#'
#' Many density plots for hurdle analysis.
#'
#' Many density plots for hurdle analysis all joined together using
#' \pkg{patchwork}.
#'
#' @inheritParams ggp_den_hrdl
#'
#' @param cols Named character vector where the names are the value variables
#'   and the string are the name of the falg variables.
#' @param titles List of titles used by \code{labs}.
#'
#' @return Object of class \code{gg}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ggp_den_hrdl_many <- function(
    data, cols, vals, fun = identity, thm, titles) {
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 2)
  checkmate::assert_character(cols, min.len = 1, any.missing = FALSE)
  checkmate::assert_character(vals, len = 2, any.missing = FALSE)

  out <- purrr::map2(
    .x = names(cols), .y = unname(cols),
    .f = function(x, y) {
      data |>
        flrpkgGraphics::ggp_den_hrdl(
          x_var = x, flag_var = y, vals = vals, fun = identity, thm = thm
        )
    }
  )
  names(out) <- names(cols)

  patchwork::wrap_plots(out) +
    patchwork::plot_layout(guides = "collect", ncol = 1) +
    patchwork::plot_annotation(
      title = titles$title,
      theme = ggplot2::theme(plot.title = ggplot2::element_text(color = "navy"))
    )
}
