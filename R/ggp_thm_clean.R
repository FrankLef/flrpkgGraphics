#' \pkg{ggplot2} \emph{clean theme} with light nice colors
#'
#' \pkg{ggplot2} \emph{clean theme} with light nice colors.
#'
#' The clean format uses light colors and reduces visual distraction much like
#' the \emph{tufte} ethics of graphics.
#'
#'
#' @param base_theme Function returning the base theme build upon.
#' @param base_family String of font family used.
#' @param legend List with legend parameters.
#' @param title_colr String with title's color.
#' @param strip_colr List of parameters for facets' strips colors.
#' @param strip_text List of parameters for facets' strips text.
#' @param panel_colr List of colors for the panel.
#' @param plot_colr List of colors for the plot.
#'
#' @return Object of class \code{theme}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ggp_thm_clean <- function(base_theme,
                          base_family = "Noto Sans Display",
                          legend = list(position = "none"),
                          title_colr = "midnightblue",
                          strip_colr = list(bgfill = "gainsboro"),
                          strip_text = list(face = "bold", color = "darkblue"),
                          panel_colr = list(fill = "snow", color = "snow"),
                          plot_colr = list(fill = "ghostwhite", color = "ghostwhite")) {
  checkmate::assert_function(base_theme)
  checkmate::assert_string(base_family, min.chars = 1)

  base_theme(base_family = base_family) +
    ggplot2::theme(
      title = ggplot2::element_text(color = title_colr),
      legend.position = legend$position,
      strip.text = ggplot2::element_text(
        face = strip_text$face, color = strip_text$color
      ),
      strip.background = ggplot2::element_rect(fill = strip_colr$bgfill),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = panel_colr$fill, color = panel_colr$color
      ),
      plot.background = ggplot2::element_rect(
        fill = plot_colr$fill, color = plot_colr$color
      ),
      complete = TRUE
    )
}
