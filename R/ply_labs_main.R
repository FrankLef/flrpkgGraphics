#' Create the string of title ans subtitle for \code{ply_scatter3D_valid}
#'
#' Create the string of title ans subtitle for \code{ply_scatter3D_valid}.
#'
#' Create the string of title ans subtitle for \code{ply_scatter3D_valid} by
#' combining \code{main} and \code{submain} with line breaks in a html string.
#'
#'
#' @param data Data.frame.
#' @param main String used for the title.
#' @param submain String used for the subtitle.
#'
#' @seealso [ply_scatter3D_valid]
#'
#' @return String with html tags..
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ply_labs_main <- function(data, main, submain) {
  a_title <- main
  a_subtitle <- sprintf("%d %s", nrow(data), submain)
  sprintf("%s<br><sup>%s</sup>", a_title, a_subtitle)
}
