#' Create a list of titles and axis labels
#'
#' Create a list of titles and axis labels.
#'
#' The list will only include the axis for which a variable is defined in
#' \code{...}. The labels are managed with the package \pkg{labelled}.
#'
#' @param data Data frame.
#' @param title String with the title.
#' @param subtitle String with the subtitle.
#' @param is_main TRUE: The title and subtitle will be put together with a line
#' break as one main title. Useful when using \pkg{plotly} for example. FALSE is
#' is the default value.
#' @param ... Paired variables.
#'
#' @return List of titles and axis labels.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
write_titles <- function(data, title, subtitle = NULL, is_main = FALSE, ...) {
  # get the lebels
  the_vars <- pairlist(...)
  the_labels <- labelled::get_variable_labels(data, null_action = "fill")
  the_labels <- the_labels[unlist(the_vars)]
  names(the_labels) <- names(the_vars)

  if (is_main) {
    title <- sprintf("%s<br><sup>%s</sup>", title, subtitle)
    subtitle <- NULL
  }

  if (!is.null(subtitle)) {
    the_titles <- list(title = title, subtitle = subtitle)
  } else {
    the_titles <- list(title = title)
  }

  append(the_titles, the_labels)
}
