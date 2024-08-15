#' Create a tree plot of non-compliant data
#'
#' Create a tree plot of non-compliant data.
#'
#' Create a tree plot of conditional tree of non-compliant with colors by
#' cluster. The conditional tree is computed with \code{partykit::ctree}.
#'
#' @param obj Object of class \code{constparty}.
#' @param fill_var String with name of the fill variable.
#' @param fill_colrs Palette of fill colors.
#'
#' @return Object of class \code{gg} created by \code{ggparty::ggparty()}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ggp_noncompliant_tree <- function(obj, fill_var, fill_colrs) {
  checkmate::assert_class(obj, "constparty")

  # format the tree nodes
  node_plot <- list(
    ggplot2::geom_bar(ggplot2::aes(x = "", fill = .data[[fill_var]]),
      position = ggplot2::position_fill()
    ),
    ggplot2::scale_fill_manual(values = fill_colrs),
    ggplot2::theme_void(),
    ggplot2::labs(x = NULL, y = NULL, fill = NULL)
  )
  # label nodes with ID, split variable and p value
  node_label_inner_line <- list(
    ggplot2::aes(label = paste("Node", id)),
    ggplot2::aes(label = splitvar)
  )
  # set graphical parameters for each line
  node_label_inner_gpar <- list(
    list(size = 8, col = "black", fontface = "bold"),
    list(size = 12)
  )
  # create the tree plot
  ggparty::ggparty(obj, terminal_space = 1 / 3) +
    ggparty::geom_edge() +
    ggparty::geom_edge_label(color = "darkblue", size = 4) +
    # labels for inner nodes
    ggparty::geom_node_label(ggplot2::aes(label = splitvar),
      line_list = node_label_inner_line,
      line_gpar = node_label_inner_gpar,
      ids = "inner"
    ) +
    # labels for terminal nodes
    ggparty::geom_node_label(ggplot2::aes(label = paste0("Node ", id, ", N = ", nodesize)),
      fontface = "bold",
      ids = "terminal",
      size = 3,
      # 0.01 nudge_y is enough to be above the node plot since a terminal
      # nodeplot's top (not center) is at the node's coordinates.
      nudge_y = 0.01
    ) +
    # geom_node_splitvar() +
    ggparty::geom_node_plot(
      gglist = node_plot,
      ids = "terminal",
      shared_axis_labels = TRUE,
      shared_legend = TRUE,
      legend_separator = FALSE
    ) +
    ggplot2::theme(title = ggplot2::element_text(color = "midnightblue"))
}


#' Create the title used by \code{ggp_noncompliant_tree}
#'
#' Create the title used by \code{ggp_noncompliant_tree}.
#'
#' The subtitle will include the number of observations.
#'
#' @param data Data.frame that was used to create the conditional tree.
#' @param prune_var String with the name of the prune variable.
#' @param main String with text for the main title.
#' @param submain String with text for the subtitle.
#'
#' @return List with \code{title} and \code{subtitle}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
#'
ggp_noncompliant_tree_title <- function(data, prune_var, main, submain) {
  a_title <- main
  a_subtitle <- sprintf("%d non-compliant %s.", nrow(data), submain)
  list("title" = a_title, "subtitle" = a_subtitle)
}
