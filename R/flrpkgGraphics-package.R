#' Graphics utilities used by Ephel
#'
#' Graphics utilities used by Ephel.
#'
#' Custom functions and objects used for graphics such as themes, plots, etc.
#'
#' @section flrpkgGraphics functions:
#' The functions ...
#'
#' @docType package
#' @name flrpkgGraphics
#'
#' @importFrom rlang .data :=
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

globalVariables(c("med"))
# used by geom_density and geom_histogram
globalVariables(c("scaled", "count"))
# used by ggparty
globalVariables(c("id", "splitvar", "nodesize"))
