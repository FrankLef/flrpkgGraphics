#' Density plot for hurdle analysis
#'
#' Density plot for hurdle analysis.
#'
#' Density plot for hurdle analysis using a custom theme.
#'
#' @param data Data.fame.
#' @param x_var String to identify the x variable.
#' @param flag_var String to identify flag variable
#' @param vals Named character() with colors.
#' @param fun Function used to transform the data. Default is \code{identity}.
#' Very common to use \code{ceiling(flrpkgTools::expm1s())}.
#' @param thm The ggplot theme.
#'
#' @return Object of class \code{gg}.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
ggp_den_hrdl <- function(
    data, x_var, flag_var, vals, fun = identity, thm = ggp_thm_tufte) {
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 2)
  checkmate::assert_string(x_var)
  checkmate::assert_string(flag_var)
  checkmate::assert_character(vals, len = 2)

  id_var <- "id"

  data <- ggp_den_hrdl_df(data,
    x_var = x_var, flag_var = flag_var,
    vals = vals, id_var = id_var
  )

  stats_df <- ggp_den_hrdl_stats(data, id_var = id_var)

  stats <- list(
    "tot" = stats_df$nb[stats_df$id == "tot"],
    "valid" = stats_df$nb[stats_df$id == "valid"],
    "invalid" = stats_df$nb[stats_df$id == "invalid"],
    "pct" = stats_df$pct[stats_df$id == "invalid"]
  )

  data |> ggplot2::ggplot(
    mapping = ggplot2::aes(x = .data[[x_var]], color = .data[[id_var]], fill = .data[[id_var]])
  ) +
    ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(scaled)),
      linewidth = 1, alpha = 0.5, na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_extended(n = 7),
      labels = \(x) {
        y <- fun(x)
        scales::label_number_auto()(y)
      }
    ) +
    ggplot2::scale_fill_manual(values = vals) +
    ggplot2::scale_color_manual(values = vals) +
    thm() +
    ggplot2::labs(
      title = sprintf("Distribution of %s", x_var),
      subtitle = sprintf(
        "%d observations, %d valid, %d invalid (%.0f%%).",
        stats$tot,
        stats$valid,
        stats$invalid,
        100 * stats$pct
      ),
      x = NULL, y = NULL
    )
}

ggp_den_hrdl_df <- function(data, x_var, flag_var, vals, id_var = "id") {
  # cat("\n", "c(x_var, flag_var)", "\n")
  # print(toString(c(x_var, flag_var)))
  the_vars <- c(x_var, flag_var)
  invalid_nm <- names(vals)[1]
  valid_nm <- names(vals)[2]
  data |>
    dplyr::select(tidyselect::all_of(the_vars)) |>
    dplyr::mutate(!!id_var := valid_nm) |>
    dplyr::mutate(!!id_var := dplyr::if_else(.data[[flag_var]], invalid_nm, .data[[id_var]])) |>
    dplyr::mutate(!!id_var := forcats::as_factor(.data[[id_var]]))
}

ggp_den_hrdl_stats <- function(data, id_var = "id") {
  df <- data |>
    dplyr::mutate(!!id_var := as.character(.data[[id_var]])) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(id_var))) |>
    dplyr::summarize(
      nb = dplyr::n()
    )

  tot <- list("tot", sum(df$nb))
  names(tot) <- c(id_var, "nb")
  # print(tot)

  df <- dplyr::bind_rows(df, tot)

  df |>
    dplyr::mutate(pct = 2 * .data[["nb"]] / sum(df$nb))
}
