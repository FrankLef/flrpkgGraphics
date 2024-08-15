test_that("ggp_pairs", {
  a_file <- testthat::test_path("data", "TubaSkin_projets.rds")
  set.seed(7607L)
  data <- readRDS(a_file) |>
    dplyr::slice_sample(prop = 0.5)

  cols <- cols <- c("vente_tot_lg", "vente_qte_lg", "jobs_work_hrs_lg")
  color_var <- "prune_id"
  pal <- c("lightseagreen", "violetred", "gold")
  titles <- list(
    title = "Data Analysis in Pairs",
    subtitle = sprintf("%d observations.", nrow(data))
  )

  p <- ggp_pairs(data,
    columns = cols, color_var = color_var, pal = pal, titles = titles
  )


  expect_s3_class(p, class = "ggmatrix")

  # source: https://stackoverflow.com/questions/71876162/ggally-extract-data-from-a-plot
  out <- lapply(p$plots, FUN = function(x) ggplot2::layer_data(x))
  # cat("\n", "out", "\n")
  # print(out)

  expect_snapshot(out)
})

# testthat::snapshot_accept('ggp_pairs')
