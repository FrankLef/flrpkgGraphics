test_that("ggp_dens_hrdl_many", {
  a_file <- testthat::test_path("data", "TubaSkin_projets.rds")
  # cat("\n", "a_file", "\n")
  # print(a_file)

  set.seed(7523L)
  data <- readRDS(a_file) |>
    dplyr::slice_sample(prop = 0.5)
  # str(data)

  cols <- c(
    "vente_tot_lg" = "vente_tot_lg_oob",
    "vente_qte_lg" = "vente_qte_lg_oob",
    "jobs_work_hrs_lg" = "jobs_work_hrs_lg_oob"
  )
  vals <- c("invalid" = "violet", "valid" = "mediumspringgreen")
  fun <- base::expm1
  thm <- ggplot2::theme_minimal()
  titles <- list(title = "Main Title", subtitle = "A subtitle")

  out <- ggp_den_hrdl_many(
    data,
    cols = cols, vals = vals, fun = fun, thm = thm, titles = titles
  )

  expect_s3_class(out, class = "patchwork")

  expect_snapshot(
    ggplot2::layer_data(out)
  )
})

# testthat::snapshot_accept('ggp_dens_hrdl_many')
