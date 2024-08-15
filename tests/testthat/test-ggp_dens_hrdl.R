test_that("ggp_dens_hrdl", {
  a_file <- testthat::test_path("data", "TubaSkin_projets.rds")
  # cat("\n", "a_file", "\n")
  # print(a_file)
  set.seed(7559L)
  data <- readRDS(a_file) |>
    dplyr::slice_sample(prop = 0.2)

  x_var <- "vente_tot_lg"
  flag_var <- "vente_tot_lg_oob"
  vals <- c("invalid" = "violet", "valid" = "mediumspringgreen")
  fun <- base::expm1

  out <- ggp_den_hrdl(
    data, x_var = x_var, flag_var = flag_var, vals = vals, fun = fun
  )

  expect_s3_class(out, class = "gg")

  expect_snapshot(
    ggplot2::layer_data(out)
  )
})

# testthat::snapshot_accept('ggp_dens_hrdl')
