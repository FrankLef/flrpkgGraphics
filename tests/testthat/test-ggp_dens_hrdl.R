test_that("ggp_dens_hrdl", {
  a_file <- testthat::test_path("data", "TubaSkin_projets.rds")
  # cat("\n", "a_file", "\n")
  # print(a_file)
  data <- readRDS(a_file)
  # str(data)

  x_var <- "vente_tot_lg"
  flag_var <- "vente_tot_lg_oob"
  vals <- c("invalid" = "violet", "valid" = "mediumspringgreen")
  fun <- base::expm1
  thm <- ggplot2::theme_minimal

  p <- ggp_den_hrdl(
    data,
    x_var = x_var, flag_var = flag_var, vals = vals,
    fun = fun, thm = thm
  )

  expect_snapshot(
    ggplot2::layer_data(p)
  )
})
