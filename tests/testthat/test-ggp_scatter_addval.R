test_that("ggp_scatter_addval", {
  a_file <- testthat::test_path("data", "TubaSkin_projets.rds")
  set.seed(7727L)
  data <- readRDS(a_file) |>
    dplyr::slice_sample(prop = 0.2)


  pal <- paletteer::paletteer_d("ggthemes::few_Dark", direction = -1)

  titles <- list(
    title = "Vente vs Valeur ajoutée",
    subtitle = "Zone rouge a une valeur ajoutée négative.",
    x = "Valeur ajoutée / projet",
    y = "Vente / projet"
  )

  out <- data |>
    ggp_scatter_addval(
      x_var = "addvalue_lg",
      y_var = "vente_tot_lg",
      group_var = "clientgrp1_id",
      pal = pal,
      fun = expm1,
      titles = titles
    )

  expect_s3_class(out, class = "gg")

  expect_snapshot(
    ggplot2::layer_data(out)
  )
})

# testthat::snapshot_accept('ggp_scatter_addval')
