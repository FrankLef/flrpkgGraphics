test_that("ggp_hist_addval", {
  a_file <- testthat::test_path("data", "TubaSkin_projets.rds")
  set.seed(7703L)
  data <- readRDS(a_file) |>
    dplyr::slice_sample(prop = 0.5)


  pal <- paletteer::paletteer_d("ggthemes::few_Dark", direction = -1)

  titles <- list(
    title = "Fréquences des valeurs ajoutée / heure",
    subtitle = NULL,
    x = "Valeur ajoutée / heure travail",
    y = "nb de projets"
  )

  out <- data |>
    ggp_hist_addval(
      x_var = "addvalue_hr_lg", group_var = "clientgrp1_id",
      pal = pal, fun = expm1, titles = titles
    )

  expect_s3_class(out, class = "gg")

  expect_snapshot(
    ggplot2::layer_data(out)
  )
})

# testthat::snapshot_accept('ggp_hist_addval')
