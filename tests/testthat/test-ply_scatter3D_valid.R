test_that("ply_scatter3D_valid", {
  a_file <- testthat::test_path("data", "TubaSkin_projets.rds")
  # cat("\n", "a_file", "\n")
  # print(a_file)
  data <- readRDS(a_file)


  x_var <- "jobs_work_hrs_lg_hrdl"
  y_var <- "vente_achat_lg_hrdl"
  z_var <- "vente_tot_lg_hrdl"
  color_var <- "prune_id"
  colrs <- list(
    prune = c("ok" = "gainsboro", "oob" = "violetred", "outl" = "grey"),
    title = "gold",
    grid = "grey",
    zeroline = "ghostwhite",
    background = "#2a2a2b",
    grid = "ghostwhite"
  )
  fun <- \(x) ceiling(base::expm1(x))
  titles <- list(
    title = "Main title",
    x = "Heures travail", y = "Matériel", z = "Ventes"
  )


  out <- ply_scatter3D_valid(
    data,
    x_var = x_var, y_var = y_var, z_var = z_var,
    color_var = color_var, colrs = colrs, fun = fun, titles = titles
  )

  expect_s3_class(out, class = "plotly")

  expect_snapshot(
    out
  )
})

# testthat::snapshot_accept('ply_scatter3D_valid')
