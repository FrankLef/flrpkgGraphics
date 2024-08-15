test_that("ggp_noncompliant_tree_title", {
  a_file <- testthat::test_path("data", "TubaSkin_projets.rds")
  set.seed(7703L)
  data <- readRDS(a_file) |>
    dplyr::slice_sample(prop = 0.2)

  main <- "Decision Tree of Non-Compliant Projects"
  submain <- "projects"
  out <- ggp_noncompliant_tree_title(
    data,
    prune_var = "clus_label",
    main = main,
    submain = submain
  )

  target <- list(
    title = main,
    subtitle = sprintf("%d non-compliant %s.", nrow(data), submain)
  )

  expect_identical(out, target)
})


test_that("ggp_noncompliant_tree", {
  a_file <- testthat::test_path("data", "TubaSkin_ctree.rds")
  ctree <- readRDS(a_file)
  expect_s3_class(ctree, class = "constparty")

  pal_prune <- paletteer::paletteer_d("ggthemes::calc")

  out <- ggp_noncompliant_tree(ctree,
    fill_var = "clus_label",
    fill_colrs = pal_prune
  )

  expect_s3_class(out, class = "gg")

  expect_snapshot(
    ggplot2::layer_data(out)
  )
})

# testthat::snapshot_accept('ggp_noncompliant_tree')
