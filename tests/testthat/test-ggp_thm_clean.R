test_that("ggp_thm_clean", {
  base_thm <- ggplot2::theme_minimal
  expect_snapshot(ggp_thm_clean(base_thm))
})
