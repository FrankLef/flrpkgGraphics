test_that("write_titles", {
  data <- df_write_titles()
  the_title <- "A Title"
  the_subtitle <- sprintf("%d observations.", nrow(data))

  out <- write_titles(data,
    title = the_title,
    x = "x_data", y = "y_data"
  )
  target <- list(title = "A Title", x = "East", y = "West")
  expect_identical(out, target)


  out <- write_titles(data,
    title = the_title,
    subtitle = the_subtitle,
    x = "x_data", y = "y_data"
  )

  target <- list(
    title = the_title, subtitle = the_subtitle,
    x = "East", y = "West"
  )
  expect_identical(out, target)


  out <- write_titles(data,
    title = the_title, subtitle = the_subtitle,
    is_main = TRUE,
    x = "x_data", y = "y_data"
  )
  target <- list(
    title = sprintf("%s<br><sup>%s</sup>", the_title, the_subtitle),
    x = "East", y = "West"
  )
  expect_identical(out, target)


  out <- write_titles(data,
    title = the_title, subtitle = the_subtitle,
    is_main = FALSE,
    x = "x_data", y = "y_data",
    color = "group", fill = "group"
  )
  target <- list(
    title = the_title, subtitle = the_subtitle,
    x = "East", y = "West", color = "group", fill = "group"
  )
  expect_identical(out, target)
})
