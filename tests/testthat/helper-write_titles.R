df_write_titles <- function(n = 9L) {
  set.seed(7823L)
  data <- data.frame(
    group = factor(x = sample(letters[1:3], size = n, replace = TRUE)),
    x_data = sample(letters, size = n, replace = TRUE),
    y_data = sample(1:9, size = n, replace = TRUE),
    z_data = round(rnorm(n = n, mean = 9, sd = 2), 1)
  )
  labelled::var_label(data) <-
    list(x_data = "East", y_data = "West", z_data = "South")
  data
}
