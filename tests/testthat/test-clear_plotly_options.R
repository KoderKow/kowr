library(plotly)

p <- plot_ly(data = mtcars, x = ~mpg, type = "histogram")

clear_p <- p %>%
  clear_plotly_options(keep_logo = TRUE)

test_that("output is a plotly object", {
  expect_is(clear_p, "plotly")
})

test_that("remove all buttons", {
  number_removed <- clear_p$x$config$modeBarButtonsToRemove %>%
    unlist() %>%
    length()
  expect_equal(number_removed, 12)
})

test_that("keep logo", {
  keep_logo <- clear_p$x$config$displaylogo
  expect_true(keep_logo)
})
