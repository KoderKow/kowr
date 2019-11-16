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

## Plotly facet axis titles
p <- mtcars %>%
  ggplot(aes(x = hp, y = mpg)) +
  geom_point() +
  facet_wrap(~ am)

p <- ggplotly(p)

test_that("returns x and y values in list", {
  x <- get_plotly_facet_axis_titles_values(p)

  expect_true(class(x) == "list")
  expect_length(x, 2)
})

test_that("x and y locations have changed", {
  p <- adjust_plotly_facet_axis_titles(p = p, x = -0.05, y = -0.04)
  new_x <- p$x$layout$annotations[[1]]$y
  new_y <- p$x$layout$annotations[[2]]$x

  expect_equal(new_x, -0.05)
  expect_equal(new_y, -0.04)
})
