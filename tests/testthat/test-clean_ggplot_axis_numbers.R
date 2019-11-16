test_that("clean numbers", {
  x <- c(1, 100, 1000, 10000, 100000, 1000000, 1000000000, 100000000000)
  expect_equal(
    clean_ggplot_axis_numbers(x),
    c("1", "100", "1K", "10K", "100K", "1M", "1B", "100B")
  )
})

test_that("negative numbers work", {
  x <- c(-1000, -1000000)
  expect_equal(
    clean_ggplot_axis_numbers(x),
    c("-1K", "-1M")
  )
})

test_that("add prefix", {
  x <- c(1000, 2000, 10000)
  expect_equal(
    clean_ggplot_axis_numbers(x, prefix = "$"),
    c("$1K", "$2K", "$10K")
  )
})
