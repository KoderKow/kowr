test_that("kow_pal works", {
  pal <- kow_pallette()
  expect_is(pal, "function")
  n <- 3
  vals <- pal(n)
  expect_is(vals, "character")
  expect_equal(length(vals), n)
})

test_that("scale_fill_gdocs works", {
  expect_is(scale_fill_kow(), "ScaleDiscrete")
})

test_that("scale_colour_gdocs works", {
  expect_is(scale_color_kow(), "ScaleDiscrete")
})

test_that("scale_color_gdocs works", {
  expect_equal(scale_color_kow(), scale_color_kow())
})
