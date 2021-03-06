test_that("back slash", {
  skip_on_cran()
  # explicitly permit clipboard access in non-interactive session
  withr::local_envvar(c(CLIPR_ALLOW = TRUE))
  skip_if_not(
    clipr::clipr_available(),
    "System clipboard is not available - skipping test."
  )
  clipr::write_clip(content = "sample\\file\\path")
  expect_match(back_to_forward(render = FALSE), "sample/file/path")
})

test_that("forward slash (just paste it!)", {
  skip_on_cran()
  withr::local_envvar(c(CLIPR_ALLOW = TRUE))
  skip_if_not(
    clipr::clipr_available(),
    "System clipboard is not available - skipping test."
  )
  clipr::write_clip(content = "sample/file/path")
  expect_match(back_to_forward(render = FALSE), "sample/file/path")
})

test_that("back slash with set text", {
  expect_match(back_to_forward(text = "sample\\file\\path",
                               render = FALSE),
               "sample/file/path")
})

test_that("forward slash with set text", {
  expect_match(back_to_forward(text = "sample/file/path",
                               render = FALSE),
               "sample/file/path")
})

test_that("default render", {
  skip_on_cran()
  withr::local_envvar(c(CLIPR_ALLOW = TRUE))
  skip_if_not(
    clipr::clipr_available(),
    "System clipboard is not available - skipping test."
  )
  skip_if_not(
    rstudioapi::isAvailable(),
    "RStudio is not available - skipping test."
  )
  expect_invisible(back_to_forward(text = "sample/file/path"),
                   "sample/file/path")
})
