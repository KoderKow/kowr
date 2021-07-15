s <- rvest::session("https://google.com")
p <- s %>% xml2::read_html()

test_that("view rvest session", {
  expect_invisible(view_rvest_session(s))
})

test_that("error if not rvest session", {
  expect_error(view_rvest_session(1))
})

test_that("view html code", {
  expect_invisible(view_html(p))
})

test_that("error if not character or xml doc", {
  expect_error(view_html(1))
})
