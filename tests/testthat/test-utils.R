## %shh%
test_that("silent", {
  expect_silent(
    x <- iris %>%
      dplyr::mutate_all(as.character) %shh%
      readr::type_convert()
  )
})

## %not_in%
test_that("not_in", {
  test <- c(1, 2, 3) %not_in% c(1, 2)
  expect_equal(test, c(FALSE, FALSE, TRUE))
})

## %replace_null%
test_that("replace_null", {
  theres_a_null <- NULL %replace_null% 1
  theres_not_a_null <- 1 %replace_null% 2
  expect_equal(theres_a_null, 1)
  expect_equal(theres_not_a_null, 1)
})

## not_na
test_that("not_na", {
  test <- not_na(c(1, NA, 3))
  expect_equal(test, c(TRUE, FALSE, TRUE))
})
