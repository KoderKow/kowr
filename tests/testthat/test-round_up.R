test_that("round 2 digits", {
  expect_equal(round_up(12), 20)
})

test_that("dollar sign", {
  round_value <- round_up(1234, format = "dollar")
  comma_count <- stringr::str_count(round_value, ",")
  dollar_count <- stringr::str_count(round_value, "$")
  expect_equal(comma_count + dollar_count, 2)
})

test_that("commas", {
  round_value <- round_up(1234567, format = "comma")
  comma_count <- stringr::str_count(round_value, ",")
  dollar_count <- stringr::str_count(round_value, "\\$")
  expect_equal(comma_count + dollar_count, 2)
})

test_that("error", {
  expect_error(round_value <- round_up(1, format = "this wont work"))
})
