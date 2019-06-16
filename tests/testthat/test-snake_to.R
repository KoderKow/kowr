dat <- data.frame(
  first_column = c(1,2,3),
  second_column = c("a", "b", "c"),
  thirdColumn = c("q", "w", "e")
)

test_that("returns a dataframe (list)", {
  dat_clean <- dat %>%
    snake_to()
  expect_type(dat_clean, "list")
})

test_that("no underscores", {
  columns <- dat %>%
    snake_to(names_only = TRUE)
  underscore_detect <- any(stringr::str_detect(columns, "_"))
  expect_equal(underscore_detect, FALSE)
})

test_that("is a vector", {
  vector_test <- dat %>%
    snake_to(names_only = TRUE) %>%
    purrr::is_vector()
  expect_equal(vector_test, TRUE)
})

test_that("to title", {
  title_test <- dat %>%
    snake_to(names_only = TRUE) %>%
    stringr::str_count("[A-Z]") %>%
    sum()
  expect_equal(title_test, 5)
})

test_that("to sentence", {
  sentence_test <- dat %>%
    snake_to(format = "sentence", names_only = TRUE) %>%
    stringr::str_count("[A-Z]") %>%
    sum()
  expect_equal(sentence_test, 3)
})

test_that("to lower", {
  lower_test <- dat %>%
    snake_to(format = "lower", names_only = TRUE) %>%
    stringr::str_count("[A-Z]") %>%
    sum()
  expect_equal(lower_test, 0)
})

test_that("to upper", {
  upper_test <- dat %>%
    snake_to(format = "upper", names_only = TRUE) %>%
    stringr::str_count("[A-Z]") %>%
    sum()
  expect_equal(upper_test, 34)
})

test_that("to normal (no transformation)", {
  normal_test <- dat %>%
    snake_to(format = "normal", names_only = TRUE) %>%
    stringr::str_count("[A-Z]") %>%
    sum()
  expect_equal(normal_test, 1)
})

test_that("test for inputing a non-option", {
  expect_error(snake_to(dat, format = "Your mother was a hamster and your father smelt of elderberries."))
})
