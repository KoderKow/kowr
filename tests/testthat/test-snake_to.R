library(ggplot2)
library(magrittr)

dat <- data.frame(
  lol_game = c(1,2,3),
  lollipop = c("a", "b", "c"),
  thirdColumn = c("q", "w", "e")
)

p <- dat %>%
  ggplot(aes(lollipop, lol_game)) +
  geom_col()

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
  expect_equal(title_test, 4)
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
  expect_equal(upper_test, 26)
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

test_that("acronyms", {
  acronym_test <- dat %>%
    snake_to(format = "normal", acronym = "lol", names_only = TRUE) %>%
    paste(., collapse = " ")
  expect_equal(acronym_test, "LOL game lollipop thirdColumn")
})

test_that("ggplot", {
  p <- p %>% snake_to()
  expect_equal(p$labels$y, "Lol Game")
})

test_that("names_only and ggplot error", {
  expect_error(p %>% snake_to(names_only = TRUE))
})

test_that("character vectors", {
  expect_equal(c("hey_u") %>% snake_to(), "Hey U")
})

test_that("numeric vector error", {
  expect_error(c(1, 2, 3) %>% snake_to())
})

test_that("ggplot title", {
  p <- p %>% snake_to(ggplot_title = TRUE)
  expect_equal(p$labels$title, "Relation Between Lollipop and Lol Game")
})
