#' @title Remove Snake Case
#'
#' @description This is useful for when the user wants to use the columns in a presentation. Shiny, Power BI, and RMarkdown are use cases where we may want clean column names for the user to read.
#'
#' @param object A data.frame, ggplot or character vector object. A data.frame will have transformed column names. A ggplot object will have transformed X and Y axes. A character vector will have clean character names.
#' @param format A string. The desired target (default is "title") case with options including:
#'  - **title**: produces title case
#'  - **lower**: produces lower case
#'  - **normal**: do not transform the string
#'  - **sentence**: produces sentence case
#'  - **upper**: produces upper case
#'
#' @param acronyms A Character. Default `NULL`. For when acronyms exist in the column names that need to be capitalized. Pass a character vector for when there is more than one acronym. Upper and/or lower case acronyms in this parameter will be accepted. This will only capitalize the wanted words, words that may contain the acronyms letter will NOT be capitalized.
#' @param names_only A Logical. Default `FALSE`. If `TRUE`, `snake_to()` will return a vector of transformed column names.
#'
#' @return Returns a data.frame with clean names, a ggplot object with clean axes, or a vector of clean strings.
#' @export
#'
#' @examples
#' \dontrun{
#' # snake to title
#' .data %>%
#'   snake_to()
#'
#' # snake to sentence
#' .data %>%
#'   snake_to(format = "sentence")
#'
#' # snake to title, keep vector of transformed column names
#' .data %>%
#'   snake_to(names_only = TRUE)
#'
#' # snake to title, capitalize acronyms
#' acronyms <- c("pyr", "cyr")
#' .data %>%
#'   snake_to(acronyms = acronyms)
#' }
snake_to <- function(object, format = "title", acronyms = NULL, names_only = FALSE) {
  if (names_only == TRUE & object_check(object, "ggplot")) {
    stop("names_only = TRUE and an object of class 'ggplot' cannot be used together.")
  }

  if (object_check(object, "data.frame")) {
    names_cleaned <- names(object) %>%
      stringr::str_replace_all(
        pattern = "_",
        replacement = " "
      )
  } else if (object_check(object, "ggplot")) {
    list_names <- names(object$labels)

    names_cleaned <- object$labels %>%
      purrr::map(~ {
        stringr::str_replace_all(
          string = .x,
          pattern = "_",
          replacement = " "
        )
      })
  } else if (object_check(object, "character_vector")) {
    names_cleaned <- object %>%
      stringr::str_replace_all(
        pattern = "_",
        replacement = " "
      )
  } else {
    stop("Object's class is not supported. Object's class needs to be a data.frame, ggplot or a character vector.")
  }

  if (format == "title") {
    names_cleaned <- names_cleaned %>%
      stringr::str_to_title()
  } else if (format == "lower") {
    names_cleaned <- names_cleaned %>%
      stringr::str_to_lower()
  } else if (format == "normal") {
    names_cleaned
  } else if (format == "sentence") {
    names_cleaned <- names_cleaned %>%
      stringr::str_to_sentence()
  } else if (format == "upper") {
    names_cleaned <- names_cleaned %>%
      stringr::str_to_upper()
  } else {
    stop("Please choose between c(\"title\", \"sentence\", \"lower\", \"upper\", \"normal\") for the format.")
  }

  if (!is.null(acronyms)) {
    words_to_cap <- stringr::str_c(
      "\\b",
      stringr::str_to_title(acronyms),
      "\\b|\\b",
      stringr::str_to_lower(acronyms),
      "\\b|\\b",
      stringr::str_to_upper(acronyms),
      "\\b",
      collapse = "|"
    )

    names_cleaned <- names_cleaned %>%
      stringr::str_replace_all(
        pattern = words_to_cap,
        replacement = stringr::str_to_upper
      )
  }

  if (names_only | object_check(object, "character_vector")) {
    names_cleaned
  } else if (object_check(object, "data.frame")) {
    names(object) <- names_cleaned

    object
  } else if (object_check(object, "ggplot")) {
    object$labels <- names_cleaned %>%
      purrr::map(~.x) %>%
      purrr::set_names(list_names)

    object
  }
}

object_check <- function(object, class) {
  if (class == "character_vector") {
    purrr::is_vector(object) & purrr::is_character(object)
  } else {
    any(class(object) == class)
  }
}
