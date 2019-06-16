#' @title Remove Snake Case
#'
#' @description This is useful for when the user wants to use the columns in a presentitive way. Shiny, Power BI, and RMarkdown are use cases where we may want clean column names for the user to read.
#'
#' @param dat A data.frame. The input data.frame.
#' @param format A string. The desired target (default is "title") case with options including:
#' \itemize{
#'  \item{`"title"` produces title case}
#'  \item{`"lower"` produces lower case}
#'  \item{`"normal"` do not transform the string}
#'  \item{`"sentence"` produces sentence case}
#'  \item{`"upper"` produces upper case}
#'  }
#' @param names_only A Logical. Default `FALSE`. If `TRUE`, `snake_to()` will return a vector of transformed column names.
#'
#' @return Returns the data.frame with clean names or a vector of strings (based on the `names_only` parameter.
#' @export
#'
#' @examples
#' \dontrun{
#' # snake to title
#' dat %>%
#'   snake_to()
#'
#' # snake to sentence
#' dat %>%
#'   snake_to(format = "sentence")
#'
#' # snake to title, keep vector of transformed column names
#' dat %>%
#'   snake_to(names_only = TRUE)
#' }
snake_to <- function(dat, format = "title", names_only = FALSE) {
  names_cleaned <- names(dat) %>%
    stringr::str_replace_all("_", " ")
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

  if (names_only) {
    names_cleaned
  } else {
    names(dat) <- names_cleaned
    dat
  }
}
