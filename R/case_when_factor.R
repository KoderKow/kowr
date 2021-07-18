#' Turn case_when output into an ordered factor
#'
#' @inheritParams dplyr::case_when
#'
#' @return An ordered factor vector of length 1 or n, matching the length of the logical input or output vectors, with the type (and attributes) of the first RHS. Inconsistent lengths or types will generate an error.
#' @export
#' @references https://stackoverflow.com/questions/49572416/r-convert-to-factor-with-order-of-levels-same-with-case-when
#'
#' @examples
#' library(dplyr)
#' mtcars2 <-
#'   mtcars %>%
#'   mutate(
#'     mpg_bin = case_when(
#'       mpg < 10 ~ "mpg < 10",
#'       mpg >= 10 & mpg < 20 ~ "10 <= mpg < 20",
#'       TRUE ~ "20 <= mpg"
#'     ),
#'     mpg_bin_factor = case_when_factor(
#'       mpg < 10 ~ "mpg < 10",
#'       mpg >= 10 & mpg < 20 ~ "10 <= mpg < 20",
#'       TRUE ~ "20 <= mpg"
#'     )
#'   ) %>%
#'   select(mpg, starts_with("mpg_bin")) %>%
#'   as_tibble()
#' mtcars2
#' class(mtcars2$mpg_bin)
#' class(mtcars2$mpg_bin_factor)
#' levels(mtcars2$mpg_bin_factor)
case_when_factor <- function(...) {
  args <- as.list(match.call())

  levels <- sapply(args[-1], function(f) f[[3]]) # extract RHS of formula

  levels <- levels[!is.na(levels)]

  factor(dplyr::case_when(...), levels = levels)
}
