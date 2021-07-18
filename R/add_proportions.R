#' Add proportions by group
#'
#' @inheritParams dplyr::count
#' @param y Variable to calculate proportions from.
#' @param percent_format A logical. If `TRUE`, transform the numeric decimal into a readable percent of class character as a new column; `{name}_format`. If this parameter is missing it will default to the last column of the dataset passed in.
#' @param percent_accuracy A numeric. If `percent_format` is `TRUE`, number to round the decimal to. Uses `scales::percent()` accuracy method for transformations.
#'
#' @return An object of the same type as `x`. The output has the same groups as the input.
#' @export
#'
#' @examples
#' mtcars %>%
#'   dplyr::count(cyl) %>%
#'   add_proportion(n)
add_proportion <- function(
  x,
  y,
  ...,
  name = "p",
  percent_format = FALSE,
  percent_accuracy = 0.01
) {
  passed_groups <- dplyr::groups(x)
  sym_name <- rlang::sym(name)
  y <- rlang::enquo(y)

  if (rlang::quo_is_missing(y)) {
    vars <- dplyr::tbl_vars(x)
    y_name <- vars[length(vars)]
    usethis::ui_info("Selecting by {usethis::ui_value({y_name})}")
    y <- rlang::sym(y_name)
  }

  if (length(passed_groups) > 0) {
    g <- rlang::exprs(!!!passed_groups, ...)
  } else {
    g <- rlang::exprs(...)
  }

  d <-
    x %>%
    dplyr::group_by(!!!g) %>%
    dplyr::mutate({{ sym_name }} := {{ y }} / sum({{ y }})) %>%
    {
      if (length(passed_groups) == 0) dplyr::ungroup(.) else dplyr::group_by(., !!!passed_groups)
    } %>%
    {
      if (percent_format == TRUE) dplyr::mutate(., "{{ sym_name }}_format" := scales::percent({{ sym_name }}, accuracy = percent_accuracy)) else dplyr::group_by(., !!!passed_groups)
    }

  return(d)
}
