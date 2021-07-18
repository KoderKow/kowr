#' Named Split data frame by group
#'
#' @param .tbl A tbl
#' @param ... Grouping specification, forwarded to [dplyr::group_by()]
#' @param arrange A character. If `NULL`, the default, this will sort the list alphabetically. Use `asc` or `desc`if sorting the list by count is needed.
#'
#' @return returns a named list of tibbles. Each tibble contains the rows of .tbl for the associated group and all the columns, including the grouping variables.
#' @export
#' @references https://github.com/tidyverse/dplyr/issues/4223
named_group_split <- function(.tbl, ..., arrange = NULL) {
  grouped <- dplyr::group_by(.tbl, ...)

  names <- rlang::eval_bare(rlang::expr(paste(!!!dplyr::group_keys(grouped), sep = " / ")))

  d <-
    grouped %>%
    dplyr::group_split() %>%
    rlang::set_names(names)

  if (arrange %in% c("asc", "desc") && !is.null(arrange)) {
    new_order <-
      d %>%
      purrr::map_dbl(nrow) %>%
      order()

    if (arrange == "desc") {
      new_order <- rev(new_order)
    }

    d <- d[new_order]
  } else {
    if (!is.null(arrange)) {
      usethis::ui_stop(
        "Incorrect value for{usethis::ui_value('arrange')}.
        - For ASC sorting by name (Default): {usethis::ui_value('NULL')}
        - For sorting by count: {usethis::ui_value('asc')} or {usethis::ui_value('desc')}
        "
      )
    }
  }

  return(d)
}
