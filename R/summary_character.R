#' Simple summary of a character type variable
#'
#' @param .data A tbl.
#' @param .col A character column that will be summarized
#' @param round A logical. If `TRUE`, the default, it will round all decimals to the hundredths position.
#'
#' @return A summarized tbl.
#' @export
summary_character <- function(.data, .col, round = TRUE) {
  enquo_col <- rlang::enquo(.col)

  res <-
    .data %>%
    dplyr::count(!!enquo_col, sort = TRUE) %>%
    dplyr::mutate(
      !!enquo_col := forcats::fct_reorder(!!enquo_col, n, .desc = TRUE),
      p = n / sum(n),
      p_format = scales::percent(p, accuracy = 0.01)
    )

  if (round == TRUE) {
    res <-
      res %>%
      dplyr::mutate(p = round(p, 2))
  }

  return(res)
}
