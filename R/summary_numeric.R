#' Simple summary of a numeric type variable
#'
#' With the given numeric column this function will return the min, Q1, median (Q2), Q3, max, mean, sd, and count of missing data.
#'
#' @param .data A tbl.
#' @param .col A numeric column that will be summarized
#' @param round A logical. If `TRUE`, the default, it will round all decimals to the hundredths position.
#'
#' @return A summarized tbl
#' @export

summary_numeric <- function(.data, .col, round = TRUE) {

  enquo_col <- rlang::enquo(.col)

  data_type <- .data %>%
    dplyr::select(!!enquo_col) %>%
    purrr::map(class) %>%
    purrr::pluck(1)

  res <- .data %>%
    dplyr::summarize(
      n        = dplyr::n(),
      min      = min(!!enquo_col, na.rm = TRUE),
      q1       = quantile(!!enquo_col, probs = 0.25, na.rm = TRUE),
      median   = median(!!enquo_col, na.rm = TRUE),
      q3       = quantile(!!enquo_col, probs = 0.75, na.rm = TRUE),
      max      = max(!!enquo_col, na.rm = TRUE),
      mean     = mean(!!enquo_col, na.rm = TRUE),
      sd       = sd(!!enquo_col, na.rm = TRUE),
      na_count = sum(is.na(!!enquo_col), na.rm = TRUE)
    )

  check <- any(data_type %in% c("Date", "POSIXct", "POSIXt"))

  if (check) {
    res <-
      res %>%
      dplyr::select(-sd)
  }

  if(round == TRUE) {
    where <- getFromNamespace("where", "tidyselect")

    res <-
      res %>%
      dplyr::mutate(
        dplyr::across(
          .cols = where(is.numeric),
          .fns  = ~ round(., digits = 2)
        )
      )
  }

  return(res)
}
