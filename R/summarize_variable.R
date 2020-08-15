#' Simple summary of a numeric type variable
#'
#' If the column is numeric or a date this function will return the min, Q1, median (Q2), Q3, max, mean, sd (only for numeric and interger), and count of missing data. If data type is of character or factor, a count of the character will be returned alonged with p (percent of column, or group if `dplyr::group_by()` is passed) and a formatted p for a clean presentation in plots and/or tables.
#'
#' @param .data A tbl.
#' @param .col A numeric column that will be summarized
#' @param round A logical. If `TRUE`, the default, it will round all decimals to the hundredths position.
#'
#' @return A summarized tbl
#' @export
summarize_variable <- function(.data, .col, round = TRUE) {

  enquo_col <- rlang::enquo(.col)

  data_type <-
    .data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!enquo_col) %>%
    purrr::map(class) %>%
    purrr::pluck(1)

  ## Numeric data type
  if (any(data_type %in% c("numeric", "integer", "Date", "POSIXct", "POSIXt"))) {

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
  } else {
    ## Character data type
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
  }

  return(res)
}
