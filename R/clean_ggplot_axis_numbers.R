#' Scale Down Numbers for Plots
#'
#' This code is a modified from this source code: https://stackoverflow.com/questions/28159936/formatting-large-currency-or-dollar-values-to-millions-billions
#'
#' @param x A numeric.
#' @param prefix A character. This will be the leading symbol of the final value. Example being a $ symbol.
#' @export
clean_ggplot_axis_numbers <- function(x, prefix = NULL) {
  clean_x <- x %>%
    gsub("\\,", "", .) %>%
    as.numeric()

  is_neg <- ifelse(clean_x < 0, "-", "")

  clean_x <- abs(clean_x)

  numeric_range <- c(0, 1e3, 1e6, 1e9, 1e12)

  index <- findInterval(clean_x, numeric_range)

  display_value <- round(clean_x / 10^(3 * (index - 1)), 2)

  display_unit <- c("", "K", "M", "B", "T")[index]

  final_display <- stringr::str_c(is_neg, display_value, display_unit)

  if (!is.null(prefix)) final_display <- stringr::str_c(prefix, final_display)

  final_display
}
