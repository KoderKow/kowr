#' @title Round Up
#'
#' @description Round a number up to the number place second to the left. This is useful for reports that need rounded minimums and maximums.
#'
#' @param x A numeric value.
#' @param format A boolean. Takes three different options to round up and stylize:
#'   - normal: The default, return the number with no formatting.
#'   - dollar: Return the number with a leading dollar sign along with commas.
#'   - comma: Return the number with commas.
#'
#' @return A numeric, or a string if format is not "normal".
#' @export
#'
#' @examples
#' \dontrun{
#' # Round a number up
#' round_up(123456)
#' # Number with only 2 numeric places:
#' round_up(19, format = "dollar")
#' }
round_up <- function(x, format = "normal") {
  if (nchar(x) > 2) {
    round_to <- as.numeric(paste0(1, paste0(rep(0, nchar(x) - 2), collapse = ""), collapse = ""))

    round_value <- ceiling(x / round_to) * round_to
  } else {
    round_value <- ceiling(x / 10) * 10
  }

  if (format == "normal") {
    round_value
  } else if (format == "comma") {
    scales::comma(round_value)
  } else if (format == "dollar") {
    scales::dollar(round_value)
  } else {
    stop("Please choose between c(\"normal\", \"comma\", \"dollar\") for the format.")
  }
}
