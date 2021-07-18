#' Round + Preserve the Sum
#'
#' 1. Round down to the specified number of decimal places
#' 2. Order numbers by their remainder values
#' 3. Increment the specified decimal place of values with ‘k’ largest remainders, where ‘k’ is the number of values that must be incremented to preserve their rounded sum
#'
#' @param x A numeric vector to apply the algorithm to. This should be a proportion vector where all values equal 1.
#' @param digits A numeric. Indicates the number of decimal places to be used.
#'
#' @export
#'
#' @references https://www.r-bloggers.com/2016/07/round-values-while-preserve-their-rounded-sum-in-r/
#' @examples
#' sum(c(0.333, 0.333, 0.334))
#'
#' sum(round(c(0.333, 0.333, 0.334), 2))
#'
#' sum(round_preserve_sum(c(0.333, 0.333, 0.334), 2))
round_preserve_sum <- function(x, digits = 4) {
  up <- 10^digits

  x <- x * up

  y <- floor(x)

  indices <- tail(order(x - y), round(sum(x)) - sum(y))

  y[indices] <- y[indices] + 1

  return(y / up)
}
