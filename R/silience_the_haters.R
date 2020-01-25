#' Silence the Haters
#'
#' For situations when you want to mute \strong{known} warnings or messages in a {dplyr} chain.
#'
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data %shh%
#'     readr::type_convert()
#' }
`%shh%` <- function(lhs, rhs) {
  w <- options()$warn
  on.exit(options(warn = w))
  options(warn = -1)
  lhs_quo = rlang::quo_name(rlang::enquo(lhs))
  rhs_quo = rlang::quo_name(rlang::enquo(rhs))
  pipe = paste(lhs_quo, "%>%", rhs_quo)
  return(suppressMessages(rlang::eval_tidy(rlang::parse_quo(pipe, env = rlang::caller_env()))))
}
