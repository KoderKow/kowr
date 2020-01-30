#' @title Silence the Haters
#'
#' @description For situations when you want to mute \strong{known} warnings or messages in a {dplyr} chain.
#'
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#'
#' @rdname quiet_pipe
#' @export
`%shh%` <- function(lhs, rhs) {
  w <- options()$warn
  on.exit(options(warn = w))
  options(warn = -1)
  lhs_quo = rlang::quo_name(rlang::enquo(lhs))
  rhs_quo = rlang::quo_name(rlang::enquo(rhs))
  pipe = paste(lhs_quo, "%>%", rhs_quo)
  return(suppressMessages(rlang::eval_tidy(rlang::parse_quo(pipe, env = rlang::caller_env()))))
}

#' @title Not In Operator
#'
#' @description Opposite of \code{\%in\%} to avoid the \strong{!} in \code{filter(!column \%in\% element)}.
#'
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#'
#' @rdname not_in_pipe
#' @export
`%not_in%` <- function(lhs, rhs) {
  results <- match(lhs, rhs, nomatch = 0L) > 0L
  return(!results)
}

#' @title Replace Null With ...
#'
#' @description A quick method to quickly handle \code{NULL} values. Thanks Colin!
#'
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#'
#' @rdname replace_null_pipe
#' @export
`%replace_null%` <- function(lhs, rhs) {
  if(is.null(lhs)) rhs else lhs
}

#' @title Detect if Not NA
#'
#' @description Opposite of \code{is.na(...)}. Avoiding \strong{!} for cleaner code.
#'
#' @param x an R object to be tested: the default method for is.na and anyNA handle atomic vectors, lists, pairlists, and NULL.
#'
#' @export
not_na <- function(x) {
  return(!is.na(x))
}
