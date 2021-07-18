#' Prep \{furrr\} Code to Run in Parallel
#'
#' @export
parallel_prep <- function() {
  future::plan(future::multisession, workers = future::availableCores() - 1)

  return(invisible())
}
