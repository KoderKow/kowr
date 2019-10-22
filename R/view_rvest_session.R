#' View rvest Session
#'
#' Handy function to see where the current rvest session is. I am not the original maker of this function. Thank you to Lorenzo Gaborini for posting this on their blog! \url{https://adventuresindata.netlify.com/post/2018-01-17-viewing-rvest-sessions/}
#'
#' @param sesssion A rvest session object created with \code{rvest::html_session()}.
#' @export
view_rvest <- function(sesssion) {
  ## Function Source: # https://adventuresindata.netlify.com/post/2018-01-17-viewing-rvest-sessions/

  ## Cast the session to character
  stopifnot(class(sesssion) == 'session')
  s_tree <- xml2::read_html(sesssion)
  s_code <- as.character(s_tree)

  ## Make a temporary file, fill it with text
  temp_file <- tempfile(fileext = '.html')
  f <- file(temp_file, open = 'w')
  write(s_code, f)
  close(f)
  utils::browseURL(temp_file)

  ## Wait a while, then delete it
  Sys.sleep(3)
  unlink(temp_file)
}
