#' View rvest Session
#'
#' Handy function to see where the current rvest session is. I am not the original maker of this function. Thank you to Lorenzo Gaborini for posting this on their blog! \url{https://adventuresindata.netlify.com/post/2018-01-17-viewing-rvest-sessions/}
#'
#' @param session A rvest session object created with \code{rvest::html_session()}.
#' @export
view_rvest_session <- function(session) {
  ## Function Source: # https://adventuresindata.netlify.com/post/2018-01-17-viewing-rvest-sessions/

  ## Cast the session to character
  stopifnot(class(session) == 'session')

  s_tree <- xml2::read_html(session)
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

#' View HTML Page
#'
#' Handy function to see what the current HTML code looks like as a webpage. I am not the original maker of this function., this is code adapted from reading rvest sessions. Thank you to Lorenzo Gaborini for posting this on their blog! \url{https://adventuresindata.netlify.com/post/2018-01-17-viewing-rvest-sessions/}
#'
#' @param p A character vector of HTML code or an xml document that has been created with \code{xml2::read_html()}.
#' @export
view_html <- function(p) {

  ## Cast the session to character
  stopifnot(class(p) == "character" | any(class(p) == "xml_document"))

  p <- as.character(p)

  ## Make a temporary file, fill it with text
  temp_file <- tempfile(fileext = '.html')

  f <- file(temp_file, open = 'w')
  write(p, f)
  close(f)
  utils::browseURL(temp_file)

  ## Wait a while, then delete it
  Sys.sleep(3)
  unlink(temp_file)
}
