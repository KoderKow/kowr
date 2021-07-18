#' @title Backslash to Forward Slash
#'
#' @description This function is primary for windows users. When copying file paths on windows they will contain backslashes, "\", which will throw an error in R because "\" is used for escape characters. Due to how R reads "\", this function reads the filepath directly from the clipboard. The user can either run *back_to_forward()* in the console to get the path with forward slashes, or put *back_to_forward()* on a hotkey for instant posting within the script.
#'
#' @details Primarily for Windows users. When copying file paths in Windows it will use a backslash to seperate files, this will throw an error in R. The process of manually changing the backslashe to forward slashes can get highly repetitive. The `back_to_forward()` function will take what is in the users clipboard and paste the path with the wanted forward slashes. There are two approaches to this; 1) run `back_to_forward()` in the console to retreive the string version with forward slashes of the path, or 2) Assign a hotkey to the addin `Back to Forward Slash`. Shout out to the `reprex` package for testing `clipr` methods.
#'
#' @param text A String. Default uses the text in your clipboard. This should not be altered from the default. Exists primarily for the sake of internal testing.
#' @param render A Logical. Defaults to `TRUE`. Exists primarily for the sake of internal testing.
#'
#' @return A string. A file path that is compatible with R.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example path in clipboard:
#' # C:\Documents\Newsletters\Summer2018.csv
#' back_to_forward()
#' }
back_to_forward <- function(text = clipr::read_clip(), render = TRUE) {
  if (stringr::str_detect(text, "\\\\")) {
    text <- gsub(
      pattern = "\\\\",
      replacement = "/",
      x = text
    )
  }

  if (render) {
    rstudioapi::insertText(text)
  } else {
    text
  }

  return(invisible(text))
}
