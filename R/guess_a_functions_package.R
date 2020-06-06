#' Where is your home?
#'
#' @return invisible
guess_a_functions_package <- function() {

  context <- rstudioapi::getActiveDocumentContext()

  id <- context$id
  selected_text <- context$selection[[1]]$text

  if (stringr::str_detect(selected_text, " ")) {
    usethis::ui_oops("Space(s) detected, please selection only the function name.")
    return(invisible(selected_text))
  }

  tidyverse_packages <- "1"

  if ("tidyverse" %in% rownames(installed.packages())) {
    ## taken from tidyverse::tidyverse_packages(), dont want to include as a dependency
    raw <- utils::packageDescription("tidyverse")$Imports
    imports <- strsplit(raw, ",")[[1]]
    parsed <- gsub("^\\s+|\\s+$", "", imports)
    tidyverse_packages <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))
    tidy_pattern <- tidyverse_packages %>%
      stringr::str_c("^", ., "$", collapse = "|")
  }

  possible_packages <- help.search(
    pattern = paste0("^", selected_text, "$"),
    agrep=FALSE
  ) %>%
    purrr::pluck("matches", "Package")

  tidy_packages <- stringr::str_subset(possible_packages, tidy_pattern, negate = TRUE)

  possible_packages <- factor(
    x = possible_packages,
    levels = c(tidyverse_packages, tidy_packages)
  ) %>%
    .[order(.)]

  possible_package <- possible_packages %>%
    stringr::str_subset("base", negate = TRUE) %>%
    .[1] %>%
    stringr::str_c(., "::", selected_text)

  if (is.na(possible_package)) {
    usethis::ui_oops("No packages could be found for this function.")
    return(invisible(selected_text))
  }

  rstudioapi::insertText(possible_package, id = id)
}
