#' Import Utility Functions
#'
#' @param name A character. Name of the file.
#'
#' @export
use_kowr_utils <- function(name = "utils_kowr") {
  new_path <- fs::path(
    "R",
    name,
    ext = "R"
  )

  usethis::use_template(
    template = "utils.R",
    save_as = new_path,
    package = "kowr",
    open = TRUE
  )

  use_dependency("magrittr", "Imports")

  ## Forgot to bring in prompt_user. Temp comment out
  # if (any(stringr::str_detect(fs::dir_ls("R"), "utils-pipe\\.R"))) {
  #   if (prompt_user("{crayon::red(cli::symbol$bullet)} 'use_utils_cla' brings in the '%>%' operator for you. The 'utils-pipe.R' file in '~/R/' is no longer needed. Would you like to remove it?")) {
  #     fs::file_delete("R/utils-pipe.R")
  #
  #     usethis::ui_done("File has been removed!")
  #   } else {
  #     usethis::ui_todo("Make sure you resolve one of the files; There should not be two functions that bring in the '%>%' operator.")
  #   }
  # }

  if (any(stringr::str_detect(fs::dir_ls("R"), "utils-pipe\\.R"))) {
    usethis::ui_todo("{crayon::red(cli::symbol$bullet)} 'use_utils_cla' brings in the '%>%' operator for you. Make sure you resolve one of the files; There should not be two functions that bring in the '%>%' operator.")
  }

  usethis::ui_done("File created at: {usethis::ui_value(new_path)}")
}

use_dependency <- getFromNamespace("use_dependency", "usethis")
