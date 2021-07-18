#' Add a reproducibility receipt to Rmarkdown
#'
#' Adds code to your clipboard that you can use to add a "reproducibility receipt" to your Rmarkdown documents.
#'
#' @return Invisible NULL; code copied to the users clipboard.
#' @export
#' @references https://twitter.com/MilesMcBain/status/1263272935197782016?s=20
#'
#' @examples
#' \dontrun{
#' ## Add code to your clipboard to copy and paste
#' reproducibility_receipt()
#' }
reproducability_receipt <- function() {
  usethis::ui_code_block(
    '# Reproducibility Receipt

```{{r, include = FALSE}}
library(details)
```

```{{details, details.summary = "Time Info", echo = FALSE}}
Sys.time()
```

```{{details, details.summary = "Repo Info", echo = FALSE}}
git2r::repository()
```

```{{details, details.summary = "Session Info", echo = FALSE}}
sessioninfo::session_info()
```
'
  )
}

