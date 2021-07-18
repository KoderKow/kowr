#' @title Turn a nested list into a tibble
#'
#' @description This function will take a nested list and make it into a tibble. This can be helpful with nested lists that have varying structures and depths.
#'
#' @param .list A list object.
#' @param col_names A character. String that represents the names of the columns after the transformation
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' json <- '
#' {
#'   "stuff": {
#'     "buried": {
#'       "deep": [
#'         {
#'           "location": "here",
#'           "name": "Laura DeCicco",
#'           "super_power": "fixing merge conflicts",
#'           "other_secret_power": []
#'         },
#'         {
#'           "location": "here",
#'           "name": "Amanda Dobbyn",
#'           "super_power": "flight",
#'           "more_nested_stuff": 4
#'         }
#'       ],
#'       "alsodeep": 2342423234,
#'       "stilldeep": {
#'         "even_deeper": [
#'           {
#'             "location": "not here",
#'             "name": "Jim Hester",
#'             "super_power": []
#'           },
#'           {
#'             "location": "here",
#'             "name": "Christine Stawitz",
#'             "super_power": "invisibility",
#'             "more_nested_stuff": 5
#'           },
#'           {
#'             "location": "here",
#'             "name": "Isabella Velasquez",
#'             "super_power": "teleportation"
#'           }
#'         ]
#'       }
#'     }
#'   }
#' }'
#'
#' my_list <- jsonlite::fromJSON(json)
#'
#' data <- nested_list_to_tibble(my_list)
#' }
nested_list_to_tibble <- function(.list, col_names = "col_") {
  data_raw <- tibble::enframe(unlist(.list))

  n_cols_max <-
    data_raw %>%
    dplyr::pull(name) %>%
    stringr::str_split("\\.") %>%
    purrr::map_dbl(length) %>%
    max()

  nms_sep <- paste0(col_names, 1:n_cols_max)

  data_sep <-
    data_raw %>%
    tidyr::separate(
      col = name,
      into = nms_sep,
      sep = "\\.",
      fill = "right"
    )

  return(data_sep)
}
