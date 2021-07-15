#' All dates from the year 2000
#'
#' This dataset consists of all dates from the year 2000. There are two additional columns to help build sample plots.
#'
#' @format A data frame with 366 rows and 3 variables:
#' \describe{
#'   \item{date}{date from the year 2000}
#'   \item{x}{random value 0-100}
#'   \item{y}{random value 0-100}
#' }
#' @source \url{https://github.com/KoderKow/kowr/blob/master/data-raw/zodiac.R}
"sample_dates"

#' Colors that relate to zodiac signs
#'
#' A vector of hex colors as values and zodiac as the name value. These colors were hand picked by [ActualToilet](https://twitter.com/actualtoilet) and [KoderKow](https://twitter.com/koderkow).
#'
#' @format A named vector of length 12.
#'
#' @source \url{https://github.com/KoderKow/kowr/blob/master/data-raw/zodiac.R}
"zodiac_sign_colors"

#' Zodiac signs modern order
#'
#' Zodiac signs based on the modern calendar year, Jan-Dec.
#'
#' @format A vector of length 12.
#'
#' @source \url{https://github.com/KoderKow/kowr/blob/master/data-raw/zodiac.R}
"zodiac_signs_modern_order"

#' Zodiac signs roman order
#'
#' Zodiac signs based on the roman calendar year, Mar - Feb.
#'
#' @format A vector of length 12.
#'
#' @source \url{https://github.com/KoderKow/kowr/blob/master/data-raw/zodiac.R}
"zodiac_signs_roman_order"
