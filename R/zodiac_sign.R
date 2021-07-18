#' Extract zodiac sign from date
#'
#' @param date A character/date in "yyyy-mm-dd" format. If value is a character the functuin will run `lubridate::ymd()` on the `date` value.
#' @param factor_order A character. Sort the factor levels by the following:
#' - roman: roman calender; Aries first (Default)
#' - modern: modern calender; Capricorn first
#' - none: return the data as a character
#'
#' @return A character value. The zodiac sign that relates to the given date
#' @export
#'
#' @examples
#' zodiac_sign("2021-07-03")
zodiac_sign <- function(date, factor_order = "roman") {
  if (is.character(date)) {
    date <- lubridate::ymd(date)
  }

  i <- cut(
    x = lubridate::month(date) * 100 + lubridate::day(date),
    breaks = c(0, 120, 218, 320, 420, 520, 621, 722, 823, 922, 1023, 1122, 1222, 1231),
    right = FALSE,
    include.lowest = TRUE
  )

  result <- c(zodiac_signs_modern_order, zodiac_signs_modern_order[1])[i]

  result <- zodiac_factor_order(result, factor_order)

  return(result)
}

#' Turn zodiac character into an ordered factor
#'
#' @inheritParams base::factor
#' @inheritParams zodiac_sign
#'
#' @return A character with the requests levels.
#' @export
#'
#' @examples
#' zodiac_factor_order("Libra")
zodiac_factor_order <- function(x, factor_order = "roman") {
  if (any(!x %in% c(zodiac_signs_roman_order, NA))) {
    stop("The input has an invalid zodiac sign.")
  }

  if (factor_order == "roman") {
    x <- factor(
      x = x,
      levels = c(zodiac_signs_roman_order, NA)
    )
  } else if (factor_order == "modern") {
    x <- factor(
      x = x,
      levels = c(zodiac_signs_modern_order, NA)
    )
  } else if (factor_order != "none") {
    stop("The value factor_order is not recognized.")
  }

  return(x)
}

#' Zodiac color palette
#'
#' Color palettes for the zodiac signs.
#' This palette includes 12 colors.
#'
#' @family zodiac
#' @export
zodiac_color_palette <- function() {
  f <- zodiac_sign_colors
  names(f) <- NULL
  f <- scales::manual_pal(f)
  attr(f, "max_n") <- length(f)

  return(f)
}

#' Discrete zodiac colors
#'
#' @param ... Extra parameters to pass to `ggplot2::discrete_scale()` when `use_factor_order` is `FALSE` (default).
#' @param use_factor_order A logical. If `TRUE`, this will assign colors based on how they match with the zodiac sign. If `FALSE` (default) then the colors will be used without looking for zodiac sign values.
#'
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' ## Use colors that match with each zodiac sign
#' p <-
#'   sample_dates %>%
#'   ## Sample 100 dates from 2020 to count
#'   sample_n(100) %>%
#'   mutate(zodiac_sign = zodiac_sign(date)) %>%
#'   ggplot() +
#'   aes(
#'     x = zodiac_sign,
#'     fill = zodiac_sign
#'   ) +
#'   geom_bar() +
#'   ## Set use_factor_order = TRUE
#'   scale_fill_zodiac(use_factor_order = TRUE)
#'
#' ## Use the colors in the palette without zodiac signs
#' p2 <-
#'   iris %>%
#'   ggplot() +
#'   aes(
#'     x = Species,
#'     fill = Species
#'   ) +
#'   geom_bar() +
#'   scale_fill_zodiac()
scale_fill_zodiac <- function(..., use_factor_order = FALSE) {
  if (use_factor_order) {
    ggplot2::scale_fill_manual(values = zodiac_sign_colors)
  } else {
    ggplot2::discrete_scale(
      aesthetics = "fill",
      scale_name = "zodiac",
      palette = zodiac_color_palette(),
      ...
    )
  }
}

#' Discrete zodiac colors
#'
#' @inheritParams scale_fill_zodiac
#'
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' ## Use colors that match with each zodiac sign
#' sample_dates %>%
#'   mutate(
#'     zodiac_sign = zodiac_sign(date)
#'   ) %>%
#'   ggplot() +
#'   aes(
#'     x = x,
#'     y = y,
#'     color = zodiac_sign
#'   ) +
#'   geom_point() +
#'   ## Set use_factor_order = TRUE
#'   scale_color_zodiac(use_factor_order = TRUE)
#'
#' ## Use the colors in the palette without zodiac signs
#' iris %>%
#'   ggplot() +
#'   aes(
#'     x = Sepal.Length,
#'     y = Sepal.Width,
#'     color = Species
#'   ) +
#'   geom_point() +
#'   scale_color_zodiac()
scale_color_zodiac <- function(..., use_factor_order = FALSE) {
  if (use_factor_order) {
    ggplot2::scale_color_manual(values = zodiac_sign_colors)
  } else {
    ggplot2::discrete_scale(
      aesthetics = "color",
      scale_name = "zodiac",
      palette = zodiac_color_palette(),
      ...
    )
  }
}
