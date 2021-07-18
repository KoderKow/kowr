#' Kow Colors
#' @export
kow_colors <- function() {
  list(
    theme = c(
      sunset_orange = "#f25f5c",
      lapis_lazuli = "#247ba0",
      green_sheen = "#70c1b3",
      mustard = "#ffe066",
      deep_koamaru = "#3b3561",
      dark_liver = "#50514f",
      sweet_brown = "#ac3931",
      slate_blue = "#6369d1",
      dark_sea_green = "#83b692",
      flax = "#e9d985",
      ucla_blue = "#586994",
      tulip = "#fe938c",
      livid = "#6699cc",
      nadeshiko_pink = "#edadc7",
      paynes_grey = "#546a76",
      mindaro = "#dbfe87"
    ),
    text_colors = c(
      black = "#1A1A1A",
      light_black = "#303030"
    )
  )
}

#' Kow Color Scales
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour kow
#' @rdname scale_kow
#' @export
scale_color_kow <- function(...) {
  ggplot2::discrete_scale("colour", "kow", kow_pallette(), ...)
}

#' @export
#' @rdname scale_kow
scale_fill_kow <- function(...) {
  ggplot2::discrete_scale("fill", "kow", kow_pallette(), ...)
}

#' Kow color palette (discrete)
#'
#' Color palettes for me.
#' This palette includes 7 colors.
#'
#' @family colour kow
#' @export
kow_pallette <- function() {
  values <- kow_colors()$theme
  names(values) <- NULL
  f <- scales::manual_pal(values)
  attr(f, "max_n") <- length(values)
  f
}
