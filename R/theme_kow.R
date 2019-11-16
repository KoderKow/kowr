#' Kow Theme for ggplot2
#'
#' @inheritParams ggplot2::theme_grey
#' @export
#' @family themes kow

theme_kow <- function(base_size = 12, base_family = "sans") {

  kow_black <- "#1A1A1A"
  kow_light_black <- "#404040"
  kow_light_grey <- "#666666"

  update_geom(c("col", "bar", "boxplot", "point"))

  base_kow_theme(base_size = base_size, base_family = base_family) +

    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "plain",
        size = ggplot2::rel(20 / 12),
        hjust = 0,
        colour = kow_black
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        size = ggplot2::rel(1),
        face = "plain",
        colour = kow_black
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = ggplot2::rel(1),
        face = "plain",
        colour = kow_black
      ),
      panel.background = ggplot2::element_rect(
        fill = NA,
        colour = NA
      ),
      panel.border = ggplot2::element_rect(
        fill = NA,
        colour = NA
      ),
      strip.text = ggplot2::element_text(
        hjust = 0,
        size = ggplot2::rel(1),
        colour = kow_black,
        face = "plain"
      ),
      strip.background = ggplot2::element_rect(
        colour = NA,
        fill = NA
      ),
      axis.title = ggplot2::element_text(
        face = "plain",
        colour = kow_black,
        size = ggplot2::rel(1)
      ),
      axis.text = ggplot2::element_text(
        face = "plain",
        colour = kow_black,
        size = ggplot2::rel(1)
      ),
      axis.line = ggplot2::element_line(colour = kow_black),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = kow_light_grey),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(colour = NA),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(1),
        colour = kow_black
      ),
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(1),
        colour = kow_black,
        face = "plain"
      ),
      legend.key = ggplot2::element_rect(
        fill = "white",
        colour = "white"
      ),
      legend.position = "right",
      legend.direction = "vertical",
      legend.box.background = ggplot2::element_blank()
    )
}

#' Kow Colors
#' @export
kow_colors <- function() {
  list(
    theme = c(
      blue = "#b0d8ed",
      pink = "#edb0d8",
      green = "#d8edb0",
      purple = "#b0baed",
      teal = "#b0ede4",
      orange = "#edc5b0"
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

update_geom <- function(geom) {
  purrr::map(geom, ~ {
    update_geom_defaults(
      geom = .x,
      new = if (.x == "point") {
        list(
          alpha = 0.8,
          size = 3
        )
      } else {
        list(
          color = "#1A1A1A",
          alpha = 0.8
        )
      }
    )
  })
}

base_kow_theme <- function(base_size = 12, base_family = "") {
  thm <- ggplot2::theme_grey(base_size = base_size, base_family = base_family)

  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }

    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }

  thm + ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA),
    legend.background = ggplot2::element_rect(colour = NA),
    line = ggplot2::element_line(colour = "black"),
    rect = ggplot2::element_rect(
      fill = "white",
      colour = "black"
    ),
    text = ggplot2::element_text(colour = "black")
  )
}
