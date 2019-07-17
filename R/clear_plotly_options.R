#' @title Remove Plotly Options
#'
#' @description Shortcut for removing the options on the top of a plotly plot.
#'
#' @param plotly_object A plotly object.
#' @param buttons_to_keep A string. Default is `NULL`. Use a vector of strings for multiple buttons to keep. Below is a list of items with the plotly code that corresponds to the text displayed on the plotly plot when hovering over the buttons.
#' \itemize{
#'   \item toImage: Download plot as a png
#'   \item zoom2d: Zoom
#'   \item pan2d: Pan
#'   \item select2d: Box Select
#'   \item lasso2d: Lasso Select
#'   \item zoomIn2d: Zoom in
#'   \item zoomOut2d: Zoom out
#'   \item autoScale2d: Autoscale
#'   \item resetScale2d: Reset axes
#'   \item toggleSpikelines: Toggle Spike Lines
#'   \item hoverClosestCartesian: Show closest data on hover
#'   \item hoverCompareCartesian: Compare data on hover
#' }
#' @param keep_logo A logical. Default is `FALSE`. Set to `TRUE`` to keep the "Produced with Plotly" button located in the top right of the plot.
#'
#' @return A plotly object.
#' @export
#'
#' @examples
#' \dontrun{
#' # remove all options
#' p %>%
#'   clear_plotly_options()
#'
#' # keep Download plot as a png button
#' p %>%
#'   clear_plotly_options(buttons_to_keep = "toImage")
#'
#' # keep zoom in and zoom out buttons
#' p %>%
#'   clear_plotly_options(buttons_to_keep = c("zoomIn2d", "zoomOut2d"))
#' }
clear_plotly_options <- function(plotly_object, buttons_to_keep = NULL, keep_logo = FALSE) {
  plotly_options <- c("toImage", "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian")

  remove_buttons <-
    plotly_options[!plotly_options %in% buttons_to_keep] %>%
    lapply(., function(x) x)

  plotly_object %>%
    plotly::config(
      displaylogo = keep_logo,
      modeBarButtonsToRemove = remove_buttons
    )
}
