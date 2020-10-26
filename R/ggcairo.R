#' Save plot by useing cairo_pdf
#'
#' Save plot by useing cairo_pdf. By default, saving last plot.
#' @param .path path
#' @param .plot ggplot object
#' @param .family character, font family.
#' @param ... other augument pa
#' @export
ggcairo = function(.path, .plot = ggplot2::last_plot(), .family = "Arial", ...) {
  cairo_pdf(.path, family = .family, ...)
  print(.plot)
  dev.off()
}
