#' Make isotope plot.
#'
#' Make isotope plot with errobar automatically. If you regulate more options, use ggiso().
#' @param .data data
#' @param .mapping mapping, wrapping by aes(), augments passing through geom_***.
#' @param .xlim range of x axis.
#' @param .ylim range of y axis.
#' @param .xlab title of x axis.
#' @param .ylab title of y axis.
#' @param .point_size size of point.
#' @param .stroke edge of point
#' @param .shape_val shape of point, passing scale_shape_manual().
#' @param .width width of errorbar.
#' @param .height height of errorbar.
#' @param .linesize width of line in errorbar.
#' @param .hjust place of label (horizontal).
#' @param .vjust place of label (vertical).
#' @param .x_breaks break of x.
#' @param .y_breaks break of y.
#' @param .family font family, default Arial.
#' @param .label_size label size.
#' @param .lab_title_size axis title.
#' @param .lab_text_size axis text.
#' @param .ratio ratio of plot.
#' @param ... passed through theme().
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' data("demo")
#'
#' by_place <- demo %>%
#'   group_by(place, label) %>%
#'   summarise(across(where(is.numeric),
#'                    list(mean = mean, sd = sd),
#'                    .names = "{.fn}_{.col}"))
#'
#' place_A <- by_place %>%
#'   filter(place == "A")
#'
#' auto_ggiso(.data = place_A,
#'            .mapping = aes(x = mean_delta13C, y = mean_delta15N,
#'                           xmin = mean_delta13C - sd_delta13C,
#'                           xmax = mean_delta13C + sd_delta13C,
#'                           ymin = mean_delta15N - sd_delta15N,
#'                           ymax = mean_delta15N + sd_delta15N,
#'                           shape = label,
#'                           label = label),
#'           .xlim = c(-36, -26),
#'           .ylim = c(-6, 4),
#'           .xlab = expression(paste(italic("δ"^{13}), "C", " (\u2030)")),
#'           .ylab = expression(paste(italic("δ"^{15}), "N", " (\u2030)")),
#'           .hjust = 0,
#'           .vjust = 0,
#'           .height = .2, .width = .2,
#'           .shape_val = c(24, 21, 16, 23),
#'           .stroke = 1,
#'           .axis_size = .8,
#'           .linesize = .8,
#'           .x_breaks = scales::breaks_extended(6),
#'           .y_breaks = scales::breaks_extended(6),
#'           .point_size = 3,
#'           .family = NULL,
#'           .label_size = 4,
#'           .lab_text_size = 15,
#'           .lab_text_size = 12,
#'           .ratio = 1/2)
auto_ggiso = function(.data,
                      .mapping,
                      .xlim, .ylim,
                      .shape_val = c(16, 17, 24),
                      .xlab, .ylab,
                      .height = .2, .width = 0.2,
                      .hjust = 0, .vjust = 0,
                      .x_breaks = scales::breaks_extended(6),
                      .y_breaks = scales::breaks_extended(6),
                      .linesize = .8,
                      .stroke = 1,
                      .point_size = 2,
                      .axis_size = .8,
                      .family = "Arial",
                      .lab_title_size = 15,
                      .lab_text_size = 12,
                      .ratio,
                      ...) {
  # make plot.
  rlt <- ggplot2::ggplot(.data, .mapping) +
      ggplot2::geom_errorbar(width = .width*(stund)*(.ratio), size = .linesize) +
      ggplot2::geom_errorbarh(height = .height, size = .linesize) +
      ggtext::geom_richtext(hjust = .hjust, vjust = .vjust,
                            fill = NA, label.color = NA,
                            size = .label_size) +
      ggplot2::geom_point(size = .point_size, fill = "white", stroke = .stroke)
    # scales and theme
  rlt2 <- rlt +
      ggplot2::scale_x_continuous(limits = .xlim,
                                  expand = c(0, 0),
                                  breaks = .x_breaks) +
      ggplot2::scale_y_continuous(limits = .ylim,
                                  expand = c(0, 0),
                                  breaks = .y_breaks) +
      ggplot2::scale_shape_manual(values = .shape_val) +
      ggplot2::theme_classic(base_family = .family) +
      ggplot2::theme(legend.position = "none",
                     axis.title = ggplot2::element_text(
                       face = "bold",
                       size = .lab_title_size),
                     axis.text = ggplot2::element_text(
                       face = "bold",
                       colour = "black",
                       size = .lab_text_size),
                     plot.margin = ggplot2::margin(.5, 1.5, .5, .5, "cm"),
                     axis.line = ggplot2::element_line(size = .axis_size),
                     axis.ticks = ggplot2::element_line(size = .axis_size),
                     ...) +
      ggplot2::ylab(.ylab) +
      ggplot2::xlab(.xlab)
  # calculate and slenderize ratio.
  stund <- (.xlim[2] - .xlim[1])/(.ylim[2] - .ylim[1])
  result <- rlt2 + ggplot2::coord_fixed(stund*.ratio)
  return(result)
}
