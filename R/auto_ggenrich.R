#' Make enrichment factor plot.
#'
#' Make enrichment factor plot. If you regulate more options, use ggenrich().
#' @param .data data.
#' @param .data_ref data of reference autotrophic plants.
#' @param .mapping mapping, wrapping by aes(), augments passing through geom_***.
#' @param .mapping_ref mapping, reference data set.
#' @param .xlim range of x axis.
#' @param .ylim range of y axis.
#' @param .xlab title of x axis.
#' @param .ylab title of y axis.
#' @param .point_size size of point.
#' @param .stroke edge of point
#' @param .shape_val shape of point, passing scale_shape_manual().
#' @param .cross_tip width and height of errorbar.
#' @param .linesize width of line in error bar.
#' @param .linetype dashed or dotted and so on.
#' @param .hjust place of label (horizontal).
#' @param .vjust place of label (vertical).
#' @param .x_breaks break of x.
#' @param .y_breaks break of y.
#' @param .family font family, default Arial.
#' @param .label_size label size.
#' @param .lab_title_size axis title.
#' @param .lab_text_size axis text
#' @param .ratio ratio of plot.
#' @param .multi logical
#' @param .scaling logical
#' @param .scale_var numeric
#' @param ... passed through theme().
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(stringr)
#'
#' data("demo")
#'
#' # summarise data.
#' by_place <- demo %>%
#'   group_by(place, label) %>%
#'   summarise(across(where(is.numeric),
#'                    list(mean = mean, sd = sd),
#'                    .names = "{.fn}_{.col}")) %>%
#'   filter(place == "A")
#'
#' # select "place is A".
#' val <- by_place %>%
#'   filter(str_detect(label, "target"))
#'
#' # reference data.
#' ref <- by_place %>%
#'  filter(str_detect(label, "ref"))
#'
#' # make plot (default).
#' auto_ggenrich(.data = val,
#'               .data_ref = ref,
#'               .mapping = aes(x = mean_epsilon13C, y = mean_epsilon15N,
#'                              xmin = mean_epsilon13C - sd_epsilon13C,
#'                              xmax = mean_epsilon13C + sd_epsilon13C,
#'                              ymin = mean_epsilon15N - sd_epsilon15N,
#'                              ymax = mean_epsilon15N + sd_epsilon15N,
#'                              shape = label,
#'                              label = label),
#'               .mapping_ref = aes(xmin = mean_epsilon13C - sd_epsilon13C,
#'                                  xmax = mean_epsilon13C + sd_epsilon13C,
#'                                  ymin = mean_epsilon15N - sd_epsilon15N,
#'                                  ymax = mean_epsilon15N + sd_epsilon15N),
#'               .xlim = c(-5, 3),
#'               .ylim = c(-3, 5),
#'               .family = NULL,
#'               .x_breaks = seq(-5, 3, 1),
#'               .y_breaks = seq(-3, 5, 1))
#'
#' # make plot (manual).
#' auto_ggenrich(.data = val,
#'               .data_ref = ref,
#'               .mapping = aes(x = mean_epsilon13C, y = mean_epsilon15N,
#'                              xmin = mean_epsilon13C - sd_epsilon13C,
#'                              xmax = mean_epsilon13C + sd_epsilon13C,
#'                              ymin = mean_epsilon15N - sd_epsilon15N,
#'                              ymax = mean_epsilon15N + sd_epsilon15N,
#'                              shape = label,
#'                              label = label),
#'               .mapping_ref = aes(xmin = mean_epsilon13C - sd_epsilon13C,
#'                                  xmax = mean_epsilon13C + sd_epsilon13C,
#'                                  ymin = mean_epsilon15N - sd_epsilon15N,
#'                                  ymax = mean_epsilon15N + sd_epsilon15N),
#'               .xlim = c(-5, 3),
#'               .ylim = c(-3, 5),
#'               .xlab = expression(paste(italic("δ"^{13}), "C", " (\u2030)")),
#'               .ylab = expression(paste(italic("δ"^{15}), "N", " (\u2030)")),
#'               .hjust = 0, .vjust = 0,
#'               .cross_tip = .3,
#'               .stroke = 1,
#'               .linesize = .8,
#'               .axis_size = .8,
#'               .shape_val = c(24, 21, 16, 23),
#'               .x_breaks = scales::breaks_extended(6),
#'               .y_breaks = scales::breaks_extended(6),
#'               .family = NULL,
#'               .lab_title_size = 15,
#'               .lab_text_size = 12,
#'               .label_size = 4,
#'               .point_size = 3,
#'               .ratio = 1/2,
#'               .multi = F)
auto_ggenrich = function(
  .data,
  .data_ref,
  .mapping,
  .mapping_ref,
  .xlim, .ylim,
  .xlab = expression(paste(italic("ε"^{13}), "C", " (\u2030)")),
  .ylab = expression(paste(italic("ε"^{15}), "N", " (\u2030)")),
  .point_size = 3,
  .stroke = 1,
  .shape_val = c(16, 17, 24),
  .cross_tip = .15,
  .linesize = .6,
  .axis_size = .6,
  .linetype = "dashed",
  .hjust = 0, .vjust = 0,
  .x_breaks = scales::breaks_extended(6),
  .y_breaks = scales::breaks_extended(6),
  .family = "Arial",
  .label_size = 4,
  .lab_title_size = 15,
  .lab_text_size = 12,
  .ratio = 2/3,
  .multi = F,
  .scaling = F,
  .scale_var = NULL,
  ...) {
  if(.scaling == T) {
    .cross_tip <- .cross_tip*((.ylim[2] - .ylim[1])/(.scale_var[2] - .scale_var[1]))
  }
  # calculate and slenderize ratio.
  stund <- (.xlim[2] - .xlim[1])/(.ylim[2] - .ylim[1])
  # make plot.
  rlt <- ggplot2::ggplot(.data, .mapping) +
      ggplot2::geom_hline(yintercept = 0,
                          colour = "grey",
                          linetype = .linetype, size = .linesize) +
      ggplot2::geom_vline(xintercept = 0,
                          colour = "grey",
                          linetype = .linetype, size = .linesize) +
      ggplot2::geom_rect(data = .data_ref, mapping = .mapping_ref,
                         fill = NA, colour = "#028760", size = .linesize) +
      ggplot2::geom_errorbar(width = .cross_tip*(stund)*(.ratio), size = .linesize) +
      ggplot2::geom_errorbarh(height = .cross_tip, size = .linesize) +
      ggtext::geom_richtext(hjust = .hjust, vjust = .vjust,
                            fill = NA, label.color = NA,
                            size = .label_size) +
      ggplot2::geom_point(size = .point_size, fill = "white", stroke = .stroke)
  # scales and theme.
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
                     axis.title = ggplot2::element_text(face = "bold",
                                                        size = .lab_title_size),
                     axis.text = ggplot2::element_text(face = "bold",
                                                       colour = "black",
                                                       size = .lab_text_size),
                     plot.margin = ggplot2::margin(.5, 1.5, .5, .5, "cm"),
                     axis.line = ggplot2::element_line(size = .axis_size),
                     axis.ticks = ggplot2::element_line(size = .axis_size),
                     ...) +
      ggplot2::ylab(.ylab) +
      ggplot2::xlab(.xlab)
  # calculate and slenderize ratio.
  result <- rlt2 + ggplot2::coord_fixed(stund*.ratio)
  return(result)
}


