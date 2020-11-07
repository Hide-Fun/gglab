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
#' @param .width width of error bar.
#' @param .height height of error bar.
#' @param .linesize width of line in error bar.
#' @param .linetype dashed or dotted and so on.
#' @param .hjust place of label (horizontal).
#' @param .vjust place of label (vertical).
#' @param .x_breaks break of x.
#' @param .y_breaks break of y.
#' @param .family font family, default Arial.
#' @param .lab_title_size axis title.
#' @param .lab_text_size axis text
#' @param .auto_fix fix ratio of plot automatically.
#' @param .ratio ratio of plot.
#' @param ... passed through theme().
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' data("demo")
#'
#' # summarise data.
#' by_place <- demo %>%
#'   group_by(place, label) %>%
#'   summarise(across(where(is.numeric),
#'                    list(mean = mean, sd = sd),
#'                    .names = "{.fn}_{.col}"))
#'
#' ref <- demo %>%
#'     filter(type == "REF") %>%
#'     group_by(place, label) %>%
#'     summarise(across(where(is.numeric),
#'                      list(mean = mean, sd = sd),
#'                      .names = "{.fn}_{.col}"))
#'
#' # select "place is A".
#' place_A <- by_place %>%
#'     filter(place == "A")
#'
#' # reference data.
#' place_A_ref <- ref %>%
#'     filter(place == "A")
#'
#'
#' # make plot.
#' auto_ggenrich(.data = place_A,
#'               .data_ref = place_A_ref,
#'               .mapping = aes(x = mean_delta13C, y = mean_delta15N,
#'                              xmin = mean_delta13C - sd_delta13C,
#'                              xmax = mean_delta13C + sd_delta13C,
#'                              ymin = mean_delta15N - sd_delta15N,
#'                              ymax = mean_delta15N + sd_delta15N,
#'                              shape = label,
#'                              label = label),
#'               .mapping_ref = aes(xmin = mean_epsilon13C - sd_epsilon13C,
#'                                  xmax = mean_epsilon13C + sd_epsilon13C,
#'                                  ymin = mean_epsilon15N - sd_epsilon15N,
#'                                  ymax = mean_epsilon15N + sd_epsilon15N),
#'               .xlim = c(-36, -26),
#'               .ylim = c(-6, 4),
#'               .xlab = expression(paste(italic("δ"^{13}), "C", " (\u2030)")),
#'               .ylab = expression(paste(italic("δ"^{15}), "N", " (\u2030)")),
#'               .hjust = 0, .vjust = 0,
#'               .height = .2, .width = .2,
#'               .stroke = 1,
#'               .linesize = .8,
#'               .axis_size = .8,
#'               .shape_val = c(24, 21, 16, 23),
#'               .x_breaks = scales::breaks_extended(6),
#'               .y_breaks = scales::breaks_extended(6),
#'               .family = NULL,
#'               .lab_title_size = 15,
#'               .lab_text_size = 12,
#'               .point_size = 3,
#'               .ratio = 1/2)
auto_ggenrich = function(
  .data,
  .data_ref,
  .mapping,
  .mapping_ref,
  .xlim, .ylim,
  .xlab, .ylab,
  .point_size = 3,
  .stroke = 1,
  .shape_val = c(16, 17, 24),
  .width = .2, .height = .2,
  .linesize = .8,
  .axis_size = .8,
  .linetype = "dashed",
  .hjust = 0, .vjust = 0,
  .x_breaks = scales::breaks_extended(6),
  .y_breaks = scales::breaks_extended(6),
  .family = "Arial",
  .lab_title_size = 15,
  .lab_text_size = 12,
  .auto_fix = T,
  .ratio,
  ...) {
  if(.auto_fix == T) {
    stund <- (.xlim[2] - .xlim[1])/(.ylim[2] - .ylim[1])
    rlt <- ggplot2::ggplot(.data, .mapping) +
      ggplot2::geom_hline(yintercept = 0,
                          colour = "grey",
                          linetype = .linetype, size = .linesize) +
      ggplot2::geom_vline(xintercept = 0,
                          colour = "grey",
                          linetype = .linetype, size = .linesize) +
      ggplot2::geom_rect(data = .data_ref, mapping = .mapping_ref,
                         fill = NA, colour = "#028760", size = .linesize) +
      ggplot2::geom_errorbar(width = .width*(stund)*(.ratio), size = .linesize) +
      ggplot2::geom_errorbarh(height = .height, size = .linesize) +
      ggtext::geom_richtext(hjust = .hjust, vjust = .vjust,
                            fill = NA, label.color = NA,
                            size = 6) +
      ggplot2::geom_point(size = .point_size, fill = "white", stroke = .stroke)

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
  } else {
    rlt2 <- ggplot2::ggplot(.data, .mapping) +
      ggplot2::geom_errorbar(width = .width, size = .linesize) +
      ggplot2::geom_errorbarh(height = .height, size = .linesize) +
      ggtext::geom_richtext(hjust = .hjust, vjust = .vjust,
                            fill = NA, label.color = NA,
                            size = 6, ...) +
      ggplot2::geom_point(size = .point_size, fill = "white", stroke = .stroke) +
      ggplot2::geom_rect(data = .data_ref, mapping = .mapping_ref,
                         fill = NA, colour = "#028760") +
      ggplot2::geom_hline(yintercept = 0,
                          colour = "grey",
                          linetype = .linetype, size = .linesize) +
      ggplot2::geom_vline(xintercept = 0,
                          colour = "grey",
                          linetype = .linetype, size = .linesize) +
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
  }
  result <- rlt2 + ggplot2::coord_fixed(stund*.ratio)
  return(result)
}


