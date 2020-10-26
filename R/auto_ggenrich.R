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
#' @param .breaks number of breaks.
#' @param .auto_fix fix ratio of plot automatically.
#' @param .ratio ratio of plot.
#' @export
auto_ggenrich = function(
  .data,
  .data_ref,
  .mapping = aes(...),
  .mapping_ref = aes(...),
  .xlim, .ylim,
  .xlab, .ylab,
  .point_size,
  .stroke,
  .shape_val = c(16, 17, 24),
  .width = .2, .height = .2,
  .linesize,
  .linetype,
  .hjust = 0, .vjust = 0,
  .breaks,
  .auto_fix = T,
  .ratio,
  ...) {
  rlt <- ggplot2::ggplot(.data, .mapping) +
    ggplot2::geom_errorbar(width = .width, size = .linesize) +
    ggplot2::geom_errorbarh(height = .height, size = .linesize) +
    ggtext::geom_richtext(hjust = .hjust, vjust = .vjust,
                  fill = NA, label.color = NA,
                  size = 6, ...) +
    ggplot2::geom_point(size = .point_size, fill = "white", stroke = .stroke) +
    ggplot2::geom_rect(data = .data_ref, mapping = .mapping_ref,
              fill = "white", colour = "#028760") +
    ggplot2::geom_hline(yintercept = 0,
               colour = "grey",
               linetype = .linetype, size = .linesize) +
    ggplot2::geom_vline(xintercept = 0,
               colour = "grey",
               linetype = .linetype, size = .linesize)  +
    ggplot2::scale_x_continuous(limits = .xlim,
                       expand = c(0, 0),
                       breaks = scales::extended_breaks(.breaks)) +
    ggplot2::scale_y_continuous(limits = .ylim,
                       expand = c(0, 0),
                       breaks = scales::extended_breaks(.breaks)) +
    ggplot2::scale_shape_manual(values = .shape_val) +
    ggplot2::theme_classic(base_family = "Arial") +
    ggplot2::theme(legend.position = "none",
          axis.title = ggplot2::element_text(face = "bold",
                                    size = 20),
          axis.text = ggplot2::element_text(face = "bold",
                                   colour = "black",
                                   size = 15),
          plot.margin = ggplot2::margin(.5, 1.5, .5, .5, "cm"),
          axis.line = ggplot2::element_line(size = .7),
          axis.ticks = ggplot2::element_line(size = .7)) +
    ggplot2::ylab(.ylab) +
    ggplot2::xlab(.xlab)
  if(.auto_fix == T) {
    ratio <- abs(.ylim[[1]] - .ylim[[2]]/.xlim[[1]] - .xlim[[2]])
    rlt <- rlt +
      ggplot2::coord_fixed(ratio*(1/ratio)*.ratio)
  }
  return(rlt)
}


