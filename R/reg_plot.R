#' Make regression plot.
#'
#' Make regression plot with intervals.
#' @param .data data
#' @param .mapping mapping, wrapping by aes(), augments passing through geom_***.
#' @param .breaks number of break
#' @param .xlab title of x axis
#' @param .ylab title of y axis
reg_plot = function(
  .data,
  .mapping = ggplot2::aes(x = x,
                          y = y,
                          ymin = lwr,
                          ymax = upr,
                          shape = shape,...),
  .breaks = scales::breaks_extended(n = n),
  .linetype = "dashed",
  .size,
  .xlab,
  .shape_val,
  .ylab)
{
  ggplot2::ggplot(.data, .mapping) +
    ggplot2::geom_ribbon(fill = "white",
                         colour = "black",
                         linetype = .linetype) +
    ggplot2::geom_point() +
    ggplot2::geom_line(size = .size) +
    ggplot2::scale_shape_manual(value = .shape_val) +
    ggplot2::scale_y_continuous(breaks = .breaks) +
    ggplot2::theme_classic(base_family = "Arial") +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                   axis.title.y = ggtext::element_markdown(colour = "black"),
                   plot.margin = ggplot2::margin(.1*3, 0.3*3, .1*3, .1*3, "cm"),
                   legend.position = "none",
                   axis.line = ggplot2::element_line(size = .5),
                   axis.ticks = ggplot2::element_line(size = .7)) +
    ggplot2::labs(x = .xlab,
                  y = .ylab)
}
