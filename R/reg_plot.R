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
  .ylab)
{
  ggplot(.data, aes(x, y)) +
    geom_ribbon(aes(x, ymin = lwr, ymax = upr),
                fill = "white", colour = "black", linetype = "dotted") +
    geom_point(aes(shape = type2)) +
    geom_line(aes(x, fit), size = .5) +
    scale_shape_manual(name = "morphology", yues = c(17, 15, 16)) +
    scale_y_continuous(breaks = .breaks) +
    theme_classic(base_family = "Arial") +
    theme(axis.title = element_text(size = 10),
          axis.title.y = ggtext::element_markdown(colour = "black"),
          plot.margin = margin(.1*3, 0.3*3, .1*3, .1*3, "cm"),
          legend.position = "none",
          axis.line = element_line(size = .5),
          axis.ticks = element_line(size = .7)) +
    labs(x = .xlab,
         y = .ylab)
}
