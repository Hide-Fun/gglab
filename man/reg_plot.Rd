% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reg_plot.R
\name{reg_plot}
\alias{reg_plot}
\title{Make regression plot.}
\usage{
reg_plot(
  .data,
  .mapping = ggplot2::aes(x = x, y = y, ymin = lwr, ymax = upr, shape = shape, ...),
  .breaks = scales::breaks_extended(n = n),
  .linetype = "dashed",
  .size,
  .xlab,
  .shape_val,
  .ylab
)
}
\arguments{
\item{.data}{data}

\item{.mapping}{mapping, wrapping by aes(), augments passing through geom_***.}

\item{.breaks}{number of break}

\item{.xlab}{title of x axis}

\item{.ylab}{title of y axis}
}
\description{
Make regression plot with intervals.
}
