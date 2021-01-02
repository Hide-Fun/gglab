library(ggplot2)
library(tidyverse)

circle <- tibble(
  x = sin(seq(0, 2 * pi, length.out = 100)),
  y = cos(seq(0, 2 * pi, length.out = 100)),
  index = 1:100,
  type = "circle"
)
spring <- circle
spring$x <- spring$x + seq(0, 1.5, length.out = 100)
spring$type <- "spring"
ggplot(rbind(circle, spring)) +
  geom_path(
    aes(x = x, y = y, group = type, alpha = index),
    show.legend = FALSE
  ) +
  facet_wrap(~ type, scales = "free_x")

create_spring <- function(x, y, xend, yend, diameter = 1, tension = 0.75, n = 50) {
  if (tension <= 0) {
    rlang::abort("`tension` must be larger than zero.")
  }
  if (diameter == 0) {
    rlang::abort("`diameter` can not be zero.")
  }
  if (n == 0) {
    rlang::abort("`n` must be greater than zero.")
  }
  # Calculate direct length of segment
  length <- sqrt((x - xend)^2 + (y - yend)^2)

  # Figure out how many revolutions and points we need
  n_revolutions <- length / (diameter * tension)
  n_points <- n * n_revolutions

  # Calculate sequence of radians and x and y offset
  radians <- seq(0, n_revolutions * 2 * pi, length.out = n_points)
  x <- seq(x, xend, length.out = n_points)
  y <- seq(y, yend, length.out = n_points)

  # Create the new data
  data.frame(
    x = cos(radians) * diameter/2 + x,
    y = sin(radians) * diameter/2 + y
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

StatSpring <- ggproto("StatSpring", Stat,
                      setup_data = function(data, params) {
                        if (anyDuplicated(data$group)) {
                          data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                        }
                        data
                      },
                      compute_panel = function(data, scales,
                                               diameter = 1,
                                               tension = 0.75,
                                               n = 50) {
                        cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
                        springs <- lapply(seq_len(nrow(data)), function(i) {
                          spring_path <- create_spring(
                            data$x[i], data$y[i],
                            data$xend[i], data$yend[i],
                            diameter = diameter,
                            tension = tension,
                            n = n
                          )
                          cbind(spring_path, unclass(data[i, cols_to_keep]))
                        })
                        do.call(rbind, springs)
                      },
                      required_aes = c("x", "y", "xend", "yend")
)

#======================================================
geom_isotope <- function(sd = F) {
  list(
    stat_summary(fun = "mean", geom = "point", fill = "grey70"),
    if (sd) {
      list(
        stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.4)
      )
    }
  )
}

data("demo")

demo <- ungroup(demo)

ggplot(demo, aes(delta13C, delta15N)) +
  stat_summary(fun = "mean", geom = "bar", fill = "grey70")
  geom_isotope()


calc_isotope <- function(x, y, errorbar = c("sd", "se"), grip) {
  if(!errorbar %in% c("sd", "se")) {
    rlang::abort("`errorbar` must select from sd, se.")
  }
  if(grip < 0) {
    rlang::abort("`grip` must be positive or 0.")
  }
  if(!is.numeric(x) || !is.numeric(y)) {
    rlang::abort("`x` and `y` must be numeric.")
  }
  if(!is.numeric(grip)) {
    rlang::abort("`grip` must be a numeric.")
  }
  x_mean <- mean(x)
  y_mean <- mean(y)
  # calculate errorbar.
  if(errorbar == "sd") {
    xmin <- x_mean - sd(x)
    xmax <- x_mean + sd(x)
    ymin <- y_mean - sd(y)
    ymax <- y_mean + sd(y)
  } else {
    xmin <- x_mean - sd(x)/sqrt(length(x))
    xmax <- x_mean + sd(x)/sqrt(length(x))
    ymin <- y_mean - sd(y)/sqrt(length(y))
    ymax <- y_mean + sd(y)/sqrt(length(y))
  }
  # data.frame.
  x <- x_mean
  y <- y_mean
  data.frame(x, y, xmin, xmax, ymin, ymax, grip)
}

data <- calc_isotope(x = rnorm(100), y = rnorm(100), errorbar = "sd", grip = 3)

ggplot(data, aes(x = x, y = y, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_point() +
  geom_errorbar(aes(width = grip)) +
  geom_errorbarh(aes(height = grip)) +
  xlim(-3 , 3) + ylim(-8, 8) +
  coord_equal()
  coord_fixed(2)
