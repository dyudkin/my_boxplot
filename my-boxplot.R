require("tidyverse")

# Summarise ---------------------------------------------------------------
my.summary <-
  function(dataset,
           x,
           y) {
    quo_x <- sym(x)
    quo_y <- sym(y)
    summary <- dataset %>% group_by(!!quo_x) %>%
      summarise(
        sd = sd(!!quo_y, na.rm = T),
        se = sd / sqrt(length(!!quo_y)),
        mean = mean(!!quo_y, na.rm = T),
        n = sum(!is.na(!!quo_y))
      ) %>%
      mutate(
        min = mean - qt(.975, n - 1) * se,
        lower = mean - se,
        upper = mean + se,
        max = mean + qt(.975, n - 1) * se
      )
    return(summary)
  }


# Boxplots ----------------------------------------------------------------
my.boxplot <-
  function(data,
           x,
           y,
           xlab = x,
           ylab = y,
           width = .8,
           fill = "steelblue",
           alpha = .8,
           height = .1,
           points = F,
           ...) {
    quo_x <- sym(x)
    quo_y <- sym(y)
    condensed <- my.summary(data, x, y)
    graph <-
      ggplot(condensed, aes_string(x = x, 
                                   y = "mean", 
                                   group = x)) +
      geom_boxplot(
        aes_string(
          ymin = "min",
          lower = "lower",
          middle = "mean",
          upper = "upper",
          ymax = "max"
        ),
        width = width,
        stat = "identity",
        fill = fill,
        alpha = alpha
      ) +
      ylab(ylab) +
      xlab(xlab) +
      my.graph.settings(0)
    if (points == "jitter") {
      graph <- graph +
        geom_jitter(
          data = data,
          aes_string(x = x, 
                     y = y),
          pch = 21,
          color = "black",
          fill = "gray88",
          position = position_jitter(width = width / 10, 
                                     height = width /10
                                     )
        )
      graph$layers <- rev(graph$layers)
    }
    else if (points == "count") {
      graph <- graph +
        geom_count (
          data = data,
          aes_string(x = x, 
                     y = y),
          pch = 21,
          color = "black",
          fill = "gray88"
          ) +
        guides(size = F)
      graph$layers <- rev(graph$layers)
    }
    
    measurevar <- y
    predictor  <- x
    f <- paste(measurevar, predictor, sep = " ~ ")
    lm <- anova(lm(f, data = data))
    print(lm)
    return(graph)
  }


# Graph Settings ----------------------------------------------------------
my.graph.settings <- function(x = 0, size = 12) {
  my.graph.settings <-
    theme_bw(base_size = size) +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      axis.text =  element_text(colour = "black"),
      axis.title = element_text(face = "bold")
    )
  if (x > 0) {
    my.graph.settings <-
      my.graph.settings + theme(axis.text.x = element_text(angle = x, hjust = 1))
  }
  return(my.graph.settings)
}
