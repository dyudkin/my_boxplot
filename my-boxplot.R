require(tidyverse)

# Summarise ---------------------------------------------------------------
my_summary <-
  function(dataset,
           x,
           y,
           ci) {
    quo_x <- sym(x)
    quo_y <- sym(y)
    ci_two_tailed <- (1 - ci)/2
    summary <- dataset %>% group_by(!!quo_x) %>%
      summarise(
        sd = sd(!!quo_y, na.rm = T),
        se = sd / sqrt(length(!!quo_y)),
        mean = mean(!!quo_y, na.rm = T),
        n = sum(!is.na(!!quo_y))
      ) %>%
      mutate(
        min = mean - qt(1 - ci_two_tailed, n - 1) * se,
        lower = mean - se,
        upper = mean + se,
        max = mean + qt(1 - ci_two_tailed, n - 1) * se
      )
    return(summary)
  }




# Graph Settings ----------------------------------------------------------
my_graph_settings <- function(text.angle, text.size) {
  my_graph_settings <-
    theme_bw(base_size = text.size) +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      axis.text =  element_text(colour = "black"),
      axis.title = element_text(face = "bold")
    )
  if (text.angle > 0) {
    my_graph_settings <-
      my_graph_settings + theme(axis.text.x = element_text(angle = text.angle, hjust = 1))
  }
  return(my_graph_settings)
}



# Boxplots ----------------------------------------------------------------
my_boxplot <-
  function(data,
           x,
           y,
           xlab = x,
           ylab = y,
           width = .8,
           fill = "#2171b5",
           alpha = .8,
           jitter.height = .1,
           ci = .95,
           points = "dotplot",
           text.angle = 0,
           text.size = 12) {
    quo_x <- sym(x)
    quo_y <- sym(y)
    summary <- my_summary(data, x, y, ci = ci)
    graph <-
      ggplot(summary, aes_string(x = x,
                                   y = "mean")) +
      geom_boxplot(
        aes_string(
          ymin = "min",
          lower = "lower",
          middle = "mean",
          upper = "upper",
          ymax = "max",
        ),
        fill = fill,
        width = width,
        stat = "identity",
        alpha = alpha
      ) +
      ylab(ylab) +
      xlab(xlab) +
      my_graph_settings(text.angle, text.size)
    if (points == "dotplot") {
      graph <- graph +
        geom_dotplot (
          data = data,
          aes_string(x = x,
                     y = y),
          pch = 21,
          color = "black",
          fill = "gray88",
          stackdir = "center",
          binaxis = "y",
          position = position_dodge(width = 2),
          dotsize = .5
        )
    }
      else if (points == "jitter") {
      graph <- graph +
        geom_jitter(
          data = data,
          aes_string(x = x,
                     y = y),
          pch = 21,
          color = "black",
          fill = "gray88",
          alpha = .7,
          position = position_jitter(width = width / 8,
                                     height = jitter.height)
        )
      
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
        )
      }
    graph$layers <- rev(graph$layers)
    
    # Get and print ANOVA
    measurevar <- y
    predictor  <- x
    f <- paste(measurevar, predictor, sep = " ~ ")
    lm <- anova(lm(f, data = data))
    print(lm)
    print(summary)
    return(graph)
  }

