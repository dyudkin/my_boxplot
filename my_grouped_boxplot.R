require(tidyverse)

# Summarise ---------------------------------------------------------------
my_grouped_summary <-
  function(dataset,
           x,
           y,
           group) {
    quo_x <- sym(x)
    quo_y <- sym(y)
    quo_group <- sym(group)
    summary <- dataset %>% group_by(!!quo_x, !!quo_group) %>%
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
my_grouped_boxplot <-
  function(data,
           x,
           y,
           group,
           xlab = x,
           ylab = y,
           width = .8,
           fill = "#2171b5",
           alpha = .8,
           jitter.height = .1,
           points = "dotplot",
           text.angle = 0,
           text.size = 12) {
    quo_x <- sym(x)
    quo_y <- sym(y)
    quo_group <- sym(group)
    summary <- my_grouped_summary(data, x, y, group)
    graph <-
      ggplot(summary, aes_string(x = x,
                                 y = "mean",
                                 fill = group)) +
      geom_boxplot(
        aes_string(
          ymin = "min",
          lower = "lower",
          middle = "mean",
          upper = "upper",
          ymax = "max",
        ),
        #fill = fill,
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
                     y = y,
                     color = group,
                     fill = group
                     ),
          pch = 21,
          color = "black",
          stackdir = "center",
          binaxis = "y",
          position = position_dodge(width = width),
          dotsize = .5
        )
    }
      else if (points == "jitter") {
      graph <- graph +
        geom_jitter(
          data = data,
          aes_string(x = x,
                     y = y,
                     color = group,
                     fill = group),
          pch = 21,
          color = "black",
          #fill = "gray88",
          alpha = .7,
          position = position_jitterdodge(jitter.width = width / 8,
                                          jitter.height = jitter.height)
        )
    }
    else if (points == "count") {
      graph <- graph +
        geom_count (
          data = data,
          aes_string(x = x,
                     y = y,
                     color = group,
                     fill = group),
          pch = 21,
          #fill = "gray88",
          color = "black",
          position = position_dodge(width = width)
          )
    }
    graph$layers <- rev(graph$layers)
    
    graph <- graph +
      scale_fill_brewer(palette="Set1") +
      scale_color_brewer(palette="Set1")
    
    measurevar <- y
    groupvars  <- c(x, group)
    f <-
      paste(measurevar, paste(groupvars, collapse = " * "), sep = " ~ ")
    lm <- anova(lm(f, data = data))
    print(lm)
    print(summary)
    return(graph)
  }

