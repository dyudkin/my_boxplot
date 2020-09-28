
# Demo my_boxplot ---------------------------------------------------------
require(tidyverse)
source("my-boxplot.R")
source("my-grouped-boxplot.R")


# Figure 1 ----------------------------------------------------------------

ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot()

# Figure 2 ----------------------------------------------------------------
my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width")

# Figure 3 ----------------------------------------------------------------
my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width",
           points = "None")

my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width",
           points = "jitter")

my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width",
           points = "count")

# Figure 4 ----------------------------------------------------------------
fills <- c("#838B8B", "#E0EEEE", "#79CDCD")

mtcars %>% mutate(cyl = factor(cyl)) %>% 
  my_boxplot(x = "cyl",
             y = "mpg",
             points = "jitter",
             jitter.height = 0,
             xlab = "Cylinders",
             ylab = "Miles Per Gallon",
             fill = fills)

# Figure 5 ----------------------------------------------------------------
my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width",
           width = .5) +
  coord_flip() +
  ggtitle("Sepal Width by Species") +
  theme(plot.title = element_text(hjust = 0.5))

# Grouped Boxplot ---------------------------------------------------------

ToothGrowth %>% mutate(supp = factor(supp),
                       dose = factor(dose)) %>% 
  my_grouped_boxplot(x = "dose",
                     y = "len",
                     group = "supp")

ToothGrowth %>% mutate(supp = factor(supp),
                       dose = factor(dose)) %>% 
  my_grouped_boxplot(x = "dose",
                     y = "len",
                     group = "supp",
                     points = "count")

ToothGrowth %>% mutate(supp = factor(supp),
                       dose = factor(dose)) %>% 
  my_grouped_boxplot(x = "dose",
                     y = "len",
                     group = "supp",
                     points = "jitter")
