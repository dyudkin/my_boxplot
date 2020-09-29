
# Demo my_boxplot ---------------------------------------------------------
require(tidyverse)
source("my_boxplot.R")
source("my_grouped_boxplot.R")


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

ToothGrowth %>% mutate(supp = factor(supp, 
                                     labels = c("Orange Juice", 
                                                "Ascorbic Acid")),
                       dose = factor(dose)) %>% 
  my_grouped_boxplot(x = "dose",
                     y = "len",
                     group = "supp",
                     xlab = "Vitamin C Dose (mg/day)",
                     ylab = "Tooth Length") +
  labs(fill = "Delivery Method")


ggsave("Figure 7.png",
       width = 6,
       height = 5)
