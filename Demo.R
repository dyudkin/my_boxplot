
# Demo my_boxplot ---------------------------------------------------------
require(tidyverse)

# Figure 1 ----------------------------------------------------------------

ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot()

ggsave("Figure 1.png",
       width = 5,
       height = 5)


# Figure 2 ----------------------------------------------------------------
my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width")

ggsave("Figure 2.png",
       width = 5,
       height = 5)



# Figure 3 ----------------------------------------------------------------
my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width",
           points = "None")

ggsave("Figure 3a.png",
       width = 5,
       height = 5)

my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width",
           points = "jitter")

ggsave("Figure 3b.png",
       width = 5,
       height = 5)

my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width",
           points = "count")

ggsave("Figure 3c.png",
       width = 5,
       height = 5)


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

ggsave("Figure 4.png",
       width = 5,
       height = 5)

# Figure 5 ----------------------------------------------------------------
my_boxplot(iris, 
           x = "Species", 
           y = "Sepal.Width",
           width = .5) +
  coord_flip() +
  ggtitle("Sepal Width by Species") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Figure 5.png",
       width = 5,
       height = 5)

# Grouped Boxplot ---------------------------------------------------------

ToothGrowth %>% mutate(supp = factor(supp),
                       dose = factor(dose)) %>% 
  my_grouped_boxplot(x = "dose",
                     y = "len",
                     group = "supp")
ggsave("Figure 6.png",
       width = 5,
       height = 5)


ToothGrowth %>% mutate(supp = factor(supp),
                       dose = factor(dose)) %>% 
  my_grouped_boxplot(x = "dose",
                     y = "len",
                     group = "supp",
                     points = "count")

ggsave("Figure 7a.png",
       width = 5,
       height = 5)

ToothGrowth %>% mutate(supp = factor(supp),
                       dose = factor(dose)) %>% 
  my_grouped_boxplot(x = "dose",
                     y = "len",
                     group = "supp",
                     points = "jitter")

ggsave("Figure 7b.png",
       width = 5,
       height = 5)
