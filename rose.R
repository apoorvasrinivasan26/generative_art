

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)




lines = tibble(
  x = seq(0, 35, by = .5),
  xend = x,
  y =  rep(0, 71),
  yend = rnorm(71, 10, 1),
  cat = sample(c(1, 2, 3), size = 71, replace = TRUE),
  dot_size = runif(71, 12, 12.01))




dots = lines %>%
  select(x, yend, cat, dot_size)


p1 = ggplot() +
  geom_segment(data = lines,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               color = "pink4", size = 0.3) +
  geom_point(data = dots,
             aes(x = x, y = yend,
             color = as.factor(cat), size = dot_size)) +
  #scale_size_continuous(range = c(3, 7)) +
  scale_color_manual(values = c("palevioletred2", "pink1", "deeppink")) +
  ylim(-2, 15) +
  coord_polar() +
  theme(
    plot.background = element_rect(
      fill = "lavenderblush1"),
    panel.background = element_rect(
      fill = "lavenderblush1"),
    panel.grid = element_blank(),
    plot.caption = element_text(
      family = "Open Sans",
      size = 6,
      color = "white"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

p1





