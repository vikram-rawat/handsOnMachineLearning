library(ggplot2)
library(gganimate)
library(colorspace)

dsamp <- diamonds[1 + 1:1000 * 50, ]

gganimate::rend
ggplot(dsamp, aes(carat, price, color = cut)) + 
  geom_point(size = 5L) +
  scale_color_discrete_sequential(palette = "Purples 3", nmax = 6, order = 2:6) +
  theme_bw() +
  transition_layers()
