library(ggplot2)
library(gganimate)
library(colorspace)

dsamp <- diamonds[1 + 1:1000 * 50,]

p <- ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(cyl))) +
  geom_boxplot() +
  # scale_fill_discrete_sequential(palette = "Purples 3", nmax = 6, order = 2:6) +
  # Here comes the gganimate code
  transition_states(gear,
                    transition_length = 2,
                    state_length = 1) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

animate(plot = p, 
        renderer = av_renderer(
          file = "some.mp4")
        )


ggplot(dsamp, aes(carat, price, color = cut)) +
  geom_point(size = 5L) +
  scale_color_discrete_sequential(palette = "Purples 3",
                                  nmax = 6,
                                  order = 2:6) +
  theme_bw() +
  transition_layers()

colorspace::choose_palette()

colourpicker::colourInput()
