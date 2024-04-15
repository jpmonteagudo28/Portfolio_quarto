## ------------------------------- ##
## Creating Lissajous figures      ##
## ------------------------------- ##
library(gganimate)                 ##
library(tidyverse)# Loading libs   ##
library(here)                      ##
library(av)                        ##
## ------------------------------- ##

params <- data.frame(
  id <- seq(1,500,1),
  t <- seq(0,2*pi, length.out = 500) 
)

figs <- function(t){
  x1 <- sin(t*1) # We're only using perfect ratios
  y1 <- sin(t*1)
  
  x2 <- sin(t*9)
  y2 <- sin(t*8)
  
  x3 <- sin(t*5)
  y3 <- sin(t*4)
  
  x4 <- sin(t*4)
  y4 <- sin(t*3)
  
  x5 <- sin(t*3)
  y5 <- sin(t*2)
  
  x6 <- sin(t*5)
  y6 <- sin(t*3)
  
  x7 <- sin(t*15)
  y7 <-  sin(t*8)
  
  x8 <- sin(t*2)
  y8 <- sin(t*1)
  
  time <- seq_along(t)
  
  data.frame(
    time, 
    x1, x2, x3, x4, x5, x6, x7, x8,
    y1, y2, y3, y4, y5, y6, y7, y8
  )
}

actual_figs <- figs(params$t)

df <- bind_cols(params, actual_figs) |>
  select(everything()) |>
  pivot_longer(x1:x8, names_to = "x_group", values_to = "x") |>
  pivot_longer(y1:y8, names_to = "y_group", values_to = "y") |>
  mutate(x_group = str_remove(x_group, "x"),
         y_group = str_remove(y_group, "y")) |>
  unite("group_id", x_group, y_group, remove = FALSE)

plot <- df|>
  ggplot(aes(x = x, y = y, color = group_id, group = group_id)) +
  geom_point(size = 3) +
  geom_path() +
  facet_grid(x_group ~ y_group) +
  coord_equal() +
  guides(color = "none") +
  theme_void() +
  transition_reveal(time) +
  ease_aes("linear")

animate(plot, duration = 30, fps = 24, height = 1080, width = 1080,
        renderer = av_renderer())

anim_save(filename = "lissajous_figs.mp4",
          path = here::here("blog","2024","04","lissajous","documents"),
          height = 1080, width = 1080)
