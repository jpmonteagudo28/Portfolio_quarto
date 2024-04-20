## -------------------------------------- ##
## Day 19: Dinosaurs; 30DayChartChallenge ##
## -------------------------------------- ##
## Loading libs                           ##
library(datasauRus)                       ##
library(tidyverse)                        ##
library(gganimate)                        ##                                                    
library(showtext)                         ##
## -------------------------------------- ##

# Setting custom theme for ggplot2 graphics

clrs <- c( # Creating custom color palette
  "#e5b858", #  yellow
  "#B72025", #  red
  "#7C225C", #  purple
  "#d3d6d9", #  dark gray
  "#f9f9f9", #  light gray
  "#394DAA") #  blue

# Import Fonts (google name, R name)
font_add_google("Source Code Pro","source_code")
font_add_google("Montserrat","mont")
showtext_auto() # enable showtext font rendering

theme_blue <- function() {
  theme_minimal(base_family = "mont") +
  theme(
    plot.title = element_text(family = "mont", face = "bold", size = rel(1.35)),
    plot.subtitle = element_text(family = "mont", colour = "royalblue", face = "plain", size = rel(1)),
    plot.background = element_rect(fill = clrs[5], colour = clrs[5]),
    panel.border = element_blank(),
    panel.background = element_rect(fill = clrs[5]),
    panel.grid.major.x = element_line(colour = clrs[6], linetype = 3, linewidth = .35),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = clrs[6], linetype = 3, linewidth = .35),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = clrs[6], face = "plain", family = "mont"),
    axis.title = element_text(colour = clrs[6], family = "mont"),
    axis.ticks = element_line(colour = clrs[6]),
    # legend at the bottom 6)
    legend.position = "none"
  )
}

theme_set(theme_blue())

ggplot2::update_geom_defaults("label", list(family = "mont", fontface = "bold"))
ggplot2::update_geom_defaults("text", list(family = "mont", fontface = "plain"))


# Checking min/max for axis transform
datasaurus <- datasaurus_dozen
summary(datasaurus)

group_datasaurus <- datasaurus %>%
  group_by(dataset) %>% 
  summarize(avg = mean(x), 
            stdev = sd(x), 
            avg_y = mean(y), 
            stdev_y = sd(y))


dino <- ggplot(datasaurus, aes(x = x, y = y))+
  geom_point(size = 3, alpha = .65, 
             colour = clrs[3]) +
  labs(x = NULL,y = NULL,
       title = "Day 19 of 30 Day Chart Challenge",
       subtitle = "The T-rex",
       caption = "Always check your data graphically") +
  theme_blue() +
  transition_states(states = dataset,
                    transition_length = 2,
                    state_length = 1,
                    wrap = TRUE) +
  ease_aes("linear")

animate(dino, duration = 15, fps = 24, height = 680, width = 720,
        renderer = gifski_renderer())

anim_save(filename = "dino.gif",
          path = here::here("blog","2024","04","dinosaurs","img"))