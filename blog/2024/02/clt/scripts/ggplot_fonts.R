library(ggplot2)
library(ragg)


# Custom ggplot theme to make pretty plots
# Get the font at https://fonts.google.com/specimen/Fira+Sans+Condensed
theme_nice <- function() {
  theme_minimal(base_family = "Fira Sans Condensed") +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(family = "Fira Sans Condensed Bold", face = "plain", size = rel(1.35)),
      plot.subtitle = element_text(family = "Fira Sans Condensed Medium", face = "plain", size = rel(1.2)),
      axis.title = element_text(family = "Fira Sans Condensed SemiBold", face = "plain", size = rel(1)),
      axis.title.x = element_text(hjust = 0),
      axis.title.y = element_text(hjust = 1),
      axis.text = element_text(family = "Fira Sans Condensed Light", face = "plain", size = rel(0.8)),
      strip.text = element_text(
        family = "Fira Sans Condensed", face = "bold",
        size = rel(1), hjust = 0
      ),
      strip.background = element_rect(fill = "grey95", color = NA)
    )
}

theme_nice_dist <- function() {
  theme_nice() +
    theme(
      panel.grid = element_blank(),
      panel.spacing.x = unit(10, units = "pt"),
      axis.ticks.x = element_line(linewidth = 0.25),
      axis.text.y = element_blank()
    )
}

theme_set(theme_nice())

ggplot2::update_geom_defaults("label", list(family = "Fira Sans Condensed SemiBold", fontface = "plain"))
ggplot2::update_geom_defaults("text", list(family = "Fira Sans Condensed SemiBold", fontface = "plain"))



clrs <- c(
  "#FFBE00",  # MCRN yellow
  "#B92F0A",  # MCRN red
  "#7C225C",  # MCRN maroon
  "#792A26",  # MCRN brown
  "#242424",  # MCRN dark gray
  "#394DAA"   # Blue from MCR flag
)
  
  
  p1 <- ggplot() +
    stat_function(
      geom = "area", fill = clrs[1],
      fun = \(x) dnorm(x, mean = 4, sd = 2)) +
    xlim(c(-4, 12)) +
    labs(y = NULL) +
    facet_wrap(vars("Normal(4, 2)")) +
    theme_nice_dist()
  
  p2 <- ggplot() +
    stat_function(
      geom = "area", fill = clrs[2],
      fun = \(x) extraDistr::dlst(x, df = 3, mu = 4, sigma = 2)) +
    xlim(c(-10, 15)) +
    labs(y = NULL) +
    facet_wrap(vars("Student t(ν = 3, µ = 4, σ = 2)")) +
    theme_nice_dist()
  
  p3 <- ggplot() +
    stat_function(
      geom = "area", fill = clrs[3],
      fun = \(x) dexp(x, rate = 1/100)) +
    xlim(c(0, 1000)) +
    labs(y = NULL) +
    facet_wrap(vars("Exponential(1/100)")) +
    theme_nice_dist()
  
  p1; p2; p3