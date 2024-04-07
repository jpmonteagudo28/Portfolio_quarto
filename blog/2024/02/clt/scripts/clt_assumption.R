## ---------------------------------------------------------------------------------- ##
## Generating two random variables that meet the i.i.d assumption
## ---------------------------------------------------------------------------------- ##
library(here)
library(ppcor)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(ragg)

## --------------------------------------------------------------------------------- ##

xx <- seq(-5, 5, by = .01)

clrs <- c( # Creating custom color palette
  "#FFBE00", #  yellow
  "#B92F0A", #  red
  "#7C225C", #  purple
  "#792A26", #  brown
  "#242424", #  dark gray
  "#394DAA"  #  Blue
)
## Saving plot image
par(las = 1, mfrow = c(3, 1), mai = c(.5, .5, .5, .1))
## Plot graphics
plot(xx, dnorm(xx, -1),
  type = "b", lwd = 1, xlab = "", ylab = "",
  col = clrs[3], main = "Identically distributed"
)
lines(xx, dnorm(xx, -1), lty = 2, lwd = 3, pch = 18, col = clrs[1])
legend("topright",
  legend = c("A", "B"),
  col = c(clrs[3], clrs[1]), lty = 1:2, cex = 0.8
)

plot(xx, dnorm(xx, -1),
  type = "l", lwd = 2, xlab = "", ylab = "",
  col = clrs[3], main = "Shift in means"
)
lines(xx, dnorm(xx, 1), lwd = 2, col = clrs[1])

plot(xx, dnorm(xx, -1),
  type = "l", lwd = 2, xlab = "", ylab = "",
  col = clrs[3], main = "Different distributions", ylim = c(0, 0.6)
)
lines(xx, dchisq(xx, 2), lwd = 2, col = clrs[1])
## Close graphics device to save plot
## dev.copy(png,"iid.png", width = 800, height = 600)
## dev.off()
## ------------------------------------------------------------------------------------- ##
## Generating two conditionally independent random variables
## ------------------------------------------------------------------------------------ ##

## Creating function to generate random variable with defined correlation to an existing var.
complement <- function(y, rho, x) { # corr. coefficient
  if (missing(x)) x <- rgamma(length(y), 2, .5) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}
set.seed(09)
def_micro <- rpois(1050, 6) # lambda = n*p, p = .006 defective microchips manufactured by company in 12 hrs
complaints <- complement(def_micro, rho = .273) # no. complaints received on 24 hr period for devices using the microchip in question
devices <- complement(def_micro, rho = .331) # weekly no. devices processed at online retailer (+) bought (-)returned
##
## Creating function to summarize list of variables
group_summary <- function(...) {
  summaries <- lapply(list(...), summary)
  names(summaries) <- paste0("Summary_", seq_along(summaries))
  return(summaries)
}

group_summary(def_micro, devices, complaints)
## $Summary_1
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.000   4.000   6.000   6.087   8.000  14.000

## $Summary_2
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## -7.066   1.058   4.577   5.922   9.674  64.885

## $Summary_3
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## -7.2494 -0.3377  3.3125  4.5368  7.9785 34.8066
## ------------------------------------------------------------------------------- ##
## Creating function to find Spearman's rho among all variables
## ------------------------------------------------------------------------------- ##

spearman_correlation <- function(...) {
  df <- data.frame(...)
  correlation_matrix <- round(cor(df, method = "spearman"), 3)
  return(correlation_matrix)
}

spearman <- spearman_correlation(def_micro, devices, complaints)
##
##              def_micro devices complaints
## def_micro      1.000   0.378      0.297
## devices        0.378   1.000      0.099
## complaints     0.297   0.099      1.000
##
## Use ppcor package to calculate partial correlation between complaints and devices

part_cor <- ppcor::pcor.test(devices, complaints, def_micro, method = "spearman")
##      estimate  p.value  statistic    n gp   Method
## 1 -0.02075897 0.501827 -0.6718506 1050  1 spearman
## relationship between devices and complaints is technically non-existent. We can say they're conditionally independent.
## -------------------------------------------------------------------------------- ##
## Creating scatterplots to visualize relationship between variables
## -------------------------------------------------------------------------------- ##

# Custom ggplot theme to make nicer plots
# Get the font at https://fonts.google.com/specimen/Fira+Sans+Condensed
theme_nice <- function() {
  theme_minimal(base_family = "Fira Sans Condensed") +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        family = "Fira Sans Condensed Bold",
        face = "plain",
        size = rel(1.35)
      ),
      plot.subtitle = element_text(
        family = "Fira Sans Condensed Medium",
        face = "plain",
        size = rel(1.2)
      ),
      axis.title = element_text(
        family = "Fira Sans Condensed SemiBold",
        face = "plain",
        size = rel(1)
      ),
      axis.title.x = element_text(hjust = 0.5),
      axis.title.y = element_text(hjust = 0.5),
      axis.text = element_text(
        family = "Fira Sans Condensed Light",
        face = "plain",
        size = rel(0.8)
      ),
      strip.text = element_text(
        family = "Fira Sans Condensed",
        face = "bold",
        size = rel(1),
        hjust = 0
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
      axis.ticks.y =element_line(linewidth = 0.25)
    )
}

theme_set(theme_nice())

ggplot2::update_geom_defaults("label",list(family = "Fira Sans Condensed SemiBold", fontface = "plain"))
ggplot2::update_geom_defaults("text",list(family = "Fira Sans Condensed SemiBold", fontface = "plain"))

# Making df with previously created variables
df1 <- data.frame(def_micro, devices, complaints)
dev_comp_plot <- ggplot2::ggplot(df1, aes(devices, complaints)) +
  geom_point(color = clrs[2]) +
  geom_smooth(
    method = "glm",
    se = FALSE,
    color = clrs[5],
    linewidth = 1
  ) +
  labs(
    title = "Devices vs. Complaints",
    x = (NULL),
    y = (NULL)
  ) +
  theme_nice_dist() # Apply a custom theme

dev_micro_plot <- ggplot2::ggplot(df1, aes(devices, def_micro)) +
  geom_point(color = clrs[3]) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = clrs[5],
    linewidth = 1
  ) +
  labs(
    title = "Devices vs. Microchips",
    x = (NULL),
    y = (NULL)
  ) +
  theme_nice_dist() # Apply a classic theme

comp_micro_plot <-
  ggplot2::ggplot(df1, aes(complaints, def_micro)) +
  geom_point(color = clrs[6]) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = clrs[5],
    linewidth = 1
  ) +
  labs(
    title = "Complaints vs. Microchips",
    x = (NULL),
    y = (NULL)
  ) +
  theme_nice_dist() # Apply a classic theme

arranged_plots <- 
  gridExtra::grid.arrange(dev_comp_plot, dev_micro_plot, 
  comp_micro_plot, nrow = 1) ## I prefer the vertical plots rather than the horizontal ones


## Saving plots to img file in blog
here::here("blog", "2024", "02", "clt", "img")
ggsave("cond_ind.png", arranged_plots,
  path = here::here("blog", "2024", "02", "clt", "img"),
  width = 800,
  height = 600,
  units = "px",
  dpi = 72)

## ------------------------------------------------------------------------------------- ##
## Additive v.s Multiplicative random processes                                          ##
## ------------------------------------------------------------------------------------- ##
## Multiplicative Process (spread of infectious disease within pop. in 260 time steps)
## Y_{0} = Y_{t-1}*v(1+e)| t: time, v: deterministic variation, e: random variation

params <- list()
params <- within(params,{ # storing variables in list for easier manipulation
  seed <- set.seed(09)
  t <- 1:260 # no. time steps in 7-day period over 5 yrs time
  e <- rexp(260,1) # The shape of dist heavily depends on random comp. dist
  v <- pmax(rnorm(260,.8,1),0) # ratio of infected to infectors R_{t} evolving thru
                                 # time as a Gaussian process.
  init_infect <- 6 # no. of hospitalizations and pos. tests 
})

day_infect <- function(params){ # Using the prev. spec parameters
  t <- params$t # extracting objects from list for convenience
  e <- params$e
  v <- params$v
  init_infect <- params$init_infect
  day_infect <- numeric() # empty vector to store daily infected count
    for(i in seq_along(t)){
      infect <- init_infect*v[i]*(1+e[i])
      day_infect <- c(day_infect, infect)
    }
  return(day_infect)
}

daily_infect <- day_infect(params) # creating variable to store daily infect count
# --------------------------------------------------------------------------------------- #
# DO NOT USE IN BLOG POST - SOME RANDOM IDEAS
# --------------------------------------------------------------------------------------- #
# Does the mean (first moment) become stable over time?
# If mean approaches rate =1 over time we might have evidence of convergence to exp. val
# Create function to calculate pairwise mean of observations in infected count
# calculate_p.means <- function(data) {
#  mean_pairs <- numeric(length(data) / 2)  # Pre-allocate vector for efficiency
  
#  for (i in seq(1, length(data), by = 2)) {
#    pair <- mean(data[i:(i + 1)])
#    mean_pairs[(i + 1) / 2] <- pair
#  }
  
#  return(mean_pairs)
# }
# pairs <- calculate_p.means(daily_infect)
# var.infect <- sd(daily_infect) 
# Weird variability metric between obs. pairs and mean of daily_infect
# m.dist <- sqrt((mean(daily_infect)- pairs)^2/length(pairs))
# Another measure of var
# var_measure <- sqrt((mean(daily_infect)- daily_infect)^2/length(daily_infect))
# --------------------------------------------------------------------------------------- #
coeff_var <- sd(daily_infect)/mean(daily_infect)*100 #112% variability

# Checking convergence of daily_infect mean to standard normal dist mean of 0
param_sim <- list()
param_sim <- within(param_sim,{
  seed <- set.seed(9)
  n.sim <- 100 # no. simulations to run
  t <- 2600 # no. samples
  epsilon <- .05
  e <- rexp(260,1) # The shape of dist heavily depends on random comp. dist
  v <- pmax(rnorm(260,.8,1),0) # ratio of infected to infectors R_{t} evolving thru
                                 # time as a Gaussian process.
  init_infect <- 6 # no. of hospitalizations and pos. tests
})

day_infect <- function(params_sim){ # Using the prev. spec parameters
  t <- params$t # extracting objects from list for convenience
  e <- params$e
  v <- params$v
  init_infect <- params$init_infect
  day_infect <- numeric() # empty vector to store daily infected count
  for(i in seq_along(t)){
    infect <- init_infect*v[i]*(1+e[i])
    day_infect <- c(day_infect, infect)
  }
  return(day_infect)
}

## Creating plots to embed in check.convergence function
p.converge <- ggplot()


