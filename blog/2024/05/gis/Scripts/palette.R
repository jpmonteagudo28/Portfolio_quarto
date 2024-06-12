## ---------------------- ##
## Creating color schemes ##
## ---------------------- ##
library(hues)
library(scales)
library(ggplot2)
library(gridExtra)
library(showtext)
## ---------------------- ##

# Creating color palette with 7 colors
pal <- c("#c62a09",
          "#ff8c44",
          "#ffce4e",
          "#008919",
          "#326683",
          "#cc4ad6",
          "#62169d")

#Plotting the color palette
swatch(pal)

## ----------------------------------------------------------------##
#| Method 2: Using the rgb() function and https://medialab.github.io/iwanthue
rgb_strings <- c("rgb(205,78,51)",
                 "rgb(190,125,67)",
                 "rgb(190,171,46)",
                 "rgb(93,157,81)",
                 "rgb(113,137,195)",
                 "rgb(172,91,196)",
                 "rgb(208,95,133)")

# Remove brackets and split into separate columns

rgb.frame <- function(string){
  rgb <- data.frame(do.call(rbind, strsplit(gsub("rgb\\(|\\)", "", string), ",")))
colnames(rgb) <- c("r", "g", "b")

# Convert columns to matrix
rgb<- sapply(rgb, as.numeric)

# Convert o values between [0,1]
rgb <- as.data.frame(rgb/255)
return(rgb)
}

# Plot color palette with increased chroma
image(1:nrow(rgb), 1, as.matrix(1:nrow(rgb)), 
      col=rgb(rgb$r, rgb$g, rgb$b),
      xlab="", ylab = "", xaxt = "n", yaxt = "n", bty = "n")

## ----------------------------------------------------------------##
#| Method 3: Using the show_col() function in scales library

# Add fonts
font_add_google("Lora","lora")
showtext_auto()

font <- "lora"
## ----------------------------------------------------------------##

rgb_strings <- c("rgb(205,78,51)",
                    "rgb(190,125,67)",
                    "rgb(190,171,46)",
                    "rgb(93,157,81)",
                    "rgb(113,137,195)",
                    "rgb(172,91,196)",
                    "rgb(208,95,133)")

rgb <- rgb.frame(rgb_strings)
show_col(rgb(rgb$r, rgb$g, rgb$b), borders = NA, ncol = 7)

# Lower saturation
strings <- c("rgb(191,105,67)",
             "rgb(168,126,81)",
             "rgb(161,151,64)",
             "rgb(106,153,108)",
             "rgb(112,144,173)",
             "rgb(138,120,195)",
             "rgb(185,106,131)")

rgb2 <- rgb.frame(strings)
show_col(rgb(rgb2$r, rgb2$g, rgb2$b), borders = NA, ncol = 7, labels = FALSE)

strings2 <- c("rgb(209,137,95)",
              "rgb(189,150,109)",
              "rgb(178,170,97)",
              "rgb(134,170,129)",
              "rgb(128,164,190)",
              "rgb(144,145,212)",
              "rgb(199,131,151)")
rgb3 <- rgb.frame(strings2)
show_col(rgb(rgb3$r, rgb3$g, rgb3$b), borders = NA, ncol = 7, labels = FALSE)

#| Method 4:Use ggplot to create 3 plots with hue, value and saturation

pal1 <- data.frame(color =rgb(rgb$r, rgb$g, rgb$b))
pal2 <- data.frame(color = rgb(rgb2$r, rgb2$g, rgb2$b))
pal3 <- data.frame(color = rgb(rgb3$r, rgb3$g, rgb3$b))


# Create plots for hue,lightness, and saturation
plot1 <- ggplot(pal1, aes(x = 1, y = seq_along(color), fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  labs(title = "Hue") +
  theme_void() +
  theme(
    plot.title = element_text(family = font, size = 14, face = "bold",
                              hjust = .5),
    plot.background = element_rect(fill ="gray100", colour = NA),
    panel.background = element_blank(),
    legend.position = "none")


plot2 <- ggplot(pal3, aes(x = 1, y = seq_along(color), fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  labs(title = "Lightness") +
  theme_void() +
  theme(
    plot.title = element_text(family = font, size = 14, face = "bold",
                              hjust = .5),
    plot.background = element_rect(fill ="gray100", colour = NA),
    panel.background = element_blank(),
    legend.position = "none")
  
plot3 <- ggplot(pal2, aes(x = 1, y = seq_along(color), fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  labs(title = "Saturation") +
  theme_void() +
  theme(
    plot.title = element_text(family = font, size = 14, face = "bold",
                              hjust = .5),
    plot.background = element_rect(fill ="gray100", colour = NA),
    panel.background = element_blank(),
    legend.position = "none")

grid.arrange(plot1, plot2, plot3, ncol =3, nrow = 1)
