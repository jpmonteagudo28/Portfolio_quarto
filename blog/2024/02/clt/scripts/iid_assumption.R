##---------------------------------------------------------------------------------- ##
## Generating two random variables that meet the i.i.d assumption
## --------------------------------------------------------------------------------- ##
library(here)
library(ppcor)
library(ggplot2)
library(ggthemes)
library(paletteer)
library(gridExtra)
## --------------------------------------------------------------------------------- ##
##
xx <- seq(-5,5,by=.01)
## Saving plot image
png("iid.png", width = 800, height = 600) # assign name and dimensions to plot
par(las=1,mfrow=c(3,1),mai=c(.5,.5,.5,.1))

## Plot graphics
plot(xx,dnorm(xx,-1),type="b",lwd=1,xlab="",ylab="",
     col = "darkorchid4", main="Identically distributed")
lines(xx,dnorm(xx,-1),lty = 2, lwd=3,pch = 18,col="orangered")
legend("topright", legend = c("A", "B"),
       col = c("darkorchid4", "orangered"), lty = 1:2, cex = 0.8)

plot(xx,dnorm(xx,-1),type="l",lwd=2,xlab="",ylab="",
     col = "darkorchid4",main="Shift in means")
lines(xx,dnorm(xx,1),lwd=2,col="orangered")

plot(xx,dnorm(x,-1),type="l",lwd=2,xlab="",ylab="",
     col = "darkorchid4",main="Different distributions",ylim=c(0,0.6))
lines(xx,dchisq(xx,2),lwd=2,col="orangered")
## Close graphics device to save plot
dev.off()
##------------------------------------------------------------------------------------- ##
## Generating two conditionally independent random variables
## ------------------------------------------------------------------------------------ ##
## 
## Creating function to generate random variable with defined correlation to an existing var.
complement <- function(y, rho, x) { #corr. coefficient
  if (missing(x)) x <- rgamma(length(y),2,.5) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}
set.seed(09)
def_micro <- rpois(1050,6) #lambda = n*p, p = .006 defective microchips manufactured by company in 12 hrs
complaints <- complement(def_micro,rho =.273) # no. complaints received on 24 hr period for devices using the microchip in question
devices <- complement(def_micro, rho = .331) # weekly no. devices processed at online retailer (+) bought (-)returned
##
## Creating function to summarize list of variables
group_summary <- function(...){
  summaries <- lapply(list(...), summary)
  names(summaries) <- paste0("Summary_", seq_along(summaries))
  return(summaries)
}

group_summary(def_micro,devices,complaints)
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
   correlation_matrix <- round(cor(df, method = "spearman"),3)
   return(correlation_matrix)
}

spearman_correlation(def_micro, devices,complaints)
##
##              def_micro devices complaints
## def_micro      1.000   0.378      0.297
## devices        0.378   1.000      0.099
## complaints     0.297   0.099      1.000
##
## Use ppcor package to calculate partial correlation between complaints and devices

ppcor::pcor.test(devices,complaints,def_micro, method = "spearman")
##      estimate  p.value  statistic    n gp   Method
## 1 -0.02075897 0.501827 -0.6718506 1050  1 spearman
## relationship between devices and complaints is technically non-existent. We can say they're conditionally independent.
## -------------------------------------------------------------------------------- ##
## Creating scatterplots to visualize relationship between variables
## -------------------------------------------------------------------------------- ##

df1 <- data.frame(def_micro,devices,complaints)
dev_comp_plot <- ggplot2::ggplot(df1, aes(devices, complaints))+
                         geom_point(color = "dodgerblue4") +
                         geom_smooth(method = "gam", se = FALSE, color = "firebrick", linewidth = 2) +
                         labs(
                           x = "No. devices processed",
                           y = "No. complaints received") +
                           ggthemes::theme_tufte() +  # Apply a classic theme
                           theme(plot.title = element_text(hjust = 0.5))  # Center the title
dev_micro_plot <- ggplot2::ggplot(df1, aes(devices, def_micro))+
                        geom_point(color = "darkcyan") +
                        geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 2) +
                        labs(
                           x = "No. devices processed",
                           y = "No. defective microchips") +
                        ggthemes::theme_tufte() +  # Apply a classic theme
                        theme(plot.title = element_text(hjust = 0.5))  # Center the title
comp_micro_plot <- ggplot2::ggplot(df1, aes(complaints, def_micro))+
                      geom_point(color = "hotpink3") +
                      geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 2) +
                      labs(
                          x = "No. complaints recieved",
                          y = "No. defective microchips") +
                      ggthemes::theme_tufte() +  # Apply a classic theme
                      theme(plot.title = element_text(hjust = 0.5))  # Center the title

## gridExtra::grid.arrange(dev_comp_plot, dev_micro_plot, comp_micro_plot, 
##                        top = "Devices, Defective microships, Complaints",ncol = 1) # I prefer the vertical plots rather than the horizontal ones
arranged_plots <- gridExtra::grid.arrange(dev_comp_plot, dev_micro_plot, comp_micro_plot, 
                        top = "Devices, Defective microships, Complaints",nrow = 1)
## Saving plots to img file in blog
here::here("blog","2024","02","clt","img")
ggsave("cond_ind.png",arranged_plots, 
                width = 800,
                height = 600,
                units = "px",
                dpi = 72)
