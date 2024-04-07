## -------------------------------------------------------------------------------- ##
## Loading Libraries
    #library(here)
## -------------------------------------------------------------------------------- ##
## Concrete example of t–test on highly skewed data 
### ### Medical cost data for two groups with two age bands with > 3000 obs each
### ### Sub-groups of people with very different cost distributions (woman v. men, chronic v. acute v. no conditions)
### ### More than half the values are zero - indicating more than 5% of people had no medical care that year
##
## Simulated data
## 5/8 of data are zeros in any case
## the remaining data have log-normal distribution
## the parameters of that distribution are arranged to reproduce the observed means 
## and third quantile provided in the stack exchange example
##
## Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.0      0.0      0.0   4536.0    302.6 395300.0 
## Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.0      0.0      0.0   4964.0    423.8 721700.0 
##
## Generate positive random values with a median of 0, given Q3,
## and given mean. Make a proportion 1-e of them true zeros.
    
rskew <- function(n, x.mean, x.q3, e=3/8) {
  beta <- qnorm(1 - (1/4)/e)
  gamma <- 2*(log(x.q3) - log(x.mean/e))
  sigma <- sqrt(beta^2 - gamma) + beta
  mu <- log(x.mean/e) - sigma^2/2
  m <- floor(n * e)
  c(exp(rnorm(m, mu, sigma)), rep(0, n-m))
}


## See how closely the summary statistics are reproduced.
## (The quantiles will be close; the maxima not too far off;
## the means may differ a lot, though.)

set.seed(9)
x <- rskew(3300, 4536, 302.6)
y <- rskew(3400, 4964, 423.8)
summary(x)
summary(y)

clrs <- c(
  "#FFBE00",  # MCRN yellow
  "#B92F0A",  # MCRN red
  "#7C225C",  # MCRN maroon
  "#792A26",  # MCRN brown
  "#242424",  # MCRN dark gray
  "#394DAA"   # Blue from MCR flag
)


## Estimate the sampling distribution of the mean.
par(las=1,mfrow=c(3,1),mai=c(.5,1,.5,.1))

sim.x <- replicate(10^4, mean(rskew(3367, 4536, 302.6)))
hist(sim.x, freq=FALSE,col = clrs[1], ylim=c(0, dnorm(0, sd=sd(sim.x))))
curve(dnorm(x, mean(sim.x), sd(sim.x)), add=TRUE, col=clrs[3])

## The histogram of these means estimates the sampling distribution of the mean. 
## The t-test is valid when this distribution is approximately Normal; 
## the extent to which it deviates from Normality 
## indicates the extent to which the Student t distribution will err.

hist(sim.x[sim.x < 10000], xlab="x", freq=FALSE, col = clrs[1],
     main = "Histogram of x < $10,000") # focusing on the observations that aren't outliers
curve(dnorm(x, mean(sim.x), sd(sim.x)), add=TRUE, col=clrs[3])

## Can a t-test detect a difference with more data?
set.seed(9)
n.factor <- 50
z <- replicate(10^3, { # storing t-test results inside variable and creating histogram of p_values.
  x <- rskew(3300*n.factor, 4536, 302.6)
  y <- rskew(3400*n.factor, 4964, 423.8)
  t.test(x,y)$p.value
})
hist(z, col = clrs[6])
pow <- mean(z < .05) # The estimated power at a 5% significance level
cat("The estimated power of our test to detect a .05 significant difference is", pow, "\n")

