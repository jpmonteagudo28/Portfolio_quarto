## 
##
## Concrete example of t–test on highly skewed data 
### ### Medical cost data for two groups with two age bands with > 3000 obs each
### ### Sub-gtoups of people with very different cost distributions (woman v. men, chronic v. acute v. no conditions)
### ### More than half the values are zero - indicating more than 5% of people had no medical care that year
##
##
##
## Simulated data
## 5/8 of data are zeros in any case
## the remaining data have log-normal distribution
## the parameters of that distribution are arranged to reproduce the observed means 
## and third quartile provided in the stack exchange example
##
## Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.0      0.0      0.0   4536.0    302.6 395300.0 
## Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.0      0.0      0.0   4964.0    423.8 721700.0 
##
## Generate positive random values with a median of 0, given Q3,
# and given mean. Make a proportion 1-e of them true zeros.
##
##
rskew <- function(n, x.mean, x.q3, e=3/8) {
  beta <- qnorm(1 - (1/4)/e)
  gamma <- 2*(log(x.q3) - log(x.mean/e))
  sigma <- sqrt(beta^2 - gamma) + beta
  mu <- log(x.mean/e) - sigma^2/2
  m <- floor(n * e)
  c(exp(rnorm(m, mu, sigma)), rep(0, n-m))
}
##
##
## See how closely the summary statistics are reproduced.
# (The quartiles will be close; the maxima not too far off;
# the means may differ a lot, though.)
##
set.seed(23)
x <- rskew(3300, 4536, 302.6)
y <- rskew(3400, 4964, 423.8)
summary(x)
summary(y)
##
## Estimate the sampling distribution of the mean.
##
set.seed(17)
sim.x <- replicate(10^4, mean(rskew(3367, 4536, 302.6)))
hist(sim.x, freq=FALSE, ylim=c(0, dnorm(0, sd=sd(sim.x))))
curve(dnorm(x, mean(sim.x), sd(sim.x)), add=TRUE, col="Red")
##
## The histogram of these means estimates the sampling distribution of the mean. 
## The t-test is valid when this distribution is approximately Normal; 
## the extent to which it deviates from Normality 
## indicates the extent to which the Student t distribution will err.
## 
##
hist(sim.x[sim.x < 10000], xlab="x", freq=FALSE) # focusing on the observations that aren't outliers
curve(dnorm(x, mean(sim.x), sd(sim.x)), add=TRUE, col="Red")
##
# Can a t-test detect a difference with more data?
##
set.seed(23)
n.factor <- 50
z <- replicate(10^3, {
  x <- rskew(3300*n.factor, 4536, 302.6)
  y <- rskew(3400*n.factor, 4964, 423.8)
  t.test(x,y)$p.value
})
hist(z)
mean(z < .05) # The estimated power at a 5% significance level