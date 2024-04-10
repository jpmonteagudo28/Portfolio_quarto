## Multiplicative Process (spread of infectious disease within pop. in 260 time steps)
## Y_{0} = Y_{t-1}*v(1+e)| t: time, v: deterministic variation, e: random variation

## 1. Generate samples from an weibull distribution
params <- list(
  seed = set.seed(9),
  n.sim = 100, # no. simulations to run
  n = seq(260, 6260, by = 40), # no. samples
  epsilon = 0.05,
  init_infect = 125 # no. of hospitalizations and pos. tests
)

day.infect <- function(params) {
  n <- params$n
  init_infect <- params$init_infect
  
  n_size <- numeric(0)
  infected <- numeric(0)
  time <- numeric(0)
  
  for (i in seq_along(n)) {
    sample_size <- n[i]
    shape <- 2.5
    scale <- 1
    e <- rweibull(sample_size, shape, scale) #The shape of dist heavily depends on random comp. dist
    mean <- 1.28
    sd <- 1
    v <- pmax(rnorm(sample_size, mean, sd), 0) # ratio of infected to infectors R_{t} evolving thru
                                              # time as a Gaussian process
    infect <- init_infect * v * (1 + e)
    n_size <- c(n_size, rep(n[i], sample_size))
    time <- c(time, rep(i, sample_size))
    infected <- c(infected, infect)
  }
  
  day_infect_df <- data.frame(time = time, size = n_size, infected = infected)
  return(day_infect_df)
}

sim_infect <- day.infect(params)

# Group samples by either time or sample sizes
# Compute the ecdf for randomly chosen groups of increasing sample size and compare to normal dist
ecdf(sim_infect$infected[sim_infect$time == 1]) # a possible way of doing it, but ugly. Use ggplot
# Also calculate Kolmogorov-Smirnov statistic for ecdf and normal CDF to check convergence in dist

#--------------------------------------------------------------------------------- # 
## If Kolmogorov-Smirnov (KS) distance of seq. of random variable X_{n} empirical 
## cdf to cdf of X approaches 0, then the ecdf of X_{n} converges in distribution to 
## X when the target disr. is continuous
## Check here: https://arxiv.org/pdf/1305.2766.pdf, here https://math.mit.edu/~rmd/465/edf-ks.pdf,
## and here https://stats.stackexchange.com/questions/562356/convergence-in-distribution-and-convergence-in-kolmogorov-distance.

