## ------------------------------------------------------------------------------- ##
## Modeling the spread of measles in a closed population
## The transmission rate (beta) describes how frequently susceptible (S) and 
## infected (I) individuals come into contact, and the recovery rate (gamma) 
## describes the the average time an individual spends infected before recovering. 
##
## Using R code to parametrize the model. Parametrizing only once 
## Specify model parameters use within() to make assignments *inside* an 
## empty (or existing) list. This is a handy R trick that allows you to 
## refer to existing list elements on right hand side (RHS)
##
## ------------------------------------------------------------------------------ ##
## Loading libraries
library(lattice)
library(here)
library(Rcpp)
library(plyr)
## ------------------------------------------------------------------------------ ##
## Loading tauleapCpp function from .cpp file
sourceCpp(here::here("blog","2024","02","clt","scripts","sir_model.cpp"))
params <- list()
params <- within(params, {
  
  ## set rng state
  seed <- 9
  tau <- 0.001 # in years
  nyears <- 10
  
  ## total number of steps
  nsteps <- nyears/tau
  
  mu <- 1/70 #death rate
  gamma <- 365/10 #recovery rate
  R0 <- 10
  ## refers to R0 above
  beta <- R0*(gamma+mu) #transmission rate
  nu <- mu #birth rate
  
  ## initial conditions, list within list
  ## use within() to modify empty list, as above
  init <- within(list(), {
    pop <- 1e6
    S <- round(pop/R0)
    I <- round(pop*mu*(1-1/R0)/(gamma+mu))
    ## refers to S,I above
    R <- pop-S-I
  })
})

set.seed(params$seed)

## run the model once
result.df <- tauleapCpp(params)

nsim <- 9

## run many sims, combine all results into one data.frame
## plyr will combine results for us
result.rep <- ldply(1:nsim, function(.nn) {
  set.seed(.nn)
  ## run the model
  result <- tauleapCpp(params)
  ## this wastes space, but is very simple and aids plotting
  result$nsim <- .nn
  return(result)
})


## lattice plot of results
plot(
  xyplot(I ~ time | sprintf("Simulation %02d",nsim), 
         data=result.rep, type=c('l','g'), as.table=T,
         ylab='No. Infected', xlab='Years',
         scales=list(y=list(alternating=F))
  )
)

dev.copy(png,here::here("blog","2024","02","clt","img","sir_mod_add_proc.png"), width = 800, height = 600)
dev.off()