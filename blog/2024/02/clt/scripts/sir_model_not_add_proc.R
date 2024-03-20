##-------------------------------------------------------------------------------- ##
## Simulating SIR epidemiologic model of infection
##-------------------------------------------------------------------------------- ##
# Function to simulate SIR model
simulate_SIR <- function(beta, gamma, population_size, initial_infected, days) {
  # Initialize compartments
  S <- population_size - initial_infected
  I <- initial_infected
  R <- 0
  
## Initialize vectors to store results
  susceptible <- numeric(days)
  infected <- numeric(days)
  recovered <- numeric(days)
  
## Run simulation
  for (day in 1:days) {
    # Calculate new infections
    new_infections <- beta * S * I / population_size
    
## Calculate new recoveries
    new_recoveries <- gamma * I
    
    # Update compartments
    S <- S - new_infections
    I <- I + new_infections - new_recoveries
    R <- R + new_recoveries
    
## Store results
    susceptible[day] <- S
    infected[day] <- I
    recovered[day] <- R
  }
  
## Return results
  return(list("Susceptible" = susceptible,
              "Infected" = infected,
              "Recovered" = recovered))
}

# Parameters
beta <- 0.3  # Transmission rate
gamma <- 0.1 # Recovery rate
population_size <- 1000
initial_infected <- 10
days <- 100

# Run simulation
simulation <- simulate_SIR(beta, gamma, population_size, initial_infected, days)

# Plot results
plot(1:days, simulation$Susceptible, type = 'l', col = 'dodgerblue', xlab = 'Days', ylab = 'Population', main = 'SIR Model Simulation', lwd = 2)
lines(1:days, simulation$Infected, col = 'firebrick4', lwd = 3)
lines(1:days, simulation$Recovered, col = 'forestgreen', lwd = 2)
legend('topright', legend = c('Susceptible', 'Infected', 'Recovered'), col = c('dodgerblue', 'firebrick4', 'forestgreen'), lty = 2)

## ------------------------------------------------------------------------------- ##
## Vectorized simulate_SIR function
## ------------------------------------------------------------------------------- ##
simulate_SIR2 <- function(beta, gamma, population_size, initial_infected, days) {
  # Initialize compartments
  S <- population_size - initial_infected
  I <- initial_infected
  R <- 0
  
  # Time sequence
  time <- seq(from = 1, to = days)
  
  # Calculate new infections and recoveries vectorized
  new_infections <- beta * S * I / population_size
  new_recoveries <- gamma * I
  
  # Update compartments (vectorized subtraction)
  S <- S - new_infections * diff(c(0, new_infections))
  I <- I + new_infections - new_recoveries
  R <- R + cumsum(new_recoveries)
  
  # Return results
  return(list(
    "Susceptible" = S,
    "Infected" = I,
    "Recovered" = R
  ))
}

