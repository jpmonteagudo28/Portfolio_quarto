##----------------------------------------------------------------------------------- ##
## Real-life example of additive process (panmictic models)
##----------------------------------------------------------------------------------- ##
##
## Modeling chance of infection of healthy population by an infected individual moving 
## in a two-dimensional space in short-range evolution.   
## A random walker (infected person) takes \tau steps on a 2D space.
## After each step, the visited site can become infected with probability p, and branches 
## off, from this location by another random walker with \tau steps that spreads the 
## disease to other sites. 
## \tau represents the length of time of the infectious period
## p represents the infectiousness
##
## Number of infected persons produced by one walker in susceptible pop., $R_{0} = p*\tau
## Three types of sites: Susceptible, Infected, Removed (SIR)
##
# Load required libraries
library(igraph)

# Function to simulate SIR model on contact network
simulate_SIR_network <- function(network, beta, gamma, initial_infected, days) {
  # Initialize node states (0 = susceptible, 1 = infected, 2 = recovered)
  V(network)$state <- rep(0, vcount(network))
  V(network)$state[sample(1:vcount(network), initial_infected)] <- 1
  
  # Run simulation
  for (day in 1:days) {
    # Update infected nodes
    infected_nodes <- which(V(network)$state == 1)
    for (node in infected_nodes) {
      neighbors <- neighbors(network, node)
      susceptible_neighbors <- neighbors[V(network)$state[neighbors] == 0]
      for (neighbor in susceptible_neighbors) {
        if (runif(1) < beta) {
          V(network)$state[neighbor] <- 1
        }
      }
      if (runif(1) < gamma) {
        V(network)$state[node] <- 2
      }
    }
  }
  
  # Return final states
  return(V(network)$state)
}

# Generate random contact network
n <- 100  # Number of nodes
p <- 0.1  # Probability of connection
network <- erdos.renyi.game(n, p)

# Parameters
beta <- 0.3  # Transmission rate
gamma <- 0.1 # Recovery rate
initial_infected <- 5
days <- 100

# Run simulation
final_states <- simulate_SIR_network(network, beta, gamma, initial_infected, days)

# Plot final states
plot(network, vertex.color = ifelse(final_states == 0, "dodgerblue", ifelse(final_states == 1, "firebrick1", "forestgreen")),
     vertex.size = 10, vertex.label = NA, main = "SIR Model on Contact Network")

## ---------------------------------------------------------------------------------- ##
## Simple random wlak algorithm with animated graph
## ---------------------------------------------------------------------------------- ##
## 
##Function to simulate random walk with disease transmission
simulate_random_walk <- function(steps, transmission_prob) {
  # Initialize variables
  position <- 0  # Initial position
  infected <- FALSE  # Initial infection status
  
## Simulate random walk
  positions <- numeric(steps)
  infections <- logical(steps)
  for (i in 1:steps) {
    # Update position
    position <- position + sample(c(-1, 1), 1)
    positions[i] <- position
    
## Check for infection
    if (!infected && runif(1) < transmission_prob) {
      infected <- TRUE
      infections[i] <- TRUE
    } else {
      infections[i] <- FALSE
    }
  }
  
## Return results
  return(list(positions = positions, infections = infections))
}

# Parameters
steps <- 100  # Number of steps in the random walk
transmission_prob <- 0.1  # Probability of disease transmission at each step

# Run simulation
sim <- simulate_random_walk(steps, transmission_prob)

# Print results
print(sim$positions)
print(sim$infections)

## Load required libraries
library(ggplot2)
library(gganimate)

# Create data frame for plotting
df <- data.frame(step = 1:steps, position = sim$positions, infection = sim$infections)

# Plot animated graph
p <- ggplot(df, aes(x = step, y = position, color = factor(infection))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("dodgerblue", "firebrick1"), labels = c("Uninfected", "Infected")) +
  labs(title = "Random Walk with Disease Transmission", x = "Step", y = "Position") +
  transition_states(step, transition_length = 0.5, state_length = 1) +
  theme_minimal()

# Save animation
animate(p, renderer = gifski_renderer("random_walk_animation.gif"))
