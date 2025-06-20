# Load necessary libraries
library(lavaan)
library(simsem)

# Define the Cross-Lagged Panel Model (CLPM)
model <- '
  # Stability paths within each variable across waves
  R3TTF ~ R2TTF
  R3OWB ~ R2OWB
  R3AU ~ R2AU

  # Cross-lagged paths
  R3TTF ~ R2OWB + R2AU
  R3OWB ~ R2TTF + R2AU
  R3AU ~ R2TTF + R2OWB

  # Covariances at Wave 2
  R2TTF ~~ R2OWB + R2AU
  R2OWB ~~ R2AU

  # Covariances at Wave 3
  R3TTF ~~ R3OWB + R3AU
  R3OWB ~~ R3AU
'

# Fit the model to get estimated parameters
fit <- sem(model, data = data)
param_estimates <- parameterEstimates(fit)

# Define a population model based on estimated parameters
population_model <- '
  R3TTF ~ 0.5*R2TTF + 0.3*R2OWB + 0.2*R2AU
  R3OWB ~ 0.4*R2TTF + 0.6*R2OWB + 0.3*R2AU
  R3AU ~ 0.3*R2TTF + 0.2*R2OWB + 0.7*R2AU
  
  R2TTF ~~ 1*R2TTF
  R2OWB ~~ 1*R2OWB
  R2AU ~~ 1*R2AU
  R3TTF ~~ 1*R3TTF
  R3OWB ~~ 1*R3OWB
  R3AU ~~ 1*R3AU
'

# Define simulation conditions
sim_model <- sim(
  nRep = 1000,               # Number of replications (simulations)
  model = model,             # Theoretical model to evaluate
  n = nrow(data),            # Sample size same as observed data
  generate = population_model # Population model for simulation
)

# Summarize simulation results
summary(sim_model)

# Extract robust standard errors
sim_se <- sim_model@coefTable[, "se"]
print("Simulation-based Standard Errors:")
print(sim_se)