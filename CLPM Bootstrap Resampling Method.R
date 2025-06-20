# Load necessary libraries
library(lavaan)
library(readr)

# Load the dataset
data <- read_csv("6th Trial.csv")

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

# Fit the model using the dataset and apply bootstrapping
fit <- sem(
  model, 
  data = data, 
  se = "bootstrap",        # Use bootstrapped standard errors
  bootstrap = 2000         # Specify the number of bootstrap samples
)

# Summarize the model results with robust standard errors
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Extract model evaluation metrics
AIC_value <- AIC(fit)
fitmeasures(fit, c("rmsea", "cfi", "tli", "chi.sq", "df", "pvalue"))

# Extract R-squared values for Wave 3
r_squared <- inspect(fit, "r2")
print("R-squared for Wave 3:")
print(r_squared[c("R3TTF", "R3OWB", "R3AU")])

# Display robust standard errors from bootstrapping
parameterEstimates(fit, standardized = TRUE, boot.ci.type = "bca.simple")