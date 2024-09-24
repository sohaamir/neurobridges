# =============================================================================
#### Info ####
# =============================================================================
# Model for Stop-signal task created for NeuroBridges 2024
# Aamir Sohail, University of Birmingham

library(tidyverse)
library(rstan)
library(bayesplot)

# Read the data
data <- read_csv("processed_data/stop_signal_preprocessed.csv")

# Define the groups
low_rumination <- c(51, 33, 4, 32, 43, 76, 84, 88, 15, 56, 83)
high_rumination <- c(8, 24, 42, 28, 72, 80, 23, 38, 22)

# Function to preprocess data and run the model
run_model <- function(data, group_ids) {
  preprocessed_data <- data %>%
    filter(id %in% group_ids) %>%
    mutate(
      participant = as.integer(factor(id)),
      stop = as.integer(task == "Stop"),
      rt_z = as.vector(rt_z),
      ssd_z = as.vector(ssd_z)
    )

  stan_data <- list(
    N = nrow(preprocessed_data),
    P = max(preprocessed_data$participant),
    participant = preprocessed_data$participant,
    rt = preprocessed_data$rt_z,
    ssd = preprocessed_data$ssd_z,
    stop = preprocessed_data$stop
  )

  model <- stan_model("stan/sst_model.stan")
  fit <- sampling(model, data = stan_data, iter = 2000, chains = 4)

  return(fit)
}

# Run the model for each group
low_rumination_fit <- run_model(data, low_rumination)
high_rumination_fit <- run_model(data, high_rumination)

# Extract posterior samples
extract_samples <- function(fit) {
  as.data.frame(rstan::extract(fit, pars = c("alpha", "beta_rt", "beta_ssd", "sigma_u")))
}

low_samples <- extract_samples(low_rumination_fit) %>% mutate(group = "Low Rumination")
high_samples <- extract_samples(high_rumination_fit) %>% mutate(group = "High Rumination")

# Combine samples
all_samples <- bind_rows(low_samples, high_samples)

# Reshape data for plotting
plot_data <- all_samples %>%
  pivot_longer(cols = c(alpha, beta_rt, beta_ssd, sigma_u), names_to = "parameter", values_to = "value")

# Plot parameter comparisons
ggplot(plot_data, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ parameter, scales = "free") +
  theme_minimal() +
  labs(title = "Comparison of Model Parameters Between Low and High Intrusive Rumination Groups",
       x = "Parameter Value", y = "Density") +
  scale_fill_manual(values = c("Low Rumination" = "blue", "High Rumination" = "red"))

# Save the plot
ggsave("parameter_intrusive_rumination_comparison_plot.png", width = 12, height = 8)

# Print summary statistics
print(summary(low_rumination_fit, pars = c("alpha", "beta_rt", "beta_ssd", "sigma_u")))
print(summary(high_rumination_fit, pars = c("alpha", "beta_rt", "beta_ssd", "sigma_u")))

################### Posterior predictive checks ################################


# Posterior Predictive Check
library(bayesplot)

# Extract posterior predictions
y_rep <- as.matrix(fit, pars = "y_pred")

# Plot posterior predictive distribution vs. observed data
ppc_dens_overlay(preprocessed_data$stop, y_rep[1:50,]) +
  labs(title = "Posterior Predictive Check: Density Overlay")

# Compare summary statistics
T_obs <- sum(preprocessed_data$stop)
T_rep <- apply(y_rep, 1, sum)

ppc_stat(preprocessed_data$stop, y_rep, stat = "sum") +
  labs(title = "Posterior Predictive Check: Sum Statistic")

# Plot predictive intervals
ppc_intervals(preprocessed_data$stop, y_rep, prob = 0.5) +
  labs(title = "Posterior Predictive Check: 50% Predictive Intervals")

# Calculate and print Bayesian p-value
bayes_p_value <- mean(T_rep >= T_obs)
print(paste("Bayesian p-value:", bayes_p_value))



