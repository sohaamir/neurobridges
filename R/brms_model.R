library(tidyverse)
library(brms)
library(bayesplot)

# Read the CSV file and store it as a variable called data
data <- read.csv('processed_data/stop_signal_preprocessed_int_rumination.csv')

# Data preparation (simplified)
data <- data %>%
  mutate(
    IntrusiveSum_z = scale(IntrusiveSum)[,1],
    responded = ifelse(task == "Stop" & incorrect == 0, 0, 1),
    rt = ifelse(responded == 0, NA, rt / 1000),
    ssd = ssd / 1000
  )

# Define the simplified model
simple_model <- bf(
  rt ~ 0 + Intercept + task + IntrusiveSum_z + (1|id)
)

# Fit the model
fit <- brm(
  simple_model,
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(0.5, 0.2), class = "b", coef = "Intercept"),
    prior(normal(0, 0.1), class = "b", coef = "taskStop"),
    prior(normal(0, 0.1), class = "b", coef = "IntrusiveSum_z")
  ),
  chains = 4,
  cores = 4,
  iter = 1000,
  control = list(adapt_delta = 0.9)
)

# Model summary
summary(fit)

# Extract posterior samples
posterior_samples <- as_draws_df(fit)

# Plot posterior distributions
mcmc_areas(posterior_samples,
           pars = c("b_Intercept", "b_taskStop", "b_IntrusiveSum_z"),
           prob = 0.95) +
  labs(title = "Posterior Distributions of Model Parameters")

# Effect of IntrusiveSum on RT
new_data <- expand_grid(
  task = c("NoStop", "Stop"),
  IntrusiveSum_z = seq(-2, 2, length.out = 100),
  id = NA
)

predictions <- fitted(fit, newdata = new_data, re_formula = NA)
pred_data <- bind_cols(new_data, predictions)

ggplot(pred_data, aes(x = IntrusiveSum_z, y = Estimate, color = task)) +
  geom_line() +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = task), alpha = 0.2) +
  labs(x = "IntrusiveSum (z-score)", y = "Predicted RT (seconds)",
       title = "Effect of IntrusiveSum on Reaction Time",
       color = "Task", fill = "Task") +
  scale_color_manual(values = c("NoStop" = "blue", "Stop" = "red")) +
  scale_fill_manual(values = c("NoStop" = "blue", "Stop" = "red")) +
  theme_minimal()

# Save plots
ggsave("simple_model_posteriors.png", width = 10, height = 6)
ggsave("intrusive_sum_effect_simple.png", width = 10, height = 6)
