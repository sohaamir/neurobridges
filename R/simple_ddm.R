install.packages("dplyr")
install.packages("rtdists")
install.packages("ggplot2")

# Load required libraries
library(dplyr)
library(rtdists)
library(ggplot2)

library(RWiener)

library(purrr)

# Main analysis
# Read the CSV file and store it as a variable called data
data <- read.csv('processed_data/stop_signal_preprocessed_int_rumination.csv')

# Assuming your data frame is called 'data'
prepared_data <- data %>%
  filter(task == "NoStop") %>%
  filter(!is.na(rt) & rt > 0) %>%
  mutate(
    accuracy = as.integer(correct),
    response = ifelse(response == "Left", -1, 1)
  ) %>%
  select(id, rt, accuracy, response, IntrusiveSum, SSRT)

# Check the first few rows of the prepared data
head(prepared_data)

# Additional data preprocessing
prepared_data_clean <- prepared_data %>%
  filter(rt >= 0.1) %>%  # Remove extremely fast RTs (less than 100ms)
  mutate(response = ifelse(response == 0, sample(c(-1, 1), n(), replace = TRUE), response))  # Randomly assign 0 responses

fit_ddm <- function(data) {
  tryCatch({
    data$resp <- ifelse(data$response == 1, "upper", "lower")
    fit <- wdm(dat = data.frame(q = data$rt, resp = data$resp))
    return(coef(fit))
  }, error = function(e) {
    warning(paste("Error fitting model for participant:", data$id[1], "-", e$message))
    return(c(alpha = NA, tau = NA, beta = NA, delta = NA))
  })
}

model_fits <- prepared_data_clean %>%
  group_by(id) %>%
  nest() %>%
  mutate(parameters = map(data, fit_ddm)) %>%
  unnest_wider(parameters)

# Remove rows with NA parameters
model_fits <- model_fits %>% filter(!is.na(alpha))

print(model_fits)

# Reshape the data for plotting
model_fits_long <- model_fits %>%
  select(id, alpha, tau, beta, delta) %>%
  pivot_longer(cols = c(alpha, tau, beta, delta),
               names_to = "parameter",
               values_to = "value")

# Create the plot
ggplot(model_fits_long, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ parameter, scales = "free", ncol = 2) +
  theme_minimal(base_size = 14) +
  labs(title = "Distribution of DDM Parameters Across Participants",
       x = "Parameter Value",
       y = "Density") +
  theme(strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold", size = 16),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

ggsave("output/img/ddm_parameter_histograms.png", width = 12, height = 10)






# 1. Parameter Recovery
# ---------------------

# Function to simulate data
simulate_ddm_data <- function(params, n_trials) {
  rwiener(n = n_trials,
          alpha = params$alpha,
          tau = params$tau,
          beta = params$beta,
          delta = params$delta)
}

# Function to fit DDM
fit_ddm <- function(data) {
  fit <- wdm(dat = data.frame(q = data$q, resp = data$resp))
  return(coef(fit))
}

# Calculate mean parameter values from your fitted model
mean_params <- model_fits %>%
  summarise(across(c(alpha, tau, beta, delta), mean, na.rm = TRUE))

# Use these mean values for parameter recovery
true_params <- list(
  alpha = mean_params$alpha,
  tau = mean_params$tau,
  beta = mean_params$beta,
  delta = mean_params$delta
)

# Then proceed with the simulation and recovery as before
simulated_data <- simulate_ddm_data(
  params = list(
    alpha = true_params$alpha,
    tau = true_params$tau,
    beta = true_params$beta,
    delta = true_params$delta
  ),
  n_trials = 1000
)

# Rest of the code remains the same
recovered_params <- fit_ddm(simulated_data)

# Compare true and recovered parameters
param_comparison <- data.frame(
  Parameter = names(true_params),
  True = unlist(true_params),
  Recovered = unlist(recovered_params)
)

# Visualize the comparison
ggplot(param_comparison, aes(x = True, y = Recovered)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  facet_wrap(~Parameter, scales = "free") +
  theme_minimal() +
  labs(title = "Parameter Recovery Using Mean Task Values") +
  theme(strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold", size = 16),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

ggsave("parameter_recovery.png", width = 12, height = 10)




# 2. Posterior Predictive Check
# -----------------------------

# Assuming you have your original data in a data frame
# with columns 'rt' for reaction time and 'response' for the response

# Function to simulate data from fitted parameters
simulate_from_fit <- function(params, n_trials) {
  if (any(is.na(params)) || params["alpha"] <= 0 || params["tau"] <= 0 || params["beta"] < 0 || params["beta"] > 1) {
    warning("Invalid parameters: ", paste(names(params), params, sep = "=", collapse = ", "))
    return(NULL)
  }

  tryCatch({
    simulated <- rwiener(n = n_trials,
                         alpha = params["alpha"],
                         tau = params["tau"],
                         beta = params["beta"],
                         delta = params["delta"])
    return(data.frame(rt = simulated$q, response = simulated$resp))
  }, error = function(e) {
    warning("Error in simulation: ", e$message)
    return(NULL)
  })
}

# Simulate data for each participant
simulated_data <- model_fits %>%
  rowwise() %>%
  mutate(simulated = list(simulate_from_fit(c(alpha = alpha, tau = tau, beta = beta, delta = delta),
                                            n_trials = nrow(filter(prepared_data_clean, id == .data$id))))) %>%
  filter(!is.null(simulated)) %>%
  unnest(simulated)

# Check if we have any simulated data
if (nrow(simulated_data) == 0) {
  stop("No valid simulations were produced. Check your model_fits data for invalid parameter values.")
}

# Prepare original data
original_data <- prepared_data_clean %>%
  mutate(response = ifelse(response == 1, "upper", "lower"))

# Combine original and simulated data for plotting
plot_data <- bind_rows(
  mutate(original_data, Type = "Original"),
  mutate(simulated_data, Type = "Simulated")
)

# Plot RT distributions
ggplot(plot_data, aes(x = rt, fill = Type)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~response) +
  theme_minimal(base_size = 14) +
  labs(title = "RT Distributions: Original vs Simulated",
       x = "Reaction Time", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  coord_cartesian(xlim = c(0, 1))

ggsave("rt_distributions.png", width = 12, height = 8)

# Compare accuracy
original_accuracy <- original_data %>%
  group_by(id) %>%
  summarise(Accuracy = mean(response == "upper"))

simulated_accuracy <- simulated_data %>%
  group_by(id) %>%
  summarise(Accuracy = mean(response == "upper"))

accuracy_comparison <- bind_rows(
  mutate(original_accuracy, Type = "Original"),
  mutate(simulated_accuracy, Type = "Simulated")
)

ggplot(accuracy_comparison, aes(x = Type, y = Accuracy, fill = Type)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  theme_minimal(base_size = 14) +
  labs(title = "Accuracy Comparison: Original vs Simulated") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

ggsave("accuracy_comparison.png", width = 10, height = 8)

# Print summary of model_fits to check for invalid values
print(summary(model_fits))


# 3. CORRELATIONS BETWEEN MODEL PARAMETERS AND RUMINATION


library(broom)

# Extract IntrusiveSum scores for each participant
intrusive_sum <- prepared_data_clean %>%
  group_by(id) %>%
  summarize(IntrusiveSum = first(IntrusiveSum))

# Merge model_fits with IntrusiveSum scores
plot_data <- model_fits %>%
  left_join(intrusive_sum, by = "id") %>%
  select(id, alpha, tau, beta, delta, IntrusiveSum) %>%
  pivot_longer(cols = c(alpha, tau, beta, delta),
               names_to = "parameter",
               values_to = "value")

# Function to calculate p-value and format it
calculate_p_value <- function(x, y) {
  model <- lm(y ~ x)
  p_value <- summary(model)$coefficients[2, 4]
  return(paste("p =", format.pval(p_value, digits = 3)))
}

# Calculate p-values for each parameter
p_values <- plot_data %>%
  group_by(parameter) %>%
  summarize(p_value = calculate_p_value(IntrusiveSum, value))

# Create the plot
ggplot(plot_data, aes(x = IntrusiveSum, y = value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ parameter, scales = "free_y", ncol = 2) +
  geom_text(data = p_values, aes(label = p_value),
            x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4) +
  theme_minimal(base_size = 14) +
  labs(title = "Relationship between DDM Parameters and IntrusiveSum Scores",
       x = "IntrusiveSum Score",
       y = "Parameter Value") +
  theme(strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold", size = 16),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

ggsave("ddm_parameters_vs_intrusivesum.png", width = 12, height = 10)

# Calculate correlations and p-values
correlations <- plot_data %>%
  group_by(parameter) %>%
  summarize(
    correlation = cor(IntrusiveSum, value, use = "complete.obs"),
    p_value = cor.test(IntrusiveSum, value)$p.value
  )

print(correlations)


