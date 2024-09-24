# Read the CSV file and store it as a variable called stop_signal_raw
rumination <- read.csv('processed_data/Data_rumination_CF.csv')

# Read the CSV file and store it as a variable called stop_signal_raw
subject_data <- read.csv('processed_data/stop_signal_preprocessed.csv')

# Ensure both dataframes have 'id' as character for consistent joining
subject_data$id <- as.character(subject_data$id)
rumination$id <- as.character(rumination$id)

# Join the data
result <- subject_data %>%
  left_join(rumination, by = "id")

# Check the result
head(result, 10)

# Save as CSV
write.csv(result, "stop_signal_preprocessed_rumination.csv", row.names = FALSE)

library(dplyr)

group_variables <- result %>%
  group_by(id) %>%
  summarize(
    Go_RT = mean(rt[task == "NoStop"], na.rm = TRUE),
    Stop_Signal_Accuracy = sum(task == "Stop" & correct == 1) / sum(task == "Stop"),
    Go_Trial_Accuracy = mean(correct[task == "NoStop"], na.rm = TRUE),
    SSRT = first(SSRT),
    ReflectionSum = first(ReflectionSum),
    BroodingSum = first(BroodingSum),
    DepressiveSum = first(DepressiveSum),
    IntrusiveSum = first(IntrusiveSum),
    DeliberateSum = first(DeliberateSum),
    ReappraisalSum = first(ReappraisalSum),
    SuppressionSum = first(SuppressionSum)
  ) %>%
  ungroup()


library(tidyverse)
library(ggpubr)

# Function to create correlation plots
create_correlation_plots <- function(data, score_col, task_measures) {
  plots <- map(task_measures, function(measure) {
    cor_test <- cor.test(data[[score_col]], data[[measure]])
    p_value <- format.pval(cor_test$p.value, digits = 3)

    ggplot(data, aes_string(x = score_col, y = measure)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "red") +
      labs(
        title = paste(score_col, "vs", measure),
        x = score_col,
        y = measure,
        subtitle = paste("r =", round(cor_test$estimate, 3), ", p =", p_value)
      ) +
      theme_pubr() +
      theme(plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 8))
  })

  arranged_plots <- ggarrange(plotlist = plots, ncol = 2, nrow = 2)
  return(arranged_plots)
}

# List of questionnaire scores and task measures
questionnaire_scores <- c("ReflectionSum", "BroodingSum", "DepressiveSum", "IntrusiveSum",
                          "DeliberateSum", "ReappraisalSum", "SuppressionSum")
task_measures <- c("SSRT", "Go_RT", "Stop_Signal_Accuracy", "Go_Trial_Accuracy")

# Generate and save plots for each questionnaire score
for (score in questionnaire_scores) {
  plots <- create_correlation_plots(group_variables, score, task_measures)
  ggsave(paste0(score, "_correlations.png"), plots, width = 10, height = 8, dpi = 300)
  cat("Processed", score, "\n")
}

# Print correlation matrix
cor_matrix <- cor(group_variables[, c(questionnaire_scores, task_measures)], use = "pairwise.complete.obs")
print(round(cor_matrix, 3))
