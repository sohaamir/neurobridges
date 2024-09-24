# Read the CSV file and store it as a variable called stop_signal_raw
stop_signal_raw <- read.csv('raw_data/StopSignal-v6+v7+v8_task-ckcn.csv')

# Filter rows where there's a value (not empty)
stop_signal_preprocessed <- stop_signal_raw[stop_signal_raw != "", ]

# Select only the specified columns
stop_signal_preprocessed <- stop_signal_preprocessed[, c("Participant.Public.ID",
                                                         "Trial.Number",
                                                         "Reaction.Time",
                                                         "Response",
                                                         "Attempt",
                                                         "Correct",
                                                         "Incorrect",
                                                         "ssd",
                                                         "display",
                                                         "ANSWER",
                                                         "Task")]

# Remove rows with empty values in the 'Response' column and reassign
stop_signal_preprocessed <- stop_signal_preprocessed[stop_signal_preprocessed$Response != "", ]

# Remove rows below 22307 and reassign
stop_signal_preprocessed <- stop_signal_preprocessed[1:22307, ]

# Remove rows where "ssd" is NA and "display" is not "craving"
stop_signal_preprocessed <- stop_signal_preprocessed[!(is.na(stop_signal_preprocessed$ssd) | stop_signal_preprocessed$display != "craving"), ]

# Remove 'display' and 'Schedule.ID' columns and reassign
stop_signal_preprocessed <- stop_signal_preprocessed[, !(names(stop_signal_preprocessed) %in% c("display", "Schedule.ID"))]

# Save the preprocessed data to a CSV file
write.csv(stop_signal_preprocessed, "stop_signal_preprocessed.csv", row.names = FALSE)

# Remove rows below 22307 and reassign
stop_signal_preprocessed_five <- stop_signal_preprocessed[1:1200, ]

# For stop_signal_preprocessed
names(stop_signal_preprocessed) <- c("id", "trial_number", "rt", "response", "attempt", "correct", "incorrect", "ssd", "answer", "task")

# Save as CSV
write.csv(stop_signal_preprocessed, "stop_signal_preprocessed.csv", row.names = FALSE)

# For stop_signal_preprocessed_five
names(stop_signal_preprocessed_five) <- c("id", "trial_number", "rt", "response", "attempt", "correct", "incorrect", "ssd", "answer", "task")




library(tidyverse)


# Preprocess the data
stop_signal_preprocessed_five_norm <- stop_signal_preprocessed_five %>%
  mutate(
    stop = as.integer(task == "Stop"),
    rt_z = scale(rt),
    ssd_z = scale(ssd)
  )

# Save as CSV
write.csv(stop_signal_preprocessed_five_norm, "stop_signal_preprocessed_five_norm.csv", row.names = FALSE)
