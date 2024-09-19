# Read the CSV file and store it as a variable called stop_signal_raw
stop_signal_raw <- read.csv('raw_data/StopSignal-v6+v7+v8_task-ckcn.csv')

# Filter rows where there's a value (not empty)
stop_signal_preprocessed <- stop_signal_raw[stop_signal_raw != "", ]

# Select only the specified columns
stop_signal_preprocessed <- stop_signal_preprocessed[, c("Schedule.ID",
                                                         "Participant.Public.ID",
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

# Save the preprocessed data to a CSV file
write.csv(stop_signal_preprocessed, "stop_signal_preprocessed.csv", row.names = FALSE)
