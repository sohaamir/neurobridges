setwd("C:/Users/gayaa/Desktop/אוניברסיטה/סמינריונית + מעשית מחקרית/Raw Data/allTogether")
NBack_raw <- read.csv("NBack-v6+v7+v8_task-ej3v.csv")
# setwd("C:/Users/Weisl/Desktop/HelpGaya")
# NBack_raw <- read.csv("nback.csv")

library(tidyverse)


# 0 - Back

# Create zero back result frame
zero_nback_result <- data.frame()
# Get subset of all 0 back trials
zero_trials = subset(NBack_raw, NBack_raw$display == "Trials_0_Back" &
                       NBack_raw$Screen.Name != "Fixation")

# For each participant number
for (participentNum in unique(zero_trials$Participant.Public.ID)) {
  # For each trial number
  for (trialNum in unique(zero_trials$Trial.Number)) {
    # Create subset for all rows with the relevant participant and trial
    tempDf = subset(zero_trials, 
                    zero_trials$Participant.Public.ID==participentNum &
                      zero_trials$Trial.Number == trialNum)
    # Get only the response rows
    only_response = tempDf[tempDf$Zone.Type == 'response_keyboard', ]
    # If there is no response
    if (nrow(only_response) == 0) {
      # Get the expected answer
      answer = tempDf$Answer[1]
      # Add row to results with the relevant participant, trial and answer
      zero_nback_result[nrow(zero_nback_result) + 1, ] = c(participentNum,
                                                           trialNum,
                                                           0, "Stimulus",
                                                           "Zone2", 
                                                           "timelimit_screen", 
                                                           -1, 'Time out',
                                                           0, 0, 0, 0,
                                                           1, 0, 0, 
                                                           "Trials_0_Back",
                                                           answer,
                                                           0,0,0,0)
    }
    # The participant did respond
    else {
      # Add the first response row to the results frame
      zero_nback_result = rbind(zero_nback_result, only_response[1, ])
    }
    # Print the result frame length (for sanity check)
    print(nrow(zero_nback_result))
  }
}

# Get all the unique value of Answer in 0 back (for sanity check)
unique(zero_nback_result$Answer)

# 2 - Back

# Create 2 back result frame
two_nback_result <- data.frame()
# Get subset for only 2 back trials
two_trials =  subset(NBack_raw, NBack_raw$display == "Trials_2_Back" &
                       NBack_raw$Screen.Name != "Fixation")

# Two back calculation

# For each unique participant from column Participant.Public.ID
for (participentNum in unique(two_trials$Participant.Public.ID)) {
  # For each unique trial number from column Trial.Number
  for (trialNum in unique(two_trials$Trial.Number)) {
    # Create subset for all the rows that fit the trialNum
    # and participant Id
    tempDf = subset(two_trials, 
                    two_trials$Participant.Public.ID==participentNum &
                      two_trials$Trial.Number == trialNum)
    # Get only the response_keyboard Zone.Type
    only_response = tempDf[tempDf$Zone.Type=='response_keyboard', ]
    # If there is no row in only_response AKA the participant did not
    # Answer this trial
    if (nrow(only_response) == 0) {
      # Get the expected answer
      answer = tempDf$Answer[1]
      # Add row for timeout trial with the expected answer in it
      two_nback_result[nrow(two_nback_result) + 1, ] = c(participentNum,
                                                         trialNum,
                                                         0, "Stimulus",
                                                         "Zone2", 
                                                         "timelimit_screen", 
                                                         -1, 'Time out',
                                                         0, 0, 0, 0,
                                                         1, 0, 0, 
                                                         "Trials_2_Back",
                                                         answer,
                                                         0,0,0,0)
    }
    # The participant answered 
    else {
      # Add the first response to the result frame
      two_nback_result = rbind(two_nback_result, only_response[1, ])
    }
    # Print the length of result frame (for sanity check)
    print(nrow(two_nback_result))
  }
}



##### Characterizing 0-back and 2-back
only_0_back <- zero_nback_result %>%
  select(sub = Participant.Public.ID,
         TrialNum = Trial.Number,
         Zone.Type,
         Response,
         Attempt,
         Answer,
         Correct,
         Letters,
         TargetLetter,
         Timed.Out,
         display) %>%
  mutate(Targeted = ifelse(Answer == "Yes", 1, 0)) %>%
  mutate(NotTargeted = ifelse(Answer == "No", 1, 0))


only_2_back <- two_nback_result %>%
  select(sub = Participant.Public.ID,
         TrialNum = Trial.Number,
         Zone.Type,
         Response,
         Attempt,
         Answer,
         Correct,
         Letters,
         Timed.Out,
         display) %>%
  mutate(Targeted = ifelse(Answer == "Yes", 1, 0)) %>%
  mutate(NotTargeted = ifelse(Answer == "No", 1, 0))




## Hits — the rate of correct response-to-target trials out of all targets.
## Correct rejections — the rate of correct no-response trials out of all 
# trials in which there was no target.

targeted_0_back <- only_0_back %>%
  filter(Targeted == 1) %>%
  mutate(Hits = ifelse(Correct == 1, 1, 0)) %>%
  group_by(sub) %>%
  summarise(HitRate_0Back = mean(Hits)) %>%
  ungroup()

notTargeted_0_back <- only_0_back %>%
  filter(NotTargeted == 1) %>%
  mutate(CorrectRejections = ifelse(Correct == 1, 1, 0)) %>%
  group_by(sub) %>%
  summarise(CorrectRejectionsRate_0Back = mean(CorrectRejections)) %>%
  ungroup()



targeted_2_back <- only_2_back %>%
  filter(Targeted == 1) %>%
  mutate(Hits = ifelse(Correct == 1, 1, 0)) %>%
  group_by(sub) %>%
  summarise(HitRate_2Back = mean(Hits)) %>%
  ungroup()

notTargeted_2_back <- only_2_back %>%
  filter(NotTargeted == 1) %>%
  mutate(CorrectRejections = ifelse(Correct == 1, 1, 0)) %>%
  group_by(sub) %>%
  summarise(CorrectRejectionsRate_2Back = mean(CorrectRejections)) %>%
  ungroup()

Merged_Nback <- left_join(targeted_0_back, notTargeted_0_back) %>%
  left_join(targeted_2_back) %>%
  left_join(notTargeted_2_back)

write.csv(Merged_Nback, file = "Merged_Nback.csv")

twoBackClean <- Merged_Nback %>%
  filter(between(HitRate_2Back,0.25,1))
exclude = df_sum_correct$participant[abs(scale(df_sum_correct$correct_rate))>3] #exclude outlier participants
