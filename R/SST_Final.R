path = "C:/Users/gayaa/Desktop/אוניברסיטה/סמינריונית + מעשית מחקרית/Raw Data/allTogether/afterCleaning"
setwd(path)

library(tidyverse)

SSTData <- read.csv("SSTData.csv")


SST.Selected <- SSTData %>%
  select(participantID = Participant.Public.ID, # Take SubjID var 
         Stimulus, # Blue/Yellow(right/left)? Added at 9.4
         StimuliType = display,
         TrialType = Task,
         ACC,
         RT,
         ssd,
         Response) %>%
  #left_join(IsSmoker, by = "SubjID")  %>% # Will add Smoker variable 
  mutate(RT = ifelse(RT=="NoResponse",NA,RT))


# Send to Analyze it -------------

SST.Exp.ANALYZEIT <- SST.Selected %>%
  mutate(
    rt = (ifelse(is.na(RT), -250, RT)),
    SSD = ssd,
    signal = as.character(plyr::revalue(TrialType, c("NoStop"="no","Stop"="yes"))),
    #participantID = interaction(Smoker,SubjID, StimuliType, sep = "."),
    response = as.factor(plyr::revalue(Response, c("NoGo" = "undefined"))),
    correct = as.logical(ACC),
    focus = "focus",
    Fullscreen = "yes",
    block_i = 1
  ) %>%
  group_by(participantID) %>%
  mutate(trial_i = 1:n()) %>% ungroup() %>%
  select(participantID,
         block_i,
         trial_i,
         signal,
         SSD,
         response,
         rt,
         correct,
         focus,
         Fullscreen
  )

write.csv(SST.Exp.ANALYZEIT,"2AnalyzeIt.csv") 

setwd("C:/Users/gayaa/Desktop/אוניברסיטה/סמינריונית + מעשית מחקרית/Raw Data/allTogether/afterCleaning")
Merge_SST <- read.csv("SST2AnalyzeIt.csv")
##############From Shir's Code##################################

stopsignal <- Merge_SST %>%
  filter(signal=="yes") %>%
  mutate(tmp = ifelse(response== "undefined", 0, 1)) %>%
  group_by(participantID) %>%
  summarise(Nstop = n(),
            presp = mean(tmp),
            ssd = round(mean(SSD))) %>%
  ungroup()


stopsignal.resp.trials <- Merge_SST %>%
  filter(signal=="yes") %>%
  filter(response!="undefined") %>%
  mutate(usRT = ifelse(is.na(rt),-250,rt)) %>%
  group_by(participantID) %>%
  summarise(usRT = round(mean(as.numeric(usRT)))) %>%
  ungroup()

gosignal <- Merge_SST %>%
  filter(signal=="no") %>%
  group_by(participantID) %>%
  mutate(tmp = ifelse(response== "undefined", 0, 1),
         Ngoo = n()) %>%
  filter(response!="undefined") %>%
  mutate(goRT = ifelse(is.na(rt),-250,rt)) %>%
  summarise(Ngo = mean(Ngoo),
            go_omission = 1-(n()/Ngo),
            goRT_all = round(mean(as.numeric(goRT))),
            goRT_sd = round(sd(goRT)),
            goRT_Max = max(goRT)) %>%
  ungroup()

calcNth <- Merge_SST %>%
  filter(signal=="no") %>%
  left_join(gosignal, by = "participantID") %>%
  left_join(stopsignal, by = "participantID") %>%
  mutate(goRT.adj = ifelse(is.na(rt),
                           goRT_Max,rt)) %>%
  group_by(participantID) %>%
  summarise(Nth = round(quantile(goRT.adj, 
                                 probs = mean(presp), type = 6))) %>%
  ungroup()



go.correct.trials <- Merge_SST %>%
  filter(signal=="no") %>%
  filter(correct==T) %>%
  group_by(participantID) %>%
  summarise(goRT_correct = round(mean(rt)),
            GoCorrect = n()) %>%
  ungroup() %>%
  left_join(gosignal) %>%
  group_by(participantID) %>%
  mutate(go_error = 1-(GoCorrect/(-1*(go_omission-1)*Ngo))) %>%
  ungroup()


go_premature <- Merge_SST %>%
  filter(signal=="no") %>%
  filter(rt<0) %>%
  group_by(participantID) %>%
  summarise(premature.trials = n()) %>%
  left_join(go.correct.trials) %>%
  mutate(go_ameture= premature.trials/(-1*(go_omission-1)*Ngo))

# Work on SST Results -----

SST_Summary <- left_join(go.correct.trials,go_premature) %>%
  left_join(calcNth) %>%
  left_join(stopsignal.resp.trials) %>%
  left_join(stopsignal) %>%
  mutate(SSRT = Nth - ssd)

#write.csv(SST_Summary, file = "SST_Summery_NOTclean.csv")

#rm(go.correct.trials)
#rm(go_premature)
#rm(calcNth)
#rm(stopsignal.resp.trials)
#rm(stopsignal)
#rm(gosignal)
#rm(Merge_SST)

SST_Summary_clean <- SST_Summary %>%
  filter(between(presp,0.25,0.75)) %>%
  filter(usRT<goRT_all) %>%
  separate(participantID, c("sub", "subject_nr","Group"))
SST_Summary_clean$sub <- SST_Summary$participantID


write.csv(SST_Summary_clean, file = "SST_Summery_clean.csv")
















ggplot(data = Temp, aes(x=RT)) +
  geom_density() + NULL
facet_wrap(~s)


