setwd("C:/Users/gayaa/Desktop/אוניברסיטה/סמינריונית + מעשית מחקרית/Raw Data/allTogether")

demographic <- read.csv("demographic-v6+v7+v8_questionnaire-39r7.csv")
ERQ <- read.csv("ERQ-v6+v7+v8_questionnaire-xu9o.csv")
RRS <- read.csv("RRS-v6+v7+v8_questionnaire-u89q.csv")
ERRI <- read.csv("ERRI-v6+v7+v8_questionnaire-j7or.csv")

library(tidyverse)

########### RRS #############

# sanity-check - filter out onyone who didn't answer *1* on sanity.check.2
RRS_sanity_check_filtered <- filter(.data = RRS, sanity.check.2 == 1)

# Calculating sums according to rumination types:
# R = Reflection (7, 11, 12, 20, 21) 
# B = Brooding (5, 10, 13, 15, 16) 
# D = Depression-Related (1, 2, 3, 4, 6, 8, 9, 14, 17, 18, 19, 22)

RRS_sanity_check_filtered$RRS_R_Sum <- (RRS_sanity_check_filtered$RRS.7 + 
                                        RRS_sanity_check_filtered$RRS.11 +
                                        RRS_sanity_check_filtered$RRS.12 + 
                                        RRS_sanity_check_filtered$RRS.20 + 
                                        RRS_sanity_check_filtered$RRS.21)
RRS_sanity_check_filtered$RRS_B_Sum <- (RRS_sanity_check_filtered$RRS.5 + 
                                        RRS_sanity_check_filtered$RRS.10 +
                                        RRS_sanity_check_filtered$RRS.13 + 
                                        RRS_sanity_check_filtered$RRS.15 + 
                                        RRS_sanity_check_filtered$RRS.16)
RRS_sanity_check_filtered$RRS_D_Sum <- (RRS_sanity_check_filtered$RRS.1 + 
                                        RRS_sanity_check_filtered$RRS.2 +
                                        RRS_sanity_check_filtered$RRS.3 + 
                                        RRS_sanity_check_filtered$RRS.4 + 
                                        RRS_sanity_check_filtered$RRS.6 +
                                        RRS_sanity_check_filtered$RRS.8 + 
                                        RRS_sanity_check_filtered$RRS.9 +
                                        RRS_sanity_check_filtered$RRS.14 + 
                                        RRS_sanity_check_filtered$RRS.17 + 
                                        RRS_sanity_check_filtered$RRS.18 +
                                        RRS_sanity_check_filtered$RRS.19 +
                                        RRS_sanity_check_filtered$RRS.22)

# Making clean table

RRS_Clean <- select(.data = RRS_sanity_check_filtered,
                    sub = Participant.Public.ID, 
                    R_Sum = RRS_R_Sum, 
                    B_Sum = RRS_B_Sum, 
                    D_Sum = RRS_D_Sum)
RRS_Clean_arranged <- arrange(.data = RRS_Clean,
                              sub)
write.csv(RRS_Clean_arranged, file = "RRS_Clean.csv")

# Summarizing

print(mean(RRS_Clean$R_Sum))
print(mean(RRS_Clean$B_Sum))
print(mean(RRS_Clean$D_Sum))


# alpha cronbach
#making table for cronbach
Cronbach_RRS_R <- select(.data = RRS_sanity_check_filtered,
                         RRS.7,
                         RRS.11,
                         RRS.12,
                         RRS.20,
                         RRS.21)

Cronbach_RRS_B <- select(.data = RRS_sanity_check_filtered,
                         RRS.5, 
                         RRS.10,
                         RRS.13,
                         RRS.15,
                         RRS.16)
                         
Cronbach_RRS_D <- select(.data = RRS_sanity_check_filtered,
                         RRS.1,
                         RRS.2,
                         RRS.3, 
                         RRS.4, 
                         RRS.6,
                         RRS.8, 
                         RRS.9,
                         RRS.14, 
                         RRS.17, 
                         RRS.18,
                         RRS.19,
                         RRS.22)

#library("ltm")
print(cronbach.alpha(Cronbach_RRS_R))
print(cronbach.alpha(Cronbach_RRS_B))
print(cronbach.alpha(Cronbach_RRS_D))


########### Demographic #############

demographic_arranged <- arrange(.data = demographic,
                                Participant.Public.ID)
demographic_arranged$whetherDignosed <- ifelse(demographic_arranged$diagnosis.mixed == "לא קיבלתי אבחנה פסיכיאטרית", "No", "Yes")

demographic_selected <- select(.data = demographic_arranged, 
                              sub = Participant.Public.ID, 
                              motherTongue = mother.tongue,
                              motherTDetailed = mother.tongue.text,
                              birthCountry = birth.country,
                              yearImigration = year.of.imigration, 
                              age,
                              sex = sex.quantised, # 1 = male, 2 = female
                              strongHand = strong.hand.quantised, # 1 = right, 2 = left, 3 = both
                              education = level.of.education,
                              maritalStatus = marital.status.quantised, # 1 = single, 2 = married
                              childNum = number.of.children,
                              religion = religion.quantised, # 1 = Jewish, 2 = Muslim
                              religionAffinity = religion.affinity, 
                              whetherDignosed, 
                              diagnosisDetailed = diagnosis)
#write.csv(demographic_selected, file = "Demographic_Clean.csv")

# Descriptive statistics
print(mean(demographic_selected$age))
print(sd(demographic_selected$age))
table(demographic_selected$sex)


########### ERRI #############

# Recoding the scales of the answers (x-1, so it will be 0-3 instead of 1-4)
# Intrusive = I (questions 1-10), Deliberate = D, (questions 11-20)
ERRI$Q1_I <- (ERRI$ERRI.intrusive.1.quantised - 1)
ERRI$Q2_I <- (ERRI$ERRI.intrusive.2.quantised - 1)
ERRI$Q3_I <- (ERRI$ERRI.intrusive.3.quantised - 1)
ERRI$Q4_I <- (ERRI$ERRI.intrusive.4.quantised - 1)
ERRI$Q5_I <- (ERRI$ERRI.intrusive.5.quantised - 1)
ERRI$Q6_I <- (ERRI$ERRI.intrusive.6.quantised - 1)
ERRI$Q7_I <- (ERRI$ERRI.intrusive.7.quantised - 1)
ERRI$Q8_I <- (ERRI$ERRI.intrusive.8.quantised - 1)
ERRI$Q9_I <- (ERRI$ERRI.intrusive.9.quantised - 1)
ERRI$Q10_I <- (ERRI$ERRI.intrusive.10.quantised - 1)

ERRI$Q1_D <- (ERRI$ERRI.deliberate.1.quantised - 1)
ERRI$Q2_D <- (ERRI$ERRI.deliberate.2.quantised - 1)
ERRI$Q3_D <- (ERRI$ERRI.deliberate.3.quantised - 1)
ERRI$Q4_D <- (ERRI$ERRI.deliberate.4.quantised - 1)
ERRI$Q5_D <- (ERRI$ERRI.deliberate.5.quantised - 1)
ERRI$Q6_D <- (ERRI$ERRI.deliberate.6.quantised - 1)
ERRI$Q7_D <- (ERRI$ERRI.deliberate.7.quantised - 1)
ERRI$Q8_D <- (ERRI$ERRI.deliberate.8.quantised - 1)
ERRI$Q9_D <- (ERRI$ERRI.deliberate.9.quantised - 1)
ERRI$Q10_D <- (ERRI$ERRI.deliberate.10.quantised - 1)

# Sum of each subject according to subscales

ERRI$IntrusiveSum <- (ERRI$Q1_I + ERRI$Q2_I + ERRI$Q3_I + ERRI$Q4_I + ERRI$Q5_I + 
                      ERRI$Q6_I + ERRI$Q7_I + ERRI$Q8_I + ERRI$Q9_I + ERRI$Q10_I)
ERRI$DeliberateSum <- (ERRI$Q1_D + ERRI$Q2_D + ERRI$Q3_D + ERRI$Q4_D + ERRI$Q5_D + 
                       ERRI$Q6_D + ERRI$Q7_D + ERRI$Q8_D + ERRI$Q9_D + ERRI$Q10_D)

# Clean data
ERRISelected <- select(.data = ERRI,
                       sub = Participant.Public.ID, 
                       IntrusiveSum,
                       DeliberateSum)
ERRIArranged <- arrange(.data = ERRISelected,
                        sub)
                       
#write.csv(ERRIArranged, file = "ERRI_Clean.csv")

# alpha cronbach
#making table for cronbach
Cronbach_ERRI_I <- select(.data = ERRI,
                          Q1_I, Q2_I, Q3_I, Q4_I, Q5_I, 
                          Q6_I, Q7_I, Q8_I, Q9_I, Q10_I)
Cronbach_ERRI_D <- select(.data = ERRI,
                          Q1_D, Q2_D, Q3_D, Q4_D, Q5_D, 
                          Q6_D, Q7_D, Q8_D, Q9_D, Q10_D)
#calculating cronbach
#library("ltm")
print(cronbach.alpha(Cronbach_ERRI_I))
print(cronbach.alpha(Cronbach_ERRI_D))

########### ERQ #############

# sanity-check - filter out anyone who didn't answer *6* on sanity.check.1
ERQ_sanity_check_filtered <- filter(.data = ERQ, sanity.check.1 == 6)


#Items 1, 3, 5, 7, 8, 10 make up the Cognitive Reappraisal facet. 
#Items 2, 4, 6, 9 make up the Expressive Suppression facet. 

# Splitting to subscales

ERQ_sanity_check_filtered$ReappraisalSum <- (ERQ_sanity_check_filtered$ERQ.1 + 
                                             ERQ_sanity_check_filtered$ERQ.3 +
                                             ERQ_sanity_check_filtered$ERQ.5 + 
                                             ERQ_sanity_check_filtered$ERQ.7 + 
                                             ERQ_sanity_check_filtered$ERQ.8 + 
                                             ERQ_sanity_check_filtered$ERQ.10)

ERQ_sanity_check_filtered$SuppressionSum <- (ERQ_sanity_check_filtered$ERQ.2 + 
                                             ERQ_sanity_check_filtered$ERQ.4 +
                                             ERQ_sanity_check_filtered$ERQ.6 + 
                                             ERQ_sanity_check_filtered$ERQ.9)
  

  
ERQ_clean <- select(.data = ERQ_sanity_check_filtered,
                    sub = Participant.Public.ID,
                    ReappraisalSum, SuppressionSum) 
ERQ_arranged <- arrange(.data = ERQ_clean,
                        sub)

#write.csv(ERQ_arranged, file = "ERQ_clean.csv")


# alpha cronbach
#making table for cronbach
Cronbach_ERQ_Reappraisal <- select(.data = ERQ_sanity_check_filtered,
                                   ERQ.1, ERQ.3, ERQ.5, ERQ.7, ERQ.8, ERQ.10)
Cronbach_ERQ_Suppression <- select(.data = ERQ_sanity_check_filtered,
                                   ERQ.2, ERQ.4, ERQ.6, ERQ.9)

#calculating cronbach
print(cronbach.alpha(Cronbach_ERQ_Reappraisal))
print(cronbach.alpha(Cronbach_ERQ_Suppression))
