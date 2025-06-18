library(tidyverse)
library(dplyr)
library(ggplot2)
ad22 <- read.csv("adult22.csv", stringsAsFactors = FALSE)

#Filter veteran information
#Select variables
vet22 <- ad22 %>%
  filter(AFVET_A == 1) %>%
  select(DEPEV_A, SOCSCLPAR_A, SOCERRNDS_A, MHTHRPY_A, DEPMED_A, MHTHND_A, MEDNG12M_A, EDUCP_A, MARITAL_A, AGEP_A)


#Factors with 2 level (1, 0)
vet22_converted <- vet22 %>%
  mutate(Depression = as.factor(ifelse(DEPEV_A == 1, 1, 
                                       ifelse(DEPEV_A == 2, 0, NA))),
         ErrandAlone = as.factor(ifelse(SOCERRNDS_A %in% c(2, 3, 4), 1, 
                                        ifelse(SOCERRNDS_A == 1, 0, NA))),
         SocialActivity = as.factor(ifelse(SOCSCLPAR_A %in% c(2, 3, 4), 1, 
                                           ifelse(SOCSCLPAR_A == 1, 0, NA))),
         MedDepression = as.factor(ifelse(DEPMED_A == 1, 1, 
                                          ifelse(DEPMED_A == 2, 0, NA))),
         Therapy = as.factor(ifelse(MHTHRPY_A == 1, 1, 
                                    ifelse(MHTHRPY_A == 2, 0, NA))),
         DeniedTherapy = as.factor(ifelse(MHTHND_A == 1, 1, 
                                          ifelse(MHTHND_A == 2, 0, NA))),
         DeniedMedCare = as.factor(ifelse(MEDNG12M_A == 1, 1, 
                                          ifelse(MEDNG12M_A == 2, 0, NA))),
         Education = as.factor(ifelse(EDUCP_A == 0, 1, #no education
                                      ifelse(EDUCP_A %in% c(1, 2, 3, 4), 2, #high school 
                                             ifelse(EDUCP_A %in% c(5, 6, 7), 3, #some degree
                                                    ifelse(EDUCP_A %in% c(8, 9, 10), 4 , NA))))), #higher education
         Marital = as.factor(ifelse(MARITAL_A %in% c(1, 2), 1, #married/partner
                                    ifelse(MARITAL_A == 3, 2, NA))), #single
         Age = AGEP_A) %>% 
  select(Depression, ErrandAlone, SocialActivity, MedDepression, Therapy, 
         DeniedTherapy, DeniedMedCare, Education, Marital, Age) %>%
  drop_na()

#Logistic regression model
model <- glm(Depression ~ ErrandAlone + SocialActivity + MedDepression + 
               Therapy + DeniedTherapy + DeniedMedCare + Education + Marital + Age,
             data = vet22_converted, family = binomial)
summary(model)

nothernomed <- glm(Depression ~ ErrandAlone + SocialActivity + DeniedTherapy + 
               DeniedMedCare + Education + Marital + Age,
             data = vet22_converted, family = binomial)
summary(nothernomed)

#demographics visual
vet22_converted$Predicted_Probability <- predict(model, type = "response")
ggplot(vet22_converted, aes(x = Education, y = Predicted_Probability)) +
  geom_boxplot() +
  labs(x = "Education Level",
       y = "Probability of Depression") +
  theme_minimal() +
  theme(legend.position = "none")

edu <- vet22_converted %>%
  group_by(Education) %>%
  summarise(count = n())

mar <- vet22_converted %>%
  group_by(Marital) %>%
  summarise(count = n())

age <- vet22_converted %>%
  group_by(Age) %>%
  summarise(count = n())

ggplot(vet22_converted, aes(x = Marital, y = Predicted_Probability)) +
  geom_boxplot() +
  labs(x = "Marital Status",
       y = "Probability of Depression") +
  theme_minimal() +
  theme(legend.position = "none")

#Predictor 1 - Social functioning
SocModel <- glm(Depression ~ ErrandAlone + SocialActivity, 
                data = vet22_converted, family = binomial)
summary(SocModel)

#Predictor2 - Financial problem
FinModel <- glm(Depression ~ DeniedTherapy + DeniedMedCare, 
                data = vet22_converted, family = binomial)
summary(FinModel)

#Predictor3 - Help seeking behavour
HelpModel <- glm(Depression ~ MedDepression + Therapy, 
                 data = vet22_converted, family = binomial)
summary(HelpModel)

#Test without Depression medication
NoMed <- glm(Depression ~ ErrandAlone + SocialActivity + 
               Therapy + DeniedTherapy + DeniedMedCare + Education + Marital + Age,
             data = vet22_converted, family = binomial)
summary(NoMed) 

NoTher <- glm(Depression ~ ErrandAlone + SocialActivity + 
                MedDepression + DeniedTherapy + DeniedMedCare + Education + Marital + Age,
              data = vet22_converted, family = binomial)
summary(NoTher) 

#Demographic
demo <- glm(Depression ~  Education + Marital + Age,
            data = vet22_converted, family = binomial)
summary(demo)
