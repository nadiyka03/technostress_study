### Digital Enterprise Quantitative Final Assignment ###
        ### Jaeden, Vanda, Nadiia, and Harel ###

### Technostress dataset cleaning ###
### Changing column names ###
names(responses_clean) <- c("Age", "Gender", "working_hours", "usefulness_1", 
                            "usefulness_2", "usefulness_3", "conflicts_1", 
                            "conflicts_2", "conflicts_3", "insecurity_1", 
                            "insecurity_2", "attention_check", "insecurity_3", 
                            "insecurity_4","overload_1", "overload_2", 
                            "overload_3", "unreliability_1", "unreliability_2", 
                            "unreliability_3", "job_satisfaction_1", "job_satisfaction_2",
                            "job_satisfaction_3", "job_performance_1", "job_performance_2",
                            "job_performance_3"
)


### Deleting answers that did not pass 'attention check' ###
responses_clean <- responses_clean[responses_clean$attention_check == 'Untrue', ]


### Changing "Cureently unemployed, but..."  answer so it is numerical ###
responses_clean$working_hours[responses_clean$working_hours == "Currently unemployed, but my previous contract was 40h"] <- "40"


### creating columns that contain means of results to be used as different variables ###
responses_clean <- mutate(responses_clean, 
                          usefulness = rowMeans(select(responses_clean, usefulness_1, usefulness_2, usefulness_3), na.rm = TRUE),
                          conflict = rowMeans(select(responses_clean, conflicts_1, conflicts_2, conflicts_3), na.rm = TRUE),
                          insecurity = rowMeans(select(responses_clean, insecurity_1, insecurity_2, insecurity_3, insecurity_4), na.rm = TRUE),
                          overload = rowMeans(select(responses_clean, overload_1, overload_2, overload_3), na.rm = TRUE),
                          unreliability = rowMeans(select(responses_clean, unreliability_1, unreliability_2, unreliability_3), na.rm = TRUE),
                          job_satisfaction = rowMeans(select(responses_clean, job_satisfaction_1, job_satisfaction_2, job_satisfaction_3), na.rm = TRUE),
                          job_performance = rowMeans(select(responses_clean, job_performance_1, job_performance_2, job_performance_3), na.rm = TRUE))


### checking the class ###
sapply(responses_clean, class)


### exporting a cleaned csv file ###
write.csv(responses_clean, file = "responses_clean.csv", row.names = FALSE)


### install/activate packages ###
install.packages("correlation")
install.packages("tidyverse")
install.packages("GGally")
install.packages("WRS2")
install.packages("boot")
install.packages("GGally")
install.packages("WRS2")
install.packages("broom")
install.packages("ggfortify")
install.packages("robust")
install.packages("sandwich")
install.packages("BayesFactor")
install.packages("pixiedust")


library(tidyverse)
library(correlation)
library(GGally)
library(WRS2)
library(boot)
library(broom)
library(dplyr)
library(GGally)
library(ggfortify)
library(ggplot2)
library(htmltools)
library(magrittr)
library(parameters)
library(robust)
library(sandwich)
library(tibble)
library(BayesFactor)
library(pixiedust)


### Loading in csv file ###
data <- read.csv("responses_clean.csv")


### Checking normality assumption visually for all variables ###

#Conflict
ggplot(data, aes(conflict))  +
  geom_histogram(binwidth = 1, fill = "#56B4E9", colour = "#336c8b", alpha = 0.2) +
  labs(y = "Score (1-7)", x = "Conflict", title = "Conflict scores") +
  theme_minimal()

#Unreliability
ggplot(data, aes(unreliability))  +
  geom_histogram(binwidth = 1, fill = "#FF2400", colour = "#7C0A02", alpha = 0.2) +
  labs(y = "Score (1-7)", x = "Unreliability", title = "Unreliability scores") +
  theme_minimal()

#Overload
ggplot(data, aes(overload))  +
  geom_histogram(binwidth = 1, fill = "#e6cc00", colour = "#e47200", alpha = 0.2) +
  labs(y = "Score (1-7)", x = "Overload", title = "Overload scores") +
  theme_minimal()

#Insecurity
ggplot(data, aes(insecurity))  +
  geom_histogram(binwidth = 1, fill = "#bf6b99", colour = "#79305a", alpha = 0.2) +
  labs(y = "Score (1-7)", x = "Insecurity", title = "Insecurity scores") +
  theme_minimal()

#Usefulness
ggplot(data, aes(usefulness))  +
  geom_histogram(binwidth = 1, fill = "#5ced73", colour = "#008631", alpha = 0.2) +
  labs(y = "Score (1-7)", x = "Usefulness", title = "Usefulness scores") +
  theme_minimal()

### adding levels to gender category ###
data <- mutate (data, Gender = as.factor(Gender))
data$Gender <- fct_relevel(data$Gender, "Male", "Female")
levels(data$Gender)


### Ensure that all column names are correct ###
columns1 <- c("unreliability", "conflict", "job_performance")

columns2 <- c("overload", "insecurity", "job_performance")

columns3 <- c("insecurity", "conflict", "job_satisfaction")

columns4 <- c("usefulness", "overload", "job_satisfaction")


### Use the correct column names with no extra spaces ###
ggscatmat(data, columns = columns1)

ggscatmat(data, columns = columns2)

ggscatmat(data, columns = columns3)

ggscatmat(data, columns = columns4)



### using robust methods because normality assumption is violated ###
                     ### robust correlation ###

correlation(data[, c("job_performance", "unreliability", "conflict")], 
            method = "percentage")
# conflict has a correlation with job performance of 0.14
# unreliability has a correlation with job performance of -0.29

correlation(data[, c("job_performance", "insecurity", "overload")], 
            method = "percentage")
# insecurity has a correlation with job performance of -0.08
# overload has a correlation with job performance of -0.06

correlation(data[, c("job_satisfaction", "insecurity", "conflict")], 
            method = "percentage")
# conflict score has a correlation with job satisfaction of 0.09
# insecurity has a correlation with job satisfaction of -0.21

correlation(data[, c("job_satisfaction", "usefulness", "overload")], 
            method = "percentage")
# usefulness has a correlation with job satisfaction of -0.08
# overload has a correlation with job satisfaction of -0.10



### Winsorized correlation ###

winall(data[, c("job_performance", "unreliability", "conflict")])
# conflict score has a correlation with job performance of 0.14 - weak positive
# unreliability has a correlation with job performance of -0.26 - weak negative
# p value is > 0.05, and is therefore insignificant

winall(data[, c("job_performance", "insecurity", "overload")])
# insecurity has a correlation with job performance of 0.02 - weak positive
# overload has a correlation with job performance of 0.06 - weak positive
# p value is > 0.05, and is therefore insignificant

winall(data[, c("job_satisfaction", "insecurity", "conflict")])
# conflict has a correlation with job satisfaction of 0.11 - weak positive
# insecurity has a correlation with job satisfaction of -0.22 - weak negative
# p value is > 0.05, and is therefore insignificant

winall(data[, c("job_satisfaction", "usefulness", "overload")])
# usefulness has a correlation with job satisfaction of -0.05 - weak negative
# overload has a correlation with job satisfaction of -0.06 - weak negative
# p value is > 0.05, and is therefore insignificant



### running a regression with 1 predictor ###

# ( unreliability - predictor, job_performance - outcome)
lm_unreliability_jp <- lm(job_performance ~ unreliability, data = data, na.action = na.exclude)
glance(lm_unreliability_jp)
summary(lm_unreliability_jp)
tidy(lm_unreliability_jp, conf.int = TRUE)
# unreliability accounts for only 6.5% of the variation in job performance
# p value is > 0.05 so the result is insignificant

# ( conflict - predictor, job_performance - outcome)
lm_conflict_jp <- lm(job_performance ~ conflict, data = data, na.action = na.exclude)
glance(lm_conflict_jp)
summary(lm_conflict_jp)
tidy(lm_conflict_jp, conf.int = TRUE)
# conflict accounts for 2.3% of the variation in job performance
# p value is > 0.05 so the result is insignificant

# ( overload - predictor, job_performance - outcome)
lm_overload_jp <- lm(job_performance ~ overload, data = data, na.action = na.exclude)
glance(lm_overload_jp)
summary(lm_overload_jp)
tidy(lm_overload_jp, conf.int = TRUE)
# overload accounts for 1.7% of the variation in job performance
# p value is > 0.05 so the result is insignificant

# ( insecurity - predictor, job_performance - outcome)
lm_insecurity_jp <- lm(job_performance ~ insecurity, data = data, na.action = na.exclude)
glance(lm_insecurity_jp)
summary(lm_insecurity_jp)
tidy(lm_insecurity_jp, conf.int = TRUE)
# insecurity accounts for - 3% of the variation in job performance
# p value is > 0.05 so the result is insignificant

# ( insecurity - predictor, job_satisfaction - outcome)
lm_insecurity_js <- lm(job_satisfaction ~ insecurity, data = data, na.action = na.exclude)
glance(lm_insecurity_js)
summary(lm_insecurity_js)
tidy(lm_insecurity_js, conf.int = TRUE)
# insecurity accounts for only 4% of the variation in job satisfaction
# p value is > 0.05 so the result is insignificant

# ( conflict - predictor, job_satisfaction - outcome)
lm_conflict_js <- lm(job_satisfaction ~ conflict, data = data, na.action = na.exclude)
glance(lm_conflict_js)
summary(lm_conflict_js)
tidy(lm_conflict_js, conf.int = TRUE)
# conflict accounts for only 0.09% of the variation in job satisfaction
# p value is > 0.05 so the result is insignificant

# ( usefulness - predictor, job_satisfaction - outcome)
lm_usefulness_js <- lm(job_satisfaction ~ usefulness, data = data, na.action = na.exclude)
glance(lm_usefulness_js)
summary(lm_usefulness_js)
tidy(lm_usefulness_js, conf.int = TRUE)
# usefulness accounts for -3% of the variation in job satisfaction
# p value is > 0.05 so the result is insignificant

# ( overload - predictor, job_satisfaction - outcome)
lm_overload_js <- lm(job_satisfaction ~ overload, data = data, na.action = na.exclude)
glance(lm_overload_js)
summary(lm_overload_js)
tidy(lm_overload_js, conf.int = TRUE)
# overload accounts for 1% of the variation in job satisfaction
# p value is > 0.05 so the result is insignificant


### regression with multiple predictors ###

full_lm1 <- lm(job_performance ~ unreliability + conflict, 
              data = data, 
              na.action = na.exclude)
glance(full_lm1)
summary(full_lm1)
tidy(full_lm1, conf.int = TRUE)
# unreliability and conflict together
# account for 14.6% of the variation in job performance
# p value is > 0.05 so the result is insignificant

full_lm2 <- lm(job_performance ~ insecurity + overload, 
               data = data, 
               na.action = na.exclude)
glance(full_lm2)
summary(full_lm2)
tidy(full_lm2, conf.int = TRUE)
# insecurity and overload together
# account for 1.8% of the variation in job performance
# p value is > 0.05 so the result is insignificant

full_lm3 <- lm(job_satisfaction ~ insecurity + conflict, 
               data = data, 
               na.action = na.exclude)
glance(full_lm3)
summary(full_lm3)
tidy(full_lm3, conf.int = TRUE)
# insecurity and conflict together
# account for 5.8% of the variation in job satisfaction
# p value is > 0.05 so the result is insignificant

full_lm4 <- lm(job_satisfaction ~ usefulness + overload, 
               data = data, 
               na.action = na.exclude)
glance(full_lm4)
summary(full_lm4)
tidy(full_lm4, conf.int = TRUE)
# usefulness and overload together
# account for 1.2% of the variation in job satisfaction
# p value is > 0.05 so the result is insignificant


### Convert categorical variables to factors if necessary ###
data <- mutate (data, Gender = as.factor(Gender))
data$Gender <- fct_relevel(data$Gender, "Male", "Female")
levels(data$Gender)


### Fit the multiple regression model ###
multiple_model1 <- lm(job_performance ~ unreliability + conflict + Age + Gender + working_hours, data = data, na.action = na.exclude)
summary(multiple_model1)
glance(multiple_model1)

tidy(anova(lm_unreliability_jp, multiple_model1))
tidy(anova(lm_conflict_jp, multiple_model1))


multiple_model2 <- lm(job_performance ~ overload + insecurity + Age + Gender + working_hours, data = data, na.action = na.exclude)
summary(multiple_model2)
glance(multiple_model2)

tidy(anova(lm_overload_jp, multiple_model2))
tidy(anova(lm_insecurity_jp, multiple_model2))


multiple_model3 <- lm(job_satisfaction ~ conflict + insecurity + Age + Gender + working_hours, data = data, na.action = na.exclude)
summary(multiple_model3)
glance(multiple_model3)

tidy(anova(lm_conflict_jp, multiple_model3))
tidy(anova(lm_insecurity_jp, multiple_model3))


multiple_model4 <- lm(job_satisfaction ~ overload + usefulness + Age + Gender + working_hours, data = data, na.action = na.exclude)
summary(multiple_model4)
glance(multiple_model4)

tidy(anova(lm_overload_js, multiple_model4))
tidy(anova(lm_usefulness_js, multiple_model4))


### robust methods ###

lm_full_rob1 <- lmRob(job_performance ~ unreliability + conflict, 
                     data = data, 
                     na.action = na.exclude)
summary(lm_full_rob1)
tidy(full_lm1, conf.int = TRUE)
# b estimates for unreliability and conflict are almost exactly the same. 
# Unreliability O:  -0.279, R: -0.2787
# Conflict O: 0.198, R: 0.198
# the original model is more than likely unbiased
# p-values are non-significant, so even if there was bias in original
# model, it wouldn't be problematic
model_parameters(full_lm1, robust = TRUE, vcov.type = "HC4", digits = 3)
# values are not much different. SE of unreliability went from 0.130 to 0.121
# SE of conflict went from 0.114 to 0.112
# CI of unreliability went from [-0.543, -0.0142] to [-0.524, -0.033]
# CI of conflict went from [-0.0344, 0.430] to [-0.031, 0.426]
# because changes are not dramatic, non-robust model could be unbiased


lm_full_rob2 <- lmRob(job_performance ~ insecurity + overload, 
                      data = data, 
                      na.action = na.exclude)
summary(lm_full_rob2)
tidy(full_lm2, conf.int = TRUE)
# b estimates for insecurity and overload are almost exactly the same. 
# Insecurity O:  0.034, R: 0.034
# Overload O: - 0.1, R: - 0.1
# the original model is more than likely unbiased
# p-values are non-significant, so even if there was bias in original
# model, it wouldn't be problematic
model_parameters(full_lm2, robust = TRUE, vcov.type = "HC4", digits = 3)
# Slight change in SE of overload, from 0.129 to 0.140
# CI of insecurity went from  [-0.271, 0.338] to [-0.273, 0.340]
# CI of overload went from  [-0.362, 0.162] to [-0.385, 0.185]
# because changes are not dramatic, non-robust model could be unbiased

lm_full_rob3 <- lmRob(job_satisfaction ~ insecurity + conflict, 
                      data = data, 
                      na.action = na.exclude)
summary(lm_full_rob3)
tidy(full_lm3, conf.int = TRUE)
# b estimates for insecurity and conflict are different.
# Insecurity O: - 0.174, R: - 0.24
# conflict O: 0.07, R: - 0.01
# the original model is more than likely biased
# p-values are non-significant, so even if there was bias in original
# model, it wouldn't be problematic
model_parameters(full_lm3, robust = TRUE, vcov.type = "HC4", digits = 3)
# slight changes in SE for insecurity & conflict
# CI of insecurity went from [-0.447, 0.100] to [-0.456, 0.109]
# CI of conflict went from [-0.137, 0.284] to [-0.160, 0.307]
# the original model is more than likely unbiased
# p-values are non-significant, so even if there was bias in original
# model, it wouldn't be problematic

lm_full_rob4 <- lmRob(job_satisfaction ~ usefulness + overload, 
                      data = data, 
                      na.action = na.exclude)
summary(lm_full_rob4)
tidy(full_lm4, conf.int = TRUE)
# b estimates for usefulness and overload are not the same. 
# usefulness o:0.024, rb: 0.1105
# overload O: -0.0740, Rb: -0.1026
# the original model could be biased
model_parameters(full_lm1, robust = TRUE, vcov.type = "HC4", digits = 3)
# values are not much different. SE of usefulness went from 0.143 to 0.141
# SE of overload went from 0.121 to 0.130
# CI of usefulness went from [-0.268, 0.316] to [-0.263, 0.311]
# because changes are not dramatic, non-robust model could be unbiased


### comparing 2 means: Variables predicted by gender ###
yuen(job_performance ~ Gender, data = data, tr = .2, alpha = .05)
# p = 0.47 > 0.05 so there is no significant difference 
yuen(job_satisfaction ~ Gender, data = data, tr = .2, alpha = .05)
# p = 0.09 > 0.05 so there is no significant difference 
yuen(unreliability ~ Gender, data = data, tr = .2, alpha = .05)
# p = 0.31 > 0.05 so there is no significant difference 
yuen(conflict ~ Gender, data = data, tr = .2, alpha = .05)
# p = 0.62 > 0.05 so there is no significant difference
yuen(insecurity ~ Gender, data = data, tr = .2, alpha = .05)
# p = 0.93 > 0.05 so there is no significant difference 
yuen(overload ~ Gender, data = data, tr = .2, alpha = .05)
# p = 0.28 > 0.05 so there is no significant difference 
yuen(usefulness ~ Gender, data = data, tr = .2, alpha = .05)
# p = 0.62 > 0.05 so there is no significant difference 


### robust moderation models ###
#using unreliability as moderator
moderation1 <- mutate(data,
                     unreliability_cent = unreliability - mean(unreliability, na.rm = TRUE),
                     conflict_cent = conflict - mean(conflict, na.rm = TRUE))

moderation_lm1 <- lm(job_performance ~ conflict_cent*unreliability_cent, data = moderation1)

model_parameters(moderation_lm1, robust = TRUE,
                 vcov.type = "HC4", 
                 digits = 3)
# p-scores are still too high so there is no significance 

# using insecurity as moderator
moderation2 <- mutate(data,
                     insecurity_cent = insecurity - mean(insecurity, na.rm = TRUE),
                     overload_cent = overload - mean(overload, na.rm = TRUE))

moderation_lm2 <- lm(job_performance ~ overload_cent*insecurity_cent, data = moderation2)

model_parameters(moderation_lm2, robust = TRUE,
                 vcov.type = "HC4", 
                 digits = 3)
# p-scores are > 0.05 so there is no significance

# using conflict as moderator
moderation3 <- mutate(data,
                     insecurity_cent = insecurity - mean(insecurity, na.rm = TRUE),
                     conflict_cent = conflict - mean(conflict, na.rm = TRUE))

moderation_lm3 <- lm(job_satisfaction ~ conflict_cent*insecurity_cent, data = moderation3)

model_parameters(moderation_lm3, robust = TRUE,
                 vcov.type = "HC4", 
                 digits = 3)
# p scores are still too high, so there is no significance

# using usefulness as moderator
moderation4 <- mutate(df,
                     usefulness_cent = usefulness - mean(usefulness, na.rm = TRUE),
                     overload_cent = overload - mean(overload, na.rm = TRUE))

moderation_lm4 <- lm(job_satisfaction ~ usefulness_cent*overload_cent, data = moderation4)

model_parameters(moderation_lm4, robust = TRUE,
                 vcov.type = "HC4", 
                 digits = 3)
# p values are not significant
