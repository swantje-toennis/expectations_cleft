library(readxl)
library(readr)
library(ggplot2)
library(ggpubr)
library(plyr)
library(dplyr)
library(ez)
library(lme4)
library(languageR)
library(car)
library(MASS)
library(fitdistrplus)
library(ordinal)
library(nlme)
library(logspline)
library(gplots)
library(stringr)
library(tidyr)
library(broom)
library(tidyselect)
library(tidyverse)
library(psy811)
library(mixedup)

#library(betareg)
#library(emmeans)
#library(lmtest)
#library(multcompView)
#library(psych)
#library(rcompanion)
#library(rstatix)



########################################################
####  ADJUST PATH!!!!   ###############
setwd("C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/preprocessed_data")

source("C:/Arbeit/R statistics/mer-utils.r")
source("C:/Arbeit/R statistics/regression-utils.r")
source("C:/Arbeit/R statistics/diagnostic_fcns.r")
source("C:/Arbeit/R statistics/boot_glmm.r")
source("C:/Arbeit/R statistics/helpers.r")

#### read preprocessed data ###########
data <- read.csv("C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/preprocessed_data/data.csv", sep = ";")

#### Interpret expectedness judgment as numbers and context, question and list as a factors #####
data$judgment<-as.numeric(data$judgment)
data$cond_c<-as.factor(data$cond_c)
data$condA<-as.factor(data$condA)
data$condB<-as.factor(data$condB)
data$list <- as.factor(data$list)
str(data)

# code preference: cleft vs. canonical
#add column for tendency: 
  #If cleft is better than canonical -> cleft
  #If canonical is better than cleft -> canonical
  #If they are the same -> equal
data_tendency <- data %>% mutate(tendency = case_when(
  (judgment < 0) ~ "canonical",
  (judgment > 0) ~ "cleft",
  (judgment == 0) ~ "equal",
))

#Define types of columns in data_tendency
data_tendency$judgment<-as.numeric(data_tendency$judgment)
data_tendency$cond_c<-as.factor(data_tendency$cond_c)
data_tendency$list <- as.factor(data_tendency$list)

#Save dataset including tendency.
#This will be the input for graphs
write.table(data_tendency, file="C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/preprocessed_data/data_t.csv", row.names = FALSE, col.names = TRUE, sep = ";")

data_t <- read.csv("C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/preprocessed_data/data_t.csv", sep = ";")

##### show the mean and standard deviation for the two conditions ##########
data_t %>% group_by(cond_c) %>%
  summarize(M = mean(judgment), SD = sd(judgment))


#######################
####   Models #########
######################


#### model 1a -- With varying intercepts and slopes for participant 
#+ context condition and list as fixed effects interaction
#item as random effect doesn't make sense because not all items were seen by all subjects
model1a <- lmer(judgment ~ cond_c * list + (1 + cond_c|id), data = data_t)

### Sanity check of model 1a ######
#<10 is reasonable collinearity, <30 is moderate collinearity, over 30 is troubling
kappa.mer(model1a)   ## good!   
#vif over 5 is troubling, over 2.5 is moderate (checks collinearity)
max(vif.mer(model1a)) ## good (checks collinearity)
# dispersion parameter should not be much larger than 1
overdisp.test(model1a) ## not good!
# residuals should be normally distributed, qq-plot plot should not show strong deviations from the line, 
# residuals against fitted values should show a rather random pattern
diagnostics.plot(model1a) ## good!
#lev.thresh should be close to 0, means that there are no influential cases
lev.thresh(model1a) ## good.

summary(model1a)
get_pvalues(model1a)

#### model 1b -- With varying intercepts and slopes for participant + list as fixed effect
model1b <- lmer(judgment ~ cond_c + list + (1 + cond_c|id), data = data_t)

anova(model1a,model1b) #If significant, keep interaction of list and context condition

#### model 2a -- With varying intercepts for participant 
#+ context condition and list as fixed effects interaction
model2a <- lmer(judgment ~ cond_c * list + (1|id), data = data_t)

#compare model 2a and model 1a (with or without slope)
anova(model2a,model1a) #If significant, keep participant slope as random factor


##### Frequency model ######
chisq.test(data_t$cond_c, data_t$tendency)






