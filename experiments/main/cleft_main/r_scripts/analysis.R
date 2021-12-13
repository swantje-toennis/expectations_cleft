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


########################################################
####  ADJUST PATH!!!!   ###############
setwd("C:/Arbeit/Expectedness/experiments/cleft_exp/main/results/preprocessed_data")

source("C:/Arbeit/R statistics/mer-utils.r")
source("C:/Arbeit/R statistics/regression-utils.r")
source("C:/Arbeit/R statistics/diagnostic_fcns.r")
source("C:/Arbeit/R statistics/boot_glmm.r")
source("C:/Arbeit/R statistics/helpers.r")

#### read preprocessed data ###########
data_t <- read.csv("C:/Arbeit/Expectedness/experiments/cleft_exp/main/results/preprocessed_data/data_t.csv", sep = ";")

#### Interpret judgment as numbers and context, tendency, condition A, condition B and list as a factors #####
data_t$judgment<-as.numeric(data_t$judgment)
data_t$cond_c<-as.factor(data_t$cond_c)
data_t$condA<-as.factor(data_t$condA)
data_t$condB<-as.factor(data_t$condB)
data_t$list <- as.factor(data_t$list)
data_t$tendency <- as.factor(data_t$tendency)
str(data_t)

##### show the mean and standard deviation for the two conditions ##########
data_t %>% group_by(cond_c) %>%
  summarize(M = mean(judgment), SD = sd(judgment))


#######################
####   Models ----
######################

#### model 1a -- With varying intercepts and slopes for participant 
#+ context condition and list as fixed effects interaction
#item as random effect doesn't make sense because not all items were seen by all subjects
model1a <- lmer(judgment ~ cond_c * list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_t)
#singular
ranef(model1a)

lm <- lm(judgment ~ cond_c, data = data_t)
summary(lm)
step(lm)

lm(formula = judgment ~ cond_c, data = data_t)

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
model1b <- lmer(judgment ~ cond_c + list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_t)
#singular
ranef(model1b)

anova(model1a,model1b) #If significant, keep interaction of list and context condition

#### model 1c -- With varying intercepts and slopes for participant without list as fixed effect
model1c <- lmer(judgment ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_t)
#singular

#compare model 1b and model 1c (with or without list)
anova(model1b,model1c) #If significant, keep list as main fixed effect

#### model 2a -- With varying slope for participant, but not item 
model2a <- lmer(judgment ~ cond_c + (1 + cond_c|id) + (1|target_no), data = data_t)
summary(model2a) # not singular -> pick this model!
ranef(model2a)

get_pvalues(model2a)

model2d <- lmer(judgment ~ cond_c + (1|id) + (1|target_no), data = data_t)
summary(model2d)
anova(model2a,model2d)

model2e <- lmer(judgment ~ cond_c + (1|id), data = data_t)


anova(model2d,model2e)

model3 <- lm(judgment ~ cond_c, data = data_t)
summary(model3)
#compare model 2a and model 1c (with or without slope for item)
anova(model2a,model1c) #If significant, keep item slope as random factor

#### model 2b -- With varying slope for item, but not participant 
model2b <- lmer(judgment ~ cond_c + (1|id) + (1 + cond_c|target_no), data = data_t)

#compare model 2b and model 1c (with or without slope for participant)
anova(model2b,model1c) #If significant, keep participant slope as random factor





