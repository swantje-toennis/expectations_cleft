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


#### ADJUST PATH #######
########################################################
setwd("C:/Arbeit/Expectedness/experiments/pretest/main/results/preprocessed_data")

source("C:/Arbeit/R statistics/mer-utils.r")
source("C:/Arbeit/R statistics/regression-utils.r")
source("C:/Arbeit/R statistics/diagnostic_fcns.r")
source("C:/Arbeit/R statistics/boot_glmm.r")
source("C:/Arbeit/R statistics/helpers.r")

#### ADJUST PATH #######
#### read preprocessed data ###########
data <- read.csv("C:/Arbeit/Expectedness/experiments/pretest/main/results/preprocessed_data/data.csv", sep = ";")

#### Interpret expectedness judgment as numbers and context, question and list as a factors #####
data$expec<-as.numeric(data$expec)
data$cond_c<-as.factor(data$cond_c)
data$cond_q<-as.factor(data$cond_q)
data$list <- as.factor(data$list)
str(data)



###############################################
#     Analysis with normalized data
###############################################
 

###### normalize data ########
zdata1 <- data %>% group_by(id,target_no,cond_c)%>%
  summarize(id, target_no, cond_c, norm_exp = 100*expec/sum(expec), expec, cond_q, list)
normalized_data <- zdata1 %>%
  mutate_at("norm_exp", str_replace, "NaN", "0")
normalized_data$norm_exp <- as.numeric(normalized_data$norm_exp)
str(normalized_data)

##### Take out all questions except pq1 ##########
normalized_data_pq1 <- subset(normalized_data, cond_q=="pq1")
normalized_data_pq1$norm_exp<-as.numeric(normalized_data_pq1$norm_exp)
str(normalized_data_pq1)

##### show the mean and standard deviation for the two context conditions ##########
normalized_data_pq1 %>% group_by(cond_c) %>%
  summarize(M = mean(norm_exp), SD = sd(norm_exp))


#######################
####   Models #########
#######################

##### t-test #######
test<-t.test(norm_exp ~ cond_c, data=normalized_data_pq1, var.equal=FALSE, na.rm=TRUE)
print(test)

#################################################
####  Accounting for the effect of the list  ####
#################################################
levels(normalized_data_pq1$list)

#### model2a with context and list as main effects ######
model2a <- lmer(norm_exp ~ cond_c + list + (1 + cond_c|id) + (1 + cond_c|target_no), data = normalized_data_pq1)
summary(model2a)

#### model2b with context and list as main effects and their interaction ######
model2b <- lmer(norm_exp ~ cond_c * list + (1 + cond_c|id) + (1 + cond_c|target_no), data = normalized_data_pq1)
summary(model2b)

anova(model2a,model2b)
#-> Not significantly different. -> no interaction between list and context condition

#### model 2 -- With varying intercepts and slopes for subject and item (without list)
model2 <- lmer(norm_exp ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), data = normalized_data_pq1)

anova(model2a,model2)
#-> Not significantly different. -> no main effect of list

#### model 4 -- With only varying intercepts for item but varying intercepts
#and slopes for participant
model4 <- lmer(norm_exp ~ cond_c + (1+ cond_c|id) + (1|target_no), data = normalized_data_pq1)

#### compare model 2 and 4
anova(model2,model4)
#### -> significantly different! -> Include item slope

#### model 4a -- With only varying intercepts for participant but varying intercepts
#and slopes for item
model4a <- lmer(norm_exp ~ cond_c + (1|id) + (1+ cond_c|target_no), data = normalized_data_pq1)

#### compare model 2 and 4a
anova(model2,model4a)
#### -> significantly different! -> Include participant slope

#### Model 2 is the right one ####
### Sanity check of model 2 ######
#<10 is reasonable collinearity, <30 is moderate collinearity, over 30 is troubling
kappa.mer(model2)   ## good!   
#vif over 5 is troubling, over 2.5 is moderate (checks collinearity)
max(vif.mer(model2)) ## good (checks collinearity)
# dispersion parameter should not be much larger than 1
overdisp.test(model2) ## not good!
# residuals should be normally distributed, qq-plot plot should not show strong deviations from the line, 
# residuals against fitted values should show a rather random pattern
diagnostics.plot(model2) ## good!
#lev.thresh should be close to 0, means that there are no influential cases
lev.thresh(model2) ## good.

#### Summary model 2  ######
summary(model2)
random_effects_model2 <- extract_random_effects(model2)

#### get the p-values for model2 ######
get_pvalues(model2)   #### p-value is 0. Does that make sense?





