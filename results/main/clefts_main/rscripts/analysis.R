# Script for analysis of cleft experiment reported in SALT paper
# "German clefts address unexpected questions"
# by Swantje Tönnis and Judith Tonhauser


#####################################################
# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Load libraries
library(lme4)
library(languageR)
library(tidyverse)
library(gazer) #get pvalues
  #For get_pvalues: load package from GitHub server
  #install.packages("remotes")
  #remotes::install_github("dmirman/gazer")

# additional sources
source("mer-utils.r")
source("diagnostic_fcns.r")

########################################################



#### load data ###########
data_t <- read.csv("../data/preprocessed_data/data_t.csv", sep = ";")
nrow(data_t) #372

#### Interpret judgment as numbers and context, tendency, condition A, condition B and list as a factors #####
data_t$judgment<-as.numeric(data_t$judgment)
data_t$cond_c<-as.factor(data_t$cond_c)
data_t$condA<-as.factor(data_t$condA)
data_t$condB<-as.factor(data_t$condB)
data_t$list <- as.factor(data_t$list)
data_t$tendency <- as.factor(data_t$tendency)
str(data_t)

##### show the mean and standard deviation for the two expectedness conditions ##########
data_t %>% group_by(cond_c) %>%
  summarize(M = mean(judgment), SD = sd(judgment))
#cond_c     M    SD
# c1      6.63  65.5
# c3     32.3   55.0


#######################
####   Models ----
######################

# Stepwise comparison of a more specified model to a less specified model
# Used anova to compare two models

# -- model 1 -- 
# context condition ('cond_c') and list as fixed effects interaction,
# varying intercepts and slopes for participant ('id')
# varying intercepts and slopes for context item ('target_no')
model1 <- lmer(judgment ~ cond_c * list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_t, 
                     REML=F,control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
# -> singular


# -- model 2 --
# context condition ('cond_c') and list as fixed effects (without interaction),
# varying intercepts and slopes for participant ('id')
# varying intercepts and slopes for context item ('target_no')
model2 <- lmer(judgment ~ cond_c + list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_t, 
               REML=F,control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
# -> singular

# comparison of model 1 and 2
anova(model1,model2) 
# -> not significant: drop interaction of list and context condition


# -- model 3 -- 
# context condition ('cond_c') as fixed effect (list dropped),
# varying intercepts and slopes for participant ('id')
# varying intercepts and slopes for context item ('target_no')
model3 <- lmer(judgment ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_t, 
           REML=F,control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
# -> singular
summary(model3)
# -> correlation of -1 for by-item slope -> remove by-item slope


# -- model 4 (reported in paper) --
# context condition ('cond_c') as fixed effect,
# varying intercepts and slopes for participant ('id')
# varying intercepts for context item ('target_no')
# The random by-item slope for the fixed effect was removed because of a correlation of -1.
model4 <- lmer(judgment ~ cond_c + (1 + cond_c|id) + (1|target_no), data = data_t, 
           REML=F,control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(model4)


### Sanity check for model 4 ######
#<10 is reasonable collinearity, <30 is moderate collinearity, over 30 is troubling
kappa.mer(model4)   ## 2.826852  -> good!   
#vif over 5 is troubling, over 2.5 is moderate (checks collinearity)
max(vif.mer(model4)) ## 1 -> good! 
# dispersion parameter should not be much larger than 1
overdisp.test(model4) ## 2060.196 -> not good!
# residuals should be normally distributed, qq-plot plot should not show strong deviations from the line, 
# residuals against fitted values should show a rather random pattern
diagnostics.plot(model4) ## ok!
#lev.thresh should be close to 0, means that there are no influential cases
lev.thresh(model4) ## 0.01612903 -> good.


# Values reported in the paper:
get_pvalues(model4)
#             Estimate Std..Error  t.value     p.normal p.normal.star
#(Intercept)  6.486269    5.67911 1.142128 2.534009e-01              
#cond_cc3    25.988296    5.81402 4.469936 7.824314e-06           ***







