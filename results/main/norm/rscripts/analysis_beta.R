library(tidyverse)
library(tidybayes)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
library(brms)
library(knitr)
library(emmeans)
library(lme4)
library(padr)
library(performance)
library(MuMIn)
theme_set(theme_bw())

########################################################
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
#setwd("C:/Arbeit/Expectedness/experiments/norm/main/results/preprocessed_data")

source("C:/Arbeit/R statistics/helpers.r")
source("C:/Arbeit/R statistics/mer-utils.r")
source("C:/Arbeit/R statistics/regression-utils.r")
source("C:/Arbeit/R statistics/diagnostic_fcns.r")
source("C:/Arbeit/R statistics/boot_glmm.r")


#### read preprocessed data ###########
data <- read.csv("C:/Arbeit/Expectedness/experiments/norm/main/results/preprocessed_data/data.csv", sep = ";")
nrow(data)  #6240

#### Interpret expectedness judgment as numbers and context, question and list as a factors #####
data$expec<-as.numeric(data$expec)
data$cond_c<-as.factor(data$cond_c)
data$cond_q<-as.factor(data$cond_q)
data$list <- as.factor(data$list)
str(data)

##### Take out all questions except pq1 ##########
data_pq1 <- subset(data, cond_q=="pq1")
str(data_pq1)
nrow(data_pq1)  #1248


##### means and sd for pq1 ##########
data_pq1 %>% group_by(cond_c) %>%
  summarize(M = mean(expec), SD = sd(expec))

##########################
####  Beta model #########
##########################

# Because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y'' = (y' ?? (n ??? 1) + 0.5)/n
data_pq1$betaresponse = (data_pq1$expec/100*(nrow(data_pq1)-1) + .5)/nrow(data_pq1)


# run beta model ##
betamodel = bf(betaresponse ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), # beta distribution's precision  )
               family = Beta())

summary(betamodel)

m.b = brm(formula = betamodel,
          family=Beta(),
          data=data_pq1, 
          cores = 4,
          control = list(adapt_delta = .95,max_treedepth=15))

summary(m.b)
## How to continue with this model?????

#### model 2 -- With varying intercepts and slopes for participant and item and list as fixed effect and interaction
model2 <- lmer(expec ~ cond_c * list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1)

#### model 2a -- With varying intercepts and slopes for participant and item and list as fixed effect without interaction
model2a <- lmer(expec ~ cond_c + list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1)

anova(model2,model2a) ## not significant -> Interaction of list out

#### model 2b -- With varying intercepts and slopes for participant and item (without list)
model2b <- lmer(expec ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1)

anova(model2a,model2b) ## not significant -> list out

#### model 2c -- With varying intercepts and slopes for item and only varying intercepts for participant
model2c <- lmer(expec ~ cond_c + (1|id) + (1 + cond_c|target_no), data = data_pq1)

anova(model2b,model2c) ## significant! -> keep slope for participant

#### model 2d -- With varying intercepts and slopes for participant and only varying intercepts for item
model2d <- lmer(expec ~ cond_c + (1 + cond_c|id) + (1|target_no), data = data_pq1)

anova(model2b,model2d) ## not significant! -> slope for item out

#### model 2e -- With varying intercepts and slopes for participant (without item as random effect)
model2e <- lmer(expec ~ cond_c + (1 + cond_c|id), data = data_pq1) ## significant -> keep intercept for item

anova(model2d,model2e) ## significant -> keep intercept for item! 

###### Use model 2d !!!!! ####
summary(model2d)

### Sanity check of model2d ######
#<10 is reasonable collinearity, <30 is moderate collinearity, over 30 is troubling
kappa.mer(model2d)   ## good!   
#vif over 5 is troubling, over 2.5 is moderate (checks collinearity)
max(vif.mer(model2d)) ## good (checks collinearity)
# dispersion parameter should not be much larger than 1
overdisp.test(model2d) ## not good!
# residuals should be normally distributed, qq-plot plot should not show strong deviations from the line, 
# residuals against fitted values should show a rather random pattern
diagnostics.plot(model2d) ## good!
#lev.thresh should be close to 0, means that there are no influential cases
lev.thresh(model2d) ## good.

#### get the p-values for model2d ######
get_pvalues(model2d) # significant difference between c1 and c3



