# Script for analysis of norming study reported in SALT paper
# "German clefts address unexpected questions"
# by Swantje Tönnis and Judith Tonhauser


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



#### load preprocessed data ###########
data <- read.csv("../data/preprocessed_data/data.csv", sep = ";")

#### Interpret expectedness judgment as numbers and context, question and list as a factors #####
data$expec<-as.numeric(data$expec)
data$cond_c<-as.factor(data$cond_c)
data$cond_q<-as.factor(data$cond_q)
data$list <- as.factor(data$list)
str(data)

##### Take out all questions except target question "pq1" ##########
data_pq1 <- subset(data, cond_q=="pq1")
str(data_pq1)

##### means and sd for pq1 ##########
data_pq1 %>% group_by(cond_c) %>%
  summarize(M = mean(expec), SD = sd(expec))
#  cond_c     M    SD
#  1 c1      73.5  25.0
#  2 c3      44.2  31.2



#######################
####   Models #########
######################


# -- model 1 -- 
# context condition ('cond_c') and list as fixed effects interaction,
# varying intercepts and slopes for participant ('id')
# varying intercepts and slopes for context item ('target_no')
model1 <- lmer(expec ~ cond_c * list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1,
               REML=F,control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))


# -- model 2 --
# context condition ('cond_c') and list as fixed effects (without interaction),
# varying intercepts and slopes for participant ('id')
# varying intercepts and slopes for context item ('target_no')
model2 <- lmer(expec ~ cond_c + list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1,
               REML=F,control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))


# comparison of model 1 and 2
anova(model1,model2) 
# -> not significant: drop interaction of list and context condition


# -- model 3 (reported in the paper) -- 
# context condition ('cond_c') as fixed effect (list dropped),
# varying intercepts and slopes for participant ('id')
# varying intercepts and slopes for context item ('target_no')
model3 <- lmer(expec ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1,
               REML=F,control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

# comparison of model 2 and 3
anova(model2,model3) ## not significant -> drop list as fixed effect

summary(model3)

### Sanity check of model3 ######
#<10 is reasonable collinearity, <30 is moderate collinearity, over 30 is troubling
kappa.mer(model3)   ## 2.827958 -> good!   
#vif over 5 is troubling, over 2.5 is moderate (checks collinearity)
max(vif.mer(model3)) ## 1 -> good (checks collinearity)
# dispersion parameter should not be much larger than 1
overdisp.test(model3) ## 416.1791 -> not good!
# residuals should be normally distributed, qq-plot plot should not show strong deviations from the line, 
# residuals against fitted values should show a rather random pattern
diagnostics.plot(model3) ## good!
#lev.thresh should be close to 0, means that there are no influential cases
lev.thresh(model3) ## 0.004807692 -> good.

# Values reported in the paper:
get_pvalues(model3)
#Estimate Std..Error  t.value p.normal p.normal.star
#(Intercept)  73.52724   2.307447  31.8652        0           ***
#cond_cc3    -29.30288   2.154243 -13.6024        0           ***


