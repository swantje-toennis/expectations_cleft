# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

library(tidyverse)
library(lme4)
library(languageR)


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

# For get_pvalues: load remote package from GitHub server
install.packages("remotes")
remotes::install_github("dmirman/gazer")
library(gazer) #get pvalues


########################################################

#source("C:/Arbeit/R statistics/mer-utils.r")
#source("C:/Arbeit/R statistics/regression-utils.r")
#source("C:/Arbeit/R statistics/diagnostic_fcns.r")
#source("C:/Arbeit/R statistics/boot_glmm.r")
#source("C:/Arbeit/R statistics/helpers.r")

#### load preprocessed data ###########
data <- read.csv("../data/preprocessed_data/data.csv", sep = ";")

#### Interpret expectedness judgment as numbers and context, question and list as a factors #####
data$expec<-as.numeric(data$expec)
data$cond_c<-as.factor(data$cond_c)
data$cond_q<-as.factor(data$cond_q)
data$list <- as.factor(data$list)
str(data)

##### Take out all questions except pq1 ##########
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


#### model 1 -- With varying intercepts and slopes for participant and context item
# and list and context condition as fixed effects interaction
model1 <- lmer(expec ~ cond_c * list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1)
summary(model1)
# list and interaction not significant

#### model 2 -- With varying intercepts and slopes for participant and item
# and list and context condition as fixed effects (without interaction)
model2 <- lmer(expec ~ cond_c + list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1)
summary(model2)

anova(model1,model2) ## not significant -> take interaction of list out

#### model 3 -- With varying intercepts and slopes for participant and context item,
# context condition as fixed effect without list as fixed effect.
model3 <- lmer(expec ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1)
summary(model3)

anova(model2,model3) ## not significant -> list out

#### model 4 -- With varying intercepts and slopes for item and only varying intercepts for participant
model4 <- lmer(expec ~ cond_c + (1|id) + (1 + cond_c|target_no), data = data_pq1)

anova(model3,model4) ## significant! -> keep slope for participant

#### model 5 -- With varying intercepts and slopes for participant and only varying intercepts for item
model5 <- lmer(expec ~ cond_c + (1 + cond_c|id) + (1|target_no), data = data_pq1)

anova(model3,model5) ## not significant! -> slope for item out

#### model 6 -- With varying intercepts and slopes for participant (without item as random effect)
model6 <- lmer(expec ~ cond_c + (1 + cond_c|id), data = data_pq1) ## significant -> keep intercept for item

anova(model5,model6) ## significant -> keep intercept for item! 

###### Use model 5 !!!!! ####
summary(model5)

### Sanity check of model5 ######
#<10 is reasonable collinearity, <30 is moderate collinearity, over 30 is troubling
kappa.mer(model5)   ## good!   
#vif over 5 is troubling, over 2.5 is moderate (checks collinearity)
max(vif.mer(model5)) ## good (checks collinearity)
# dispersion parameter should not be much larger than 1
overdisp.test(model5) ## not good!
# residuals should be normally distributed, qq-plot plot should not show strong deviations from the line, 
# residuals against fitted values should show a rather random pattern
diagnostics.plot(model5) ## good!
#lev.thresh should be close to 0, means that there are no influential cases
lev.thresh(model5) ## good.

#### get the p-values for model2d ######
get_pvalues(model5) # significant difference between two context conditions



