# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

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
source("C:/Arbeit/R statistics/mer-utils.r")
source("C:/Arbeit/R statistics/regression-utils.r")
source("C:/Arbeit/R statistics/diagnostic_fcns.r")
source("C:/Arbeit/R statistics/boot_glmm.r")
source("C:/Arbeit/R statistics/helpers.r")

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

#### model 1 -- With varying intercepts and slopes for participant ('id')
# and context item ('target_no'),
# context condition ('cond_c') and list as fixed effects interaction
model1 <- lmer(judgment ~ cond_c * list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_t)
#singular

#### model 2 -- With varying intercepts and slopes for participant ('id')
# and context item ('target_no'),
# context condition ('cond_c') and list as fixed effects
model2 <- lmer(judgment ~ cond_c + list + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_t)
#singular

# comparison of model 1 and 2
anova(model1,model2) # not significant, drop interaction

#### model 3 -- With varying intercepts and slopes for participant ('id')
# and context item ('target_no'),
# context condition ('cond_c') as fixed effects (whithout list)
model3 <- lmer(judgment ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_t)
#singular

# comparison of model 2 and 3
anova(model2,model3) # not significant, drop list as fixed effect

#### model 4 -- With varying intercepts and slopes for participant ('id')
# and varying intercepts for context item ('target_no'),
# context condition ('cond_c') as fixed effects (whithout list)
model4 <- lmer(judgment ~ cond_c + (1 + cond_c|id) + (1|target_no), data = data_t)
# not singular

# comparison of model 3 and 4
anova(model3,model4) # not significant, drop item slopes

#### model 5 -- With varying intercepts and slopes for participant ('id'),
# context condition ('cond_c') as fixed effects (whithout list)
model5 <- lmer(judgment ~ cond_c + (1 + cond_c|id), data = data_t)
# not singular

# comparison of model 4 and 5
anova(model4,model5) # not significant, drop item as random effect

#### model 6 -- With varying intercepts for participant ('id') and
# context condition ('cond_c') as fixed effects (whithout list)
model6 <- lmer(judgment ~ cond_c + (1|id), data = data_t)
# not singular

# comparison of model 5 and 6
anova(model5,model6) # significant, keep by participant varying slopes 

#### pick model 5!  ##########

summary(model5)
get_pvalues(model5)

#              Estimate Std..Error  t.value     p.normal p.normal.star
# (Intercept)  6.629032   5.504970 1.204190 2.285160e-01              
# cond_cc3    25.688172   5.915915 4.342215 1.410535e-05           ***

### Sanity check for model 5 ######
#<10 is reasonable collinearity, <30 is moderate collinearity, over 30 is troubling
kappa.mer(model5)   ## 2.826852  -> good!   
#vif over 5 is troubling, over 2.5 is moderate (checks collinearity)
max(vif.mer(model5)) ## 1 -> good! 
# dispersion parameter should not be much larger than 1
overdisp.test(model5) ## 2091.563 -> not good!
# residuals should be normally distributed, qq-plot plot should not show strong deviations from the line, 
# residuals against fitted values should show a rather random pattern
diagnostics.plot(model5) ## ok!
#lev.thresh should be close to 0, means that there are no influential cases
lev.thresh(model5) ## 0.01075269 -> good.
