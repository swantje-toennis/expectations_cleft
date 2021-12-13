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
setwd("C:/Arbeit/Expectedness/experiments/pretest/pilot2/results/preprocessed_data")

source("C:/Arbeit/R statistics/mer-utils.r")
source("C:/Arbeit/R statistics/regression-utils.r")
source("C:/Arbeit/R statistics/diagnostic_fcns.r")
source("C:/Arbeit/R statistics/boot_glmm.r")
source("C:/Arbeit/R statistics/helpers.r")


data <- read.csv("C:/Arbeit/Expectedness/experiments/pretest/pilot2/results/preprocessed_data/data.csv", sep = ";")

#### Interpret expectedness judgment as numbers and condition as a factor #####
data$expec<-as.numeric(data$expec)
data$cond_c<-as.factor(data$cond_c)
data$cond_q<-as.factor(data$cond_q)
str(data)

###### means for all questions ##############
means <-  data %>%
  group_by(target_no,cond_c, cond_q) %>% 
  summarize(Mean=mean(expec),CILow=ci.low(expec),CIHigh=ci.high(expec))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

#### Means just for pq1 and pq3 ######
means_pq1_pq3 <-  data[(data$cond_q=="pq1" | data$cond_q=="pq3"),] %>%
  group_by(target_no,cond_c, cond_q) %>% 
  summarize(Mean=mean(expec),CILow=ci.low(expec),CIHigh=ci.high(expec))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)



##### Plot comparing all questions per item ######
ggplot(means, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  #geom_point(data=subjmeans,aes(fill=itemType,color=itemType),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21, 21, 21, 21, 21))+#,labels=c("lower probability","higher probability"),name="Fact") +
  #scale_fill_manual(values=c("black","darkorange1","ivory1","red1","skyblue"))+ #labels=c("lower probability","higher probability"),name="Fact") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  #scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"),
                    # values=c("#56B4E9","#E69F00")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
 # coord_flip() +
  ylab("Expectedness mean") +
  xlab("Context condititon") +
  facet_wrap(~target_no, nrow=2)
#ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)


##### Plot comparing means of pq1 and pq3 per item ######
ggplot(means_pq1_pq3, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  #geom_point(data=subjmeans,aes(fill=itemType,color=itemType),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21, 21))+#,labels=c("lower probability","higher probability"),name="Fact") +
  #scale_fill_manual(values=c("black","darkorange1","ivory1","red1","skyblue"))+ #labels=c("lower probability","higher probability"),name="Fact") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  #scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"),
  # values=c("#56B4E9","#E69F00")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  # coord_flip() +
  ylab("Expectedness mean") +
  xlab("Context condititon") +
  facet_wrap(~target_no, nrow=2)
#ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)

  
##### Plots for irrelevant control #############
data_irr <- subset(data, cond_q=="irr")
data_irr %>% ggplot(aes(x = cond_c, y = expec, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot for irrelevant question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')
data_irr %>% ggplot(aes(x = cond_c, y = expec, fill = cond_c)) +
  geom_point() + theme_minimal() + labs(title = "Boxplot for irrelevant question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

data_irr %>% ggplot(aes(x = id, y=expec, group=cond_c))+
  geom_point() + theme_minimal() + labs(title = "Boxplot for irrelevant question") + xlab("participant") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

##### means and sd for irrelevant q ##########
data_irr %>% group_by(cond_c) %>%
  summarize(M = mean(expec), SD = sd(expec))

##### Plots for then-question ##########
data_then <- subset(data, cond_q=="then")
data_then %>% ggplot(aes(x = cond_c, y = expec, fill = cond_c)) +
  geom_point() + theme_minimal() + labs(title = "Scatterplot for then-question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

data_then %>% ggplot(aes(x = cond_c, y = expec, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot for then-question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

data_then %>% ggplot(aes(x = id, y=expec, group=cond_c))+
  geom_point() + theme_minimal() + labs(title = "Boxplot for relevant question") + xlab("participant") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr') +
  facet_wrap(~target_no)

##### means and sd for then q ##########
data_then %>% group_by(cond_c) %>%
  summarize(M = mean(expec), SD = sd(expec))


##### Take out all questions except pq1 ##########
data_pq1 <- subset(data, cond_q=="pq1")
str(data_pq1)

##### show the mean and standard deviation for the two conditions ##########
data_pq1 %>% group_by(cond_c) %>%
  summarize(M = mean(expec), SD = sd(expec))

##### plot means and sd ############
data_pq1 %>% ggplot(aes(x = cond_c, y = expec, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

data_pq1 %>% ggplot(aes(x = cond_c, y = expec, fill = cond_c)) +
  geom_point() + theme_minimal() + labs(title = "Scatterplot") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

##### plot means and sd for each item separately############
target_data<- ddply(data_pq1, .(target_no, cond_c), summarise,
                           target.mean=mean(expec))

data_pq1 %>% ggplot(aes(x = cond_c, y = expec, group=cond_c )) +
  geom_boxplot() + theme_minimal() + labs(title = "Scatterplot") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr') +
  facet_wrap(~target_no)

data_pq3 <- subset(data, cond_q=="pq3")
data_pq3 %>% ggplot(aes(x = cond_c, y = expec, group=cond_c )) +
  geom_boxplot() + theme_minimal() + labs(title = "Scatterplot") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr') +
  facet_wrap(~target_no)

#####Dummy variables because my variable is categorical
# The code below is unnecessary because R does this automatically
#dummy(data_pq1$cond_c, "c3")

#### model 1 -- Without random effects
model1 <- lm(expec ~ cond_c, data = data_pq1)
### Sanity check of model 1 #####
summary(model1)

#### get the p-values for model1 ######
get_pvalues(model1)

#### model 2 -- With varying intercepts and slopes
model2 <- lmer(expec ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1)
### Sanity check of model 2 ######
kappa.mer(model2)   ## good!
max(vif.mer(model2)) ## good (checks collinearity)
overdisp.test(model2) ## not good! Is it because the sd is very high compared to the mean?
diagnostics.plot(model2) ## 
lev.thresh(model2) ## good.


summary(model2)

#### get the p-values for model2 ######
get_pvalues(model2)

#### model 3 without the (only) predictor context
model3 <- lmer(expec ~ 1 + (1 + cond_c|id) + (1 + cond_c|target_no), data = data_pq1)
summary(model3)
#### Problem? ###
isSingular(model3, tol = 1e-4)

##### compare the two models with and without predictor######
anova(model2, model3)


#### model 4 -- With varying intercepts
model4 <- lmer(expec ~ cond_c + (1|id) + (1|target_no), data = data_pq1)
summary(model4)
get_pvalues(model4)

#### model 5 without the (only) predictor context
model5 <- lmer(expec ~ 1 + (1|id) + (1|target_no), data = data_pq1)
summary(model5)

##### compare the two models with and without predictor (both including varying intercepts)######
anova(model4, model5)





###############################################
#Version with normalized data
###############################################

#### normalize data ###############
zdata1 <- data %>% group_by(id,target_no,cond_c)%>%
  summarize(id, target_no, cond_c, norm_exp = 100*expec/sum(expec), expec, cond_q, list)
normalized_data <- zdata1 %>%
  mutate_at("norm_exp", str_replace, "NaN", "0")
normalized_data$norm_exp <- as.numeric(normalized_data$norm_exp)
normalized_data$list <- as.factor(normalized_data$list)
str(normalized_data)

means1 <- normalized_data %>% 
  group_by(target_no,cond_c, cond_q) %>% 
  summarize(Mean=mean(norm_exp),CILow=ci.low(norm_exp),CIHigh=ci.high(norm_exp))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means1

#### Means just for pq1 and pq3 grouped by target ######
means1_pq1_pq3 <-  normalized_data[(normalized_data$cond_q=="pq1" | normalized_data$cond_q=="pq3"),] %>%
  group_by(target_no,cond_c, cond_q) %>% 
  summarize(Mean=mean(norm_exp),CILow=ci.low(norm_exp),CIHigh=ci.high(norm_exp))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

##### Plot comparing means of pq1 and pq3 per participant ######
ggplot(means1_pq1_pq3, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  #geom_point(data=subjmeans,aes(fill=itemType,color=itemType),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21, 21))+#,labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("red1","skyblue"))+ #labels=c("lower probability","higher probability"),name="Fact") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  #scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"),
  # values=c("#56B4E9","#E69F00")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  # coord_flip() +
  ylab("Expectedness mean") +
  xlab("Context condititon") +
  facet_wrap(~target_no, nrow=2)
#ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)


#### Plot comparing means for all questions per item ######
ggplot(means1, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  #geom_point(data=subjmeans,aes(fill=itemType,color=itemType),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(21, 21,21, 21,21))+#,labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("sienna4","darkorange1","ivory1","red1","skyblue"))+ #labels=c("lower probability","higher probability"),name="Fact") +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  #scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"),
  # values=c("#56B4E9","#E69F00")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  # coord_flip() +
  ylab("Expectedness mean") +
  xlab("Context condititon") +
  facet_wrap(~target_no, nrow=2)
#ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)

#### Means just for pq1 and pq3 grouped by participant ######
means2_pq1_pq3 <-  normalized_data[(normalized_data$cond_q=="pq1" | normalized_data$cond_q=="pq3"),] %>%
  group_by(id,cond_c, cond_q) %>% 
  summarize(Mean=mean(norm_exp),CILow=ci.low(norm_exp),CIHigh=ci.high(norm_exp))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

##### Plot comparing means of pq1 and pq3 per participant ######
ggplot(means2_pq1_pq3, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  #geom_point(data=subjmeans,aes(fill=itemType,color=itemType),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21, 21))+#,labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("red1","skyblue"))+ #labels=c("lower probability","higher probability"),name="Fact") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  #scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"),
  # values=c("#56B4E9","#E69F00")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  # coord_flip() +
  ylab("Expectedness mean") +
  xlab("Context condititon") +
  facet_wrap(~id, nrow=2)
#ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)


##### Plots for irrelevant control #############
normalized_data_irr <- subset(normalized_data, cond_q=="irr")
normalized_data_irr %>% ggplot(aes(x = cond_c, y = norm_exp, fill = cond_c)) +
  geom_point() + theme_minimal() + labs(title = "Scatterplot for irrelevant question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')
normalized_data_irr %>% ggplot(aes(x = cond_c, y = norm_exp, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot for irrelevant question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

##### Plots for then-question ##########
normalized_data_then <- subset(normalized_data, cond_q=="then")
normalized_data_then %>% ggplot(aes(x = cond_c, y = norm_exp, fill = cond_c)) +
  geom_point() + theme_minimal() + labs(title = "Scatterplot for then-question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')
normalized_data_then %>% ggplot(aes(x = cond_c, y = norm_exp, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot for then-question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

normalized_data_then %>% group_by(cond_c) %>%
  summarize(M = mean(norm_exp), SD = sd(norm_exp))

##### Take out all questions except pq3 #########
normalized_data_pq3 <- subset(normalized_data, cond_q=="pq3")
str(normalized_data_pq3)

##### plot means and sd for pq3 ############
normalized_data_pq3 %>% ggplot(aes(x = cond_c, y = norm_exp, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot pq3") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

normalized_data_pq3 %>% ggplot(aes(x = cond_c, y = norm_exp, fill = cond_c)) +
  geom_point() + theme_minimal() + labs(title = "Scatterplot pq3") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

##### Take out all questions except pq1 ##########
normalized_data_pq1 <- subset(normalized_data, cond_q=="pq1")
normalized_data_pq1$norm_exp<-as.numeric(normalized_data_pq1$norm_exp)
str(normalized_data_pq1)

##### show the mean and standard deviation for the two conditions ##########
normalized_data_pq1 %>% group_by(cond_c) %>%
  summarize(M = mean(norm_exp), SD = sd(norm_exp))

##### plot means and sd ############
normalized_data_pq1 %>% ggplot(aes(x = cond_c, y = norm_exp, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot pq1 normalized") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

normalized_data_pq1 %>% ggplot(aes(x = cond_c, y = norm_exp, fill = cond_c)) +
  geom_point() + theme_minimal() + labs(title = "Scatterplot pq1 normalized") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

normalized_data_pq1 %>% ggplot(aes(x = cond_c, y = norm_exp, group=cond_c )) +
  geom_boxplot() + theme_minimal() + labs(title = "Scatterplot") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr') +
  facet_wrap(~target_no)

#####Dummy variables because my variable is categorical
# The code below is unnecessary because R does this automatically
#dummy(normalized_data_pq1$cond_c, "c3")

##### t-test #######
test<-t.test(norm_exp ~ cond_c, data=normalized_data_pq1, var.equal=FALSE, na.rm=TRUE)
print(test)

#### model 1 -- Without random effects
model1 <- lm(norm_exp ~ cond_c, data = normalized_data_pq1)
### Sanity check of model 1 ######
kappa.mer(model1)


summary(model1)
#head(fitted(model1))  #Shows the fitted values: It should be just two values repeating 


#### get the p-values for model1 ######
get_pvalues(model1)

#### model 2 -- With varying intercepts and slopes
model2 <- lmer(norm_exp ~ cond_c + (1 + cond_c|id) + (1 + cond_c|target_no), data = normalized_data_pq1)
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

#### model2a with context and list as main effects ######
model2a <- lmer(norm_exp ~ cond_c + list + (1 + cond_c|id) + (1 + cond_c|target_no), data = normalized_data_pq1)
summary(model2a)

#### model2b with context and list as main effects and their interaction ######
model2b <- lmer(norm_exp ~ cond_c * list + (1 + cond_c|id) + (1 + cond_c|target_no), data = normalized_data_pq1)
summary(model2b)

anova(model2a,model2b)
anova(model2a,model2)
#### get the p-values for model2a ######
get_pvalues(model2a)

levels(normalized_data_pq1$list)

summary(model2)
random_effects_model2 <- extract_random_effects(model2)

#### get the p-values for model2 ######
get_pvalues(model2)

#### model 3 without the (only) predictor context
model3 <- lmer(norm_exp ~ 1 + (1 + cond_c|id) + (1 + cond_c|target_no), data = normalized_data_pq1)
summary(model3)
### Sanity check of model 3 ######
#<10 is reasonable collinearity, <30 is moderate collinearity, over 30 is troubling
kappa.mer(model3)   ## good!   
#vif over 5 is troubling, over 2.5 is moderate (checks collinearity)
max(vif.mer(model3)) ## good (checks collinearity)
# dispersion parameter should not be much larger than 1
overdisp.test(model3) ## good!
# residuals should be normally distributed, qq-plot plot should not show strong deviations from the line, 
# residuals against fitted values should show a rather random pattern
diagnostics.plot(model3) ## good!
#lev.thresh should be close to 0, means that there are no influential cases
lev.thresh(model3) ## good.


##### compare the two models with and without predictor######
anova(model2, model3)


#### model 4 -- With varying intercepts
model4 <- lmer(norm_exp ~ cond_c + (1|id) + (1|target_no), data = normalized_data_pq1)
summary(model4)
extract_random_effects(model4)

#### model 5 -- With varying intercepts without predictor
model5 <- lmer(norm_exp ~ 1 + (1|id) + (1|target_no), data = normalized_data_pq1)
summary(model5)
extract_random_effects(model5)

##### compare the two models with and without predictor######
anova(model2, model5)





