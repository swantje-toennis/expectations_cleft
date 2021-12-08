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

####  ADJUST PATH!!!!   ###############
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

########################################################
#                         Plots----
########################################################

##### Plot means with 95% CIs ######
#1. calculate means
means <-  data_t %>%
  group_by(cond_c) %>% 
  summarize(Mean=mean(judgment),CILow=ci.low(judgment),CIHigh=ci.high(judgment))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

subjmeans = data_t %>%
  group_by(cond_c,id) %>%
  summarize(Mean = mean(judgment)) 

levels(means$cond_c)
levels(subjmeans$cond_c)

ggplot() +
  geom_violin(data = data_t, aes(x = cond_c, y = judgment), alpha = .5) +
  geom_point(data = means, aes(x = cond_c, y = Mean), stroke=.5,size=3,color="black") +
  geom_errorbar(data = means, aes(x = cond_c, ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  geom_dotplot(data = subjmeans, aes(x = cond_c, y = Mean), binaxis = "y", binwidth=8,
               stackdir = "center", dotsize = 0.3, shape=21,fill="gray70", alpha=.3, color="gray40") +
  scale_y_continuous(breaks = c(-100, -50, 0, 50, 100), labels = c("-100\ncanonical better", "-50", "0\nboth equally good", "50", "100\ncleft better")) +
  labs(x='Expectedness of PQ', y='Kernel probability density of preference ratings') #+
 # coord_flip()
ggsave("exp2.pdf",height=3,width=5)

#2. plot means
ggplot(means, aes(x=cond_c, y=Mean)) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c("red1"))+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Number of context sentences', y='Mean rating', title='Mean rating by context condition')


##### Plot -- counts of preferences per condition ######

#useful summary to plot frequencies
dt <- data_t %>% group_by(cond_c,tendency) %>% summarize(n=n()) %>% mutate(freq=n/sum(n)) 
# Barplot in colors
ggplot(dt, aes(x=cond_c, fill = tendency, group = tendency)) +
  geom_bar(aes(y=freq), stat="identity", position = "dodge") +
theme_minimal() + 
  labs(x='Number of context sentences', y='Count of preference', title='Counts of preferences per context condition') +
  theme(plot.title = element_text(hjust=0.5, size=12, face='bold')) +
  scale_fill_manual('Preference', values=c('darkorange', 'steelblue', 'deeppink4'))
# Barplot in grey
ggplot(dt, aes(x=cond_c, fill = tendency, group = tendency)) +
  geom_bar(aes(y=freq), stat="identity", position = "dodge") +
  theme_minimal() + 
  labs(x='Number of context sentences', y='Count of preference', title='Counts of preferences per context condition') +
  theme(plot.title = element_text(hjust=0.5, size=12, face='bold')) +
  scale_fill_manual('Preference', values=c('grey77','grey44', 'grey14'))

#calculate means per item
means_target <-  data_t %>%
  group_by(cond_c,target_no) %>% 
  summarize(Mean=mean(judgment),CILow=ci.low(judgment),CIHigh=ci.high(judgment))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
#plot means per item
ggplot(means_target, aes(x=cond_c, y=Mean)) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c("red1"))+ 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Number of context sentences', y='Mean rating', title='Mean rating by context condition and item') +
  facet_wrap(~target_no, nrow=2)


