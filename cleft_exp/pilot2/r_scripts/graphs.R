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
setwd("C:/Arbeit/Expectedness/experiments/cleft_exp/pilot2/results/preprocessed_data")

source("C:/Arbeit/R statistics/mer-utils.r")
source("C:/Arbeit/R statistics/regression-utils.r")
source("C:/Arbeit/R statistics/diagnostic_fcns.r")
source("C:/Arbeit/R statistics/boot_glmm.r")
source("C:/Arbeit/R statistics/helpers.r")

#### read preprocessed data ###########
data_t <- read.csv("C:/Arbeit/Expectedness/experiments/cleft_exp/pilot2/results/preprocessed_data/data_t.csv", sep = ";")

#### Interpret judgment as numbers and context, tendency, condition A, condition B and list as a factors #####
data_t$judgment<-as.numeric(data_t$judgment)
data_t$cond_c<-as.factor(data_t$cond_c)
data_t$condA<-as.factor(data_t$condA)
data_t$condB<-as.factor(data_t$condB)
data_t$list <- as.factor(data_t$list)
data_t$tendency <- as.factor(data_t$tendency)
str(data_t)

########################################################
#                         Plots
########################################################

## Plot of means 
data_t %>% ggplot(aes(x = cond_c, y = judgment, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot") + xlab("Number of context sentences") + ylab("Strength of preference")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')

##### Plot means with error bars ######
#calculate means
means <-  data_t %>%
  group_by(cond_c) %>% 
  summarize(Mean=mean(judgment),CILow=ci.low(judgment),CIHigh=ci.high(judgment))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

#plot
ggplot(means, aes(x=cond_c, y=Mean)) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c("red1"))+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Number of context sentences', y='Mean rating', title='Mean rating by context condition - Pilot 2')


##### Plot counts of preferences per condition ######
#useful summary to plot frequencies
dt <- data_t %>% group_by(cond_c,tendency) %>% summarize(n=n()) %>% mutate(freq=n/sum(n)) 
# Barplot
ggplot(dt, aes(x=cond_c, fill = tendency, group = tendency)) +
  geom_bar(aes(y=freq), stat="identity", position = "dodge") +
theme_minimal() + 
  labs(x='Number of context sentences', y='Count of preference', title='Counts of preferences per context condition') +
  theme(plot.title = element_text(hjust=0.5, size=12, face='bold')) +
  scale_fill_manual('Preference', values=c('darkorange', 'steelblue', 'deeppink4'))

# plot in grey
dt <- data_t %>% group_by(cond_c,tendency) %>% summarize(n=n()) %>% mutate(freq=n/sum(n)) 
# Barplot
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

dtb <- ddply(data_t, .(id, cond_c), summarise,
             Bewertung.mean=mean(judgment))
### plot means aggregiert
plotmeans(dtb$Bewertung.mean ~ dtb$cond_c,xlab="Context condition",
          ylab="Mean Judgment", main="Mean Plot\nwith 95% CI")

###plot means nicht aggregiert
plotmeans(data_t$judgment ~ data_t$cond_c,xlab="Context condition",
          ylab="Mean Judgment", main="Mean Plot\nwith 95% CI")


