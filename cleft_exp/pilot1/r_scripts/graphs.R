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
setwd("C:/Arbeit/Expectedness/experiments/pretest/pilot3/results/preprocessed_data")

source("C:/Arbeit/R statistics/mer-utils.r")
source("C:/Arbeit/R statistics/regression-utils.r")
source("C:/Arbeit/R statistics/diagnostic_fcns.r")
source("C:/Arbeit/R statistics/boot_glmm.r")
source("C:/Arbeit/R statistics/helpers.r")

#### read coded data ###########
data_t <- read.csv("C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/preprocessed_data/data_t.csv", sep = ";")

#### Define types of columns in data_t
data_t$tendency <- as.factor(data_t$tendency)
data_t$judgment<-as.numeric(data_t$judgment)
data_t$cond_c<-as.factor(data_t$cond_c)
data_t$list <- as.factor(data_t$list)

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
  #geom_point(data=subjmeans,aes(fill=itemType,color=itemType),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21))+#,labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("red1"))+ #labels=c("lower probability","higher probability"),name="Fact") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  #scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"),
  # values=c("#56B4E9","#E69F00")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  # coord_flip() +
  labs(x='Number of context sentences', y='Mean rating', title='Mean rating by context condition')
#ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)+ facet_wrap(~target_no, nrow=2)

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
  #geom_point(data=subjmeans,aes(fill=itemType,color=itemType),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21))+#,labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("red1"))+ #labels=c("lower probability","higher probability"),name="Fact") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  #scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"),
  # values=c("#56B4E9","#E69F00")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  # coord_flip() +
  labs(x='Number of context sentences', y='Mean rating', title='Mean rating by context condition and item') +
 facet_wrap(~target_no, nrow=2)
#ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)+ facet_wrap(~target_no, nrow=2)


dtb <- ddply(data_t, .(id, cond_c), summarise,
             Bewertung.mean=mean(judgment))
### plot means aggregiert
plotmeans(dtb$Bewertung.mean ~ dtb$cond_c,xlab="Context condition",
          ylab="Mean Judgment", main="Mean Plot\nwith 95% CI")

###plot means nicht aggregiert
plotmeans(data_t$judgment ~ data_t$cond_c,xlab="Context condition",
          ylab="Mean Judgment", main="Mean Plot\nwith 95% CI")

## Plot per item
data_t %>% ggplot(aes(x = cond_c, y = judgment, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot") + xlab("Number of context sentences") + ylab("Strength of preference")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')+ facet_wrap(~target_no, nrow=2)

##### Plot comparing all questions per item ######
ggplot(means, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  #geom_point(data=subjmeans,aes(fill=itemType,color=itemType),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21))+#,labels=c("lower probability","higher probability"),name="Fact") +
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

##### Take out all questions except pq1 ##########
data_pq1 <- subset(data, cond_q=="pq1")
str(data_pq1)

##### show the mean and standard deviation for the two conditions ##########
data_pq1 %>% group_by(cond_c) %>%
  summarize(M = mean(expec), SD = sd(expec))

##### plot means and sd for pq1 ############
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




########################################################
#          Plots for normalized data
########################################################


#### normalize data ###############
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



