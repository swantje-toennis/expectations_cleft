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

### Judith's part ####
nrow(data)
#View(data_t)
length(unique(data$id))

data$expec<-as.numeric(data$expec)
data$cond_c<-as.factor(data$cond_c)
data$cond_q<-as.factor(data$cond_q)
data$list <- as.factor(data$list)
data$target_no <- as.factor(data$target_no)
str(data$target_no)

data = data %>%
  mutate(cond_c=recode(cond_c, c1 = "0", c3 = "2")) %>%
  mutate(target_no=recode(target_no, t1="1",t2="2",t3="3",
                          t4="4",t5="5",t6="6",t7="7",
                          t8="8",t9="9",t10="10",
                          t11="11",t12="12",t13="13",t14="14",t15="15",t16="16"))

str(data$target_no)

#### normalize data ###############
zdata1 <- data %>% group_by(id,target_no,cond_c)%>%
  summarize(id, target_no, cond_c, norm_exp = 100*expec/sum(expec), expec, cond_q, list)
normalized_data <- zdata1 %>%
  mutate_at("norm_exp", str_replace, "NaN", "0")
normalized_data$norm_exp <- as.numeric(normalized_data$norm_exp)
#str(normalized_data)


########################################################
#          Plots for normalized data
########################################################


levels(normalized_data$cond_c)
normalized_data$cond_c <- relevel(normalized_data$cond_c, ref = "2")
table(normalized_data$cond_q)

normalized_data <- normalized_data %>%
  filter(cond_q == "pq1") %>%
  droplevels()

####Calculate means for normalized data
means1 <- normalized_data %>% 
  group_by(target_no,cond_c, cond_q) %>% 
  summarize(Mean=mean(norm_exp),CILow=ci.low(norm_exp),CIHigh=ci.high(norm_exp))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means1

high = means1 %>%
  filter(cond_c == "2") %>%
  mutate(target_no = fct_reorder(target_no,Mean))

means1 = means1 %>%
  mutate(target_no = fct_relevel(target_no,levels(high$target_no))) %>% 
  mutate(cond_c = fct_relevel(cond_c,"0"))
means1

subjmeans = normalized_data %>%
  group_by(id,target_no,cond_c) %>%
  summarize(Mean = mean(norm_exp)) %>%
  ungroup() %>% 
  mutate(cond_c = fct_relevel(as.factor(as.character(cond_c)),"0"))
subjmeans$target_no <- factor(subjmeans$target_no, levels = unique(levels(means1$target_no)))

#### Calculate means just for pq1 and pq3 grouped by item ######
# means1_pq1_pq3 <-  normalized_data[(normalized_data$cond_q=="pq1" | normalized_data$cond_q=="pq3"),] %>%
#   group_by(target_no,cond_c, cond_q) %>% 
#   summarize(Mean=mean(norm_exp),CILow=ci.low(norm_exp),CIHigh=ci.high(norm_exp))%>%
#   ungroup() %>%
#   mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

# ##### Plot comparing means of pq1 and pq3 per item ######
# ggplot(means1_pq1_pq3, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
#   geom_point(stroke=.5,size=3,color="black") +
#   scale_shape_manual(values=c(21, 21))+
#   scale_fill_manual(values=c("red1","skyblue"))+ 
#   geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
#   theme(legend.position = "top", legend.text=element_text(size=12)) +
#   labs(x='Number of context sentences', y='Expectedness mean', 
#        title='Expectedness mean by context condition and item') +
#   facet_wrap(~target_no, nrow=2)

#### Plot comparing means for all questions per item ######
levels(means1$cond_c)
levels(subjmeans$cond_c)

ggplot(means1, aes(x=target_no, y=Mean, color=cond_c, shape=cond_c, fill=cond_c)) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_point(data=subjmeans,aes(fill=cond_c,color=cond_c),shape=21,alpha=.1) +
  scale_shape_manual(values=c(21, 24),labels=c("0","2"),name="Distance to PQ-raising sentence",guide = guide_legend(reverse = TRUE) ) +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("0","2"),name="Distance to PQ-raising sentence",guide = guide_legend(reverse = TRUE) ) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Item', y='Mean normalized expectedness of PQ') +
  #coord_flip() +
  scale_y_continuous(limits = c(0,75),breaks = c(0,25,50,75), 
                     labels= c("0","25","50","75")) +
  scale_color_manual(name="Fact", breaks=c("xx","yy"), labels=c("xx","yy"),  values=c("#56B4E9","#E69F00")) 
ggsave("exp1.pdf",height=3,width=5)

### End Judith's part ####

#### Interpret expectedness judgment as numbers and context, question and list as a factors #####
data$expec<-as.numeric(data$expec)
data$cond_c<-as.factor(data$cond_c)
data$cond_q<-as.factor(data$cond_q)
data$list <- as.factor(data$list)
str(data)

#### normalize data ###############
zdata1 <- data %>% group_by(id,target_no,cond_c)%>%
  summarize(id, target_no, cond_c, norm_exp = 100*expec/sum(expec), expec, cond_q, list)
normalized_data <- zdata1 %>%
  mutate_at("norm_exp", str_replace, "NaN", "0")
normalized_data$norm_exp <- as.numeric(normalized_data$norm_exp)
#str(normalized_data)


########################################################
#          Plots for normalized data
########################################################

####Calculate means for normalized data
means1 <- normalized_data %>% 
  group_by(target_no,cond_c, cond_q) %>% 
  summarize(Mean=mean(norm_exp),CILow=ci.low(norm_exp),CIHigh=ci.high(norm_exp))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means1

#### Calculate means just for pq1 and pq3 grouped by item ######
means1_pq1_pq3 <-  normalized_data[(normalized_data$cond_q=="pq1" | normalized_data$cond_q=="pq3"),] %>%
  group_by(target_no,cond_c, cond_q) %>% 
  summarize(Mean=mean(norm_exp),CILow=ci.low(norm_exp),CIHigh=ci.high(norm_exp))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

##### Plot comparing means of pq1 and pq3 per item ######
ggplot(means1_pq1_pq3, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("red1","skyblue"))+ 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Number of context sentences', y='Expectedness mean', 
       title='Expectedness mean by context condition and item') +
  facet_wrap(~target_no, nrow=2)

#### Plot comparing means for all questions per item ######
ggplot(means1, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21, 21, 21, 21, 21))+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Number of context sentences', y='Expectedness mean', title='Expectedness mean of all questions by context condition and item\nfor normalized data with 95% CIs')+
  facet_wrap(~target_no, nrow=2)

##### Take out all questions except pq1 ##########
normalized_data_pq1 <- subset(normalized_data, cond_q=="pq1")
normalized_data_pq1$norm_exp<-as.numeric(normalized_data_pq1$norm_exp)

##### show the mean and standard deviation for the two conditions for pq1 ##########
normalized_data_pq1 %>% group_by(cond_c) %>%
  summarize(M = mean(norm_exp), SD = sd(norm_exp))


##### plot means and and 95% CIs for pq1 ############
###
##### Calculate means for PQ1 ######
mean_norm_pq1 <-  normalized_data[(normalized_data$cond_q=="pq1"),] %>%
  group_by(cond_c) %>% 
  summarize(Mean=mean(norm_exp),CILow=ci.low(norm_exp),CIHigh=ci.high(norm_exp))%>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

### Plot means for pq1 with 95% CIs####
ggplot(mean_norm_pq1, aes(x=cond_c, y=Mean)) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c("red1"))+ 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Number of context sentences', y='Expectedness mean', title='Expectedness mean of cleft question by context condition\nwith 95% CIs')

