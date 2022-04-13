# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)

# set theme for figures
theme_set(theme_bw())

# load data
data_t <- read.csv("../data/data_t.csv", sep = ";")
nrow(data_t) #372
summary(data_t)

#### Interpret judgment as numbers and context, tendency, condition A, condition B and list as a factors #####
data_t$judgment<-as.numeric(data_t$judgment)
data_t$cond_c<-as.factor(data_t$cond_c)
data_t$condA<-as.factor(data_t$condA)
data_t$condB<-as.factor(data_t$condB)
data_t$list <- as.factor(data_t$list)
data_t$tendency <- as.factor(data_t$tendency)
str(data_t)

data_t = data_t %>%
  mutate(cond_c=recode(cond_c, c1 = "higher", c3 = "lower"))


##### Plot means with 95% CIs

# calculate means
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

# SALT violin plot
ggplot() +
  geom_violin(data = data_t, aes(x = cond_c, y = judgment), alpha = .5) +
  geom_point(data = means, aes(x = cond_c, y = Mean), stroke=.5,size=3,color="black") +
  geom_errorbar(data = means, aes(x = cond_c, ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  geom_dotplot(data = subjmeans, aes(x = cond_c, y = Mean), binaxis = "y", binwidth=8,
               stackdir = "center", dotsize = 0.3, shape=21,fill="gray70", alpha=.3, color="gray40") +
  scale_y_continuous(breaks = c(-100, -50, 0, 50, 100), labels = c("-100\ncanonical better", "-50", "0\nboth equally good", "50", "100\ncleft better")) +
  scale_x_discrete(limits = rev(levels(data_t$cond_c)))+
  labs(x='Expectedness of question', y='Kernel probability density of preference ratings') +
  coord_flip()
ggsave("../graphs/means-by-condition-salt.pdf",height=3,width=6)

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


