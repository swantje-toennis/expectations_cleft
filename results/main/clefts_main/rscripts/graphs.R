# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)

# set theme for figures
theme_set(theme_bw())

# load data
data_t <- read.csv("../data/preprocessed_data/data_t.csv", sep = ";")
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
# per subject
subjmeans = data_t %>%
  group_by(cond_c,id) %>%
  summarize(Mean = mean(judgment)) 
# per item
itemmeans = data_t %>%
  group_by(cond_c,target_no) %>%
  summarize(Mean = mean(judgment)) 

levels(means$cond_c)
levels(subjmeans$cond_c)
levels(itemmeans$cond_c)
itemmeans$target_no <- as.factor(itemmeans$target_no)
levels(itemmeans$target_no)

# SALT violin plot of mean preference ratings depending on the expectedness
# of the target question
ggplot() +
  geom_violin(data = data_t, aes(x = cond_c, y = judgment), alpha = .5) +
  geom_point(data = means, aes(x = cond_c, y = Mean), stroke=.5,size=3,color="black") +
  geom_errorbar(data = means, aes(x = cond_c, ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  geom_dotplot(data = subjmeans, aes(x = cond_c, y = Mean), binaxis = "y", binwidth=8,
               stackdir = "center", dotsize = 0.3, shape=21,fill="gray70", alpha=.3, color="gray40") +
  scale_y_continuous(breaks = c(-100, -50, 0, 50, 100), labels = c("-100\ncanonical much better", "-50", "0\nboth equally good", "50", "100\ncleft much better")) +
  scale_x_discrete(limits = rev(levels(data_t$cond_c)))+
  labs(x='Expectedness of target question', y='Mean preference rating') +
  coord_flip()
ggsave("../graphs/results_main_experiment.png",height=3,width=6.7)

# Plot variability between items (i.e., context + target question)
ggplot(itemmeans, aes(x= target_no, y=Mean, color=cond_c, shape=cond_c, fill=cond_c)) +
  geom_point(stroke=.5,size=3,color="black") +
  #geom_point(data=subjmeans,aes(fill=cond_c,color=cond_c),shape=21,alpha=.1) +
  scale_shape_manual(values=c(21, 24),labels=c("higher","lower"),name="Expectedness of Q1",guide = guide_legend(reverse = TRUE) ) +
  scale_fill_manual(values=c("#2596BE","#FF8000"),labels=c("higher","lower"),name="Expectedness of Q1",guide = guide_legend(reverse = TRUE) ) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  #theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Item', y='Mean preference rating') +
  coord_flip() +
  scale_y_continuous(limits = c(-100,100),breaks = c(-100,-50,0,50,100), 
                     labels= c("-100\ncanonical much better",-50,0,50,"100\ncleft much better")) 
  #scale_color_manual(name="Fact", breaks=c("xx","yy"), labels=c("xx","yy"),  values=c("#2596BE","#FF8000")) 
ggsave("../graphs/item_variability_main.png",height=3,width=6)


##### Plot -- counts of preferences per context condition ######

#Setup in order to plot frequencies
dt <- data_t %>% group_by(cond_c,tendency) %>% summarize(n=n()) %>% mutate(freq=n/sum(n)) 

# Barplot in colors
ggplot(dt, aes(x=cond_c, fill = tendency, group = tendency)) +
  geom_bar(aes(y=freq), stat="identity", position = "dodge") +
theme_minimal() + 
  labs(x='Context condition', y='Count of preference', title='Counts of preferences per context condition') +
  theme(plot.title = element_text(hjust=0.5, size=12, face='bold')) +
  scale_fill_manual('Preference', values=c('darkorange', 'steelblue', 'deeppink4'))
ggsave("../graphs/frequency_plot_main(color).png",height=3,width=6)

# Barplot in grey
ggplot(dt, aes(x=cond_c, fill = tendency, group = tendency)) +
  geom_bar(aes(y=freq), stat="identity", position = "dodge") +
  theme_minimal() + 
  labs(x='Context condition', y='Count of preference', title='Counts of preferences per context condition') +
  theme(plot.title = element_text(hjust=0.5, size=12, face='bold')) +
  scale_fill_manual('Preference', values=c('grey77','grey44', 'grey14'))
ggsave("../graphs/frequency_plot_main(grey).png",height=3,width=6)


