# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(ggrepel)

# set theme for figures
theme_set(theme_bw())

# load data
data <- read.csv("../data/preprocessed_data/data.csv", sep = ";")
nrow(data) #6240

#View(data_t)
length(unique(data$id)) #78 participants

# adjust columns
data$expec<-as.numeric(data$expec)
data$cond_c<-as.factor(data$cond_c)
data$cond_q<-as.factor(data$cond_q)
data$list <- as.factor(data$list)
data$target_no <- as.factor(data$target_no)
str(data$target_no)
str(data$cond_c)
table(data$cond_c)

# spelling out the target question for each context item
data = data %>%
  mutate(cond_c=recode(cond_c, c1 = "1 sentence", c3 = "3 sentences")) %>%
  mutate(target_no=recode(target_no, t1="Who ate the last roll?",
                          t2="Who locked the door?",
                          t3="Who took the lawn chair?",
                          t4="Who was the last to shower?",
                          t5="Who spilled ketchup?",
                          t6="Who parked on Heike's spot?",
                          t7="Who took the dice?",
                          t8="Who parked in front of Benni's bicycle?",
                          t9="Who turned up the radio?",
                          t10="Who soiled Martin's shoes?",
                          t11="Who used up the coffee powder?",
                          t12="Who turned off the water?",
                          t13="Who cut out the newspaper article?",
                          t14="Who used up the gas?",
                          t15="Who watered the flowers?",
                          t16="Who last used the playstation?"))
str(data$target_no)


levels(data$cond_c)
data$cond_c <- relevel(data$cond_c, ref = "3 sentences")
table(data$cond_q)

# plot mean for target question in the two conditions by context item

# restrict data to only pq1
data_pq1 <- data %>%
  filter(cond_q == "pq1") %>%
  droplevels()

str(data_pq1$exp)

# calculate mean for pq1 by condition
means1 <- data_pq1 %>% 
  group_by(target_no,cond_c, cond_q) %>% 
  summarize(Mean=mean(expec),CILow=ci.low(expec),CIHigh=ci.high(expec)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means1

# sort items by mean for pq1 in condition 2
high = means1 %>%
  filter(cond_c == "3 sentences") %>%
  mutate(target_no = fct_reorder(target_no,Mean))

means1 = means1 %>%
  mutate(target_no = fct_relevel(target_no,levels(high$target_no))) %>% 
  mutate(cond_c = fct_relevel(cond_c,"1 sentence"))
means1

# calculate participants' mean responses
subjmeans = data_pq1 %>%
  group_by(id,target_no,cond_c) %>%
  summarize(Mean = mean(expec)) %>%
  ungroup() %>% 
  mutate(cond_c = fct_relevel(as.factor(as.character(cond_c)),"1 sentence"))
subjmeans$target_no <- factor(subjmeans$target_no, levels = unique(levels(means1$target_no)))

levels(means1$cond_c)
levels(subjmeans$cond_c)

# plot of mean expectedness by target question
ggplot(means1, aes(x=target_no, y=Mean, color=cond_c, shape=cond_c, fill=cond_c)) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_point(data=subjmeans,aes(fill=cond_c,color=cond_c),shape=21,alpha=.1) +
  scale_shape_manual(values=c(21, 24),labels=c("1 sentence","3 sentences"),name="Context condition",guide = guide_legend(reverse = TRUE) ) +
  scale_fill_manual(values=c("#2596BE","#FF8C19"),labels=c("1 sentence","3 sentences"),name="Context condition",guide = guide_legend(reverse = TRUE) ) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Target question', y='Mean expectedness rating') +
  coord_flip() +
  scale_y_continuous(limits = c(0,100),breaks = c(0,25,50,75,100), 
                     labels= c("0","25","50","75","100")) +
  scale_color_manual(name="Fact", breaks=c("xx","yy"), labels=c("xx","yy"),  values=c("#2596BE","#FF8C19")) 
ggsave("../graphs/results_norming_by_question.png",height=3,width=6.5) 


#### plot mean for all five questions in the two conditions ####

data = data %>%
  mutate(cond_c=recode(cond_c, `0` = "1 sentence", `2` = "3 sentences")) %>%
  mutate(cond_q=recode(cond_q, irr = "Q-", then = "Q+", pq1 = "Q1", pq2 = "Q2", pq3 = "Q3"))

table(data$cond_q)
levels(data$cond_c)
data$cond_c <- relevel(data$cond_c, ref = "1 sentence")
table(data$cond_q)


# calculate mean for pq1 by condition
means1 <- data %>% 
  group_by(cond_c, cond_q) %>% 
  summarize(Mean=mean(expec),CILow=ci.low(expec),CIHigh=ci.high(expec)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means1

# calculate item mean responses
# itemmeans = data %>%
#   group_by(target_no,cond_c,cond_q) %>%
#   summarize(Mean = mean(expec)) %>%
#   ungroup() %>% 
#   mutate(cond_c = fct_relevel(as.factor(as.character(cond_c)),"1"))

# make sure that levels are the same
#levels(means1$cond_c)
#levels(itemmeans$cond_c)

# color-blind-friendly palette
cbPalette <- c("#56B4E9","red","#56B4E9","#56B4E9","#56B4E9")
#"#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","darkgrey") # c("#999999",

# grey color palette
#cbPalette <- c("grey20","grey10","grey20","grey20","grey20")

# set alpha
alpha <- ifelse(means1$cond_q == "Q1", 1, 1)

# set size and font
size <- ifelse(means1$cond_q == "Q1", 6, 5)
font <- ifelse(means1$cond_q == "Q1", 2, 1)

levels(means1$cond_q)

# plot
ggplot(means1, aes(x=cond_c, y=Mean)) +
  geom_point(stroke=.5,size=1,color="black") +
  geom_text_repel(aes(label=cond_q), size = 5,segment.size=1,segment.color="black",segment.alpha=.2,nudge_x=.4,nudge_y=-.1,force=1) +
  #geom_text(aes(label=cond_q,alpha = alpha,fontface=font),size=size) +
  theme(legend.position="none") +
  scale_shape_manual(values=c(21)) +
  #scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("0","2"),name="Distance to PQ-raising sentence",guide = guide_legend(reverse = TRUE) ) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  #theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Context condition', y='Mean question expectedness') +
  scale_y_continuous(limits = c(0,100),breaks = c(0,25,50,75,100), 
                     labels= c("0","25","50","75","100")) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette)
ggsave("../graphs/main_results_norming.pdf",height=2.5,width=2.5) 

