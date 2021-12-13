# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)

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

data = data %>%
   mutate(cond_c=recode(cond_c, c1 = "0", c3 = "2")) %>%
   mutate(target_no=recode(target_no, t1="1",t2="2",t3="3",
                           t4="4",t5="5",t6="6",t7="7",
                           t8="8",t9="9",t10="10",
                           t11="11",t12="12",t13="13",t14="14",t15="15",t16="16"))
str(data$target_no)
 
 
levels(data$cond_c)
data$cond_c <- relevel(data$cond_c, ref = "2")
table(data$cond_q)
 
 # plot in SALT abstract ---- 
 # plot mean for PQ1 in the two conditions by item
 

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
  filter(cond_c == "2") %>%
  mutate(target_no = fct_reorder(target_no,Mean))

means1 = means1 %>%
  mutate(target_no = fct_relevel(target_no,levels(high$target_no))) %>% 
  mutate(cond_c = fct_relevel(cond_c,"0"))
means1

# calculate participants' mean responses
subjmeans = data_pq1 %>%
  group_by(id,target_no,cond_c) %>%
  summarize(Mean = mean(expec)) %>%
  ungroup() %>% 
  mutate(cond_c = fct_relevel(as.factor(as.character(cond_c)),"0"))
subjmeans$target_no <- factor(subjmeans$target_no, levels = unique(levels(means1$target_no)))

levels(means1$cond_c)
levels(subjmeans$cond_c)

# plot
ggplot(means1, aes(x=target_no, y=Mean, color=cond_c, shape=cond_c, fill=cond_c)) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_point(data_pq1=subjmeans,aes(fill=cond_c,color=cond_c),shape=21,alpha=.1) +
  scale_shape_manual(values=c(21, 24),labels=c("0","2"),name="Distance to PQ-raising sentence",guide = guide_legend(reverse = TRUE) ) +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("0","2"),name="Distance to PQ-raising sentence",guide = guide_legend(reverse = TRUE) ) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Item', y='Mean expectedness of PQ') +
  coord_flip() +
  scale_y_continuous(limits = c(0,100),breaks = c(0,25,50,75,100), 
                     labels= c("0","25","50","75","100")) +
  scale_color_manual(name="Fact", breaks=c("xx","yy"), labels=c("xx","yy"),  values=c("#56B4E9","#E69F00")) 
ggsave("exp1.1.pdf",height=3,width=5) 

# plot in ELM2 abstract ---- 
# plot mean for all five questions in the two conditions

data = data %>%
  mutate(cond_c=recode(cond_c, `0` = "1", `2` = "3")) %>%
  mutate(cond_q=recode(cond_q, irr = "Q-", then = "Q+", pq1 = "Q1", pq2 = "Q2", pq3 = "Q3"))

table(data$cond_q)
levels(data$cond_c)
data$cond_c <- relevel(data$cond_c, ref = "1")
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

# set alpha
alpha <- ifelse(means1$cond_q == "Q1", 1, 1)

# set size
size <- ifelse(means1$cond_q == "Q1", 6, 5)

levels(means1$cond_q)
# plot
ggplot(means1, aes(x=cond_c, y=Mean,color=cond_q,fill=cond_q)) +
  #geom_point(stroke=.5,size=3,color="black") +
  geom_text(aes(label=cond_q,alpha = alpha),size=size) +
  theme(legend.position="none") +
  scale_shape_manual(values=c(21)) +
  #scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("0","2"),name="Distance to PQ-raising sentence",guide = guide_legend(reverse = TRUE) ) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  #theme(legend.position = "top", legend.text=element_text(size=12)) +
  labs(x='Number of context sentences', y='Mean expectedness') +
  scale_y_continuous(limits = c(0,100),breaks = c(0,25,50,75,100), 
                     labels= c("0","25","50","75","100")) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette)
ggsave("../graphs/exp1-elm.pdf",height=2.5,width=2.5) 

#### end Judith #####


########################################################
#          Plots for non-normalized data
########################################################


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

##### Plot comparing means for all questions per item ######
ggplot(means, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21, 21, 21, 21, 21))+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  ylab("Expectedness mean") +
  xlab("Context condititon") +
  facet_wrap(~target_no, nrow=2)


##### Plot comparing means of pq1 and pq3 per item ######
ggplot(means_pq1_pq3, aes(x=cond_c, y=Mean, color=cond_q, shape=cond_q, fill=cond_q)) +
  geom_point(stroke=.5,size=3,color="black") +
  scale_shape_manual(values=c(21, 21))+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.15) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  ylab("Expectedness mean") +
  xlab("Context condititon") +
  facet_wrap(~target_no, nrow=2)


##### Plots for irrelevant control #############
data_irr <- subset(data, cond_q=="irr")
#Boxplot
data_irr %>% ggplot(aes(x = cond_c, y = expec, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot for irrelevant question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')


##### Plots for then-question ##########
#Boxplot
data_then <- subset(data, cond_q=="then")
data_then %>% ggplot(aes(x = cond_c, y = expec, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot for then-question") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')


##### Take out all questions except pq1 ##########
data_pq1 <- subset(data, cond_q=="pq1")
str(data_pq1)

##### show the mean and standard deviation for the two conditions ##########
data_pq1 %>% group_by(cond_c) %>%
  summarize(M = mean(expec), SD = sd(expec))

##### plot means and sd for pq1 ############
#Boxplot
data_pq1 %>% ggplot(aes(x = cond_c, y = expec, fill = cond_c)) +
  geom_boxplot() + theme_minimal() + labs(title = "Boxplot") + xlab("Number of context sentences") + ylab("Expectedness")  + guides(fill = guide_legend(title = "Context")) +
  scale_fill_brewer(palette = 'PuOr')
