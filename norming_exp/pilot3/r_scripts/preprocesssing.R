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
library(tidyverse)
library(tidyselect)

########################################################
setwd("C:/Arbeit/Expectedness/experiments/pretest/pilot3/results/raw_data")

### Dataset #######   ---- ADJUST PATH!!!!----------
#liste 1
file1 <- "C:/Arbeit/Expectedness/experiments/pretest/pilot3/results/raw_data/liste 1_results.csv"
liste1 <- read.csv(file1, sep = ";")
list1 <- c(1)
liste1$list <- list1
#spalten umbenennen
names(liste1) <- c("x1","id","x2","x3","x4","x5","x6","cond1","cond2","x7","cond_q","x8","expec","x9","q2","x10","exp2","x11","q3","x12","exp3","x13","q4","x14","exp4","x15","q5","x16","exp5","x17","x18", "list" )
#liste 2
file2 <- "C:/Arbeit/Expectedness/experiments/pretest/pilot3/results/raw_data/liste 2_results.csv"
liste2 <- read.csv(file2, sep = ";")
list2 <- c(2)
liste2$list <- list2
#spalten umbenennen
names(liste2) <- c("x1","id","x2","x3","x4","x5","x6","cond1","cond2","x7","cond_q","x8","expec","x9","q2","x10","exp2","x11","q3","x12","exp3","x13","q4","x14","exp4","x15","q5","x16","exp5","x17","x18", "list" )

results_all <- rbind(liste1,liste2)
summary(results_all)

#unnötige Spalten raus
results <- subset(results_all, select = c("id","cond1","cond2","cond_q","expec","q2","exp2","q3","exp3","q4","exp4","q5","exp5","list"))

#Personendaten abspeichern (Wie speichert man das?)
personendaten<-results[(results$cond1=="feedback" | results$cond1=="<id:prolificID>"),]
write.table(personendaten, file="C:/Arbeit/Expectedness/experiments/pretest/pilot3/results/preprocessed_data/personendaten.csv", row.names = FALSE, col.names = TRUE, sep = ";")


#Unnötige Zeilen von Onexp rauswerfen
res2a<-results[!(results$cond1=="submit" | results$cond1=="expectedness1" | results$cond1=="feedback" | results$cond1=="<id:prolificID>"),]

#column cond2 rauswerfen, weil unnötig
res2 <- subset(res2a, select = -c(cond2))

#unnötige Zeichen raus
res3 <- res2 %>%
  mutate_at("cond1", str_replace, "<id:", "")
res3 <- res3 %>%
  mutate_at("cond_q", str_replace, "<id:", "")
res3 <- res3 %>%
  mutate_at("q2", str_replace, "<id:", "")
res3 <- res3 %>%
  mutate_at("q3", str_replace, "<id:", "")
res3 <- res3 %>%
  mutate_at("q4", str_replace, "<id:", "")
res3 <- res3 %>%
  mutate_at("q5", str_replace, "<id:", "")
res3 <- res3 %>%
  mutate_at("cond1", str_replace, ">", "")
res3 <- res3 %>%
  mutate_at("cond_q", str_replace, ">", "")
res3 <- res3 %>%
  mutate_at("q2", str_replace, ">", "")
res3 <- res3 %>%
  mutate_at("q3", str_replace, ">", "")
res3 <- res3 %>%
  mutate_at("q4", str_replace, ">", "")
res3 <- res3 %>%
  mutate_at("q5", str_replace, ">", "")

#cond1 aufteilen in context condition cond_c und target_no
res4 <- res3 %>% separate(cond1, c("cond_c", "target_no"), "-")

#create subsets of dataframe per question at position 1-5
res4_1 <- subset(res4, select = c(id, target_no, cond_c, cond_q, expec, list))

res4_2 <- subset(res4, select = c(id, target_no, cond_c, q2, exp2, list))
names(res4_2) <- c("id", "target_no", "cond_c", "cond_q", "expec","list")

res4_3 <- subset(res4, select = c(id, target_no, cond_c, q3, exp3, list))
names(res4_3) <- c("id", "target_no", "cond_c", "cond_q", "expec","list")

res4_4 <- subset(res4, select = c(id, target_no, cond_c, q4, exp4, list))
names(res4_4) <- c("id", "target_no", "cond_c", "cond_q", "expec","list")

res4_5 <- subset(res4, select = c(id, target_no, cond_c, q5, exp5, list))
names(res4_5) <- c("id", "target_no", "cond_c", "cond_q", "expec","list")

#join all subsets to receive a big dataframe in the correct format 
res5 <- rbind(res4_1,res4_2,res4_3,res4_4,res4_5)

#Filler rauswerfen
data1 <-res5[!(res5$cond_c=="f10" | res5$cond_c=="f11" | res5$cond_c=="f12" | res5$cond_c=="f13" | res5$cond_c=="f14" | res5$cond_c=="f1" | res5$cond_c=="f2" | res5$cond_c=="f3" | res5$cond_c=="f4" | res5$cond_c=="f5" | res5$cond_c=="f6" | res5$cond_c=="f7" | res5$cond_c=="f8" | res5$cond_c=="f9"),]
fillers <-res5[(res5$cond_c=="f10" | res5$cond_c=="f11" | res5$cond_c=="f12" | res5$cond_c=="f13" | res5$cond_c=="f14" | res5$cond_c=="f1" | res5$cond_c=="f2" | res5$cond_c=="f3" | res5$cond_c=="f4" | res5$cond_c=="f5" | res5$cond_c=="f6" | res5$cond_c=="f7" | res5$cond_c=="f8" | res5$cond_c=="f9"),]
#Fillers abspeichern
write.table(fillers, file="C:/Arbeit/Expectedness/experiments/pretest/pilot3/results/preprocessed_data/fillers.csv", row.names = FALSE, col.names = TRUE, sep = ";")

#### Why this? - To make sure the entries are interpreted as numbers? #####
data1$expec<-as.numeric(data1$expec)

###### Calculate means and standard deviation for irrelevant and relevant Q
mean.irr <- mean(data1$expec[data1$cond_q=="irr"])
mean.irr
sd.irr <- sd(data1$expec[data1$cond_q=="irr"])
sd.irr
mean.then <- mean(data1$expec[data1$cond_q=="then"])
sd.then <- sd(data1$expec[data1$cond_q=="then"])



#exclude participants whose mean rating is 2 sd below general mean for then_Q
#and whose mean rating is 2 sd above the general mean for irr_Q
#participants who always choose the middle are not excluded. Should they?
#1. helpful subsets
data2 <- ddply(data1, .(id, cond_q), summarise, judge.mean=mean(expec))
data3 <- subset(data2, (cond_q=="irr"))
data4 <- subset(data2, (cond_q=="then"))
#2. creating the subset of excluded participants
excludedperson <- rbind(subset(data3, (judge.mean > (mean.irr+2*sd.irr))),subset(data4, (judge.mean < (mean.then-2*sd.then)))) 
excluded_data <- subset(data1, id %in% excludedperson$id)
#Save the excluded data
write.table(excluded_data, file="C:/Arbeit/Expectedness/experiments/pretest/pilot3/results/preprocessed_data/excluded_data.csv", row.names = FALSE, col.names = TRUE, sep = ";")
#3. Exclude participants which are in the set of excluded persons for the preprocessed data set
`%notin%` <- Negate(`%in%`)
data <- subset(data1, id %notin% excludedperson$id)

#Save preprocessed data (this will be input for analysis)
write.table(data, file="C:/Arbeit/Expectedness/experiments/pretest/pilot3/results/preprocessed_data/data.csv", row.names = FALSE, col.names = TRUE, sep = ";")




