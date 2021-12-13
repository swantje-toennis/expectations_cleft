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
setwd("C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/raw_data")

### Dataset #######   ---- ADJUST PATH!!!!----------
#liste 1
file1 <- "C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/raw_data/liste 1_results.csv"
liste1 <- read.csv(file1, sep = ";")
#spalten umbenennen
names(liste1) <- c("x1","id","x2","x3","x4","x5","x6","cond1","x7","x8","condA","x9","condB","x10","judgment","x12","x13","x14","x15","list" )
list1 <- c(1)
liste1$list <- list1

#liste 2
file2 <- "C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/raw_data/liste 2_results.csv"
liste2 <- read.csv(file2, sep = ";")
#spalten umbenennen
names(liste2) <- c("x1","id","x2","x3","x4","x5","x6","cond1","x7","x8","condA","x9","condB","x10","judgment","x12","x13","x14","x15","list" )
list2 <- c(2)
liste2$list <- list2

#liste 3
file3 <- "C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/raw_data/liste 3_results.csv"
liste3 <- read.csv(file3, sep = ";")
#spalten umbenennen
names(liste3) <- c("x1","id","x2","x3","x4","x5","x6","cond1","x7","x8","condA","x9","condB","x10","judgment","x12","x13","x14","x15","list" )
list3 <- c(3)
liste3$list <- list3

#liste 4
file4 <- "C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/raw_data/liste 4_results.csv"
liste4 <- read.csv(file4, sep = ";")
#spalten umbenennen
names(liste4) <- c("x1","id","x2","x3","x4","x5","x6","cond1","x7","x8","condA","x9","condB","x10","judgment","x12","x13","x14","x15","list" )
list4 <- c(4)
liste4$list <- list4

#liste 5
file5 <- "C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/raw_data/liste 5_results.csv"
liste5 <- read.csv(file5, sep = ";")
#spalten umbenennen
names(liste5) <- c("x1","id","x2","x3","x4","x5","x6","cond1","x7","x8","condA","x9","condB","x10","judgment","x12","x13","x14","x15","list" )
list5 <- c(5)
liste5$list <- list5

#liste 6
file6 <- "C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/raw_data/liste 6_results.csv"
liste6 <- read.csv(file6, sep = ";")
#spalten umbenennen
names(liste6) <- c("x1","id","x2","x3","x4","x5","x6","cond1","x7","x8","condA","x9","condB","x10","judgment","x12","x13","x14","x15","list" )
list6 <- c(6)
liste6$list <- list6

#liste 7
file7 <- "C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/raw_data/liste 7_results.csv"
liste7 <- read.csv(file7, sep = ";")
#spalten umbenennen
names(liste7) <- c("x1","id","x2","x3","x4","x5","x6","cond1","x7","x8","condA","x9","condB","x10","judgment","x12","x13","x14","x15","list" )
list7 <- c(7)
liste7$list <- list7

#liste 8
file8 <- "C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/raw_data/liste 8_results.csv"
liste8 <- read.csv(file8, sep = ";")
#spalten umbenennen
names(liste8) <- c("x1","id","x2","x3","x4","x5","x6","cond1","x7","x8","condA","x9","condB","x10","judgment","x12","x13","x14","x15","list" )
list8 <- c(8)
liste8$list <- list8


#results_all <- rbind(liste1,liste2)
results_all <- rbind(liste1,liste2,liste3,liste4,liste5,liste6,liste7,liste8)
summary(results_all)

#Personendaten abspeichern
personendaten<-results_all[(results_all$cond1=="feedback" | results_all$cond1=="<id:prolificID>"),]
write.table(personendaten, file="C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/preprocessed_data/personendaten.csv", row.names = FALSE, col.names = TRUE, sep = ";")

#unnötige Spalten raus
results <- subset(results_all, select = c("id","cond1","condA","condB","judgment","list"))

#Unnötige Zeilen von Onexp rauswerfen
res2<-results[!(results$cond1=="feedback" | results$cond1=="submit" | results$cond1=="judgement" | results$cond1=="<id:prolificID>"),]

#unnötige Zeichen raus
res3 <- res2 %>%
  mutate_at("cond1", str_replace, "<id:", "")
res3 <- res3 %>%
  mutate_at("condA", str_replace, "<id:", "")
res3 <- res3 %>%
  mutate_at("condB", str_replace, "<id:", "")
res3 <- res3 %>%
  mutate_at("cond1", str_replace, ">", "")
res3 <- res3 %>%
  mutate_at("condA", str_replace, ">", "")
res3 <- res3 %>%
  mutate_at("condB", str_replace, ">", "")

#cond1 aufteilen in context condition cond_c und target_no
res4 <- res3 %>% separate(cond1, c("cond_c", "target_no"), "-")


#Filler rauswerfen
data1 <-res4[!(res4$cond_c=="f" | res4$cond_c=="control" ),]
fillers <-res4[(res4$cond_c=="f" | res4$cond_c=="control"),]
fillers$judgment<-as.numeric(fillers$judgment)
#Fillers abspeichern
write.table(fillers, file="C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/preprocessed_data/fillers.csv", row.names = FALSE, col.names = TRUE, sep = ";")

#### To make sure the entries are interpreted as numbers in judgment #####
data1$judgment<-as.numeric(data1$judgment)

###### Calculate means and standard deviation for controls
mean.control1 <- mean(fillers$judgment[fillers$cond_c=="control"& fillers$target_no=="1"])
sd.control1 <- sd(fillers$judgment[fillers$cond_c=="control"& fillers$target_no=="1"])
mean.control2 <- mean(fillers$judgment[fillers$cond_c=="control"& fillers$target_no=="2"])
sd.control2 <- sd(fillers$judgment[fillers$cond_c=="control"& fillers$target_no=="2"])
mean.control3 <- mean(fillers$judgment[fillers$cond_c=="control"& fillers$target_no=="3"])
sd.control3 <- sd(fillers$judgment[fillers$cond_c=="control"& fillers$target_no=="3"])
mean.control4 <- mean(fillers$judgment[fillers$cond_c=="control"& fillers$target_no=="4"])
sd.control4 <- sd(fillers$judgment[fillers$cond_c=="control"& fillers$target_no=="4"])


#exclude participants whose ratings are 2 sd below  the general mean
#for control 1-4
#1. control subset
data2 <- subset(fillers, (cond_c=="control"))
#2. creating the subset of excluded participants: Is this too strict?
excludedperson1 <- subset(data2, (target_no == 1 & judgment < (mean.control1-2*sd.control1)))
excludedperson2 <- subset(data2, (target_no == 2 & judgment < (mean.control2-2*sd.control2)))
excludedperson3 <- subset(data2, (target_no == 3 & judgment < (mean.control3-2*sd.control3)))
excludedperson4 <- subset(data2, (target_no == 4 & judgment < (mean.control4-2*sd.control4)))
excludedperson <- rbind(excludedperson1,excludedperson2,excludedperson3,excludedperson4) 
excluded_data <- subset(data2, id %in% excludedperson$id)
#Save the excluded data
write.table(excluded_data, file="C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/preprocessed_data/excluded_control-data.csv", row.names = FALSE, col.names = TRUE, sep = ";")

`%notin%` <- Negate(`%in%`)
#3. Exclude participants which are in the set of excluded persons
data <- subset(data1, id %notin% excludedperson$id)


#Save preprocessed data (this will be input for analysis) No participants excluded yet
write.table(data, file="C:/Arbeit/Expectedness/experiments/cleft_exp/pilot1/results/preprocessed_data/data.csv", row.names = FALSE, col.names = TRUE, sep = ";")




