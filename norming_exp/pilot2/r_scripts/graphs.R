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

########################################################
setwd("C:/Arbeit/Expectedness/experiments/pretest/pilot2/results/graphs")

data <- read.csv("C:/Arbeit/Expectedness/experiments/pretest/pilot2/results/preprocessed_data/data.csv", sep = ";")

#### Why this? - To make sure the entries are interpreted as numbers? #####
data$exp<-as.numeric(data$exp)

##aggregieren: Mittelwert in exp pro Teilnehmer pro combination aus Kontext und Frage
#Der Mittelwert sollte hier immer nur 2 Werte mitteln. Ist das sinnlos?
dtb <- ddply(data, .(id, cond_c, cond_q), summarise,
             judge.mean=mean(exp))
summary(dtb)

#create subsets of database
#1. only pq1
dtb_pq1 <- subset(dtb, cond_q=="pq1")
dtb_pq1
#2. only pq2
dtb_pq2 <- subset(dtb, cond_q=="pq2")
dtb_pq2
#3. only pq3
dtb_pq3 <- subset(dtb, cond_q=="pq3")
dtb_pq3
#4. only then
dtb_then <- subset(dtb, cond_q=="then")
dtb_then
#5. only irr
dtb_irr <- subset(dtb, cond_q=="irr")
dtb_irr
#6. only c1
dtb_c1 <- subset(dtb, cond_c=="c1")
#8. only c3
dtb_c3 <- subset(dtb, cond_c=="c3")

#plot means
#pq1
plotmeans(dtb_pq1$judge.mean~ dtb_pq1$cond_c, xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for PQ1 with 95% CI")

#pq2
plotmeans(dtb_pq2$judge.mean~ interaction(dtb_pq2$cond_c, sep ="   "),xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for PQ2 with 95% CI")

#pq3
plotmeans(dtb_pq3$judge.mean~ interaction(dtb_pq3$cond_c, sep ="   "),xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for PQ3 with 95% CI")

#then
plotmeans(dtb_then$judge.mean~ interaction(dtb_then$cond_c, sep ="   "),xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for 'then' Q with 95% CI")

#irr
plotmeans(dtb_irr$judge.mean~ interaction(dtb_irr$cond_c, sep ="   "),xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for irrelevant Q with 95% CI")

#c1
plotmeans(dtb_c1$judge.mean~ interaction(dtb_c1$cond_q, sep ="   "),xlab="Questions",
          ylab="Mean Judgment", main="Mean Plot for c1 with 95% CI")

#c3
plotmeans(dtb_c3$judge.mean~ interaction(dtb_c3$cond_q, sep ="   "),xlab="Questions",
          ylab="Mean Judgment", main="Mean Plot for c3 with 95% CI")
