library(readxl)
library(ggplot2)
library(ggpubr)
library(plyr)
library(ez)
library(lme4)
library(languageR)
library(car)
library(MASS)
library(fitdistrplus)
library (ordinal)
library(nlme)
library(logspline)
library(gplots)

#schlechte teilnehmer ausfiltern
#goodperson <- subset(dtb, Bewertung.mean != 7 & Bewertung.mean !=1)
#goodperson



########################################################


### Dataset #######
summary(joined_without_fillers)

#spalten umbenennen
names(joined_without_fillers) <- c("id","target_no","cond_c","cond_q","exp", "list","position")

#### Why this? - To make sure the entries are interpreted as numbers? #####
joined_without_fillers$exp<-as.numeric(joined_without_fillers$exp)

##aggregieren: Mittelwert in exp pro Teilnehmer pro combination aus Kontext und Frage
#Der Mittelwert sollte hier immer nur 2 Werte mitteln. Ist das sinnlos?
dtb <- ddply(joined_without_fillers, .(id, cond_c, cond_q), summarise,
             Bewertung.mean=mean(exp))
summary(dtb)
dtb

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
#7. only c2
dtb_c2 <- subset(dtb, cond_c=="c2")
#8. only c3
dtb_c3 <- subset(dtb, cond_c=="c3")

#Anovas
#pq1
anova1 <- aov(data = dtb_pq1, formula = Bewertung.mean ~ cond_c)
anova1
summary(anova1)
#pq2
anova2 <- aov(data = dtb_pq2, formula = Bewertung.mean ~ cond_c)
anova2
summary(anova2)
#pq3
anova3 <- aov(data = dtb_pq3, formula = Bewertung.mean ~ cond_c)
anova3
summary(anova3)
#then
anovathen <- aov(data = dtb_then, formula = Bewertung.mean ~ cond_c)
anovathen
summary(anovathen)
#irr
anovairr <- aov(data = dtb_irr, formula = Bewertung.mean ~ cond_c)
anovairr
summary(anovairr)
#c3
anovac3 <- aov(data = dtb_c3, formula = Bewertung.mean ~ cond_q)
anovairr
summary(anovairr)


#plot means
#pq1
plotmeans(dtb_pq1$Bewertung.mean~ dtb_pq1$cond_c, xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for PQ1 with 95% CI")

#pq2
plotmeans(dtb_pq2$Bewertung.mean~ interaction(dtb_pq2$cond_c, sep ="   "),xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for PQ2 with 95% CI")

#pq3
plotmeans(dtb_pq3$Bewertung.mean~ interaction(dtb_pq3$cond_c, sep ="   "),xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for PQ3 with 95% CI")

#then
plotmeans(dtb_then$Bewertung.mean~ interaction(dtb_then$cond_c, sep ="   "),xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for 'then' Q with 95% CI")

#irr
plotmeans(dtb_irr$Bewertung.mean~ interaction(dtb_irr$cond_c, sep ="   "),xlab="Context",
          ylab="Mean Judgment", main="Mean Plot for irrelevant Q with 95% CI")

#c1
plotmeans(dtb_c1$Bewertung.mean~ interaction(dtb_c1$cond_q, sep ="   "),xlab="Questions",
          ylab="Mean Judgment", main="Mean Plot for c1 with 95% CI")

#c2
plotmeans(dtb_c2$Bewertung.mean~ interaction(dtb_c2$cond_q, sep ="   "),xlab="Questions",
          ylab="Mean Judgment", main="Mean Plot for c2 with 95% CI")

#c3
plotmeans(dtb_c3$Bewertung.mean~ interaction(dtb_c3$cond_q, sep ="   "),xlab="Questions",
          ylab="Mean Judgment", main="Mean Plot for c3 with 95% CI")


#Verschiedene Modelle + ANOVAS über Modelle
nlm.1 <- lm(data = joined_without_fillers, formula = exp ~ cond_c * cond_q)
summary(nlm.1)

nlm.2 <- lm(data = dtb, formula = Bewertung.mean ~ cond_c * cond_q)
summary(nlm.2)


glm.1 <- glmer(data = joined_without_fillers, formula = exp ~ cond_c + cond_q + (1|target_no) + (1|id))

glm.2 <- glmer(data = joined_without_fillers, formula = exp ~ cond_c + cond_q + (1|target_no))

glm.3 <- glmer(data = joined_without_fillers, formula = exp ~ cond_c + cond_q + (1|id))

summary(glm.1)
Anova(glm.1)

summary(glm.2)
Anova(glm.2)

summary(glm.3)
Anova(glm.3)


glm.big <- glmer(data = joined_without_fillers, formula = exp ~ cond_c * cond_q + (1|target_no) + (1|id))

summary(glm.big)
Anova(glm.big) 



