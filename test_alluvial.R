# Testing out alluvial plots with ggalluvial

library(ggplot2)
library(ggalluvial)
library(alluvial)
library(tidyverse)
library(rio)


#check out UCB Admissions table
UCBAdmissions

#make into data frame with counts for each category
ucb2 <- as.data.frame(UCBAdmissions)

#now plot with ggalluvial
ggplot(ucb2, aes(weight = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill=Admit), width= 1/12) +
  geom_stratum(width = 1/12, fill= "black", color="grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("Class", "Dept")) +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")

#now try with alluvial
alluvial(
  select(ucb2, Gender, Dept, Admit),
  freq = ucb2$Freq,
  col = ifelse(ucb2$Admit== "Admitted", "green", "red"),
  border = ifelse(ucb2$Admit== "Admitted", "green", "red"),
  layer = ucb2$Admit != "Rejected",
  alpha = 0.4,
  blocks=FALSE
)


##-----------------Titanic

#check out Titanic table
Titanic

#make into data frame with counts for each category
t2 <- as.data.frame(Titanic)

#now plot
#note that knot.pos in geom_alluvium affects placement of curves.
# try 0.4, 0.33, 0.25
ggplot(t2, aes(weight=Freq, axis1=Survived, axis2 = Sex, axis3 = Class)) +
  geom_alluvium(aes(fill=Class), width = 0, knot.pos =0.4,  reverse=FALSE) +
  guides(fill=FALSE) +
  geom_stratum(width = 1/10,reverse=FALSE) +
  geom_text(stat = "stratum", label.strata =TRUE, reverse=FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class"))+
  coord_flip() +
  ggtitle("Titanic Survival by Class and Sex") 


##----------------- now try sgi data
sgi <- import("sgi_2016.xls")

glimpse(sgi)

#now generate variables to indicate states
sgi <- sgi %>% 
  mutate(Symptoms = ifelse(symptoms == 1, "Yes", "No")) %>% 
  mutate(Provider = ifelse(ordering_provider == 1, "GI", "NonGI")) %>% 
  mutate(Result = case_when(
    Crohns == 1 ~ "CD",
    UC == 1 ~ "UC",
    Inconclusive == 1 ~ "U",
    IBD == 0 ~ "Negative"
  )) %>% 
  mutate(Scoped = ifelse(diagnosis > 0, "Yes", "No")) %>% 
  mutate(Diagnosis = case_when(
    diagnosis == 1 ~ "CD",
    diagnosis == 2 ~ "UC",
    diagnosis == 3 ~ "U",
    diagnosis == 4 ~ "Negative"
  ))

sgi2 <- sgi %>% select(Symptoms, Provider, Result, Scoped, Diagnosis)
sgi3 <- as.data.frame(table(sgi2))

sum(sgi3$Freq) #note 57 remain

#now plot sgi3
ggplot(sgi3, aes(weight=Freq, axis1=Symptoms, axis2 = Provider, 
               axis3 = Result, axis4 = Scoped, axis5 = Diagnosis)) +
  geom_alluvium(aes(fill=Diagnosis), width = 0, knot.pos =0.4,  reverse=TRUE) +
  guides(fill=FALSE) +
  geom_stratum(width = 1/8,reverse=TRUE) +
  geom_text(stat = "stratum", label.strata =TRUE, reverse=TRUE) +
  scale_x_continuous(breaks = 1:5, labels = c("Symptoms", "Provider", "Result",
                                              "Scoped", "Diagnosis"))+
  coord_flip() +
  ggtitle("SGI Tests and Diagnoses by Symptoms, Provider, Result and Scope") 


#replot
#now plot sgi3
ggplot(sgi3, aes(weight=Freq, axis1= Provider, 
                 axis2 = Result, axis3 = Diagnosis)) +
  geom_alluvium(aes(fill=Diagnosis), width = 0, knot.pos =0.33,  reverse=TRUE) +
  guides(fill=FALSE) +
  geom_stratum(width = 1/15,reverse=TRUE) +
  geom_text(stat = "stratum", label.strata =TRUE, reverse=TRUE) +
  scale_x_continuous(breaks = 1:3, labels = c("Provider", "Result",
                                              "Diagnosis"))+
  coord_flip() +
  ggtitle("SGI Results and Diagnoses by Provider and Result") +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=12)) +
  scale_fill_manual(values = c("purple", "green", "blue", "red")) +
  theme(panel.background = element_blank(), panel.border = element_blank())
