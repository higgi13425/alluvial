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
    diagnosis == 4 ~ "Negative",
    diagnosis == 0 ~ "Not scoped"
  ))

sgi$IBD_lastdx[sgi$IBD_lastdx == 0 & sgi$Diagnosis == "Not scoped"] <- "Negative"

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
#export with width =1000
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
  theme(panel.background = element_blank(), panel.border = element_blank()) + 
  annotate("text", label="6", x=1.1, y=3.5, size=5) +
  annotate("text", label="1", x=1.1, y=0.5, size=5) +
  annotate("text", label="2", x=1.1, y=7.3, size=5) +
  annotate("text", label="2", x=1.1, y=9.3, size=5) +
  annotate("text", label="2", x=1.1, y=11.3, size=5) +
  annotate("text", label="6", x=1.1, y=16.5, size=5) +
  annotate("text", label="2", x=1.1, y=22, size=5) +
  annotate("text", label="2", x=1.1, y=24, size=5) +
  annotate("text", label="24", x=1.1, y=36, size=5) +
  annotate("text", label="2", x=1.1, y=52, size=5) +
  annotate("text", label="2", x=1.1, y=50, size=5) +
  annotate("text", label="4", x=1.1, y=55, size=5) +
  
  annotate("text", label="22", x=1.9, y=33, size=5) +
  annotate("text", label="4", x=1.9, y=55, size=5) +
  annotate("text", label="2", x=1.9, y=48, size=5) +
  annotate("text", label="2", x=1.9, y=51, size=5) +
  annotate("text", label="6", x=1.9, y=10, size=5) +
  annotate("text", label="6", x=1.9, y=5, size=5) +
  annotate("text", label="2", x=1.9, y=15, size=5) +
  annotate("text", label="2", x=1.9, y=17, size=5) +
  annotate("text", label="2", x=1.9, y=19, size=5) +
  annotate("text", label="2", x=1.9, y=21, size=5) +
  
  annotate("text", label="24", x=2.1, y=33, size=5) +
  annotate("text", label="4", x=2.1, y=18, size=5) +
  annotate("text", label="2", x=2.1, y=15, size=5) + 
  annotate("text", label="12", x=2.1, y=7, size=5) +
  annotate("text", label="4", x=2.1, y=50, size=5) +
  annotate("text", label="4", x=2.1, y=55, size=5) +
  
  annotate("text", label="40", x=2.9, y=28, size=5) +
  annotate("text", label="4", x=2.9, y=4.6, size=5) +
  annotate("text", label="2", x=2.9, y=51, size=5) +
  annotate("text", label="4", x=2.9, y=55, size=5) +
  xlab("") + ylab("")


##--------------Replot with 5 strata
##--------------


ggplot(sgi3, aes(weight=Freq, axis1= Symptoms, axis2 =Provider, 
                 axis3 = Result, axis4 = Scoped, axis5=Diagnosis)) +
  geom_alluvium(aes(fill=Diagnosis), width = 0, knot.pos =0.33,  reverse=TRUE) +
  guides(fill=FALSE) +
  geom_stratum(width = 1/10,reverse=TRUE) +
  geom_text(stat = "stratum", label.strata =TRUE, reverse=TRUE) +
  scale_x_continuous(breaks = 1:5, labels = c("Symptoms", "Provider", "Result",
                                              "Scoped", "Diagnosis"))+
  coord_flip() +
  ggtitle("SGI Results and Diagnoses by Symptoms,Provider, and Scope") +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=12)) +
  scale_fill_manual(values = c("purple", "green", "blue", "red")) +
  theme(panel.background = element_blank(), panel.border = element_blank()) + 
  xlab("") + ylab("")




###--------------------
##---------------------
 # now for Shail adherence paper

df <- data.frame(
  drug <- c(rep("Humira",4), rep("Cimzia",4)),
  missed <- c(rep("Yes", 2), rep("No", 2),rep("Yes", 2), rep("No", 2)),
  hosp <- c(rep(c("Yes", "No"),4)),
  count <- c(420, 672, 1305, 2928, 143, 173, 126, 281)
)
names(df) <- c("drug", "missed", "hosp", "count")

#now plot with annotation
ggplot(df, aes(weight=count, axis1= drug, 
                 axis2 = missed, axis3 = hosp)) +
  geom_alluvium(aes(fill=missed), width = 0, knot.pos =0.33,  reverse=TRUE) +
  guides(fill=FALSE) +
  geom_stratum(width = 1/15,reverse=TRUE) +
  geom_text(stat = "stratum", label.strata =TRUE, reverse=TRUE) +
  scale_x_continuous(breaks = 1:3, labels = c("Drug", "Missed At Least \n1 in 7 Doses",
                                              "Flare or \nHospitalization"))+
  coord_flip() +
  ggtitle("Missing Doses Increases the Risk of Flares and Hospitalizations") +
  theme(title = element_text(size=14)) +
  theme(plot.title = element_text(hjust = 0.25)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  scale_fill_manual(values = c("blue", "red")) +
  theme(panel.background = element_blank(), panel.border = element_blank()) + 
  annotate("text", label="420", x=1.1, y=190, size=5) +
  annotate("text", label="672", x=1.1, y=700, size=5) +
  annotate("text", label="1305", x=1.1, y=1710, size=5) +
  annotate("text", label="2928", x=1.1, y=4050, size=5) +
  annotate("text", label="143", x=1.1, y=5250, size=5) +
  annotate("text", label="173", x=1.15, y=5400, size=5) +
  annotate("text", label="126", x=1.1, y=5700, size=5) +
  annotate("text", label="281", x=1.2, y=5900, size=5) +
  
  annotate("text", label="420", x=1.9, y=190, size=5) +
  annotate("text", label="143", x=1.9, y=600, size=5) +
  annotate("text", label="672", x=1.85, y=900, size=5) + 
  annotate("text", label="173", x=1.9, y=1400, size=5) +
  annotate("text", label="1305", x=1.9, y=2000, size=5) +
  annotate("text", label="126", x=1.9, y=2830, size=5) +
  annotate("text", label="2928", x=1.9, y=4300, size=5) +
  annotate("text", label="281", x=1.9, y=5900, size=5) +
  
  annotate("text", label="563", x=2.1, y=300, size=5) +
  annotate("text", label="845", x=2.1, y=1050, size=5) +
  annotate("text", label="1431", x=2.1, y=2100, size=5) + 
  annotate("text", label="2928", x=2.1, y=4300, size=5) +
  annotate("text", label="281", x=2.1, y=5900, size=5) +
  
  annotate("text", label="543", x=2.9, y=300, size=5) +
  annotate("text", label="845", x=2.9, y=2340, size=5) +
  annotate("text", label="1431", x=2.9, y=1200, size=5) +
  annotate("text", label="3209", x=2.9, y=4500, size=5) +
  xlab("") + ylab("Patients") +
  labs(caption = "Flow of patients by adherence. Numbers indicate the number of patients in each flow. 
Red = missed more than 1 in 7 doses, Blue = missed less than 1 in 7 doses.
23% of patients miss at least 1 in 7 doses, and 39% of these patients have a flare or hospitalization.") +
  theme(plot.caption = element_text(hjust = 0.0)) 
