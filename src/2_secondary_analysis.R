

rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
source(here("plant_trial_functions.R"))

load(here("data/analysis_dataset.Rdata"))


#

#Subset to Y, A, and W variables
colnames(d)
d$X28.Age <- as.numeric(d$X28.Age)
Wvars <- subset(d, select = c(
  "Wolf.attitude",
  "X28.Age",	"X29.Gender",	"X30.Ethnicity",	"X31.Education",	"X32.income",	"X33.urbanrural",	"political.affiliatio",
  "X36.rancher",	"X37.hunter",	"X38.pets"))



#check which outcomes are binary vs. continious
  # "X25.similarwolveshuma",	"wolf.impact",	"X12.descrip.local",	
  # "X13.descrip.global",	#"injunctive.family", #Analyze injuctive family seperately as order logistic
  # "X18.competence", "X19.likability"
  # 

#Clean outcomes - convert blanks to NA's
for(i in 1:length(Y)){
  d[,Y[i]][d[,Y[i]]==""] <- NA
}
d <- droplevels(d)

#Convert continious outcomes to numeriC
d$descrip.local <- as.numeric(gsub("%","",d$X12.descrip.local))
d$descrip.global <- as.numeric(gsub("%","",d$X13.descrip.global))


#Convert likert-type to numeric
d <- d %>% mutate(
  similarwolveshuma = case_when(
    X25.similarwolveshuma == "Not at all similar" ~ 1,
    X25.similarwolveshuma == "Slightly similar" ~ 2,
    X25.similarwolveshuma == "Moderately similar" ~ 3,
    X25.similarwolveshuma == "Very similar" ~ 4,
    X25.similarwolveshuma == "Extremely similar" ~ 5),
  competence = case_when(
    X18.competence == "Not competent at all" ~ 1,
    X18.competence == "Slightly competent" ~ 2,
    X18.competence == "Moderately competent" ~ 3,
    X18.competence == "Very competent" ~ 4,
    X18.competence == "Extremely competent" ~ 5),
  wolf.impact = case_when(
    wolf.impact == "Extremely negative" ~ 1,
    wolf.impact == "Moderately negative" ~ 2,
    wolf.impact == "Slightly negative" ~ 3,
    wolf.impact == "Neither positive nor negative" ~ 4,
    wolf.impact == "Slightly positive" ~ 5,
    wolf.impact == "Moderately positive" ~ 6,
    wolf.impact == "Extremely positive" ~ 7),
  likability = case_when(
    X19.likability == "Extremely unlikeable" ~ 1,
    X19.likability == "Moderately unlikeable" ~ 2,
    X19.likability == "Slightly unlikeable" ~ 3,
    X19.likability == "Neither unlikeable nor likeable" ~ 4,
    X19.likability == "Slightly likeable" ~ 5,
    X19.likability == "Moderately likeable" ~ 6,
    X19.likability == "Extremely likeable" ~ 7),
  DV.vote.certain = case_when(
    DV.vote.certain == "Not at all certain" ~ 1,
    DV.vote.certain == "Slightly certain" ~ 2,
    DV.vote.certain == "Moderately certain" ~ 3,
    DV.vote.certain == "Very certain " ~ 4,
    DV.vote.certain == "Extremely certain" ~ 5),
  injunctive.family = factor(injunctive.family, levels = c(
    "My family would probably expect me to vote against the initiative",
    "My family wouldn't care how I vote" ,
    "My family would probably expect me to vote in favor of the initiative"  
  ))
)

                            

table(d$similarwolveshuma)
table(d$competence)
table(d$wolf.impact)
table(d$likability)
table(d$injunctive.family)
table(d$DV.vote.certain)


Y <- c(
  "similarwolveshuma",	"wolf.impact",	"descrip.local",	
  "descrip.global",	"competence", "likability","DV.vote.certain")


d <- droplevels(d)


#Estimate models for all outcomes - unadjusted
res1<-wolf.glm(Yname=Y[1], Ws=NULL, data=d)
res2<-wolf.glm(Yname=Y[2], Ws=NULL, data=d)
res3<-wolf.glm(Yname=Y[3], Ws=NULL, data=d)
res4<-wolf.glm(Yname=Y[4], Ws=NULL, data=d)
res5<-wolf.glm(Yname=Y[5], Ws=NULL, data=d)
res6<-wolf.glm(Yname=Y[6], Ws=NULL, data=d)
res7<-wolf.glm(Yname=Y[7], Ws=NULL, data=d)

save(Y, res1, res2, res3, res4, res5, res6, res7, 
     file=here("results/unadjusted_regression_secondary_results.rdata"))


#Estimate models for all outcomes - adjusted
res1<-wolf.glm(Yname=Y[1])
res2<-wolf.glm(Yname=Y[2])
res3<-wolf.glm(Yname=Y[3])
res4<-wolf.glm(Yname=Y[4])
res5<-wolf.glm(Yname=Y[5])
res6<-wolf.glm(Yname=Y[6])
res7<-wolf.glm(Yname=Y[7])

save(Y, res1, res2, res3, res4, res5, res6, res7,
     file=here("results/adjusted_regression_secondary_results.rdata"))

