

rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
source(here("wolf_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))



#Code moderator variable as 3 levels
d <- d %>% mutate(
  wolf_mod = Wolf.attitude) %>%
  #Drop sparse dislike category
  filter(wolf_mod != "dislike") %>%
  mutate(
    wolf_mod = factor(wolf_mod, levels = c("neutral", "like"))
  )



#Subset to Y, A, and W variables
colnames(d)
d$X28.Age <- as.numeric(d$X28.Age)
Wvars <- subset(d, select = c(
  #"Wolf.attitude",
  "X28.Age",	"X29.Gender",	"X30.Ethnicity",	"X31.Education",	"X32.income",	"X33.urbanrural",	"political.affiliatio",
  "X36.rancher",	"X37.hunter",	"X38.pets"))







#Estimate models for all outcomes - control as reference
res1<-glm_mod_format(Yvar=Y[1],  family="binomial")
res2<-glm_mod_format(Yvar=Y[2])
res3<-glm_mod_format(Yvar=Y[3])
res4<-glm_mod_format(Yvar=Y[4])
res5<-glm_mod_format(Yvar=Y[5])


#Subgroup contrasts
res1_viewstrength<-glm_mod_format(Yvar=Y[1],  family="binomial",  control="Moderate", contrasts="Extreme")
res2_viewstrength<-glm_mod_format(Yvar=Y[2], control="Moderate", contrasts="Extreme")
res3_viewstrength<-glm_mod_format(Yvar=Y[3], control="Moderate", contrasts="Extreme")
res4_viewstrength<-glm_mod_format(Yvar=Y[4], control="Moderate", contrasts="Extreme")
res5_viewstrength<-glm_mod_format(Yvar=Y[5], control="Moderate", contrasts="Extreme")

res1_norms<-glm_mod_format(Yvar=Y[1],  family="binomial", control="Norms", contrasts="Competing norms")
res2_norms<-glm_mod_format(Yvar=Y[2], control="Norms", contrasts="Competing norms")
res3_norms<-glm_mod_format(Yvar=Y[3], control="Norms", contrasts="Competing norms")
res4_norms<-glm_mod_format(Yvar=Y[4], control="Norms", contrasts="Competing norms")
res5_norms<-glm_mod_format(Yvar=Y[5], control="Norms", contrasts="Competing norms")




save(Y, 
     res1, res2, res3, res4, res5,
     res1_viewstrength, res2_viewstrength, res3_viewstrength, res4_viewstrength, res5_viewstrength,
     res1_norms, res2_norms, res3_norms, res4_norms, res5_norms,
     file=here("results/adjusted_moderation_analysis_results.rdata"))



#Run unadjusted
Wvars <- data.frame(W=rep(1, nrow(d)))


#Estimate models for all outcomes - control as reference
res1<-glm_mod_format(Yvar=Y[1],  family="binomial")
res2<-glm_mod_format(Yvar=Y[2])
res3<-glm_mod_format(Yvar=Y[3])
res4<-glm_mod_format(Yvar=Y[4])
res5<-glm_mod_format(Yvar=Y[5])


#Subgroup contrasts
res1_viewstrength<-glm_mod_format(Yvar=Y[1],  family="binomial",  control="Moderate", contrasts="Extreme")
res2_viewstrength<-glm_mod_format(Yvar=Y[2], control="Moderate", contrasts="Extreme")
res3_viewstrength<-glm_mod_format(Yvar=Y[3], control="Moderate", contrasts="Extreme")
res4_viewstrength<-glm_mod_format(Yvar=Y[4], control="Moderate", contrasts="Extreme")
res5_viewstrength<-glm_mod_format(Yvar=Y[5], control="Moderate", contrasts="Extreme")

res1_norms<-glm_mod_format(Yvar=Y[1],  family="binomial", control="Norms", contrasts="Competing norms")
res2_norms<-glm_mod_format(Yvar=Y[2], control="Norms", contrasts="Competing norms")
res3_norms<-glm_mod_format(Yvar=Y[3], control="Norms", contrasts="Competing norms")
res4_norms<-glm_mod_format(Yvar=Y[4], control="Norms", contrasts="Competing norms")
res5_norms<-glm_mod_format(Yvar=Y[5], control="Norms", contrasts="Competing norms")



save(Y, 
     res1, res2, res3, res4, res5,
     res1_viewstrength, res2_viewstrength, res3_viewstrength, res4_viewstrength, res5_viewstrength,
     res1_norms, res2_norms, res3_norms, res4_norms, res5_norms,
     file=here("results/unadjusted_moderation_analysis_results.rdata"))




