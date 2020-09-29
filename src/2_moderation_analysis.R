

rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(SmartEDA)
source(here("plant_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))


#Code moderator variables
df <- d %>% 
  subset(., select=c(
         SE_plant,	
         RE_env_plant,	
         RE_social_plant,	
         IN_sanctioning_plant,	
         dyn_plant,	
         SE_encourage,	
         IN_praise_plant,	
         RE_social_encourage,	
         RE_env_encourage,	
         IN_encourage	,
         competent_encourage))
ExpCTable(df)

# d <- d %>% 
#   mutate(
#     SE_plant=factor(SE_plant),	
#     RE_env_plant=factor(RE_env_plant),	
#     RE_social_plant=factor(RE_social_plant),	
#     IN_sanctioning_plant=factor(IN_sanctioning_plant),	
#     dyn_plant=factor(dyn_plant),	
#     SE_encourage=factor(SE_encourage),	
#     IN_praise_plant=factor(IN_praise_plant),	
#     RE_social_encourage=factor(RE_social_encourage),	
#     RE_env_encourage=factor(RE_env_encourage),	
#     IN_encourage=factor(IN_encourage),
#     competent_encourage=factor(competent_encourage))


#Estimate models for all outcomes - control as reference

Vvars = c("SE_plant",	
  "RE_env_plant",	
  "RE_social_plant",	
  "IN_sanctioning_plant",	
  "dyn_plant",	
  "SE_encourage",	
  "IN_praise_plant",	
  "RE_social_encourage",	
  "RE_env_encourage",	
  "IN_encourage"	,
  "competent_encourage")

resfull <- NULL
for(i in Vvars){
  res1<-glm_mod_format(d=d,Yvar=Y[1], Wvars=Wvars, family="binomial", V=i)
  res2<-NULL
  #res2<-glm_mod_format(d=d,Yvar=Y[2]) #must update for polr
  res3<-glm_mod_format(d=d,Yvar=Y[3], Wvars=Wvars, family="binomial", V=i)
  
  resfull <- bind_rows(resfull, res1, res2, res3)
}





save(Y, 
     resfull,
     file=here("results/adjusted_moderation_analysis_results.rdata"))



#Run unadjusted
d$W=rep(1, nrow(d))


resfull_unadj <- NULL
for(i in Vvars){
  res1<-glm_mod_format(d=d,Yvar=Y[1], Wvars="W", family="binomial", V=i)
  res2<-NULL
  #res2<-glm_mod_format(d=d,Yvar=Y[2]) #must update for polr
  res3<-glm_mod_format(d=d,Yvar=Y[3], Wvars="W",  family="binomial", V=i)
  
  resfull_unadj <- bind_rows(resfull_unadj, res1, res2, res3)
}

resfull_unadj <- resfull_unadj %>% distinct( control, treatment, outcome, V, int.p)

save(Y, 
     resfull_unadj, 
     file=here("results/unadjusted_moderation_analysis_results.rdata"))




