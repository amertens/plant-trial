

rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(SmartEDA)
library(here)
library(multcomp)
library(broom)
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

i=Vvars[1]
Yvar=Y[1]
family="binomial"
Yvar=Y[2]
family="polr"
V=i
control="control"
contrasts=c("norms", "efficacy", "combined")

resfull <- NULL
for(i in Vvars){
  print(i)
  res1 <- res1_sub1 <- res1_sub2 <- res2 <- res2_sub1 <- res2_sub2 <- res3 <- res3_sub1 <- res3_sub2 <- NULL
  res1<-glm_mod_format(d=d,Yvar=Y[1], Wvars=Wvars, family="binomial", V=i)
  res1_sub1<-glm_mod_format(d=d,Yvar=Y[1], Wvars=Wvars, family="binomial", V=i,  control="norms", contrasts=c("efficacy", "combined"))
  res1_sub2<-glm_mod_format(d=d,Yvar=Y[1], Wvars=Wvars, family="binomial", V=i,  control="efficacy", contrasts=c("combined"))

  if(!(i %in% c("IN_praise_plant","IN_encourage"))){
  res2<-glm_mod_format(d=d,Yvar=Y[2], Wvars=Wvars, family="polr", V=i) #must update for polr
  res2_sub1<-glm_mod_format(d=d,Yvar=Y[2], Wvars=Wvars, family="polr", V=i,  control="norms", contrasts=c("efficacy", "combined")) #must update for polr
  res2_sub2<-glm_mod_format(d=d,Yvar=Y[2], Wvars=Wvars, family="polr", V=i,  control="efficacy", contrasts=c("combined")) #must update for polr
  }
  res3<-glm_mod_format(d=d,Yvar=Y[3], Wvars=Wvars, family="binomial", V=i)
  res3_sub1<-glm_mod_format(d=d,Yvar=Y[3], Wvars=Wvars, family="binomial", V=i,  control="norms", contrasts=c("efficacy", "combined"))
  res3_sub2<-glm_mod_format(d=d,Yvar=Y[3], Wvars=Wvars, family="binomial", V=i,  control="efficacy", contrasts=c("combined"))
  
  resfull <- bind_rows(resfull, res1, res1_sub1, res1_sub2, res2, res2_sub1, res2_sub2,  res3, res3_sub1, res3_sub2)
}





save(Y, 
     resfull,
     file=here("results/adjusted_moderation_analysis_results.rdata"))



#Run unadjusted
d$W=rep(1, nrow(d))


resfull_unadj <- NULL
i=Vvars[1]
for(i in Vvars){
  print(i)
  res1 <- res1_sub1 <- res1_sub2 <- res2 <- res2_sub1 <- res2_sub2 <- res3 <- res3_sub1 <- res3_sub2 <- NULL
  res1<-glm_mod_format(d=d,Yvar=Y[1], Wvars="W", family="binomial", V=i)
  res1_sub1<-glm_mod_format(d=d,Yvar=Y[1], Wvars="W", family="binomial", V=i,  control="norms", contrasts=c("efficacy", "combined"))
  res1_sub2<-glm_mod_format(d=d,Yvar=Y[1], Wvars="W", family="binomial", V=i,  control="efficacy", contrasts=c("combined"))
 
  if(!(i %in% c("IN_praise_plant","IN_encourage"))){
    res2<-glm_mod_format(d=d,Yvar=Y[2], Wvars="W", family="polr", V=i) #must update for polr
    res2_sub1<-glm_mod_format(d=d,Yvar=Y[2], Wvars="W", family="polr", V=i,  control="norms", contrasts=c("efficacy", "combined")) #must update for polr
    res2_sub2<-glm_mod_format(d=d,Yvar=Y[2], Wvars="W", family="polr", V=i,  control="efficacy", contrasts=c("combined")) #must update for polr
  }
  res3<-glm_mod_format(d=d,Yvar=Y[3], Wvars="W", family="binomial", V=i)
  res3_sub1<-glm_mod_format(d=d,Yvar=Y[3], Wvars="W", family="binomial", V=i,  control="norms", contrasts=c("efficacy", "combined"))
  res3_sub2<-glm_mod_format(d=d,Yvar=Y[3], Wvars="W", family="binomial", V=i,  control="efficacy", contrasts=c("combined"))
  
  resfull_unadj <- bind_rows(resfull_unadj, res1, res1_sub1, res1_sub2, res2, res2_sub1, res2_sub2,  res3, res3_sub1, res3_sub2)
}

res_unadj <- resfull_unadj %>% distinct( control, treatment, outcome, V, int.p)

save(Y, res_unadj,
     resfull_unadj, 
     file=here("results/unadjusted_moderation_analysis_results.rdata"))




