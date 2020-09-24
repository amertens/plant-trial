

rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(multcomp)
library(here)

source(here("plant_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))




#Estimate models for all outcomes - unadjusted
res_self_unadj <- plant.glm(Yname=Y[1], Ws=NULL, data=d, family="binomial")
res_friend_unadj <- polr_format(Yvar=Y[2], Ws=NULL, df=d)$res

#sensitivity analysis, binary outcome of any friend usage
res_friend_bin_unadj <- plant.glm(Yname=Y[3], Ws=NULL, data=d, family="binomial")

save(res_self_unadj, res_friend_unadj, res_friend_bin_unadj, 
  file=here("results/unadjusted_regression_results.rdata"))

# 
# #Estimate models for all outcomes - adjusted
# res1<-plant.glm(Yname=Y[1], family="binomial")
# res2<-plant.glm(Yname=Y[2])
# 
# 
# save(Y, res1, res2, res3, res4, res5, 
#      file=here("results/adjusted_regression_results.rdata"))


