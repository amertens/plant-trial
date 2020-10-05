

rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(multcomp)
library(here)

source(here("plant_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))



#-------------------------------------------------------
#Estimate models for all outcomes - unadjusted
#-------------------------------------------------------

res_self_unadj <- plant.glm(Yname=Y[1], Ws=NULL, data=d, family="binomial")
res_friend_unadj <- polr_format(Yvar=Y[2], Ws=NULL, df=d)$res

#sensitivity analysis, binary outcome of any friend usage
res_friend_bin_unadj <- plant.glm(Yname=Y[3], Ws=NULL, data=d, family="binomial")

save(Y, res_self_unadj, res_friend_unadj, res_friend_bin_unadj, 
  file=here("results/unadjusted_regression_results.rdata"))


#Subgroup analyses
df1 <- d %>% filter(sample==1)
res_self_unadj_samp1 <- plant.glm(Yname=Y[1], Ws=NULL, data=df1, family="binomial")
res_friend_unadj_samp1 <- polr_format(Yvar=Y[2], Ws=NULL, df=df1)$res
res_friend_bin_unadj_samp1 <- plant.glm(Yname=Y[3], Ws=NULL, data=df1, family="binomial")

df2 <- d %>% filter(sample==2)
res_self_unadj_samp2 <- plant.glm(Yname=Y[1], Ws=NULL, data=df2, family="binomial")
res_friend_unadj_samp2 <- polr_format(Yvar=Y[2], Ws=NULL, df=df2)$res
res_friend_bin_unadj_samp2 <- plant.glm(Yname=Y[3], Ws=NULL, data=df2, family="binomial")

save(Y, res_self_unadj_samp1, res_friend_unadj_samp1, res_friend_bin_unadj_samp1,
     res_self_unadj_samp2, res_friend_unadj_samp2, res_friend_bin_unadj_samp2,
     file=here("results/unadjusted_regression_results_subsample.rdata"))













#-------------------------------------------------------
#Estimate models for all outcomes - adjusted
#-------------------------------------------------------


# res_self_adj<- plant.glm(Yname=Y[1], Ws=Wdf, data=d, family="binomial")
# res_friend_adj<- polr_format(Yvar=Y[2], Ws=Wdf, df=d)$res
# 
# #sensitivity analysis, binary outcome of any friend usage
# res_friend_bin_adj<- plant.glm(Yname=Y[3], Ws=Wdf, data=d, family="binomial")
# 
# save(res_self_adj, res_friend_adj, res_friend_bin_adj, 
#      file=here("results/adjusted_regression_results.rdata"))


#Subgroup analyses
df1 <- d %>% filter(sample==1)

for(i in Wvars){
  cat(i, ":\n")
  print(table(is.na(df1[[i]])))
}

Wdf <- df1 %>% subset(., select=Wvars)

res_self_adj_samp1 <- plant.glm(Yname=Y[1], Ws=Wdf, data=df1, family="binomial")
res_friend_adj_samp1 <- polr_format(Yvar=Y[2], Ws=Wdf, df=df1)$res
res_friend_bin_adj_samp1 <- plant.glm(Yname=Y[3], Ws=Wdf, data=df1, family="binomial")

 df2 <- d %>% filter(sample==2)
 for(i in Wvars){
   cat(i, ":\n")
   print(table(is.na(df2[[i]])))
 }
# res_self_adj_samp2 <- plant.glm(Yname=Y[1], Ws=Wdf, data=df1, family="binomial")
# res_friend_adj_samp2 <- polr_format(Yvar=Y[2], Ws=Wdf, df=df1)$res
# res_friend_bin_adj_samp2 <- plant.glm(Yname=Y[3], Ws=Wdf, data=df1, family="binomial")

save(Y, res_self_adj_samp1, res_friend_adj_samp1, res_friend_bin_adj_samp1,
     #res_self_adj_samp2, res_friend_adj_samp2, res_friend_bin_adj_samp2,
     file=here("results/adjusted_regression_results_subsample.rdata"))
