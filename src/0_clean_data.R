


rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(here)

#Read in the data
d <- read.csv(here("data/FullSampleReducedVarsMASTER.csv"))

#clean data
d$condition[d$condition=="norm"] <- "norms"
d$condition <- factor(d$condition, levels=c("control","norms","efficacy","combined"))

#simple tabulation of primary outcomes by num_condition and sample
table(d$condition, d$selftimesused)
table(d$condition, d$friendtimesused)

d %>% group_by(condition) %>% summarize(sum(friendtimesused))

table(d$sample)

d1 <- d %>% filter(sample==1)
d2 <- d %>% filter(sample==2)
table(d1$condition, d1$selftimesused)
table(d1$condition, d1$friendtimesused)
d1 %>% group_by(condition) %>% summarize(sum(friendtimesused))

table(d2$condition, d2$selftimesused)
table(d2$condition, d2$friendtimesused)
d2 %>% group_by(condition) %>% summarize(sum(friendtimesused))



#Subset to Y, A, and W variables and clean W variables
colnames(d)
d <- d %>% 
  rename(id=ï..survey_ID,
         tr=condition) %>%
  mutate(friendused=1*(friendtimesused>0),
         friendused=ifelse(is.na(friendtimesused),NA, friendused),
         friendtimesused=factor(friendtimesused, levels=c("0","1","2","3"))) %>%
  subset(., select=c(id, sample, tr, selftimesused, friendtimesused, friendused,
         SE_plant, RE_env_plant, RE_social_plant,
         in_sanctioning_plant, dyn_plant, SE_encourage,       
         IN_praise_plant,     RE_social_encourage,
         RE_env_encourage,    in_encourage,       
         competent_encourage))

Wvars=NULL
Y <- c("selftimesused", "friendtimesused","friendused")


#Save cleaned analysis data
save(d, Y, Wvars, file=here("data/analysis_dataset.Rdata"))
