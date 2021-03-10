


rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(here)

#Read in the data
d <- read.csv(here("data/AllSampleReducedVarsMASTER.csv"))

#clean data
d$condition[d$condition=="norm"] <- "norms"
d$condition <- factor(d$condition, levels=c("control","norms","efficacy","combined"))

d$yard.sign.requests <- ifelse(d$yard.sign.requests=="",0,1)
table(d$yard.sign.requests)
table(d$condition, d$yard.sign.requests)

#simple tabulation of primary outcomes by num_condition and sample
table(d$condition, d$selftimesused)
table(d$condition, d$friendtimesused)

d %>% group_by(condition) %>% summarize(sum(friendtimesused))

table(d$NIC_Random)

d1 <- d %>% filter(NIC_Random==1)
d2 <- d %>% filter(NIC_Random==2)
table(d1$condition, d1$selftimesused)
table(d1$condition, d1$friendtimesused)
d1 %>% group_by(condition) %>% summarize(sum(friendtimesused))

table(d2$condition, d2$selftimesused)
table(d2$condition, d2$friendtimesused)
d2 %>% group_by(condition) %>% summarize(sum(friendtimesused))



#Subset to Y, A, and W variables and clean W variables
colnames(d)
d <- d %>% 
  rename(id=survey_ID,
         sample=NIC_Random,
         tr=condition) %>%
  mutate(friendused=1*(friendtimesused>0),
         friendused=ifelse(is.na(friendtimesused),NA, friendused),
         friendtimesused=factor(friendtimesused, levels=c("0","1","2","3"))) %>%
  subset(., select=c(id, sample, tr, selftimesused, friendtimesused, friendused, yard.sign.requests,
         SE_plant, RE_env_plant, RE_social_plant,
         IN_sanctioning_plant, dyn_plant, SE_encourage,       
         IN_praise_plant,     RE_social_encourage,
         RE_env_encourage,    IN_encourage,       
         competent_encourage,
         rent_own, Gender, Race, Ethnicity, Age, Education, native_plant, convinceOther))

#------------------------------
#Clean covariates
#------------------------------

#prior engagement in planting of native plants, 
#home ownership, gender, age, education, race, and ethnicity 
table(d$Race, d$Ethnicity)
summary(d$Age)

Wvars=c("rent_own", "Gender", "Race", "Ethnicity", "Age", "Education", "native_plant", "convinceOther")
Y <- c("selftimesused", "friendtimesused","friendused")


#Save cleaned analysis data
save(d, Y, Wvars, file=here("data/analysis_dataset.Rdata"))
write.csv(d, file=here("data/analysis_dataset.csv"))



#Desc stats
df <- d %>% filter(sample==1)
round(prop.table(table(df$Gender))*100,2)
round(prop.table(table(df$Race))*100,2)
round(prop.table(table(df$Ethnicity))*100,2)
summary(df$Age)
round(prop.table(table(df$rent_own))*100,2)
round(prop.table(table(df$Education))*100,2)
39.08 + 47.13

round(prop.table(table(df$native_plant))*100,2)
round(prop.table(table(df$convinceOther))*100,2)

#Desc stats
df <- d %>% filter(sample==2)
round(prop.table(table(df$Gender))*100,1)
round(prop.table(table(df$Race))*100,1)
round(prop.table(table(df$Ethnicity))*100,2)
summary(df$Age)
round(prop.table(table(df$Education))*100,2)
round(prop.table(table(df$rent_own))*100,2)

round(prop.table(table(df$native_plant))*100,2)
round(prop.table(table(df$convinceOther))*100,2)


#% bachelor's degree or higher (education 4 or 5) for sample 1?
prop.table(table(df$Education))

# % of those who selftimesused=1 with native_plant=1 for sample 1 and 2
df <- d %>% filter(native_plant==1)
prop.table(table(df$selftimesused==1, df$sample),2)*100


# % of those who friendused=1 with convinceOther=1 for sample 1 and 2
df <- d %>% filter(convinceOther==1)
prop.table(table(df$friendused==1, df$sample),2)*100




#voucher usage
table(d$selftimesused)
table(d$selftimesused, d$sample)

table(d$friendtimesused)
84 +  2*22  + 3*10

round(prop.table(table(d$selftimesused, d$sample), 1)*100,2)
table(d$friendtimesused, d$sample)


#
df <- d %>% filter(native_plant==2)
round(prop.table(table(df$selftimesused, df$sample), 2)*100,2)

