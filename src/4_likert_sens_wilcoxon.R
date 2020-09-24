

rm(list=ls())
library(tidyverse)
library(caret)
library(washb)
library(multcomp)
library(here)

source(here("wolf_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))


res2<-wolf.wilcox(Yname=Y[2], data=d)
res3<-wolf.wilcox(Yname=Y[3], data=d)
res4<-wolf.wilcox(Yname=Y[4], data=d)
res5<-wolf.wilcox(Yname=Y[5], data=d)

prim.res <- bind_rows(res2, res3, res4, res5)

save(Y, prim.res, file=here("results/primary_wilcoxon_results.rdata"))


#secondary outcomes
#Clean outcomes - convert blanks to NA's



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
    DV.vote.certain == "Extremely certain" ~ 5)
)

Y <- c(
  "similarwolveshuma",	"wolf.impact",	"competence", "likability","DV.vote.certain")

d <- droplevels(d)

table(d$similarwolveshuma)
table(d$competence)
table(d$wolf.impact)
table(d$likability)
table(d$DV.vote.certain)


res1<-wolf.wilcox(Yname=Y[1], data=d)
res2<-wolf.wilcox(Yname=Y[2], data=d)
res3<-wolf.wilcox(Yname=Y[3], data=d)
res4<-wolf.wilcox(Yname=Y[4], data=d)
res5<-wolf.wilcox(Yname=Y[5], data=d)

sec.res <- bind_rows(res1, res2, res3, res4, res5)

save(Y, sec.res, file=here("results/secondary_wilcoxon_results.rdata"))




#merge together linear regression and wilcoxon P-values 
load(here("results/unadjusted_regression_results.rdata"))
prim.reg <- bind_rows(res2, res3, res4, res5)
prim.reg <- prim.reg %>% subset(., select = c(Y,intervention, reference, p, corrected.p)) %>%
  rename(reg.p=p, reg.corrected.p=corrected.p)
prim <- left_join(prim.reg, prim.res, by=c("Y", "reference", "intervention"))
prim <- prim %>% rename(wilcoxon.p=p, wilcoxon.corrected.p=corrected.p) %>% 
  subset(., select=c(Y, reference, intervention, reg.p, wilcoxon.p, reg.corrected.p, wilcoxon.corrected.p)) %>%
  filter(!is.na(wilcoxon.corrected.p))


load(here("results/unadjusted_regression_secondary_results.rdata"))
sec.reg <- bind_rows(res1, res2, res5, res6, res7)
sec.reg <- sec.reg %>% subset(., select = c(Y,intervention, reference, p, corrected.p)) %>%
  rename(reg.p=p, reg.corrected.p=corrected.p)
sec <- left_join(sec.reg, sec.res, by=c("Y", "reference", "intervention"))
sec <- sec %>% rename(wilcoxon.p=p, wilcoxon.corrected.p=corrected.p) %>% 
  subset(., select=c(Y, reference, intervention, reg.p, wilcoxon.p, reg.corrected.p, wilcoxon.corrected.p)) %>%
  filter(!is.na(wilcoxon.corrected.p))


#Clean up names for table
prim$intervention[prim$intervention=="Wolves= people"] <- "Anthropomorphism"
prim$reference[prim$reference=="Wolves= people"] <- "Anthropomorphism"
sec$intervention[sec$intervention=="Wolves= people"] <- "Anthropomorphism"
sec$reference[sec$reference=="Wolves= people"] <- "Anthropomorphism"

#outcome names
prim <- prim %>% mutate(Y = case_when(
  Y == "X1.DV.Vote" ~ "Voting",
  Y == "DV.anti.share" ~ "Sharing Anti Wolf Info",
  Y == "DV.organize.for" ~ "Organizing Pro Wolf",
  Y == "DV.organizeanti" ~ "Organizing Anti Wolf", 
  Y == "DV.pro.share" ~ "Sharing Pro Wolf Info"
))
#outcome names
sec <- sec %>% mutate(Y = case_when(
  Y == "similarwolveshuma" ~ "Wolf Similarity to People",
  Y == "wolf.impact" ~ "Impact of Wolves",
  Y == "descrip.global" ~ "Descriptive Norms",
  Y == "competence" ~ "Perceived Competence",
  Y == "likability" ~ "Perceived Likability",
  Y == "DV.vote.certain" ~ "Vote certainty"
))

colnames(prim) <- c("Outcome","Reference", "Intervention", "Regression P-value", "Wilcoxon P-value", "Corrected Regression P-value", "Corrected Wilcoxon P-value")
colnames(sec) <- c("Outcome","Reference", "Intervention", "Regression P-value", "Wilcoxon P-value", "Corrected Regression P-value", "Corrected Wilcoxon P-value")

save(prim, sec, file=here("results/wilcoxon_table_dat.rdata"))

