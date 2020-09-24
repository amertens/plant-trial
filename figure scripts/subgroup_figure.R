


# DV Vote= Voting
# DV anti share= Organizing Anti Wolf
# DV organize for= Organizing Pro Wolf
# DV organize anti= Sharing Anti Wolf Info
# DV pro share= Sharing Pro Wolf Info

# wolf.impact= Impact of Wolves
# descrip.local (NO FIGURE)
# descrip.global= Descriptive Norms
# 14 injunctive friend= Injunctive Norms (friends)
# injunctive family= Injunctive Norms (family)
# competence= Perceived Competence
# likability= Perceived Likability
# similarwolveshuma= Wolf Similarity to People

rm(list=ls())
library(tidyverse)
library(cowplot)
tableau10 <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
theme_set(theme_bw())

load(here("results/unadjusted_moderation_analysis_results.rdata"))


d <- bind_rows(
  data.frame(res1),
  data.frame(res2),
  data.frame(res3),
  data.frame(res4),
  data.frame(res5)
)

#Calc 95% CI
d$CI1 <- d$est - 1.96*d$se
d$CI2 <- d$est + 1.96*d$se

head(d)



#Format for plot
d$treatment[d$treatment=="Wolves= people"] <- "Anthropomorphism"
d$Intervention <- factor(d$treatment, levels=c("Norms", "Competing norms", "Moderate", "Extreme", "Anthropomorphism"))

#outcome names
d <- d %>% mutate(Yname = case_when(
  outcome == "X1.DV.Vote" ~ "Voting",
  outcome == "DV.anti.share" ~  "Sharing Anti Wolf Info",
  outcome == "DV.organize.for" ~ "Organizing Pro Wolf",
  outcome == "DV.organizeanti" ~ "Organizing Anti Wolf",
  outcome == "DV.pro.share" ~ "Sharing Pro Wolf Info"
))


#Subgroup names
d <- d %>% mutate(Subgroup = case_when(
  subgroup == "like" ~ "Like wolves",
  subgroup == "neutral" ~ "Neutral about\nwolves"))


 


p <- ggplot(d[is.na(d$RR),], aes(x=Intervention, y=est, group=Subgroup)) + 
  geom_point(aes(color=Intervention, shape=Subgroup), size = 3, position = position_dodge(width = 0.4)) +
  geom_linerange(aes(ymin=CI1, ymax=CI2, color=Intervention),
                 alpha=0.5, size = 1,  position = position_dodge(width = 0.4)) +
  facet_grid(~Yname) +
  labs(y ="Adjusted difference from control message" , x = "Intervention message") +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values=tableau10) + 
  scale_shape_manual(values=c(16,2)) +
  ggtitle("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(p, file = here("figures/subgroup_continious.png"), height=6, width=14)





pRR <- ggplot(d[!is.na(d$RR),], aes(x=Intervention, y=RR, group=Subgroup)) + 
  geom_point(aes(color=Intervention,  shape=Subgroup), size = 3, position = position_dodge(width = 0.4)) +
  geom_linerange(aes(ymin=RR.lb, ymax=RR.ub, color=Intervention, shape=Subgroup),
                 alpha=0.5, size = 1, position = position_dodge(width = 0.4)) +
  scale_y_continuous(trans = "log10") +
  labs(y ="Adjusted Relative Risk" , x = "Intervention message") +
  geom_hline(yintercept = 1) +
  scale_colour_manual(values=tableau10) + 
  scale_shape_manual(values=c(16,2)) +
  ggtitle("Plan on voting yes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(pRR, file = here("figures/subgroup_binary.png"), height=6, width=14)


