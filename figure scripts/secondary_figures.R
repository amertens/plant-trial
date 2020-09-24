


# wolf.impact= Impact of Wolves
# descrip.local (NO FIGURE)
# descrip.global= Descriptive Norms
# 14 injunctive friend= Injunctive Norms (friends)
# injunctive family= Injunctive Norms (family)
# competence= Perceived Competence
# likability= Perceived Likability
# similarwolveshuma= Wolf Similarity to People

library(here)
library(tidyverse)
library(cowplot)
tableau10 <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
theme_set(theme_bw())

load(here("results/adjusted_regression_secondary_results.rdata"))

             
d <- bind_rows(
  data.frame(res1, Y=Y[1]),
  data.frame(res2, Y=Y[2]),
  data.frame(res3, Y=Y[3]),
  data.frame(res4, Y=Y[4]),
  data.frame(res5, Y=Y[5]),
  data.frame(res6, Y=Y[6]),
  data.frame(res7, Y=Y[7])) 
d <- d %>%
  filter(reference=="Control") %>%
  rename(tr=intervention)

#Calc 95% CI
d$CI1 <- d$est - 1.96*d$se
d$CI2 <- d$est + 1.96*d$se

#calculate corrected CI's
Z<-qnorm(.025/5,lower.tail=FALSE)
d$adjCI1 <- d$est - Z*d$se
d$adjCI2 <- d$est + Z*d$se
d$adjCI1[!is.na(d$RR)] <- exp(d$est[!is.na(d$RR)] - Z*d$se[!is.na(d$RR)])
d$adjCI2[!is.na(d$RR)] <- exp(d$est[!is.na(d$RR)] + Z*d$se[!is.na(d$RR)])


head(d)



#Format for plot
d$tr <- as.character(d$tr)
d$tr[d$tr=="Wolves= people"] <- "Anthropomorphism"

d$Intervention <- factor(d$tr, levels=c("Norms", "Competing norms", "Moderate", "Extreme", "Anthropomorphism"))


d <- d %>% filter(Y != "descrip.local")

#outcome names
d <- d %>% mutate(Yname = case_when(
  Y == "similarwolveshuma" ~ "Wolf Similarity to People",
  Y == "wolf.impact" ~ "Impact of Wolves",
  Y == "descrip.global" ~ "Descriptive Norms",
  Y == "competence" ~ "Perceived Competence",
  Y == "likability" ~ "Perceived Likability",
  Y == "DV.vote.certain" ~ "Vote certainty"
))







p <- ggplot(d, aes(x=Intervention, y=est)) + 
  geom_point(aes(color=Intervention), size = 3) +
  geom_linerange(aes(ymin=CI1, ymax=CI2, color=Intervention),
                 alpha=0.5, size = 1) +
  facet_wrap(~Yname, scales="free_y") +
  labs(y ="Adjusted difference from control message" , x = "Intervention message") +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values=tableau10) + 
  ggtitle("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(p, file = here("figures/secondary_continious.png"), height=6, width=14)






