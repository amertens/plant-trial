


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


library(tidyverse)
library(cowplot)
tableau10 <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
theme_set(theme_bw())


load(file="C:/Users/andre/Dropbox/Wolf trial/results/adjusted_ordered_logit_results.rdata")

Yfact 
res1
res2
res3
res4


d <- bind_rows(
  data.frame(tr=rownames(res1$Control), res1$Control, Y=Y[1]),
  data.frame(tr=rownames(res2$Control), res2$Control, Y=Y[2]),
  data.frame(tr=rownames(res3$Control),res3$Control, Y=Y[3]),
  data.frame(tr=rownames(res4$Control),res4$Control, Y=Y[4]),
  data.frame(tr=rownames(res5$Control),res5$Control, Y=Y[5]))


#Calc 95% CI
d$CI1 <- d$est - 1.96*d$se
d$CI2 <- d$est + 1.96*d$se

#Calc RR 95% CI
d$CI1[!is.na(d$RR)] <- exp(d$est[!is.na(d$RR)] - 1.96*d$se[!is.na(d$RR)])
d$CI2[!is.na(d$RR)] <- exp(d$est[!is.na(d$RR)] + 1.96*d$se[!is.na(d$RR)])

head(d)

#Drop wolves = dogs
d <- d %>% filter(tr!="trWolves= dog")
d$tr <- as.character(d$tr)
d$tr[d$tr=="trWolves= people"] <- "Anthropomorphism"

#Format for plot
d$tr <- gsub("tr","",d$tr)
d$tr[d$tr=="Exeme"] <- "Extreme"
d$Intervention <- factor(d$tr, levels=c("Norms", "Competing norms", "Moderate", "Extreme", "Anthropomorphism"))

#outcome names
d <- d %>% mutate(Yname = case_when(
  Y == "X1.DV.Vote" ~ "Voting",
  Y == "DV.anti.share" ~ "Sharing Anti Wolf Info",
  Y == "DV.organize.for" ~ "Organizing Pro Wolf",
  Y == "DV.organizeanti" ~ "Organizing Anti Wolf", 
  Y == "DV.pro.share" ~ "Sharing Pro Wolf Info"
))



p <- ggplot(d[is.na(d$RR),], aes(x=Intervention, y=est)) + 
  geom_point(aes(color=Intervention), size = 3) +
  geom_linerange(aes(ymin=CI1, ymax=CI2, color=Intervention),
                 alpha=0.5, size = 1) +
  facet_wrap(~Yname, scales="free_y") +
  labs(y ="Adjusted difference from control message" , x = "Intervention message") +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values=tableau10) + 
  ggtitle("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggsave(p, file = "C:/Users/andre/Dropbox/Wolf trial/figures/primary_continious.png", height=6, width=14)






pRR <- ggplot(d[!is.na(d$RR),], aes(x=Intervention, y=RR)) + 
  geom_point(aes(color=Intervention), size = 3) +
  geom_linerange(aes(ymin=CI1, ymax=CI2, color=Intervention),
                 alpha=0.5, size = 1) +
  scale_y_continuous(trans = "log10") +
  labs(y ="Adjusted Relative Risk" , x = "Intervention message") +
  geom_hline(yintercept = 1) +
  scale_colour_manual(values=tableau10) + 
  ggtitle("Plan on voting yes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(pRR, file = "C:/Users/andre/Dropbox/Wolf trial/figures/primary_binary.png", height=6, width=14)


