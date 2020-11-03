

rm(list=ls())
library(tidyverse)
library(cowplot)
library(DescTools)
tableau10 <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
theme_set(theme_bw())

load(here("data/analysis_dataset.Rdata"))

BinomCI(sum(d$selftimesused), nrow(d))

res1 <- d %>% group_by(tr) %>%
  do(as.data.frame(BinomCI(sum(.$selftimesused), nrow(.)))) %>%
  mutate(Outcome="Self voucher used")
res2 <- d %>% group_by(tr) %>%
  do(as.data.frame(BinomCI(sum(.$friendused), nrow(.)))) %>%
  mutate(Outcome="Friend-and-neighbor voucher used")


res <- bind_rows(res1, res2)
res$est <- res$est * 100
res$lwr.ci <- res$lwr.ci * 100
res$upr.ci <- res$upr.ci * 100


res$tr <- str_to_title(res$tr)
res$tr <- factor(res$tr, levels =c("Control","Norms","Efficacy","Combined"))

p <- ggplot(res, aes(x=tr, y=est)) + 
  geom_point(aes(color=tr, shape=tr), size = 3) +
  geom_linerange(aes(ymin=lwr.ci, ymax=upr.ci),
                 alpha=0.5, color="black", size = 1) +
  facet_wrap(~Outcome, scales="fixed") +
  labs(y ="Proportion used" , x = "Intervention message") +
  scale_colour_manual(values=rep("black",4)) + 
  scale_shape_manual(values=c(19,17,18,15)) +
  ggtitle("") + theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)) 
p

ggsave(p, file = here("figures/primary_prevalence.png"), height=4, width=8)




