


rm(list=ls())
library(tidyverse)
library(here)
library(MESS)

source(here("plant_trial_functions.R"))


load(here("data/analysis_dataset.Rdata"))

head(d)

#https://daniellakens.blogspot.com/2014/12/observed-power-and-what-to-do-if-your.html
Y
Y1 <- d %>% group_by(tr) %>%
  filter(!is.na(selftimesused)) %>%
  summarize(N=n(), mn=mean(selftimesused)) %>%
  mutate(outcome="selftimesused")
Y2 <- d %>% group_by(tr) %>%
  filter(!is.na(friendused)) %>%
  summarize(N=n(), mn=mean(friendused)) %>%
  mutate(outcome="friendused")


# power_prop_test(n = 698, p1 = 0.07, p2 = 0.05, sig.level = 0.05,
#                 power = NULL, ratio = 2, alternative = c("two.sided"))


power.prop.test(n = 698,
                p1 = ,         ## Proportion with a value of 1 in the absence of treatment
                p2 = .063,         ## Proportion with a value of 1 with the treatment 
                sig.level = 0.05,
                power = 0.8,
                alternative = "two.sided")


power.prop.test(n = 698,
                p1 = .063,         ## Proportion with a value of 1 in the absence of treatment
                p2 = .053,         ## Proportion with a value of 1 with the treatment 
                sig.level = 0.05,
                power = ,
                alternative = "two.sided")

power.prop.test(n = 698,
                p1 = .0544,         ## Proportion with a value of 1 in the absence of treatment
                p2 = .0315,         ## Proportion with a value of 1 with the treatment 
                sig.level = 0.05,
                power = ,
                alternative = "two.sided")




library(MBESS)
library(pwr)

nSims <- 1000 #number of simulated experiments
p <-numeric(nSims) #set up empty container for all simulated p-values
obs_pwr <-numeric(nSims) #set up empty container
t <-numeric(nSims) #set up empty container
d_all<-numeric(nSims) 
N<-698 #number of participants

for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = N, mean = .0315, sd = .2) #produce 100 simulated participants
  y<-rnorm(n = N, mean = .0544, sd = .2) #produce 100 simulated participants
  z<-t.test(x,y) #perform the t-test
  d<-smd(Mean.1= mean(x), Mean.2=mean(y), s.1=sd(x), s.2=sd(y), n.1=N, n.2=N, Unbiased=TRUE)
  d_all[i]<-d
  obs_pwr[i]<-pwr.t.test(n = N, d = d, sig.level = 0.05, type = "two.sample", alternative = "two.sided")$power
  p[i]<-z$p.value #get the p-value and store it
  t[i]<-t.test(x, y, alternative = "two.sided", paired = FALSE, var.equal = TRUE, conf.level = 0.95)$statistic
}

#Calculate power in simulation
cat ("The power (in %) is")
sum(p < 0.05)/nSims*100 


#now plot histograms of p-values, t-values, d, observed power
hist(p, main="Histogram of p-values", xlab=("Observed p-value"))
hist(t, main="Histogram of t-values", xlab=("Observed t-value"))
hist(d_all, main="Histogram of d", xlab=("Observed d"))
hist(obs_pwr, main="Histogram of observed power", xlab=("Observed power (%)"))
plot(p,obs_pwr)
abline(v = 0.05, lwd = 2, col = "red", lty = 2)
abline(h = 0.5, lwd = 2, col = "red", lty = 2)
