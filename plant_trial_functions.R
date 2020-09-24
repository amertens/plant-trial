

#plot parameters
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")



#functions to analyze trial arms
plant.glm <- function(Yname, Ws=Wvars, data=d, family="gaussian"){
  
  Yvar = data %>% subset(., select = Yname)
  data <- data.frame(y=as.numeric(Yvar[,1]), tr=data$tr)
  #prescreen covariates
  if(!is.null(Ws)){
    prescreened_varnames<-washb_prescreen(Y=data[,1], Ws, family=family, print=F, pval = 0.2)
    Ws <- Ws %>% subset(., select = prescreened_varnames)
    data <- cbind(data, Ws)
  }
  data <- data %>% filter(!is.na(y))
  if(length(nearZeroVar(data[,-c(1:2)])>0)){
    data[,-c(1:2)] <- data[,-(2+nearZeroVar(data[,-c(1:2)]))]
  }
  data <- droplevels(data)
  
  #fit model
  m1=glm(formula(y~tr + .), data=data, family=family)
  
  #Get contrasts for all treatment arms
  l1 <- glht(m1, linfct = mcp(tr = "Tukey"))
  summary(l1)
  
  res_list <- summary(l1)$test
  res <- data.frame(est=res_list$coefficients, 
                    se=res_list$sigma,
                    Zval=res_list$tstat)
  res$p <- 2*pnorm(-abs(res$Zval))
  res$corrected.p<- res$p*5
  res$corrected.p<-ifelse(res$corrected.p > 1, 1, res$corrected.p)
  
  if(family=="binomial"){
    res$RR = exp(res$est) 
  }
  
  contrasts<-data.frame(str_split(rownames(res), " - ", n = 2, simplify = T))
  res <- data.frame(Y=Yname, intervention=contrasts[,1], reference=contrasts[,2], res)
  rownames(res) <- NULL
  
  return(res)
}              







#Run ordered logistic regression
polr_format<- function(Yvar, Ws=Wvars, df=d){
  if(is.null(Ws)){
    Wsub <- NULL
  }else{
    Wsub<-droplevels(Ws)
  }
  Yvar = subset(df, select = Yvar)
  df <- data.frame(y=(Yvar[,1]), tr=df$tr)
  NYlevels <- length(unique(df[,1]))
  if(!is.null(Ws)){
    prescreened_varnames<-logit_prescreen(Y=df[,1], Ws, print=F)
    Ws <- Ws %>% subset(., select = prescreened_varnames)
    df <- cbind(df, Ws)
  }
  
  df <- df %>% filter(!is.na(y))
  if(length(nearZeroVar(df[,-c(1:2)])>0)){
    df[,-c(1:2)] <- df[,-(2+nearZeroVar(df[,-c(1:2)]))]
  }
  df <- droplevels(df)
  
  
  fit <- polr(formula(y~tr + .), data=df, Hess = T)
  res=coef(summary(fit))
  p <- pnorm(abs(res[, "t value"]), lower.tail = FALSE) * 2
  res <- cbind(res, p)
  res <- data.frame(res)
  
  #correct P-values
  res$corrected.p<- res$p*5
  
  #Return just the treatment effects and outcome estimates
  ressub <-  res[c(1:5, (nrow(res) - (NYlevels-3)):nrow(res)),]
  ressub <- data.frame(coef=rownames(ressub), ressub)
  rownames(ressub) <- NULL
  
  return(list(res=ressub, fit=fit))
}


logit_prescreen <- function (Y, Ws,  pval = 0.2, print = TRUE) 
{
  require(lmtest)
  
  if (pval > 0.99 | pval < 0) {
    stop("P-value threshold not set between 0 and 1.")
  }
  Ws <- as.data.frame(Ws)
  dat <- data.frame(Ws, Y)
  dat <- dat[complete.cases(dat), ]
  nW <- ncol(Ws)
  LRp <- matrix(rep(NA, nW), nrow = nW, ncol = 1)
  rownames(LRp) <- names(Ws)
  colnames(LRp) <- "P-value"
  for (i in 1:nW) {
    dat$W <- dat[, i]
    if (class(dat$W) == "factor" & dim(table(dat$W)) == 
        1) {
      fit1 <- fit0 <- polr(Y ~ 1, data = dat, family = family)
    }
    else {
      fit1 <- polr(Y ~ W, data = dat)
      fit0 <- polr(Y ~ 1, data = dat)
    }
    LRp[i] <- lrtest(fit1, fit0)[2, 5]
  }
  
  p20 <- ifelse(LRp < pval, 1, 0)
  if (print == TRUE) {
    cat("\nLikelihood Ratio Test P-values:\n")
    print(round(LRp, 5))
    if (sum(p20) > 0) {
      LRps <- matrix(LRp[p20 == 1, ], ncol = 1)
      rownames(LRps) <- names(Ws)[p20 == 1]
      colnames(LRps) <- "P-value"
      cat(paste("\n\nCovariates selected (P<", pval, "):\n", 
                sep = ""))
      print(LRps)
    }
    else {
      cat(paste("\nNo covariates were associated with the outcome at P<", 
                pval))
    }
  }
  return(names(Ws)[p20 == 1])
}



logit_plot <- function(fit, outcome, col=tableau10[1]){
  
  df=summary(lsmeans(fit, pairwise ~ tr, mode = "mean"),type="response")$lsmeans
  
  
  p<-ggplot(df, aes(tr, mean.class)) + geom_bar(stat="identity", position="dodge", fill=col, width=0.6) + 
    geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), width=0.15, position=position_dodge(width=0.9)) + 
    theme_few(base_size=18) + xlab("Treatment arm") + ylab("Mean outcome") + 
    ggtitle(paste0("Outcome: ", outcome)) 
    #coord_cartesian(ylim = c(1, 9)) + scale_y_continuous(breaks=1:9)
    
    
    return(p)
}







glm_mod_format<- function(Yvar, family="gaussian", control="Control", contrasts=c("Norms", "Moderate", "Extreme",  "Wolves= people", "Competing norms")){
  
  full_res <- NULL
  for(i in contrasts){
    res1 <- washb_glm(Y=d[,Yvar], tr=d$tr, W=data.frame(wolf_mod=d$wolf_mod, Wvars), V="wolf_mod", id=1:nrow(d), contrast = c(control, i), pval=.2, print=F)     
    res2 <- washb_glm(Y=d[,Yvar], tr=d$tr, W=data.frame(wolf_mod=d$wolf_mod, Wvars), id=1:nrow(d), contrast = c(control, i), pval=.2, print=F)     
    int.p <- lrtest(res2$glmModel, res1$glmModel)$`Pr(>Chisq)`[2]
    
    res <- res1$lincom
    colnames(res)[1] <- "subgroup"
    if(family=="binomial"){
      res <- data.frame(subgroup = res$subgroup, 
                        RR = exp(res$est), 
                        RR.lb = exp(res$est - 1.96 * res$se.est), 
                        RR.ub = exp(res$est + 1.96 * res$se.est), 
                        P=res$P) 
    }
    res$control <- control
    res$treatment <- i
    res$outcome <- Yvar
    res$int.p <- c(as.character(round(int.p,4)), "")
    full_res <- rbind(full_res, res)
  }
  
  
  return(full_res)
}




#function to analyze likert outcome across trial arms with non-parametric Mann-Whitney-Wilcoxon Test
wolf.wilcox <- function(Yname,  data=d){
  
  Yvar = subset(data, select = Yname)
  data <- data.frame(y=as.numeric(Yvar[,1]), tr=data$tr)
  data <- data %>% filter(!is.na(y))
  data <- droplevels(data)
  
  #get test statistics
  res <- NULL
  for(i in c("Norms", "Moderate", "Extreme", "Wolves= people", "Competing norms")){
    test <- wilcox.test(y ~ tr,
                        data=data[data$tr %in% c("Control",i),])
    res <- rbind(res, data.frame(Y=Yname, reference="Control", intervention=i, p=test$p.value))
  }
  for(i in c( "Moderate", "Extreme", "Wolves= people", "Competing norms")){
    test <- wilcox.test(y ~ tr,
                        data=data[data$tr %in% c("Norms",i),])
    res <- rbind(res, data.frame(Y=Yname, reference="Norms", intervention=i, p=test$p.value))
  }
  for(i in c( "Extreme", "Wolves= people", "Competing norms")){
    test <- wilcox.test(y ~ tr,
                        data=data[data$tr %in% c("Moderate",i),])
    res <- rbind(res, data.frame(Y=Yname, reference="Moderate", intervention=i, p=test$p.value))
  }
  for(i in c( "Wolves= people", "Competing norms")){
    test <- wilcox.test(y ~ tr,
                        data=data[data$tr %in% c("Extreme",i),])
    res <- rbind(res, data.frame(Y=Yname, reference="Extreme", intervention=i, p=test$p.value))
  }
    test <- wilcox.test(y ~ tr,
                        data=data[data$tr %in% c("Wolves= people","Competing norms"),])
    res <- rbind(res, data.frame(Y=Yname, reference="Wolves= people", intervention="Competing norms", p=test$p.value))
  
  res$corrected.p<- res$p*5
  res$corrected.p<-ifelse(res$corrected.p > 1, 1, res$corrected.p)
  return(res)
}              




  
