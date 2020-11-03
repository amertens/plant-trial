

#plot parameters
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")



#functions to analyze trial arms
plant.glm <- function(Yname, Ws=Wvars, data=d, family="gaussian"){
  
  Yvar = data %>% subset(., select = Yname)
  data <- data.frame(y=as.numeric(Yvar[,1]), tr=data$tr)
  #prescreen covariates
  if(!is.null(Ws)){
    prescreened_varnames<-washb_prescreen(Y=data[,1], Ws, family=family, print=T, pval = 0.2)
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
  print(summary(m1))
  
  
  
  #Get contrasts for all treatment arms
  l1 <- glht(m1, linfct = mcp(tr = "Tukey"))
  summary(l1)
  
  res_list <- summary(l1)$test
  res <- data.frame(est=res_list$coefficients, 
                    se=res_list$sigma,
                    Zval=res_list$tstat)
  res$p <- 2*pnorm(-abs(res$Zval))
  res$corrected.p  <- p.adjust(res$p, method = "BH")

  if(family=="binomial"){
    res$OR = exp(res$est) 
  }
  
  contrasts<-data.frame(str_split(rownames(res), " - ", n = 2, simplify = T))
  res <- data.frame(Y=Yname, intervention=contrasts[,1], reference=contrasts[,2], res)
  rownames(res) <- NULL
  
  return(res)
}              







#Run ordered logistic regression
polr_format<- function(Yvar, Ws=Wvars, df=d, ref){
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
  res$corrected.p  <- p.adjust(res$p, method = "BH")
  res$ref <- ref

  #Return just the treatment effects and outcome estimates
  ressub <-  res[c(1:(length(levels(df$tr))-1)),]
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





glm_mod_format<- function(d=d,Yvar, Wvars=Wvars, family="gaussian", control="control", contrasts=c("norms", "efficacy", "combined"), V){
  
  d$plant_mod=d[[V]]
  
  W <- d %>% subset(., select=Wvars)
  
  full_res <- NULL
  for(i in contrasts){
    #print(i)
    res1 <- plant_mod_glm(Y=d[,Yvar], tr=d$tr, W=data.frame(plant_mod=d$plant_mod, W), family=family, contrast=c(control,i), V="plant_mod",  pval=.2)     
    res2 <- plant_mod_glm(Y=d[,Yvar], tr=d$tr, W=data.frame(plant_mod=d$plant_mod, W), family=family, contrast=c(control,i),  pval=.2)  
    
    if(family=="polr"){
      int.p <- lrtest(res2$fit, res1$fit)$`Pr(>Chisq)`[2]
      res <- res1$res
      res <- res %>% 
        rename(subgroup=term,OR=estimate, se.est=std.error, OR.lb=conf.low, OR.ub=conf.high, P=p) %>%
        subset(., select = c(subgroup, OR, OR.lb, OR.ub, se.est, P)) 
      
      res$control <- control
      res$treatment <- i
      res$outcome <- Yvar
      res$V <- V
      res$int.p <- int.p
      
    }else{
      int.p <- lrtest(res2$glmModel, res1$glmModel)$`Pr(>Chisq)`[2]
      
      res <- res1$fit[2:4,]
      res <- data.frame(subgroup=rownames(res), res)
      rownames(res) <- NULL
      
      if(colnames(res)[2]=="OR"){
        
        res <- res %>% 
          rename(OR=OR, OR.lb =X2.5., OR.ub =X97.5., se.est=Std..Error, Zvalue=z.value, P=Pr...z..) %>%
          subset(., select = c(subgroup, OR, OR.lb, OR.ub, se.est, Zvalue, P))  
      }else{
        
        res <- res %>% 
          rename(est=Coef., se.est=Std..Error, Zvalue=z.value, P=Pr...z..) %>%
          subset(., select = c(subgroup, est, se.est, Zvalue, P)) 
      
      
      if(family=="binomial"){
        res <- data.frame(subgroup = res$subgroup,
                          OR = exp(res$est),
                          OR.lb = exp(res$est - 1.96 * res$se.est),
                          OR.ub = exp(res$est + 1.96 * res$se.est),
                          est=res$est,
                          se.est=res$se.est,
                          P=res$P)
        }
      }
      res$control <- control
      res$treatment <- i
      res$outcome <- Yvar
      res$V <- V
      #res$int.p <- c(as.character(round(int.p,4)), rep("",length(res$subgroup)))
      res$int.p <- int.p
    }
    
    
    full_res <- rbind(full_res, res)
  }
  
  
  return(full_res)
}


# 
# i=Vvars[1]
# 
# Yvar="friendtimesused"
# Wvars="W"
# d$W=rep(1, nrow(d))
# family="polr"
# V=i
# control="norms"
# contrasts=c("efficacy", "combined")
# d$plant_mod=d[[i]]
# W <- d %>% subset(., select=Wvars)
# 
# full_res <- NULL
# i = contrasts[1]
# 
# Y=d[,Yvar]
# tr=d$tr
# W=data.frame(plant_mod=d$plant_mod, W)
# contrast=c(control,i)
# 
# 
# pair = NULL
# forcedW = NULL
# V="plant_mod"
# pval = 0.2
# dfull<-dmat

plant_mod_glm <- function(Y, tr, pair = NULL, W = NULL, forcedW = NULL, V = NULL, 
                          contrast, family = "gaussian", pval = 0.2){
  require(sandwich)
  require(lmtest)
  options(scipen = 20)
  Subgroups = NULL
  
  id=1:length(Y)
 
  if (!is.null(W)) {
    W <- data.frame(W)
    if (sum("tr" %in% colnames(W)) > 0) {
      colnames(W)[which(colnames(W) == "tr")] <- "trW"
    }
  }
  
  if (!is.null(pair)) {
    if (!is.null(W)) {
      glmdat <- data.frame(id, Y, tr, pair, W)
    }
    else {
      glmdat <- data.frame(id, Y, tr, pair)
    }
    glmdat$tr <- factor(glmdat$tr, levels = contrast[1:2])
    glmdat$pair <- factor(glmdat$pair)
  }else {
    if (!is.null(W)) {
      glmdat <- data.frame(id, Y, tr, W)
    }
    else {
      glmdat <- data.frame(id, Y, tr)
    }
    glmdat$tr <- factor(glmdat$tr, levels = contrast[1:2])
  }
  
  glmdat <- subset(glmdat, tr == contrast[1] | tr == contrast[2])
  glmdat$tr <- factor(glmdat$tr, levels = contrast[1:2])
  glmdat$pair=NULL
  
  n.orig <- dim(glmdat)[1]
  rowdropped <- rep(1, nrow(glmdat))
  rowdropped[which(complete.cases(glmdat))] <- 0
  glmdat <- glmdat[complete.cases(glmdat), ]
  n.sub <- dim(glmdat)[1]

  
  if (!is.null(W)) {
    colnamesW <- names(W)
  }
  
  if (!is.null(W)) {
    if (!is.null(V)) {
      forcedW = c(V, forcedW)
    }
    if (!is.null(forcedW)) {
      screenW <- subset(glmdat, select = colnamesW)
      toexclude <- names(screenW) %in% forcedW
      if (length(which(toexclude == TRUE)) != length(forcedW)) 
        stop("A forcedW variable name is not a variable within the W data frame.")
      screenW = screenW[!toexclude]
      if (ncol(screenW) == 0) {
        screenW <- NULL
      }
  
      
    }
    else {
      screenW <- subset(glmdat, select = colnamesW)
    }
  }else {
    screenW <- NULL
  }
  if (!is.null(screenW)) {
      if(family == "polr"){
        suppressWarnings(Wscreen <- logit_prescreen(Y = glmdat$Y, 
                                                    Ws = screenW, pval = 0.2, print = F))      
      }else{
        suppressWarnings(Wscreen <- washb_prescreen(Y = glmdat$Y, 
                                                    Ws = screenW, family = family, pval = 0.2, print = F))
      }
    }else { 
      Wscreen = NULL
  }
  if (!is.null(pair)) {
    if (!is.null(forcedW)) {
      if (!is.null(Wscreen)) {
        dmat <- subset(glmdat, select = c("Y", 
                                          "tr", forcedW, Wscreen, "pair"))
      }
      else {
        dmat <- subset(glmdat, select = c("Y", 
                                          "tr", forcedW, "pair"))
      }
    }
    else {
      if (!is.null(Wscreen)) {
        dmat <- subset(glmdat, select = c("Y", 
                                          "tr", Wscreen, "pair"))
      }
      else {
        dmat <- subset(glmdat, select = c("Y", 
                                          "tr", "pair"))
      }
    }
  }else{
    if (!is.null(forcedW)) {
      if (!is.null(Wscreen)) {
        dmat <- subset(glmdat, select = c("Y", 
                                          "tr", forcedW, Wscreen))
      }
      else {
        dmat <- subset(glmdat, select = c("Y", 
                                          "tr", forcedW))
      }
    }
    else {
      if (!is.null(Wscreen)) {
        dmat <- subset(glmdat, select = c("Y", 
                                          "tr", Wscreen))
      }
      else {
        dmat <- subset(glmdat, select = c("Y", 
                                          "tr"))
      }
    }
  }
  
    
  
  if (family[1] == "binomial" | family[1] == "poisson" | 
      family[1] == "gaussian"){
   
 
      if (!is.null(V)) {
        colnames(dmat)[which(colnames(dmat) == V)] <- "V"
          Subgroups <- unique(dmat$tr:factor(dmat$V))
            suppressWarnings(fit <- glm(Y ~ tr * V + ., family = family, 
                                    data = dmat))
        vcovCL <- sandwichSE(dmat, fm = fit, cluster = glmdat$id)
        rfit <- coeftest(fit, vcovCL)
      }
      else {
        suppressWarnings(fit <- glm(Y ~ ., family = family, 
                                    data = dmat))
        vcovCL <- sandwichSE(dmat, fm = fit, cluster = glmdat$id)
        rfit <- coeftest(fit, vcovCL)
      }
      modelfit <- washb_glmFormat(glmModel = fit, rfit = rfit, 
                                  dmat = dmat, rowdropped = rowdropped, contrast = contrast, 
                                  pair = pair, vcovCL = vcovCL, family = family, 
                                  V = V, Subgroups = Subgroups,
                                  print=F, verbose=F)
      return(modelfit)
      
      
    }else{
      
      if (!is.null(V)) {
      colnames(dmat)[which(colnames(dmat) == V)] <- "V"
      #Subgroups <- unique(dmat$tr:factor(dmat$V))
      
      if(ncol(dmat)>5){
        vars <- colnames(dmat)[-c(1:3)]
        form <- formula(paste0("Y ~ tr + V + tr:V + ", paste(vars, collapse=" + ")))
        suppressWarnings(fit <- polr(form, data = dmat, Hess=T))
        res <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>% filter(coefficient_type=="coefficient") %>%
          subset(., select = - c(coefficient_type))
      }else{
        form <- formula(paste0("Y ~ tr + V + tr:V"))
        suppressWarnings(fit <- polr(form, data = dmat, Hess=T))
        res <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>% filter(coefficient_type=="coefficient") %>%
          subset(., select = - c(coefficient_type))
      }
        coef=coef(summary(fit))
        p <- pnorm(abs(coef[, "t value"]), lower.tail = FALSE) * 2
        p <- data.frame(term=names(p), p=p)
        
        res <- left_join(res, p, by="term")
        res <- res %>% filter(grepl("V",term)|grepl("tr",term))
        
      }else{
        if(ncol(dmat)>4){
          vars <- colnames(dmat)[-c(1:3)]
          form <- formula(paste0("Y ~ tr + ", paste(vars, collapse=" + ")))
          suppressWarnings(fit <- polr(form, data = dmat, Hess=T))
          res <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>% filter(coefficient_type=="coefficient") %>%
            subset(., select = - c(coefficient_type))
        }else{
          form <- formula(paste0("Y ~ tr"))
          suppressWarnings(fit <- polr(form, data = dmat, Hess=T))
          res<-NULL
          # res <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>% filter(coefficient_type=="coefficient") %>%
          #   subset(., select = - c(coefficient_type))
        }
        # coef=coef(summary(fit))
        # p <- pnorm(abs(coef[, "t value"]), lower.tail = FALSE) * 2
        # p <- data.frame(term=names(p), p=p)
        # 
        # res <- left_join(res, p, by="term")
      }
        
        return(list(res=res, fit=fit))
      
    }
}


  
