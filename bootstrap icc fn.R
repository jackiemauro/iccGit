# bootstrap estimator fn 
# runs regression based on parameters
# finds bootstrap standard errors for icc's


# usual covariates
cov.list = "+ ed + marital + working + work.type +
            age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
inc.ed + gender + victim + yrs.nbh.noNA"


bootstrap.icc <- function(data, 
                       covariates = FALSE, 
                       zip.incl = FALSE,
                       block.type = FALSE,
                       n = 100){
  
  # get dataset with our without block type
  if(block.type != FALSE) {d = data[data$SID.type == block.type,]} 
  else {d = data}
  
  # pick regression
  reg.list = "1 + (1|SID)"
  if(covariates != FALSE){reg.list = paste(reg.list, covariates)}
  if(zip.incl == TRUE) {
    reg.list = paste(reg.list, " + (1|zip)")
    reg = eval(parse(text = paste("lmer(pi.jk ~ ", reg.list, ", data = d)")))
    mySumm <- function(.) {
      denom = sum(as.data.frame(VarCorr(.))[1:3,4])
      sid.icc = as.data.frame(VarCorr(.))[1,4]/denom
      zip.icc = as.data.frame(VarCorr(.))[2,4]/denom
      c(sid.icc, zip.icc)
    }
  }
  else{
    reg = eval(parse(text = paste("lmer(pi.jk ~ ", reg.list, ")")))
    mySumm <- function(.) {
      denom = sum(as.data.frame(VarCorr(.))[1:2,4])
      as.data.frame(VarCorr(.))[1,4]/denom
    }
  }
  
  # bootstrap errors
  require(MASS)
  require(boot)
  bootMer(reg, FUN = mySumm, nsim = n, type = "parametric") 
}


bootreg.all <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                      age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                      inc.ed + gender + victim + yrs.nbh.noNA + 
                      (1|SID) + (1|zip), data = for.icc67.noNA)
bootMer(bootreg.all, FUN = mySumm, nsim = 100, type = "parametric") 
confint <- confint(bootreg.all) 
write.csv(confint, file = "q6 q7 bootstrap confints noNA.csv") 