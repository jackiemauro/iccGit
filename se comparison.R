# compare parametric standard errors and bootstrap standard errors
# parametric SE's come from Yadav & Agarwal (2012)
# currently only available for no covariate regression
# need to figure out extension to model with covariates

require(plyr)
require(lme4)
require(MASS)
require(boot)
attach(for.icc67.noNA)

bootreg <- lmer(pi.jk~1 + (1|SID), data = for.icc67.noNA)
a <- as.data.frame(VarCorr(bootreg))
iccSIDonly <- a[1,4]/(a[1,4]+a[2,4])

############################ run bootstrap #####################################
# function calculates icc for 2 level model
mySumm <- function(.) {
  denom = sum(as.data.frame(VarCorr(.))[1:2,4])
  as.data.frame(VarCorr(.))[1,4]/denom
}

# command simulates nsim times and returns results of FUN
b <- bootMer(bootreg, FUN = mySumm, nsim = 1000, type = "parametric") 

# 0.008232202


########################### run parametric #######################################

Yadav <- function(data, cluster, response){
  attach(data)
  n = length(unique(cluster))                     # number of clusters
  cluster.index = unique(cluster)     
  m = c(rep(NA, n))                               # number of rows in each cluster
  for(ii in 1:n){m[ii] = length(which(cluster == cluster.index[ii]))}
  m0 = sum(m)                                     # total number of rows                    
  mbar = (m0-sum(m^2)/m0)/(n-1)
  
  ybar = NULL                                     # mean pi in each cluster
  ybar0 = mean(response, na.rm = TRUE)               # overall mean pi
  for(ii in 1:n){ybar[ii] = mean(response[which(cluster == cluster.index[ii])], na.rm = TRUE)}
  
  SSA = sum((m*(ybar - ybar0)^2)) 
  
  pre = NULL
  for(ii in 1:m0){
    pre[ii] = (response[ii] - ybar[which(cluster.index == cluster[ii])])^2
    SSE = sum(pre, na.rm = TRUE)
  }
  
  MSA = SSA/(n-1)
  MSE = SSE/(m0-n)
  sig.A = (MSA - MSE)/mbar
  
  rho.hat = (MSA - MSE)/(MSA + (mbar - 1)*MSE)
  
  eta.A = n-1
  eta.E = sum((m-1))
  Var.rho = 2 * ((MSE/(MSA^2 + MSE^2))^4)*(MSA^2 + MSE^2/mbar)*(1/eta.A + 1/eta.E)
  detach(data)
  return(c(rho.hat, Var.rho))
}

Yadav(data=for.icc67.noNA, cluster = SID, response = pi.jk)
 
# 0.0001185246
# much smaller than bootstrap, and good estimate of rho (0.1071452981)

Yadav(data = for.icc67.noNA.dedup, cluster = SID, response = pi.jk)
# ICC est: 0.0211
# Var(ICC): 0.0313