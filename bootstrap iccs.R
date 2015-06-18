# bootstrap for ICC's for social efficacy by hotspot type
require(plyr)
require(lme4)
require(MASS)
require(boot)
attach(for.icc67)


bootreg.cold <- lmer(pi.jk~1 + ed + marital + working + work.type +
                       age + age2 + race + eth + children + income + 
                       inc.ed + gender + victim + yrs.in.nbh + 
                       (1|SID) + (1|zip), data = for.icc67[SID.type == "Cold Spot",])

# built in commands - not sure how they work. refer to lme4 guide.

# function calculates icc
mySumm <- function(.) {
  denom = sum(as.data.frame(VarCorr(.))[1:3,4])
  c(as.data.frame(VarCorr(.))[1,4]/denom,
    as.data.frame(VarCorr(.))[2,4]/denom,
    as.data.frame(VarCorr(.))[3,4]/denom)
}

# mySumm <- function(.) {
#   c(sigma=sigma(.),sig01=unlist(VarCorr(.)))
# }

# command simulates nsim times and returns results of FUN
bootMer(bootreg.cold, FUN = mySumm, nsim = 10, type = "parametric") #works???

# command gives confidence intervals for coefficients
confint(bootreg.cold) 

cold.vcov <- as.data.frame(VarCorr(bootreg.cold))


bootreg.v <- lmer(pi.jk~1 + ed + marital + working + work.type +
                       age + age2 + race + eth + children + income + 
                       inc.ed + gender + victim + yrs.in.nbh + 
                       (1|SID) + (1|zip), data = for.icc67[SID.type == "Violent Spot",])

bootMer(bootreg.v, FUN = mySumm, nsim = 10, type = "parametric") 
confint(bootreg.v) 

viol.vcov <- as.data.frame(VarCorr(bootreg.v))


bootreg.all <- lmer(pi.jk~1 + ed + marital + working + work.type +
                      age + age2 + race + eth + children + income + 
                      inc.ed + gender + victim + yrs.in.nbh + 
                      (1|SID) + (1|zip), data = for.icc67)
bootMer(bootreg.all, FUN = mySumm, nsim = 10, type = "parametric") 
confint(bootreg.all) 


bootreg.cool <- lmer(pi.jk~1 + ed + marital + working + work.type +
                    age + age2 + race + eth + children + income + 
                    inc.ed + gender + victim + yrs.in.nbh + 
                    (1|SID) + (1|zip), data = for.icc67[SID.type == "Cool Spot",])

bootMer(bootreg.cool, FUN = mySumm, nsim = 10, type = "parametric") 

bootreg.drug <- lmer(pi.jk~1 + ed + marital + working + work.type +
                    age + age2 + race + eth + children + income + 
                    inc.ed + gender + victim + yrs.in.nbh + 
                    (1|SID) + (1|zip), data = for.icc67[SID.type == "Drug Spot",])

bootMer(bootreg.drug, FUN = mySumm, nsim = 10, type = "parametric") 

bootreg.combo <- lmer(pi.jk~1 + ed + marital + working + work.type +
                    age + age2 + race + eth + children + income + 
                    inc.ed + gender + victim + yrs.in.nbh + 
                    (1|SID) + (1|zip), data = for.icc67[SID.type == "Combined",])

bootMer(bootreg.combo, FUN = mySumm, nsim = 10, type = "parametric") 

# xi = rnorm(1, 0, cold.vcov[2,4])      # zip level variance
# eta = rnorm(1, 0, cold.vcov[1,4])     # sid level  variance
# epsilon = rnorm(1, 0, cold.vcov[3,4]) # person level variance
# 
# gamma = pi0 + xi
# beta0 = gamma + eta
# y = beta0 + coefs + epsilon