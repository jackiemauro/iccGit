# bootstrap for ICC's for social efficacy by hotspot type
require(plyr)
require(lme4)
require(MASS)
require(boot)
attach(for.icc67.noNA)

# function calculates icc for 3 level model
mySumm <- function(.) {
  denom = sum(as.data.frame(VarCorr(.))[1:3,4])
  c(as.data.frame(VarCorr(.))[1,4]/denom,
    as.data.frame(VarCorr(.))[2,4]/denom,
    as.data.frame(VarCorr(.))[3,4]/denom)
}

bootreg.cold <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                       age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                       inc.ed + gender + victim + yrs.nbh.noNA + 
                       (1|SID) + (1|zip), data = for.icc67.noNA[SID.type == "Cold Spot",])


# command simulates nsim times and returns results of FUN
b.cold <- bootMer(bootreg.cold, FUN = mySumm, nsim = 100, type = "parametric") 


bootreg.v <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                    age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                    inc.ed + gender + victim + yrs.nbh.noNA +
                    (1|SID) + (1|zip), data = for.icc67.noNA[SID.type == "Violent Spot",])

b.viol <- bootMer(bootreg.v, FUN = mySumm, nsim = 100, type = "parametric") 

bootreg.cool <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                       age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                       inc.ed + gender + victim + yrs.nbh.noNA + 
                       (1|SID) + (1|zip), data = for.icc67.noNA[SID.type == "Cool Spot",])

b.cool <- bootMer(bootreg.cool, FUN = mySumm, nsim = 100, type = "parametric") 

bootreg.drug <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                       age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                       inc.ed + gender + victim + yrs.nbh.noNA +
                       (1|SID) + (1|zip), data = for.icc67.noNA[SID.type == "Drug Spot",])

b.drug <- bootMer(bootreg.drug, FUN = mySumm, nsim = 100, type = "parametric") 

bootreg.combo <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                        age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                        inc.ed + gender + victim + yrs.nbh.noNA + 
                        (1|SID) + (1|zip), data = for.icc67.noNA[SID.type == "Combined",])

b.combo <- bootMer(bootreg.combo, FUN = mySumm, nsim = 100, type = "parametric") 


bootreg.all <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                      age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                      inc.ed + gender + victim + yrs.nbh.noNA + 
                      (1|SID) + (1|zip), data = for.icc67.noNA)
bootMer(bootreg.all, FUN = mySumm, nsim = 100, type = "parametric") 
confint <- confint(bootreg.all) 
write.csv(confint, file = "q6 q7 bootstrap confints noNA.csv") 




# xi = rnorm(1, 0, cold.vcov[2,4])      # zip level variance
# eta = rnorm(1, 0, cold.vcov[1,4])     # sid level  variance
# epsilon = rnorm(1, 0, cold.vcov[3,4]) # person level variance
# 
# gamma = pi0 + xi
# beta0 = gamma + eta
# y = beta0 + coefs + epsilon