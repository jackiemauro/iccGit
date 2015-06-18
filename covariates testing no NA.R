# Re runs covariates analysis on the file with no NA's #

covariates.noNA <- for.icc67.noNA[,c(17,23:36)]

attach(for.icc67.noNA)
require(plyr)
require(lme4)

covs.both.67.type.noNA<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                          age.noNA+ age.noNA.sq + race + eth + children.noNA + income + 
                          inc.ed + gender + victim + yrs.nbh.noNA + SID.type + 
                          (1|SID) + (1|zip))
#keeps all observations but drops 2 columns--more collinearity with NA's taken out


# suppressing intercept, all covariates are significant
not.sig = list()
not.sigF = list()
for(ii in 1:length(covariates)){
  res <- summary(lm(pi.jk ~ covariates[,ii]  - 1))
  temp <- which(res$coefficients[,4] >= 0.05)
  Fpval <- 1 - pf(res$fstatistic[1], res$fstatistic[2], res$fstatistic[3]) 
  tempF <- which(Fpval >= 0.05)
  not.sig[[ii]] = c(temp)
  not.sigF[[ii]] = c(tempF)
}
which(not.sigF > 0)

#not suppressing intercept
not.sig2 = list()
not.sigF2 = list()
for(ii in 1:length(covariates)){
  res <- summary(lm(pi.jk ~ covariates[,ii]))
  temp <- which(res$coefficients[,4] >= 0.05)
  not.sig2[[ii]] = c(temp)
  Fpval <- 1 - pf(res$fstatistic[1], res$fstatistic[2], res$fstatistic[3]) 
  tempF <- which(Fpval >= 0.05)
  not.sig[[ii]] = c(temp)
  not.sigF2[[ii]] = c(tempF)
}

which(not.sigF2 > 0)
# ethnicity and victim not overall significant

# adding incrementally
unchanging.demogs.lmer <- lmer(pi.jk~1 + age.noNA+ age.noNA.sq + race + eth + gender
                               + SID.type + (1|SID) + (1|zip))
summary(unchanging.demogs.lmer)
print.icc(unchanging.demogs.lmer)
#different scales warning (assuming its age)


with.fam.lmer <- lmer(pi.jk~1 + age.noNA + age.noNA.sq + race + eth + gender
                      + marital + children.noNA 
                      + SID.type + (1|SID) + (1|zip))
print.icc(with.fam.lmer)

with.ed.lmer <- lmer(pi.jk~1 + age.noNA+ age.noNA.sq + race + eth + gender
                     + marital + children.noNA 
                     + ed 
                     + SID.type + (1|SID) + (1|zip))
print.icc(with.ed.lmer)


with.prof.lmer <- lmer(pi.jk~1 + age.noNA+ age.noNA.sq + race + eth + gender
                       + marital + children.noNA 
                       + ed 
                       + income + working + work.type
                       + SID.type + (1|SID) + (1|zip))
print.icc(with.prof.lmer)


with.workbyed.lmer <- lmer(pi.jk~1 + age.noNA+ age.noNA.sq + race + eth + gender
                           + marital + children.noNA 
                           + ed 
                           + income + working + work.type
                           + inc.ed
                           + SID.type + (1|SID) + (1|zip))
#becomes rank deficient with interaction
print.icc(with.workbyed.lmer)

full.lmer <- lmer(pi.jk~1 + age.noNA+ age.noNA.sq + race + eth + gender
                  + marital + children.noNA 
                  + ed 
                  + income + working + work.type
                  + inc.ed
                  + yrs.nbh.noNA + victim
                  + SID.type + (1|SID) + (1|zip))
print.icc(full.lmer)

# professional type bumps icc up a lot, other things change zip a little
# but not much. 
