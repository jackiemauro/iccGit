# Reruns ICC analyses with a dataset arranged such that 
# each row represents a unique household ID


for.icc67.noNA.dedup = unique(for.icc67.noNA[,c(2, 15:length(for.icc67.noNA))]) 
require(lme4)


cov.list = "+ ed + marital + working + work.type +
            age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
            inc.ed + gender + victim + yrs.nbh.noNA"

# no covs
all = get.icc.fn(for.icc67.noNA.dedup, covariates = FALSE, zip.incl = TRUE)
cold = get.icc.fn(for.icc67.noNA.dedup, covariates = FALSE, zip.incl = TRUE, block.type = "Cold Spot")
cool = get.icc.fn(for.icc67.noNA.dedup, covariates = FALSE, zip.incl = TRUE, block.type = "Cool Spot")
viol = get.icc.fn(for.icc67.noNA.dedup, covariates = FALSE, zip.incl = TRUE, block.type = "Violent Spot")
drug  = get.icc.fn(for.icc67.noNA.dedup, covariates = FALSE, zip.incl = TRUE, block.type = "Drug Spot")
combo = get.icc.fn(for.icc67.noNA.dedup, covariates = FALSE, zip.incl = TRUE, block.type = "Combined")

var = c(all[[1]][1,4], all[[1]][2,4],
        cold[[1]][1,4], cool[[1]][1,4], 
        viol[[1]][1,4],drug[[1]][1,4], combo[[1]][1,4])
iccs = c(all[[2]],cold[[2]][1], cool[[2]][1], 
         viol[[2]][1],drug[[2]][1], combo[[2]][1])

nocov.out = data.frame(variance = var, iccs = iccs)
write.csv(nocov.out, file = "q6 q7 vars noNA dedup.csv") 

all.cov = get.icc.fn(for.icc67.noNA.dedup, covariates = cov.list, zip.incl = TRUE)
cold.cov = get.icc.fn(for.icc67.noNA.dedup, covariates = cov.list, zip.incl = TRUE, block.type = "Cold Spot")
cool.cov = get.icc.fn(for.icc67.noNA.dedup, covariates = cov.list, zip.incl = TRUE, block.type = "Cool Spot")
drug.cov= get.icc.fn(for.icc67.noNA.dedup, covariates = cov.list, zip.incl = TRUE, block.type = "Drug Spot")
viol.cov = get.icc.fn(for.icc67.noNA.dedup, covariates = cov.list, zip.incl = TRUE, block.type = "Violent Spot")
combo.cov = get.icc.fn(for.icc67.noNA.dedup, covariates = cov.list, zip.incl = TRUE, block.type = "Combined")

var.cov = c(all.cov[[1]][1,4], all.cov[[1]][2,4],
        cold.cov[[1]][1,4], cool.cov[[1]][1,4], 
        viol.cov[[1]][1,4],drug.cov[[1]][1,4], combo.cov[[1]][1,4])
iccs.cov = c(all.cov[[2]],cold.cov[[2]][1], cool.cov[[2]][1], 
             viol.cov[[2]][1],drug.cov[[2]][1], combo.cov[[2]][1])

cov.out = data.frame(variance = var.cov, iccs = iccs.cov)
write.csv(cov.out, file = "q6 q7 vars w cov noNA dedup.csv") 
#these are much much lower!

################# adding covs incrementally #################
cov1 = "+ age.noNA + age.noNA.sq + race + eth + gender"
cov2 = paste(cov1, "+ marital + children.noNA")
cov3 = paste(cov2, "+ ed")
cov4 = paste(cov3, "+ income + working + work.type")
cov5 = paste(cov4, "+ inc.ed")
cov6 = paste(cov5, "+ yrs.nbh.noNA + victim")

all1 = get.icc.fn(for.icc67.noNA.dedup, covariates = cov1, zip.incl = TRUE)
all2 = get.icc.fn(for.icc67.noNA.dedup, covariates = cov2, zip.incl = TRUE)
all3 = get.icc.fn(for.icc67.noNA.dedup, covariates = cov3, zip.incl = TRUE)
all4 = get.icc.fn(for.icc67.noNA.dedup, covariates = cov4, zip.incl = TRUE)
all5 = get.icc.fn(for.icc67.noNA.dedup, covariates = cov5, zip.incl = TRUE)
all6 = get.icc.fn(for.icc67.noNA.dedup, covariates = cov6, zip.incl = TRUE)


###################### bootstrap for dedup #########################
bootreg.dedup <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                      age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                      inc.ed + gender + victim + yrs.nbh.noNA + 
                      (1|SID) + (1|zip), data = for.icc67.noNA.dedup)

write.csv(cbind(summary(bootreg.dedup)$coef[,1], summary(bootreg.dedup)$coef[,3]),
          file = "q6 q7 coefs noNA dedup.csv")

mySumm2 <- function(.) {
  denom = sum(as.data.frame(VarCorr(.))[1:2,4])
  as.data.frame(VarCorr(.))[1,4]/denom
}

mySumm3 <- function(.) {
  denom = sum(as.data.frame(VarCorr(.))[1:3,4])
  sid.icc = as.data.frame(VarCorr(.))[1,4]/denom
  zip.icc = as.data.frame(VarCorr(.))[2,4]/denom
  c(sid.icc, zip.icc)
}

b = bootMer(bootreg.dedup, FUN = mySumm3, nsim = 100, type = "parametric") 
mean1 = mean(as.data.frame(b))[,1])
mean2 = mean(as.data.frame(b))[,2])
sd1 = sd(as.data.frame(b))[,1])
sd2 = sd(as.data.frame(b))[,2])
confint <- confint(bootreg.dedup) 
write.csv(confint, file = "q6 q7 bootstrap confints noNA dedup.csv") 