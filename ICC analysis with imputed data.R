require(lme4)

cov.list = "+ ed + marital + working + work.type +
            age + age2 + race + eth + children + income + 
            inc.ed + gender + victim + yrs.in.nbh"

output.iccs.imputed <- function(mids.obj, iteration = 1){
  # no covs
  data = complete(mids.obj, iteration)
  all = get.icc.fn(data, covariates = FALSE, zip.incl = TRUE)
  cold = get.icc.fn(data, covariates = FALSE, zip.incl = TRUE, block.type = "Cold Spot")
  cool = get.icc.fn(data, covariates = FALSE, zip.incl = TRUE, block.type = "Cool Spot")
  viol = get.icc.fn(data, covariates = FALSE, zip.incl = TRUE, block.type = "Violent Spot")
  drug  = get.icc.fn(data, covariates = FALSE, zip.incl = TRUE, block.type = "Drug Spot")
  combo = get.icc.fn(data, covariates = FALSE, zip.incl = TRUE, block.type = "Combined")
  
  vars = c(all[[1]][1,4], all[[1]][2,4],
           cold[[1]][1,4], cool[[1]][1,4], 
           viol[[1]][1,4],drug[[1]][1,4], combo[[1]][1,4])
  iccs = c(all[[2]],cold[[2]][1], cool[[2]][1], 
           viol[[2]][1],drug[[2]][1], combo[[2]][1])
  
  nocov.imp = data.frame(variance = vars, iccs = iccs)
  filename1 = paste("q6 q7 vars noNA imputed", iteration,  ".csv")
  write.csv(nocov.imp, file = filename1) 
  
  all.cov = get.icc.fn(data, covariates = cov.list, zip.incl = TRUE)
  cold.cov = get.icc.fn(data, covariates = cov.list, zip.incl = TRUE, block.type = "Cold Spot")
  cool.cov = get.icc.fn(data, covariates = cov.list, zip.incl = TRUE, block.type = "Cool Spot")
  drug.cov= get.icc.fn(data, covariates = cov.list, zip.incl = TRUE, block.type = "Drug Spot")
  viol.cov = get.icc.fn(data, covariates = cov.list, zip.incl = TRUE, block.type = "Violent Spot")
  combo.cov = get.icc.fn(data, covariates = cov.list, zip.incl = TRUE, block.type = "Combined")
  
  var.cov = c(all.cov[[1]][1,4], all.cov[[1]][2,4],
              cold.cov[[1]][1,4], cool.cov[[1]][1,4], 
              viol.cov[[1]][1,4],drug.cov[[1]][1,4], combo.cov[[1]][1,4])
  iccs.cov = c(all.cov[[2]],cold.cov[[2]][1], cool.cov[[2]][1], 
               viol.cov[[2]][1],drug.cov[[2]][1], combo.cov[[2]][1])
  
  cov.imp = data.frame(variance = var.cov, iccs = iccs.cov)
  filename2 = paste("q6 q7 vars w cov noNA imputed", iteration, ".csv")
  write.csv(cov.imp, file = filename2) 
  cov.imp
}

output.iccs.imputed(imp.pi, 1)
output.iccs.imputed(imp.pi, 2)
output.iccs.imputed(imp.pi, 3)
output.iccs.imputed(imp.pi, 4)
output.iccs.imputed(imp.pi, 5)
################# adding covs incrementally #################
cov1 = "+ age.noNA + age.noNA.sq + race + eth + gender"
cov2 = paste(cov1, "+ marital + children.noNA")
cov3 = paste(cov2, "+ ed")
cov4 = paste(cov3, "+ income + working + work.type")
cov5 = paste(cov4, "+ inc.ed")
cov6 = paste(cov5, "+ yrs.nbh.noNA + victim")

all1 = get.icc.fn(dat1, covariates = cov1, zip.incl = TRUE)
all2 = get.icc.fn(dat1, covariates = cov2, zip.incl = TRUE)
all3 = get.icc.fn(dat1, covariates = cov3, zip.incl = TRUE)
all4 = get.icc.fn(dat1, covariates = cov4, zip.incl = TRUE)
all5 = get.icc.fn(dat1, covariates = cov5, zip.incl = TRUE)
all6 = get.icc.fn(dat1, covariates = cov6, zip.incl = TRUE)


###################### bootstrap for dedup #########################
age.miss <- ifelse(is.na(for.mi$age), 1, 0)
age2.miss <- age.miss
yrs.miss <- ifelse(is.na(for.mi$yrs.in.nbh), 1, 0)
child.miss <- ifelse(is.na(for.mi$children), 1, 0)
q1.miss <- ifelse(is.na(for.mi$q1), 1, 0)
q2.miss <- ifelse(is.na(for.mi$q2.rev), 1, 0)
q3.miss <- ifelse(is.na(for.mi$q3), 1, 0)
q4.miss <- ifelse(is.na(for.mi$q4.rev), 1, 0)
q5.miss <- ifelse(is.na(for.mi$q5.rev), 1, 0)
q6.miss <- ifelse(is.na(for.mi$q6), 1, 0)
q7.miss <- ifelse(is.na(for.mi$q7), 1, 0)
q8.miss <- ifelse(is.na(for.mi$q8), 1, 0)
q9.miss <- ifelse(is.na(for.mi$q9), 1, 0)
q10.miss <- ifelse(is.na(for.mi$q10), 1, 0)
q11.miss <- ifelse(is.na(for.mi$q11), 1, 0)
q12.miss <- ifelse(is.na(for.mi$q12), 1, 0)
pi.miss <- ifelse(is.na(for.mi$pi.jk),1,0)
full.respondent <- ifelse(max(age.miss, yrs.miss, child.miss, q1.miss,
                               q2.miss, q3.miss, q4.miss, q5.miss, q6.miss,
                               q7.miss, q8.miss, q9.miss, q10.miss, q11.miss,
                               q12.miss, pi.miss) == 1, 0, 1)

for.boot <- data.frame(for.mi, pi.miss, q1.miss, q2.miss, q3.miss,
                       q4.miss, q5.miss, q6.miss, q7.miss, q8.miss,
                       q9.miss, q10.miss, q11.miss, q12.miss, age.miss,
                       age2.miss, child.miss, yrs.miss)
for.boot$age2 = for.boot$age^2
sample1.rows <- sample(c(1:length(pi.miss)), length(pi.miss), replace = T)
sample1 <- 

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

bootstrap.imputed <- function(iteration, mids.object){
  bootreg.imp <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                         age + age2 + race + eth + children + income + 
                         inc.ed + gender + victim + yrs.in.nbh + 
                         (1|SID) + (1|zip), 
                       data = complete(mids.object,iteration))
  b = bootMer(bootreg.imp1, FUN = mySumm3, nsim = 100, type = "parametric") 
  confint <- confint(bootreg.imp) 
  filename = paste("q6 q7 bootstrap confints no NA imp", iteration, ".csv")
  write.csv(confint, file = filename) 
}

bootstrap.imputed(1, imp.pi)
for(ii in 1:5){
  bootstrap.imputed(ii, imp.pi)
}

bootreg.imp1 <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                       age + age2 + race + eth + children + income + 
                       inc.ed + gender + victim + yrs.in.nbh + 
                       (1|SID) + (1|zip), 
                     data = complete(imp.pi,1))
b1 = bootMer(bootreg.imp1, FUN = mySumm3, nsim = 100, type = "parametric")

bootreg.imp2 <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                       age + age2 + race + eth + children + income + 
                       inc.ed + gender + victim + yrs.in.nbh + 
                       (1|SID) + (1|zip), 
                     data = complete(imp.pi,2))
b2 = bootMer(bootreg.imp2, FUN = mySumm3, nsim = 100, type = "parametric")
bootreg.imp3 <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                       age + age2 + race + eth + children + income + 
                       inc.ed + gender + victim + yrs.in.nbh + 
                       (1|SID) + (1|zip), 
                     data = complete(imp.pi,3))
b3 = bootMer(bootreg.imp3, FUN = mySumm3, nsim = 100, type = "parametric")
bootreg.imp4 <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                       age + age2 + race + eth + children + income + 
                       inc.ed + gender + victim + yrs.in.nbh + 
                       (1|SID) + (1|zip), 
                     data = complete(imp.pi,4))
b4 = bootMer(bootreg.imp4, FUN = mySumm3, nsim = 100, type = "parametric")
bootreg.imp5 <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                       age + age2 + race + eth + children + income + 
                       inc.ed + gender + victim + yrs.in.nbh + 
                       (1|SID) + (1|zip), 
                     data = complete(imp.pi,5))
b5 = bootMer(bootreg.imp5, FUN = mySumm3, nsim = 100, type = "parametric")

booted <- rbind(b1$t, b2$t, b3$t, b4$t, b5$t)
mean(booted[,1])
var(booted[,1])
booted.df <- data.frame(SID = booted[,1], zip = booted[,2])
write.csv(booted.df, file = "full_bootstrapped_imputed.csv")
