require(plyr)
require(lme4)
attach(for.icc67.noNA)

######################## look at combined level with covariates #######################
covs.both.67<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                     age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                     inc.ed + gender + victim + yrs.nbh.noNA + 
                     (1|SID) + (1|zip))
df.covs.67.both <- as.data.frame(VarCorr(covs.both.67))
icc.covs.both.67.sid <- df.covs.67.both[1,4]/(df.covs.67.both[1,4]+df.covs.67.both[2,4]+
                                                df.covs.67.both[3,4]) #0.1084244
icc.covs.both.67.zip <- df.covs.67.both[2,4]/(df.covs.67.both[1,4]+df.covs.67.both[2,4]+
                                                df.covs.67.both[3,4]) #0

write.csv(cbind(summary(covs.both.67)$coef[,1], summary(covs.both.67)$coef[,3]),
          file = "q6 q7 coefs noNA.csv") 


############################ with covs by sid type 6&7 #############################

icc.covs.67.SID.lev = NULL
icc.covs.67.zip.lev = NULL
df.67.SID.lev.covs = NULL
for(type in unique(SID.type)){
  covs.both.67.lev <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                             age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
                             inc.ed + gender + victim + yrs.nbh.noNA + 
                             (1|SID) + (1|zip), data = for.icc67.noNA[SID.type == type,])
  summary(covs.both.67.lev)
  df.covs.67.lev <- as.data.frame(VarCorr(covs.both.67.lev))
  icc.covs.67.SID.lev[type] <- df.covs.67.lev[1,4]/(df.covs.67.lev[1,4]+
                                                      df.covs.67.lev[2,4]+df.covs.67.lev[3,4]) 
  icc.covs.67.zip.lev[type] <- df.covs.67.lev[2,4]/(df.covs.67.lev[1,4]+
                                                      df.covs.67.lev[2,4]+df.covs.67.lev[3,4])
  df.67.SID.lev.covs <- c(df.67.SID.lev.covs, df.covs.67.lev[1,4])
}


var.icc.67 = data.frame(vars = df.67.SID.lev.covs, iccs = icc.covs.67.SID.lev)
overalls.67 = data.frame(vars = c(df.covs.67.both[2,4], df.covs.67.both[1,4]),
                         iccs = c(icc.covs.both.67.zip, icc.covs.both.67.sid))
output.unordered = rbind(overalls.67, var.icc.67)
output = rbind(output.unordered[1:2,],
               output.unordered[which(labels(output.unordered)[[1]] == "Cold Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Cool Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Violent Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Drug Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Combined"),])
write.csv(output, file = "q6 q7 vars noNA.csv")

######################## look at combined level no covariates #######################
basic.both.67<-lmer(pi.jk~1 + (1|SID) + (1|zip))
df.basic.67.both <- as.data.frame(VarCorr(basic.both.67))
icc.basic.both.67.sid <- df.basic.67.both[1,4]/(df.basic.67.both[1,4]+df.basic.67.both[2,4]+
                                                df.basic.67.both[3,4]) #0.1078097
icc.basic.both.67.zip <- df.basic.67.both[2,4]/(df.basic.67.both[1,4]+df.basic.67.both[2,4]+
                                                df.basic.67.both[3,4]) #2.613472e-14


############################ no covs by sid type 6&7  #############################

icc.basic.67.SID.lev = NULL
icc.bsic.67.zip.lev = NULL
df.67.SID.lev.basic = NULL
for(type in unique(SID.type)){
  basic.both.67.lev <- lmer(pi.jk~1 + (1|SID) + (1|zip), data = for.icc67.noNA[SID.type == type,])
  summary(basic.both.67.lev)
  df.basic.67.lev <- as.data.frame(VarCorr(basic.both.67.lev))
  icc.basic.67.SID.lev[type] <- df.basic.67.lev[1,4]/(df.basic.67.lev[1,4]+
                                                      df.basic.67.lev[2,4]+df.basic.67.lev[3,4]) 
  icc.basic.67.zip.lev[type] <- df.basic.67.lev[2,4]/(df.basic.67.lev[1,4]+
                                                      df.basic.67.lev[2,4]+df.basic.67.lev[3,4])
  df.67.SID.lev.basic <- c(df.67.SID.lev.basic, df.basic.67.lev[1,4])
}


# write.csv(cbind(summary(basic.both.67.lev)$coef[,1], summary(basic.both.67.lev)$coef[,3]),
#           file = "q6 q7 coefs noNA nocovs.csv") 

var.icc.67 = data.frame(vars = df.67.SID.lev.basic, iccs = icc.basic.67.SID.lev)
overalls.67 = data.frame(vars = c(df.basic.67.both[2,4], df.basic.67.both[1,4]),
                         iccs = c(icc.basic.both.67.zip, icc.basic.both.67.sid))
output.unordered = rbind(overalls.67, var.icc.67)
output = rbind(output.unordered[1:2,],
               output.unordered[which(labels(output.unordered)[[1]] == "Cold Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Cool Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Violent Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Drug Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Combined"),])
write.csv(output, file = "q6 q7 vars noNA no covs.csv")