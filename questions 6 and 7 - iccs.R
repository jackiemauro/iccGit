setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
source("create covariates dataset.R")
source("questions 6 and 7 - pi regressions.R")

#################### dataset creation ##############
# merge datasets and run ICC's

# make sure all subjects are in both sets
test<-strsplit(names(pi.67),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people<-data.frame(person = has.dummies, pi.jk = pi.67)

test<-strsplit(names(pi6),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people6<-data.frame(person = has.dummies, pi.jk = pi6)

test<-strsplit(names(pi7),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people7<-data.frame(person = has.dummies, pi.jk = pi7)

# merge dummies, pi's and geographies
for.icc67 <- join_all(list(for.pi, dummy.people, covs.set))
for.icc6 <- join_all(list(for.pi6, dummy.people6, covs.set))
for.icc7 <- join_all(list(for.pi7, dummy.people7, covs.set))

detach(for.pi)


########################### get ICCs ###############################
#                                                                  #
#                     start w all combined                         #
#                                                                  #
####################################################################

################## basic ICCs 6 & 7 ################################
attach(for.icc67)
#SID level
basic.SID.67 <- lmer(for.icc67$pi.jk~1 + (1|SID))
summary(basic.SID.67)
df.basic.67.SID <- as.data.frame(VarCorr(basic.SID.67))
icc.basic.67.SID <- df.basic.67.SID[1,4]/(df.basic.67.SID[1,4]+df.basic.67.SID[2,4]) #0.1078097

# # block grp level
# lmer.block.grp<-lmer(pi.jk~1 + (1|block.grp))
# summary(lmer.block.grp)
# df.block<-as.data.frame(VarCorr(lmer.block.grp))
# icc.block<-df.block[1,4]/(df.block[1,4]+df.block[2,4]) #1.614484e-13
# 
# # tract level
# lmer.tract<-lmer(pi.jk~1 + (1|tract))
# summary(lmer.tract)
# df.tract<-as.data.frame(VarCorr(lmer.tract))
# icc.tract<-df.tract[1,4]/(df.tract[1,4]+df.tract[2,4]) #0.04406416

# zip level
basic.zip.67<-lmer(pi.jk~1 + (1|zip))
summary(basic.zip.67)
df.basic.67.zip<-as.data.frame(VarCorr(basic.zip.67))
icc.basic.67.zip<-df.basic.67.zip[1,4]/(df.basic.67.zip[1,4]+df.basic.67.zip[2,4]) #0.01316786

# sid + zip level
basic.both.67<-lmer(pi.jk~1 + (1|zip) + (1|SID))
summary(basic.both.67)
df.basic.both.67<-as.data.frame(VarCorr(basic.both.67))
icc.basic.both.67.zip<-df.basic.both.67[2,4]/(df.basic.both.67[1,4]+df.basic.both.67[2,4]+df.basic.both.67[3,4]) #2.613472e-14
icc.basic.both.67.sid<-df.basic.both.67[1,4]/(df.basic.both.67[1,4]+df.basic.both.67[2,4]+df.basic.both.67[3,4]) #0.1078097


############################ no covs by sid type 6&7 #######################

icc.basic.67.SID.lev = NULL
icc.basic.67.zip.lev = NULL
for(type in unique(SID.type)){
  #SID level
  basic.both.67.lev <- lmer(pi.jk~1 + (1|SID) + (1|zip), data = for.icc67[SID.type == type,])
  summary(basic.both.67.lev)
  df.basic.67.lev <- as.data.frame(VarCorr(basic.both.67.lev))
  icc.basic.67.SID.lev[type] <- df.basic.67.lev[1,4]/(df.basic.67.lev[1,4]+df.basic.67.lev[2,4]+df.basic.67.lev[3,4]) 
  icc.basic.67.zip.lev[type] <- df.basic.67.lev[2,4]/(df.basic.67.lev[1,4]+df.basic.67.lev[2,4]+df.basic.67.lev[3,4])
}
# drug and combined have highest icc

Spot = factor(names(icc.basic.67.SID.lev), 
              levels = c("Cold Spot", "Cool Spot", "Drug Spot", "Violent Spot", "Combined"), 
              ordered = TRUE)

bar.basic.67 = data.frame(Spot = Spot,
                    SID = icc.basic.67.SID.lev, ZIP = icc.basic.67.zip.lev)

library(plyr)
mm <- ddply(bar.basic.67, "Spot", summarise, test = mean(SID))
ggplot(mm, aes(x = factor(Spot), y = test)) + 
  geom_bar(stat = "identity")+ 
  xlab("Hotspot type") + 
  geom_hline(aes(yintercept = icc.basic.both.67.sid), col = "red") + 
  geom_hline(aes(yintercept = icc.basic.both.67.zip), col = "blue") +
  ylab("SID level ICC") + 
  ggtitle("ICC for combined ZIP and SID")

############################ with covs 6&7 #######################
# says rank deficient after adding new vars

#SID level
covs.SID.67 <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                         age + + age2 + race + eth + children + income + 
                         inc.ed + gender + victim + yrs.in.nbh + 
                         (1|SID))
df.covs.67.SID <- as.data.frame(VarCorr(covs.SID.67))
icc.covs.67.SID <- df.covs.67.SID[1,4]/(df.covs.67.SID[1,4]+df.covs.67.SID[2,4]) #0.2624956

# zip level
covs.zip.67<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                     age + + age2 + race + eth + children + income + 
                     inc.ed + gender + victim + yrs.in.nbh + 
                     (1|zip))
df.covs.67.zip <- as.data.frame(VarCorr(covs.zip.67))
icc.covs.67.zip <- df.covs.67.zip[1,4]/(df.covs.67.zip[1,4]+df.covs.67.zip[2,4]) #0.01986521

#look at combined level
covs.both.67<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                      age + + age2 + race + eth + children + income + 
                      inc.ed + gender + victim + yrs.in.nbh + 
                      (1|SID) + (1|zip))
df.covs.67.both <- as.data.frame(VarCorr(covs.both.67))
icc.covs.both.67.sid <- df.covs.67.both[1,4]/(df.covs.67.both[1,4]+df.covs.67.both[2,4]+
                                            df.covs.67.both[3,4]) #0.2584314
icc.covs.both.67.zip <- df.covs.67.both[2,4]/(df.covs.67.both[1,4]+df.covs.67.both[2,4]+
                                            df.covs.67.both[3,4]) #0.004694903


############################ with covs by sid type 6&7 #######################

icc.covs.67.SID.lev = NULL
icc.covs.67.zip.lev = NULL
df.67.SID.lev.covs = NULL
for(type in unique(SID.type)){
  #SID level
  covs.both.67.lev <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                             age + + age2 + race + eth + children + income + 
                             inc.ed + gender + victim + yrs.in.nbh + 
                             (1|SID) + (1|zip), data = for.icc67[SID.type == type,])
  summary(covs.both.67.lev)
  df.covs.67.lev <- as.data.frame(VarCorr(covs.both.67.lev))
  icc.covs.67.SID.lev[type] <- df.covs.67.lev[1,4]/(df.covs.67.lev[1,4]+
                                                   df.covs.67.lev[2,4]+df.covs.67.lev[3,4]) 
  icc.covs.67.zip.lev[type] <- df.covs.67.lev[2,4]/(df.covs.67.lev[1,4]+
                                                   df.covs.67.lev[2,4]+df.covs.67.lev[3,4])
  df.67.SID.lev.covs <- c(df.67.SID.lev.covs, df.covs.67.lev[1,4])
}

#output files: first regression coeffs, then iccs and vars
write.csv(cbind(summary(covs.both.67.lev)$coef[,1], summary(covs.both.67.lev)$coef[,3]),
          file = "q6 q7 coefs.csv") 

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
write.csv(output, file = "q6 q7 vars.csv")


#barchart
Spot = factor(names(icc.covs.67.SID.lev), 
              levels = c("Cold Spot", "Cool Spot", "Drug Spot", "Violent Spot", "Combined"), 
              ordered = TRUE)

bar.df = data.frame(Spot = Spot,
                    SID = icc.covs.67.SID.lev, ZIP = icc.covs.67.zip.lev)

library(plyr)
mm <- ddply(bar.df, "Spot", summarise, test = mean(SID))
ggplot(mm, aes(x = factor(Spot), y = test)) + 
  geom_bar(stat = "identity")+ 
  xlab("Hotspot type") + 
  geom_hline(aes(yintercept = icc.covs.both.67.sid), col = "red") + 
  geom_hline(aes(yintercept = icc.covs.both.67.zip), col = "blue") +
  ylab("SID level ICC") + 
  ggtitle("ICC for combined ZIP and SID with covariates")
  


###################### 6 and 7 separate ######################
detach(for.icc67)
#SID level - Q6
uncond.SID6 <- lmer(pi.jk~1 + (1|SID), data = for.icc6)
summary(uncond.SID6)
df.SID6 <- as.data.frame(VarCorr(uncond.SID6))
icc.SID6 <- df.SID6[1,4]/(df.SID6[1,4]+df.SID6[2,4]) #0.1068758

# zip level - Q6
lmer.zip6<-lmer(pi.jk~1 + (1|zip), data = for.icc6)
summary(lmer.zip6)
df.zip6<-as.data.frame(VarCorr(lmer.zip6))
icc.zip6<-df.zip6[1,4]/(df.zip6[1,4]+df.zip6[2,4]) #0.01285916

#SID level - Q7
uncond.SID7 <- lmer(pi.jk~1 + (1|SID), data = for.icc7)
summary(uncond.SID7)
df.SID7 <- as.data.frame(VarCorr(uncond.SID7))
icc.SID7 <- df.SID7[1,4]/(df.SID7[1,4]+df.SID7[2,4]) #0.1105118

# zip level - Q7
lmer.zip7<-lmer(pi.jk~1 + (1|zip), data = for.icc7)
summary(lmer.zip7)
df.zip7<-as.data.frame(VarCorr(lmer.zip7))
icc.zip7<-df.zip7[1,4]/(df.zip7[1,4]+df.zip7[2,4]) #0.01376106
