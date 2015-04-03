setwd("C:/Users/jackie/Desktop/own research/icc")
source("create covariates dataset.R")
source("questions 6 and 7 - pi regressions.R")

# merge datasets and run ICC's

# make sure all subjects are in both sets
test<-strsplit(names(pi),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people<-data.frame(person = has.dummies, pi.jk = pi)

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
for.icc <- join_all(list(for.pi, dummy.people, covs.set))
for.icc6 <- join_all(list(for.pi6, dummy.people6, covs.set))
for.icc7 <- join_all(list(for.pi7, dummy.people7, covs.set))

detach(for.pi)


########################### get ICCs ###############################
#                                                                  #
#                     start w all combined                         #
#                                                                  #
####################################################################

attach(for.icc)
#SID level
uncond.SID <- lmer(pi.jk~1 + (1|SID))
summary(uncond.SID)
df.SID <- as.data.frame(VarCorr(uncond.SID))
icc.SID <- df.SID[1,4]/(df.SID[1,4]+df.SID[2,4]) #0.1078097

# block grp level
lmer.block.grp<-lmer(pi.jk~1 + (1|block.grp))
summary(lmer.block.grp)
df.block<-as.data.frame(VarCorr(lmer.block.grp))
icc.block<-df.block[1,4]/(df.block[1,4]+df.block[2,4]) #1.614484e-13

# tract level
lmer.tract<-lmer(pi.jk~1 + (1|tract))
summary(lmer.tract)
df.tract<-as.data.frame(VarCorr(lmer.tract))
icc.tract<-df.tract[1,4]/(df.tract[1,4]+df.tract[2,4]) #0.04406416

# zip level
lmer.zip<-lmer(pi.jk~1 + (1|zip))
summary(lmer.zip)
df.zip<-as.data.frame(VarCorr(lmer.zip))
icc.zip<-df.zip[1,4]/(df.zip[1,4]+df.zip[2,4]) #0.01316786

# sid + zip level
lmer.both<-lmer(pi.jk~1 + (1|zip) + (1|SID))
summary(lmer.both)
df.both<-as.data.frame(VarCorr(lmer.both))
icc.both.zip<-df.both[2,4]/(df.both[1,4]+df.both[2,4]+df.both[3,4]) #2.613472e-14
icc.both.sid<-df.both[1,4]/(df.both[1,4]+df.both[2,4]+df.both[3,4]) #0.1078097


# by sid type 

icc.SID = NULL
icc.zip = NULL
for(type in unique(SID.type)){
  #SID level
  uncond.SID <- lmer(pi.jk~1 + (1|SID), data = for.icc[SID.type == type,])
  summary(uncond.SID)
  df.SID <- as.data.frame(VarCorr(uncond.SID))
  icc.SID[type] <- df.SID[1,4]/(df.SID[1,4]+df.SID[2,4]) 
}
# drug and combined have highest icc

# with covariates
# says rank deficient after adding new vars

#SID level
uncond.SID.cov <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                         age + + age2 + race + eth + children + income + 
                         inc.ed + gender + victim + yrs.in.nbh + 
                         (1|SID))
df.SID.cov <- as.data.frame(VarCorr(uncond.SID.cov))
icc.SID.cov <- df.SID.cov[1,4]/(df.SID.cov[1,4]+df.SID.cov[2,4]) #0.2624956

# zip level
lmer.zip.cov<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                     age + + age2 + race + eth + children + income + 
                     inc.ed + gender + victim + yrs.in.nbh + 
                     (1|zip))
df.zip.cov <- as.data.frame(VarCorr(lmer.zip.cov))
icc.zip.cov <- df.zip.cov[1,4]/(df.zip.cov[1,4]+df.zip.cov[2,4]) #0.01986521

#look at combined level
lmer.both.cov<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                      age + + age2 + race + eth + children + income + 
                      inc.ed + gender + victim + yrs.in.nbh + 
                      (1|SID) + (1|zip))
df.both.cov <- as.data.frame(VarCorr(lmer.both.cov))
icc.sid.both.cov <- df.both.cov[1,4]/(df.both.cov[1,4]+df.both.cov[2,4]+
                                    df.both.cov[3,4]) #0.2584314
icc.zip.both.cov <- df.both.cov[2,4]/(df.both.cov[1,4]+df.both.cov[2,4]+
                                        df.both.cov[3,4]) #0.004694903


#by sid type

icc.SID.lev.covs = NULL
icc.zip.lev.covs = NULL
for(type in unique(SID.type)){
  #SID level
  SID.lev.covs <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                             age + + age2 + race + eth + children + income + 
                             inc.ed + gender + victim + yrs.in.nbh + 
                             (1|SID) + (1|zip), data = for.icc[SID.type == type,])
  summary(SID.lev.covs)
  df.SID.covs <- as.data.frame(VarCorr(SID.lev.covs))
  icc.SID.lev.covs[type] <- df.SID.covs[1,4]/(df.SID.covs[1,4]+df.SID.covs[2,4]+df.SID.covs[3,4]) 
  icc.zip.lev.covs[type] <- df.SID.covs[2,4]/(df.SID.covs[1,4]+df.SID.covs[2,4]+df.SID.covs[3,4])
}

# SID
# Cool Spot    Drug Spot Violent Spot    Cold Spot     Combined 
# 0.2209708    0.2746691    0.3309531    0.2237687    0.5610899 

# zip
# Cool Spot    Drug Spot Violent Spot    Cold Spot     Combined 
# 0.01402593   0.02397835   0.00000000   0.00000000   0.00000000 


###################### 6 and 7 separate ######################
detach(for.icc)
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
