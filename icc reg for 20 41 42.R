setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
source("create covariates dataset.R")

# put source file for question X: "question X prep.R"
# for 6 and 7 use file "questions 6 and 7 - iccs. R"
source("question 41 prep.R")


########################### get ICCs no covs ###############################

#SID level
uncond.SID <- lmer(pi.jk~1 + (1|SID))
summary(uncond.SID)
df.SID <- as.data.frame(VarCorr(uncond.SID))
icc.SID <- df.SID[1,4]/(df.SID[1,4]+df.SID[2,4]) 

# block grp level
lmer.block.grp<-lmer(pi.jk~1 + (1|block.grp))
summary(lmer.block.grp)
df.block<-as.data.frame(VarCorr(lmer.block.grp))
icc.block<-df.block[1,4]/(df.block[1,4]+df.block[2,4]) 

# tract level
lmer.tract<-lmer(pi.jk~1 + (1|tract))
summary(lmer.tract)
df.tract<-as.data.frame(VarCorr(lmer.tract))
icc.tract<-df.tract[1,4]/(df.tract[1,4]+df.tract[2,4]) 

# zip level
lmer.zip<-lmer(pi.jk~1 + (1|zip))
summary(lmer.zip)
df.zip<-as.data.frame(VarCorr(lmer.zip))
icc.zip<-df.zip[1,4]/(df.zip[1,4]+df.zip[2,4]) 

# sid + zip level
lmer.both<-lmer(pi.jk~1 + (1|zip) + (1|SID))
summary(lmer.both)
df.both<-as.data.frame(VarCorr(lmer.both))
icc.both.zip<-df.both[2,4]/(df.both[1,4]+df.both[2,4]+df.both[3,4])
icc.both.sid<-df.both[1,4]/(df.both[1,4]+df.both[2,4]+df.both[3,4]) 


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

bar.df = data.frame(Spot = names(icc.SID.lev.covs),
                    SID = icc.SID.lev.covs, ZIP = icc.zip.lev.covs)

mm <- ddply(bar.df, "Spot", summarise, test = mean(SID))
ggplot(mm, aes(x = factor(Spot), y = test)) + 
  geom_bar(stat = "identity")+ 
  xlab("Hotspot type") + 
  geom_hline(aes(yintercept = icc.sid.both.cov), col = "red") + 
  geom_hline(aes(yintercept = icc.zip.both.cov), col = "blue") +
  ylab("SID level ICC") + 
  ggtitle("ICC for combined ZIP and SID without covariates")

########################### with covariates ######################
# says rank deficient after adding new vars

#SID level
uncond.SID.cov <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                         age + + age2 + race + eth + children + income + 
                         inc.ed + gender + victim + yrs.in.nbh + 
                         (1|SID))
df.SID.cov <- as.data.frame(VarCorr(uncond.SID.cov))
icc.SID.cov <- df.SID.cov[1,4]/(df.SID.cov[1,4]+df.SID.cov[2,4]

# zip level
uncond.zip.cov<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                     age + + age2 + race + eth + children + income + 
                     inc.ed + gender + victim + yrs.in.nbh + 
                     (1|zip))
df.zip.cov <- as.data.frame(VarCorr(uncond.zip.cov))
icc.zip.cov <- df.zip.cov[1,4]/(df.zip.cov[1,4]+df.zip.cov[2,4]) 

#look at combined level
lmer.both.cov<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                      age + + age2 + race + eth + children + income + 
                      inc.ed + gender + victim + yrs.in.nbh + 
                      (1|SID) + (1|zip))
df.both.cov <- as.data.frame(VarCorr(lmer.both.cov))
icc.sid.both.cov <- df.both.cov[1,4]/(df.both.cov[1,4]+df.both.cov[2,4]+
                                        df.both.cov[3,4])
icc.zip.both.cov <- df.both.cov[2,4]/(df.both.cov[1,4]+df.both.cov[2,4]+
                                        df.both.cov[3,4])

########################### by sid type with covs ##############

icc.SID.lev.covs = NULL
icc.zip.lev.covs = NULL
df.SID.lev.covs = NULL
for(type in unique(SID.type)){
  #SID level
  SID.lev.covs <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                         age + + age2 + race + eth + children + income + 
                         inc.ed + gender + victim + yrs.in.nbh + 
                         (1|SID) + (1|zip), data = for.icc[SID.type == type,])
  summary(SID.lev.covs)
  df.covs <- as.data.frame(VarCorr(SID.lev.covs))
  icc.SID.lev.covs[type] <- df.covs[1,4]/(df.covs[1,4]+df.covs[2,4]+df.covs[3,4]) 
  icc.zip.lev.covs[type] <- df.covs[2,4]/(df.covs[1,4]+df.covs[2,4]+df.covs[3,4])
  df.SID.lev.covs <- c(df.SID.lev.covs, df.covs[1,4])
}

Spot = factor(names(icc.SID.lev.covs), 
              levels = c("Cold Spot", "Cool Spot", "Drug Spot", "Violent Spot", "Combined"), 
              ordered = TRUE)
bar.df = data.frame(Spot = Spot,
                    SID = icc.SID.lev.covs, ZIP = icc.zip.lev.covs)

mm <- ddply(bar.df, "Spot", summarise, test = mean(SID))
ggplot(mm, aes(x = factor(Spot), y = test)) + 
  geom_bar(stat = "identity")+ 
  xlab("Hotspot type") + 
  geom_hline(aes(yintercept = icc.sid.both.cov), col = "red") + 
  geom_hline(aes(yintercept = icc.zip.both.cov), col = "blue") +
  ylab("SID level ICC") + 
  ggtitle("ICC for combined ZIP and SID with covariates")

#output regression results for tables
write.csv(cbind(summary(SID.lev.covs)$coef[,1], summary(SID.lev.covs)$coef[,3]),
          file = "q41 coefs.csv")
