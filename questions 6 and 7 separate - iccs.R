setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
source("create covariates dataset.R")
source("questions 6 and 7 - pi regressions.R")

##################### dataset creation ###################

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


for.icc6 <- join_all(list(for.pi6, dummy.people6, covs.set))
for.icc7 <- join_all(list(for.pi7, dummy.people7, covs.set))

detach(for.pi67)

###################### 6 and 7 separate ######################

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
