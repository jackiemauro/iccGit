setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
source("create covariates dataset.R")
source("questions 6 and 7 - pi regressions.R")
require(plyr)
require(lme4)

#################### dataset creation ##############
# merge datasets and run ICC's

# make sure all subjects are in both sets
test<-strsplit(names(pi.67),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people<-data.frame(person = has.dummies, pi.jk = pi.67)


# merge dummies, pi's and geographies
for.icc67 <- join_all(list(for.pi67, dummy.people, covs.set))

# create dataset with no NA's
noNA <- as.matrix(for.icc67)[,c(18:21, 24, 25, 27:30)] 
#exclude age, children, years in nbh b/c otherwise turns into categorical
noNA[is.na(noNA)] <- "Blank"
age.noNA = for.icc67$age
# replacing with mean, discuss this with Amelia
age.noNA[is.na(for.icc67$age)] <- mean(for.icc67$age, na.rm = T)
age.noNA.sq = age.noNA^2
children.noNA = for.icc67$children
children.noNA[is.na(children.noNA)] <- mean(children.noNA, na.rm = T)
yrs.nbh.noNA = for.icc67$yrs.in.nbh
yrs.nbh.noNA[is.na(yrs.nbh.noNA)] <- mean(yrs.nbh.noNA, na.rm = T)


noNA <- as.data.frame(noNA)
for.icc67.noNA <- cbind(for.icc67[,1:17],for.icc67[,32:36],
                        noNA, age.noNA, age.noNA.sq, children.noNA, yrs.nbh.noNA)
remove(age.noNA)
remove(age.noNA.sq)
remove(children.noNA)
remove(yrs.nbh.noNA)

detach(for.pi67)


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
for(type in unique(SID.type)[-NA]){
  basic.both.67.lev <- lmer(pi.jk~ 1 + (1|SID) + (1|zip), data = for.icc67[SID.type == type,])
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
                         age + age2 + race + eth + children + income + 
                         inc.ed + gender + victim + yrs.in.nbh + 
                         (1|SID))
df.covs.67.SID <- as.data.frame(VarCorr(covs.SID.67))
icc.covs.67.SID <- df.covs.67.SID[1,4]/(df.covs.67.SID[1,4]+df.covs.67.SID[2,4]) #0.2624956

# zip level
covs.zip.67<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                     age + age2 + race + eth + children + income + 
                     inc.ed + gender + victim + yrs.in.nbh + 
                     (1|zip))
df.covs.67.zip <- as.data.frame(VarCorr(covs.zip.67))
icc.covs.67.zip <- df.covs.67.zip[1,4]/(df.covs.67.zip[1,4]+df.covs.67.zip[2,4]) #0.01986521

#look at combined level
covs.both.67<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                      age + age2 + race + eth + children + income + 
                      inc.ed + gender + victim + yrs.in.nbh + 
                      (1|SID) + (1|zip))
df.covs.67.both <- as.data.frame(VarCorr(covs.both.67))
icc.covs.both.67.sid <- df.covs.67.both[1,4]/(df.covs.67.both[1,4]+df.covs.67.both[2,4]+
                                            df.covs.67.both[3,4]) #0.2584314
icc.covs.both.67.zip <- df.covs.67.both[2,4]/(df.covs.67.both[1,4]+df.covs.67.both[2,4]+
                                            df.covs.67.both[3,4]) #0.004694903
#with type
covs.both.67.type<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                     age + age2 + race + eth + children + income + 
                     inc.ed + gender + victim + yrs.in.nbh + SID.type + 
                     (1|SID) + (1|zip))
df.covs.67.both.type <- as.data.frame(VarCorr(covs.both.67.type))
icc.covs.both.67.sid.type <- df.covs.67.both.type[1,4]/(df.covs.67.both.type[1,4]+df.covs.67.both.type[2,4]+
                                                     df.covs.67.both.type[3,4]) #0.2602174
icc.covs.both.67.zip.type <- df.covs.67.both.type[2,4]/(df.covs.67.both.type[1,4]+df.covs.67.both.type[2,4]+
                                                          df.covs.67.both.type[3,4]) #0.004350159

############################ with covs by sid type 6&7 #######################

icc.covs.67.SID.lev = NULL
icc.covs.67.zip.lev = NULL
df.67.SID.lev.covs = NULL
for(type in unique(SID.type)){
  covs.both.67.lev <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                             age + age2 + race + eth + children + income + 
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

############## output files: first regression coeffs, then iccs and vars #######
write.csv(cbind(summary(covs.both.67)$coef[,1], summary(covs.both.67)$coef[,3]),
          file = "q6 q7 coefs.csv") #redo this one!!

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
  


