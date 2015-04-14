setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
source("ICC prelim.R")
require(plyr)
library(base)

#reduce data frame to just questions we're interested in
reduced<-data.frame(SID=SID,person=household_ID,q1=worried_19a,
                    q2=worried_19b, q3=worried_19c, 
                    q4=worried_19e, q5=worried_19f)
detach(hotspot)

# Throw out 19d, check levels of refused and don't know HAVE THIS IN DRIVE

# experiment with averaging or summing answers instead of regression weighty guy
# lower levels of ICC but similar patterns

########################### create dataset ####################
#replace all the non-responses with NA
for(ii in 1:length(reduced)){
  reduced[,ii] = ifelse(reduced[,ii] %in% c("DK", "REF", NA),NA,reduced[,ii])
}

attach(reduced)

#transform data to get binary answer-person combinations
tab1<-as.data.frame(table(q1,person,useNA="always"))
names(tab1)<-c("Answer","person","Q1")
tab2<-as.data.frame(table(q2,person,useNA="always"))
names(tab2)<-c("Answer","person","Q2")
tab3<-as.data.frame(table(q3,person,useNA="always"))
names(tab3)<-c("Answer","person","Q3")
tab4<-as.data.frame(table(q4,person,useNA="always"))
names(tab4)<-c("Answer","person","Q4")
tab5<-as.data.frame(table(q5,person,useNA="always"))
names(tab5)<-c("Answer","person","Q5")


#limiting to rows with some answer
pre<-join_all(list(tab1,tab2,tab3,tab4,tab5))
pre$sum<-rowSums(pre[,3:length(pre)])

#4 households have >1 row, excluding these
table.dup.hhid<-as.data.frame(table(hotspot$household_ID))
dup.hhids<-table.dup.hhid$Var1[which(table.dup.hhid$Freq>1)]
pre.no.dup<-pre[-which(pre$person %in% dup.hhids),]

detach(reduced)
attach(pre.no.dup)

# get rid of sum 
for.pi<-pre.no.dup[pre$sum != 0, ] 
detach(pre.no.dup)
attach(for.pi)

if(any(is.na(for.pi$person))){
  for.pi <- for.pi[-which(is.na(for.pi$person)),]
}

# find pi's
pi.reg <- lm(as.numeric(Answer)~person+Q1+Q2+Q3+Q4+Q5-1)
pi <-pi.reg$coefficients[1:(length(pi.reg$coefficients)-5)] 
alphas = pi.reg$coefficients[(length(pi.reg$coefficients)-4):length(pi.reg$coefficients)] 

#alt: find average
for.avg <- reduced[-(which(reduced$person %in% dup.hhids)),]
for.avg$avg <- rowMeans(for.avg[,3:7], na.rm = T)

#alt: find sum
for.sum <- reduced[-(which(reduced$person %in% dup.hhids)),]
for.sum$sum <- rowSums(for.avg[,3:7], na.rm = T)

# find icc
source("create covariates dataset.R")

# make sure all subjects are in both sets
test<-strsplit(names(pi),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people<-data.frame(person = has.dummies, pi.jk = pi)

# merge dummies, pi's and geographies
for.icc.19 <- join_all(list(for.pi,dummy.people,covs.set))
for.icc.avg <- join_all(list(for.avg, dummy.people, covs.set))
for.icc.sum <- join_all(list(for.sum, dummy.people, covs.set))

detach(for.pi)
attach(for.icc.19)

######################### basic regs ####################

#SID level
uncond.SID <- lmer(pi.jk~1 + (1|SID))
summary(uncond.SID)
df.SID <- as.data.frame(VarCorr(uncond.SID))
icc.SID <- df.SID[1,4]/(df.SID[1,4]+df.SID[2,4]) 

# zip level
lmer.zip<-lmer(pi.jk~1 + (1|zip))
summary(lmer.zip)
df.zip<-as.data.frame(VarCorr(lmer.zip))
icc.zip<-df.zip[1,4]/(df.zip[1,4]+df.zip[2,4]) 

###################### by sid type #######################

icc.SID = NULL
icc.zip = NULL
for(type in unique(SID.type)){
  #SID level
  uncond.SID <- lmer(pi.jk~1 + (1|SID), data = for.icc.19[SID.type == type,])
  summary(uncond.SID)
  df.SID <- as.data.frame(VarCorr(uncond.SID))
  icc.SID[type] <- df.SID[1,4]/(df.SID[1,4]+df.SID[2,4]) 
}


################# with covariates ###############

#SID level
covs.SID.19 <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                      age + + age2 + race + eth + children + income + 
                      inc.ed + gender + victim + yrs.in.nbh + 
                      (1|SID))
df.covs.19.SID <- as.data.frame(VarCorr(covs.SID.19))
icc.covs.67.SID <- df.covs.19.SID[1,4]/(df.covs.19.SID[1,4]+df.covs.19.SID[2,4]) #0.06316817

# zip level
covs.zip.19<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                    age + + age2 + race + eth + children + income + 
                    inc.ed + gender + victim + yrs.in.nbh + 
                    (1|zip))
df.covs.19.SID <- as.data.frame(VarCorr(covs.zip.19))
icc.covs.19.SID <- df.covs.19.SID[1,4]/(df.covs.19.SID[1,4]+df.covs.19.SID[2,4]) #0.00418688

#look at combined level
covs.both.19<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                     age + + age2 + race + eth + children + income + 
                     inc.ed + gender + victim + yrs.in.nbh + 
                     (1|SID) + (1|zip))
df.covs.19.both <- as.data.frame(VarCorr(covs.both.19))
icc.covs.both.19.sid <- df.covs.19.both[1,4]/(df.covs.19.both[1,4]+df.covs.19.both[2,4]+
                                                df.covs.19.both[3,4]) #0.0631682
icc.covs.both.19.zip <- df.covs.19.both[2,4]/(df.covs.19.both[1,4]+df.covs.19.both[2,4]+
                                                df.covs.19.both[3,4]) #0


#by sid type

icc.covs.19.SID.lev = NULL
icc.covs.19.zip.lev = NULL
df.19.SID.lev.covs = NULL
for(type in unique(SID.type)){
  #SID level
  covs.both.19.lev <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                             age + + age2 + race + eth + children + income + 
                             inc.ed + gender + victim + yrs.in.nbh + 
                             (1|SID) + (1|zip), data = for.icc.19[SID.type == type,])
  summary(covs.both.19.lev)
  df.covs.19.lev <- as.data.frame(VarCorr(covs.both.19.lev))
  icc.covs.19.SID.lev[type] <- df.covs.19.lev[1,4]/(df.covs.19.lev[1,4]+df.covs.19.lev[2,4]+df.covs.19.lev[3,4]) 
  icc.covs.19.zip.lev[type] <- df.covs.19.lev[2,4]/(df.covs.19.lev[1,4]+df.covs.19.lev[2,4]+df.covs.19.lev[3,4])
  df.19.SID.lev.covs <- c(df.19.SID.lev.covs, df.covs.19.lev[1,4])
}

write.csv(cbind(summary(covs.both.19.lev)$coef[,1], summary(covs.both.19.lev)$coef[,3]),
          file = "q19 coefs.csv")

var.icc.19 = data.frame(vars = df.19.SID.lev.covs, iccs = icc.covs.19.SID.lev)
overalls.19 = data.frame(vars = c(df.covs.19.both[2,4], df.covs.19.both[1,4]),
                         iccs = c(icc.covs.both.19.zip, icc.covs.both.19.sid))
output.unordered = rbind(overalls.19, var.icc.19)
output = rbind(output.unordered[1:2,],
               output.unordered[which(labels(output.unordered)[[1]] == "Cold Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Cool Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Violent Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Drug Spot"),],
               output.unordered[which(labels(output.unordered)[[1]] == "Combined"),])
write.csv(output, file = "q19 vars.csv")


Spot = factor(names(icc.SID.lev.covs), 
              levels = c("Cold Spot", "Cool Spot", "Drug Spot", "Violent Spot", "Combined"), 
              ordered = TRUE)

bar.df = data.frame(Spot = Spot, SID = icc.SID.lev.covs, ZIP = icc.zip.lev.covs)

mm <- ddply(bar.df, "Spot", summarise, test = mean(SID))
ggplot(mm, aes(x = factor(Spot), y = test)) + 
  geom_bar(stat = "identity")+ 
  xlab("Hotspot type") + 
  geom_hline(aes(yintercept = icc.sid.both.cov), col = "red") + 
  geom_hline(aes(yintercept = icc.zip.both.cov), col = "blue") +
  ylab("SID level ICC") + 
  ggtitle("ICC for combined ZIP and SID with covariates q19")


detach(for.icc.19)
attach(for.icc.avg)


########################### get ICCs for averages ##############

#SID level
uncond.SID <- lmer(avg~1 + (1|SID))
summary(uncond.SID)
df.SID <- as.data.frame(VarCorr(uncond.SID))
icc.SID <- df.SID[1,4]/(df.SID[1,4]+df.SID[2,4]) 

# zip level
lmer.zip<-lmer(avg~1 + (1|zip))
summary(lmer.zip)
df.zip<-as.data.frame(VarCorr(lmer.zip))
icc.zip<-df.zip[1,4]/(df.zip[1,4]+df.zip[2,4]) 

###################### by sid type for avg################

icc.SID = NULL
icc.zip = NULL
for(type in unique(SID.type)){
  #SID level
  uncond.SID <- lmer(avg~1 + (1|SID), data = for.icc[SID.type == type,])
  summary(uncond.SID)
  df.SID <- as.data.frame(VarCorr(uncond.SID))
  icc.SID[type] <- df.SID[1,4]/(df.SID[1,4]+df.SID[2,4]) 
}


################# with covariates ###############

#SID level
uncond.SID.cov <- lmer(avg~1 + ed + marital + working + work.type + 
                         age + + age2 + race + eth + children + income + 
                         inc.ed + gender + victim + yrs.in.nbh + 
                         (1|SID))
df.SID.cov <- as.data.frame(VarCorr(uncond.SID.cov))
icc.SID.cov <- df.SID.cov[1,4]/(df.SID.cov[1,4]+df.SID.cov[2,4]) #0.1461535

# zip level
lmer.zip.cov<-lmer(avg~1 + ed + marital + working + work.type + 
                     age + + age2 + race + eth + children + income + 
                     inc.ed + gender + victim + yrs.in.nbh + 
                     (1|zip))
df.zip.cov <- as.data.frame(VarCorr(lmer.zip.cov))
icc.zip.cov <- df.zip.cov[1,4]/(df.zip.cov[1,4]+df.zip.cov[2,4]) #0.006594692

#look at combined level
lmer.both.cov<-lmer(avg~1 + ed + marital + working + work.type + 
                      age + + age2 + race + eth + children + income + 
                      inc.ed + gender + victim + yrs.in.nbh + 
                      (1|SID) + (1|zip))
df.both.cov <- as.data.frame(VarCorr(lmer.both.cov))
icc.sid.both.cov <- df.both.cov[1,4]/(df.both.cov[1,4]+df.both.cov[2,4]+
                                        df.both.cov[3,4]) #0.1454937
icc.zip.both.cov <- df.both.cov[2,4]/(df.both.cov[1,4]+df.both.cov[2,4]+
                                        df.both.cov[3,4]) #0.0007717615


#by sid type

icc.SID.lev.covs = NULL
icc.zip.lev.covs = NULL
for(type in unique(SID.type)){
  #SID level
  SID.lev.covs <- lmer(avg~1 + ed + marital + working + work.type + 
                         age + + age2 + race + eth + children + income + 
                         inc.ed + gender + victim + yrs.in.nbh + 
                         (1|SID) + (1|zip), data = for.icc.avg[SID.type == type,])
  summary(SID.lev.covs)
  df.SID.covs <- as.data.frame(VarCorr(SID.lev.covs))
  icc.SID.lev.covs[type] <- df.SID.covs[1,4]/(df.SID.covs[1,4]+df.SID.covs[2,4]+df.SID.covs[3,4]) 
  icc.zip.lev.covs[type] <- df.SID.covs[2,4]/(df.SID.covs[1,4]+df.SID.covs[2,4]+df.SID.covs[3,4])
}

# icc.SID.lev.covs
# Cool Spot Violent Spot    Drug Spot     Combined    Cold Spot 
# 1.514432e-02 1.430654e-02 6.248788e-05 3.454218e-02 0.000000e+00 
# icc.zip.lev.covs
# Cool Spot Violent Spot    Drug Spot     Combined    Cold Spot 
# 0.0006267617 0.0141485117 0.0000000000 0.0000000000 0.0000000000 


require(ggplot2)
require(plyr)

Spot = factor(names(icc.SID.lev.covs), 
              levels = c("Cold Spot", "Cool Spot", "Drug Spot", "Violent Spot", "Combined"), 
              ordered = TRUE)

bar.df = data.frame(Spot = Spot, SID = icc.SID.lev.covs, ZIP = icc.zip.lev.covs)

mm <- ddply(bar.df, "Spot", summarise, test = mean(SID))
ggplot(mm, aes(x = factor(Spot), y = test)) + 
  geom_bar(stat = "identity")+ 
  xlab("Hotspot type") + 
  geom_hline(aes(yintercept = icc.sid.both.cov), col = "red") + 
  geom_hline(aes(yintercept = icc.zip.both.cov), col = "blue") +
  ylab("SID level ICC") + 
  ggtitle("ICC for combined ZIP and SID with covariates")

################### ICCs for sums w covs ###################

#look at combined level
lmer.both.cov<-lmer(sum~1 + ed + marital + working + work.type + 
                      age + + age2 + race + eth + children + income + 
                      inc.ed + gender + victim + yrs.in.nbh + 
                      (1|SID) + (1|zip))
df.both.cov <- as.data.frame(VarCorr(lmer.both.cov))
icc.sid.both.cov <- df.both.cov[1,4]/(df.both.cov[1,4]+df.both.cov[2,4]+
                                        df.both.cov[3,4]) #0.03668239
icc.zip.both.cov <- df.both.cov[2,4]/(df.both.cov[1,4]+df.both.cov[2,4]+
                                        df.both.cov[3,4]) #0.005340977


#by sid type

icc.SID.lev.covs = NULL
icc.zip.lev.covs = NULL
for(type in unique(SID.type)){
  #SID level
  SID.lev.covs <- lmer(sum~1 + ed + marital + working + work.type + 
                         age + + age2 + race + eth + children + income + 
                         inc.ed + gender + victim + yrs.in.nbh + 
                         (1|SID) + (1|zip), data = for.icc.sum[SID.type == type,])
  summary(SID.lev.covs)
  df.SID.covs <- as.data.frame(VarCorr(SID.lev.covs))
  icc.SID.lev.covs[type] <- df.SID.covs[1,4]/(df.SID.covs[1,4]+df.SID.covs[2,4]+df.SID.covs[3,4]) 
  icc.zip.lev.covs[type] <- df.SID.covs[2,4]/(df.SID.covs[1,4]+df.SID.covs[2,4]+df.SID.covs[3,4])
}

# Cool Spot Violent Spot    Drug Spot     Combined    Cold Spot 
# 0.04703118   0.09485493   0.06552409   0.04193855   0.00000000 





require(ggplot2)
require(plyr)

Spot = factor(names(icc.SID.lev.covs), 
              levels = c("Cold Spot", "Cool Spot", "Drug Spot", "Violent Spot", "Combined"), 
              ordered = TRUE)

bar.df = data.frame(Spot = Spot, SID = icc.SID.lev.covs, ZIP = icc.zip.lev.covs)

mm <- ddply(bar.df, "Spot", summarise, test = mean(SID))
ggplot(mm, aes(x = factor(Spot), y = test)) + 
  geom_bar(stat = "identity")+ 
  xlab("Hotspot type") + 
  #geom_hline(aes(yintercept = icc.sid.both.cov), col = "red") + 
  #geom_hline(aes(yintercept = icc.zip.both.cov), col = "blue") +
  ylab("SID level ICC") + 
  ggtitle("ICC for combined ZIP and SID with covariates")