setwd("C:/Users/jackie/Desktop/own research/icc")
source("ICC prelim.R")
require(plyr)
library(base)

#reduce data frame to just questions we're interested in
reduced<-data.frame(SID=SID,person=household_ID,q1=cohesion_6a,
                q2=cohesion_6b, q3=cohesion_6c, q4=cohesion_6d,
                q5=cohesion_6e, q6=cohesion_6f,q7=soc_control_7a,
                q8=soc_control_7b, q9=soc_control_7c,q10=soc_control_7d,
                q11=soc_control_7e, q12=soc_control_7f)
detach(hotspot)

#replace all the non-responses with NA
for(ii in 1:length(reduced)){
  reduced[,ii] = ifelse(reduced[,ii] %in% c("DK", "REF", NA),NA,reduced[,ii])
}

#need to reverse code negatives to be consistent
reduced$q2.rev = reduced$q2*(-1) + 5
reduced$q4.rev = reduced$q4*(-1) + 5
reduced$q5.rev = reduced$q5*(-1) + 5


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
tab6<-as.data.frame(table(q6,person,useNA="always"))
names(tab6)<-c("Answer","person","Q6")
tab7<-as.data.frame(table(q7,person,useNA="always"))
names(tab7)<-c("Answer","person","Q7")
tab8<-as.data.frame(table(q8,person,useNA="always"))
names(tab8)<-c("Answer","person","Q8")
tab9<-as.data.frame(table(q9,person,useNA="always"))
names(tab9)<-c("Answer","person","Q9")
tab10<-as.data.frame(table(q10,person,useNA="always"))
names(tab10)<-c("Answer","person","Q10")
tab11<-as.data.frame(table(q11,person,useNA="always"))
names(tab11)<-c("Answer","person","Q11")
tab12<-as.data.frame(table(q12,person,useNA="always"))
names(tab12)<-c("Answer","person","Q12")
tab2.rev<-as.data.frame(table(q2.rev,person,useNA="always"))
names(tab2.rev)<-c("Answer","person","Q2.rev")
tab4.rev<-as.data.frame(table(q4.rev,person,useNA="always"))
names(tab4.rev)<-c("Answer","person","Q4.rev")
tab5.rev<-as.data.frame(table(q5.rev,person,useNA="always"))
names(tab5.rev)<-c("Answer","person","Q5.rev")

#limiting to rows with some answer
pre<-join_all(list(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,
                 tab9,tab10,tab11,tab12,tab2.rev, tab4.rev, tab5.rev))
pre$sum<-rowSums(pre[,3:length(pre)])

#4 households have >1 row, excluding these
table.dup.hhid<-as.data.frame(table(hotspot$household_ID))
dup.hhids<-table.dup.hhid$Var1[which(table.dup.hhid$Freq>1)]
which(pre$person %in% dup.hhids)
pre.no.dup<-pre[-which(pre$person %in% dup.hhids),]

detach(reduced)
attach(pre.no.dup)

# get rid of sum and unreversed codes 
# so they don't figure into dummy calc
drops<-c("Q2","Q4","Q5","sum")
for.pi<-pre.no.dup[pre$sum != 0, !names(pre.no.dup) %in% drops] 
detach(pre.no.dup)
attach(for.pi)

if(any(is.na(for.pi$person))){
  for.pi <- for.pi[-which(is.na(for.pi$person)),]
}

# find pi's
pi.reg <- lm(as.numeric(Answer)~person+Q1+Q2.rev+Q3+Q4.rev+Q5.rev+Q6+Q7+Q8+Q9+Q10+Q11-1)
pi <-pi.reg$coefficients[1:(length(pi.reg$coefficients)-11)] 
alphas = pi.reg$coefficients[(length(pi.reg$coefficients)-10):length(pi.reg$coefficients)] 

# pi for just 6
drops<-c("Q7","Q8","Q9","Q10", "Q11", "Q12")
for.pi6<-for.pi[, !names(for.pi) %in% drops] 
pi.reg6 <- lm(as.numeric(Answer)~person+Q1+Q2.rev+Q3+Q4.rev+Q5.rev-1)
pi6 <-pi.reg6$coefficients[1:(length(pi.reg6$coefficients)-5)] 

# pi for just 7
drops<-c("Q1","Q2.rev","Q3","Q4.rev", "Q5.rev", "Q6")
for.pi7<-for.pi[, !names(for.pi) %in% drops] 
pi.reg7 <- lm(as.numeric(Answer)~person+Q7+Q8+Q9+Q10+Q11-1)
pi7 <-pi.reg7$coefficients[1:(length(pi.reg7$coefficients)-5)] 


# data set with geographies and at
geo.id<-read.csv("geographic ids.csv")
merge.geo<-merge(hotspot,geo.id)

geographies.to.merge<-data.frame(SID=merge.geo$SID,
                                 SID.type = merge.geo$ColdSpot3,
                                 person=merge.geo$household_ID,
                                 tract=merge.geo$Census.Tract,
                                 block.grp=merge.geo$Block.Group,
                                 state.fips=merge.geo$State.FIPS,
                                 county.fips=merge.geo$County.FIPS,
                                 zip = merge.geo$Zip.Code) 

# make sure all subjects are in both sets
test<-strsplit(names(pi),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people<-data.frame(person = has.dummies, pi.jk = pi)
dummy.people6<-data.frame(person = has.dummies, pi.jk = pi6)
dummy.people7<-data.frame(person = has.dummies, pi.jk = pi7)

# merge dummies, pi's and geographies
for.icc<-join_all(list(for.pi,dummy.people,geographies.to.merge))
for.icc6<-join_all(list(for.pi6,dummy.people6,geographies.to.merge))
for.icc7<-join_all(list(for.pi7,dummy.people7,geographies.to.merge))

detach(for.pi)
attach(for.icc)

# get ICCs

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


###################### by sid type ############################

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

####################### with covariates ############################
#                                                                  #
#        Covariates: education, marital status (maybe),            #
#    working, age, race/ethnicity, # children in household (maybe),# 
#              income, gender, victimization (maybe),              #
#          self-report variables (maybe), self-control             #
#                                                                  #
####################################################################


# data set with geographies and at
covs.set      <-       data.frame(SID=merge.geo$SID,
                                 SID.type = merge.geo$ColdSpot3,
                                 person=merge.geo$household_ID,
                                 ed = merge.geo$school_comp_124,
                                 marital = merge.geo$marital_127,
                                 working = merge.geo$you_working_132,
                                 work.type = merge.geo$work_type_134,
                                 age = merge.geo$age,
                                 race = merge.geo$race_138,
                                 eth = merge.geo$ethnicity_137,
                                 children = merge.geo$minors_live_142,
                                 income = merge.geo$income_145,
                                 gender = merge.geo$gender_146,
                                 victim = merge.geo$crime_victim_23,
                                 #self.report = merge.geo$,
                                 #self.control = merge.geo$,
                                 tract=merge.geo$Census.Tract,
                                 block.grp=merge.geo$Block.Group,
                                 state.fips=merge.geo$State.FIPS,
                                 county.fips=merge.geo$County.FIPS,
                                 zip = merge.geo$Zip.Code) 

# merge dummies, pi's and geographies
for.icc<-join_all(list(for.pi,dummy.people,covs.set))
detach(for.pi)
attach(for.icc)

# get ICCs

#SID level
uncond.SID <- lmer(pi.jk~1 + ed + marital + working + work.type + 
                     age + race + eth + children + income + gender + 
                     victim +  
                     (1|SID))
summary(uncond.SID)
df.SID <- as.data.frame(VarCorr(uncond.SID))
icc.SID <- df.SID[1,4]/(df.SID[1,4]+df.SID[2,4]) #0.2623424

# zip level
lmer.zip<-lmer(pi.jk~1 + ed + marital + working + work.type + 
                 age + race + eth + children + income + gender + 
                 victim + 
                 (1|zip))
summary(lmer.zip)
df.zip<-as.data.frame(VarCorr(lmer.zip))
icc.zip<-df.zip[1,4]/(df.zip[1,4]+df.zip[2,4]) #0.01917204


###################### by sid type ############################

icc.SID = NULL
icc.zip = NULL
for(type in unique(SID.type)){
  #SID level
  uncond.SID <- lmer(pi.jk~1 + (1|SID), data = for.icc[SID.type == type,])
  summary(uncond.SID)
  df.SID <- as.data.frame(VarCorr(uncond.SID))
  icc.SID[type] <- df.SID[1,4]/(df.SID[1,4]+df.SID[2,4]) 
}

####################with replication regression#####################
#                                                                  #
#        refer to "investigating pi's" file for this regression    #
#                                                                  #
####################################################################

names.rep<-strsplit(names(pi.replic),split="person")
has.dummies.replic = NULL
for(ii in 1:length(names.rep)){
  has.dummies.replic[ii]<-as.character(paste(unlist(names.rep[ii]),collapse=""))
}
dummy.people.replic<-data.frame(person = has.dummies.replic, 
                               pi.jk = pi.replic)

for.icc.replic<-join_all(list(for.pi,dummy.people.replic,geographies.to.merge))

detach(for.pi)
attach(for.icc.replic)

icc.replic.lm<-lmer(pi.jk~1 + (1|SID))
summary(icc.replic.lm)
df.replic<-as.data.frame(VarCorr(icc.replic.lm))
icc.replic<-df.replic[1,4]/(df.replic[1,4]+df.replic[2,4]) #0.1044779



