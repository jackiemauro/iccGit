#Feelings about the law

setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
source("ICC prelim.R")
require(plyr)
library(base)

#reduce data frame to just questions we're interested in
reduced<-data.frame(SID=SID,person=household_ID,q1=police_legit_41a,
                    q2=police_legit_41b, q3=police_legit_41c, q4=police_legit_41d,
                    q5=police_legit_41e, q6=police_legit_41f, q7=police_legit_41g)
detach(hotspot)

n = dim(reduced)[2] - 2

#replace all the non-responses with NA
for(ii in 1:length(reduced)){
  reduced[,ii] = ifelse(reduced[,ii] %in% c("DK", "Refused", "REF", NA),
                        NA, reduced[,ii])
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
tab6<-as.data.frame(table(q6,person,useNA="always"))
names(tab6)<-c("Answer","person","Q6")
tab7<-as.data.frame(table(q7,person,useNA="always"))
names(tab7)<-c("Answer","person","Q7")


#limiting to rows with some answer
pre<-join_all(list(tab1,tab2,tab3,tab4,tab5,tab6,tab7))
pre$sum<-rowSums(pre[,3:length(pre)])

#4 households have >1 row, excluding these
table.dup.hhid<-as.data.frame(table(hotspot$household_ID))
dup.hhids<-table.dup.hhid$Var1[which(table.dup.hhid$Freq>1)]
which(pre$person %in% dup.hhids)
pre.no.dup<-pre[-which(pre$person %in% dup.hhids),]

detach(reduced)
attach(pre.no.dup)

# get rid of sum 
for.pi<-pre.no.dup[pre$sum != 0, -(length(pre.no.dup))] 
detach(pre.no.dup)
attach(for.pi)

if(any(is.na(for.pi$person))){
  for.pi <- for.pi[-which(is.na(for.pi$person)),]
}

# find pi's
pi.reg <- lm(as.numeric(Answer)~person+Q1+Q2+Q3+Q4+Q5+Q6+Q7-1)
pi.41 <- pi.reg$coefficients[1:(length(pi.reg$coefficients)-n)] 
alphas <- pi.reg$coefficients[(length(pi.reg$coefficients)-(n - 1)):length(pi.reg$coefficients)] 

# find icc
source("create covariates dataset.R")

# make sure all subjects are in both sets
test<-strsplit(names(pi.41),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people<-data.frame(person = has.dummies, pi.jk = pi.41)

# merge dummies, pi's and geographies
for.icc.41 <- join_all(list(for.pi,dummy.people,covs.set))

detach(for.pi)
attach(for.icc.41)