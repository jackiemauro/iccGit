#Perceptions of crime and disorder

setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
source("ICC prelim.R")
require(plyr)
library(base)

#reduce data frame to just questions we're interested in
reduced<-data.frame(SID=SID,person=household_ID,q1=soc_disord_20a,
                    q2=soc_disord_20b, q3=soc_disord_20c, q4=soc_disord_20d,
                    q5=soc_disord_20e, q6=soc_disord_20f, q7=soc_disord_20g,
                    q8=soc_disord_20h)
detach(hotspot)


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
tab8<-as.data.frame(table(q8,person,useNA="always"))
names(tab8)<-c("Answer","person","Q8")


#limiting to rows with some answer
pre<-join_all(list(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8))
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
pi.reg <- lm(as.numeric(Answer)~person+Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8-1)
pi <- pi.reg$coefficients[1:(length(pi.reg$coefficients)-8)] 
alphas <- pi.reg$coefficients[(length(pi.reg$coefficients)-7):length(pi.reg$coefficients)] 

# find icc
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

# merge dummies, pi's and geographies
for.icc <- join_all(list(for.pi,dummy.people,geographies.to.merge))

detach(for.pi)
attach(for.icc)