# Create the data set for questions 6 and 7

setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
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

# transform data to get binary answer-person combinations
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

#also ended up with an NA at the end from table process, dropping
pre.no.dup <- pre.no.dup[-which(is.na(pre.no.dup$person)),]

detach(reduced)
attach(pre.no.dup)

# get rid of sum and unreversed codes 
# so they don't figure into dummy calc
drops<-c("Q2","Q4","Q5","sum")
for.pi67<-pre.no.dup[pre$sum != 0, !names(pre.no.dup) %in% drops] 
detach(pre.no.dup)
attach(for.pi67)

if(any(is.na(for.pi67$person))){
  for.pi67 <- for.pi67[-which(is.na(for.pi67$person)),]
}

# get pi's
pi.reg <- lm(as.numeric(Answer)~person+Q1+Q2.rev+Q3+Q4.rev+Q5.rev+Q6+Q7+Q8+Q9+Q10+Q11-1)
pi67 <-pi.reg$coefficients[1:(length(pi.reg$coefficients)-11)] 

#################### dataset for regression ##############
# merge datasets

# make sure all subjects are in both sets
test<-strsplit(names(pi67),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people<-data.frame(person = has.dummies, pi.jk = pi67)


# merge dummies, pi's and geographies
for.icc67 <- join_all(list(for.pi67, dummy.people, covs.set))

# replace -99 with NA for years in nbh
for.icc67$yrs.in.nbh[for.icc67$yrs.in.nbh == -99] <- NA

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
yrs.nbh.noNA[yrs.nbh.noNA == -99] <- NA
yrs.nbh.noNA[is.na(yrs.nbh.noNA)] <- mean(yrs.nbh.noNA, na.rm = T)

noNA <- as.data.frame(noNA)
for.icc67.noNA <- cbind(for.icc67[,1:17],for.icc67[,32:36],
                        noNA, age.noNA, age.noNA.sq, children.noNA, yrs.nbh.noNA)
remove(age.noNA)
remove(age.noNA.sq)
remove(children.noNA)
remove(yrs.nbh.noNA)

detach(for.pi67)
