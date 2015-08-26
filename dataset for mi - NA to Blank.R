#######################################################################
# this file runs mice package on icc data, where categorical vars
# are changed to "Blank"
# first, estimate pi based on unimputed data
# then, change NA in categorical vars to "Blank"
# we will not impute categorical vars
# impute missing continuous vars
#
# REMAINING ISSUES: 
# 1. Age2 is not age squared: this needs to be resolved
# 2. Bootstrapping requires some finesse. Steps are given in 
#    Shao and Sitter 1996. 
#      * create dummies recording placement of missing values
#      * impute 1 dataset
#      * draw sample from imputed dataset by sampling >= 2 Sids w/ replacement
#        from each Zip then sample people w/ replacement from the sampled Sids
#      * knock out values that "ought" to be missing 
#        (same place as truly missing using dummies)
#      * rerun same imputation
######################################################################



setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")

require(foreign)
require(mice)
require(lme4)
require(plyr)

source("icc reg function.R") # function to run lmer regs and output ICCs
source("bootstrap and imputing function.R") #bootstrap and impute fn 
source("Yadav.R") #tests standard error methods



################## load raw dataset and clean ##########################
hotspot<-read.spss("PostDrop09252014 - Copy.sav", to.data.frame=TRUE)
geo.id<-read.csv("geographic ids.csv")

merge.geo = merge(hotspot, geo.id)

attach(merge.geo)

refusedInc.educ = ifelse(income_145 == "Refused", school_comp_124, 0)

reduced<-data.frame(SID=as.factor(SID),person=as.factor(household_ID),
                    q1=as.numeric(cohesion_6a), q2=as.numeric(cohesion_6b), 
                    q3=as.numeric(cohesion_6c), q4=as.numeric(cohesion_6d),
                    q5=as.numeric(cohesion_6e), q6=as.numeric(cohesion_6f),
                    q7=as.numeric(soc_control_7a),q8=as.numeric(soc_control_7b),
                    q9=as.numeric(soc_control_7c),q10=as.numeric(soc_control_7d),
                    q11=as.numeric(soc_control_7e), q12=as.numeric(soc_control_7f),
                    ed =  school_comp_124, marital =  marital_127, 
                    working =  you_working_132, work.type =  work_type_134, 
                    age =  age, age2 =  age^2, race =  race_138, 
                    eth =  ethnicity_137, children =  minors_live_142,
                    income =  income_145, inc.ed = factor(refusedInc.educ),
                    gender =  gender_146, victim =  crime_victim_23, 
                    yrs.in.nbh =  lived_yrs_2a, zip =  as.factor(Zip.Code), 
                    SID.type = ColdSpot3) 

detach(merge.geo)
attach(reduced)

#need to reverse code negatives to be consistent
reduced$q2.rev = reduced$q2*(-1) + 5
reduced$q4.rev = reduced$q4*(-1) + 5
reduced$q5.rev = reduced$q5*(-1) + 5

# replace -99 with NA for years in nbh
reduced$yrs.in.nbh[reduced$yrs.in.nbh == -99] <- NA

# 4 households have >1 row, excluding these
# takes you from 3770 to 3762 rows
table.dup.hhid<-as.data.frame(table(reduced$person))
dup.hhids<-table.dup.hhid$Var1[which(table.dup.hhid$Freq>1)]
reduced<-reduced[-which(reduced$person %in% dup.hhids),]

# get rid of unreversed codes 
# so they don't figure into dummy calc
drops<-c("q2","q4","q5")
reduced<-reduced[, -which(names(reduced) %in% drops)] 

# for now, drop 112 year old
# now 3761 individuals
reduced <- reduced[-which(reduced$age > 100),]

# create dataset with no NA's in categorical vars and Q1-Q12
leave.NA <- c("SID", "person", "q1", "q2.rev", "q3", "q4.rev",
              "q5.rev", "q6", "q7", "q8", "q9", "q10", "q11",
              "q12", "age", "age2", "children", "yrs.in.nbh")
noNA <- as.matrix(reduced)[,-which(names(reduced) %in% leave.NA)] 
noNA[is.na(noNA)] <- "Blank"
reduced.noNA <- data.frame(reduced[,which(names(reduced) %in% leave.NA)], noNA)

#################### calculate pi's ################################
detach(reduced)
attach(reduced.noNA)
# transform data to get binary answer-person combinations
tab1<-as.data.frame(table(q1,person,useNA="always"))
names(tab1)<-c("Answer","person","Q1")
tab3<-as.data.frame(table(q3,person,useNA="always"))
names(tab3)<-c("Answer","person","Q3")
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
pre<-join_all(list(tab1,tab3,tab6,tab7,tab8,
                   tab9,tab10,tab11,tab12,tab2.rev, tab4.rev, tab5.rev))
pre$sum<-rowSums(pre[,3:length(pre)])


#also ended up with an NA at the end from table process, dropping
pre <- pre[-which(is.na(pre$person)),]
# 5 extra people in pre
# get rid of entries with 0 answers to all questions
# gets you down to 3753 people
# this means 7 people will have NA for pi
pre <- pre[which(!is.na(pre$sum)),]
pre <-pre[which(pre$sum != 0), ] 
piCalc <- pre
detach(reduced.noNA)
attach(piCalc)

# get pi's
pi.reg <- lm(as.numeric(Answer)~person+Q1+Q2.rev+Q3+Q4.rev+Q5.rev+Q6+Q7+Q8+Q9+Q10+Q11-1)
pi67 <-pi.reg$coefficients[1:(length(pi.reg$coefficients)-11)] 

# rejoin to dataset
# get person name associated with coef
test<-strsplit(names(pi67),split="person")
has.dummies = NULL
for(ii in 1:length(test)){
  has.dummies[ii]<-as.character(paste(unlist(test[ii]),collapse=""))
}
dummy.people<-data.frame(person = has.dummies, pi.jk = pi67)

noNA.df <- merge(reduced.noNA, dummy.people, all.x = TRUE)

################################ run imputation ##########################
for.mi <- noNA.df[,-(names(noNA.df) == "person")]
ini <- mice(for.mi, max = 0, print = FALSE)
meth <- ini$meth
# meth["age2"] <- "~age^2" doesn't seem to work
pred <- ini$pred                            # create prediction matrix
# # pred <- quickpred(test, include = "SID")
# for(ii in 1:dim(pred)[1]){                    # avoid multicollinearity
#   if(ii %% 2 == 0){pred[ii, "age"] <-0 }      # can't have age and age2 both
#   else{pred[ii, "age2"] <- 0}                 # 1 in matrix b/c lin dependent
# }
# pred["age","age2"] <- 0                       # same as above, collinearity

imp.pi <- mice(for.mi, meth = meth, pred = pred, seed = 0, maxit = 1)

# save and output files
write.table(complete(imp.pi), file = "imputed_mice_1.csv")
write.table(complete(imp.pi,2), file = "imputed_mice_2.csv")
write.table(complete(imp.pi,3), file = "imputed_mice_3.csv")
write.table(complete(imp.pi,4), file = "imputed_mice_4.csv")
write.table(complete(imp.pi,5), file = "imputed_mice_5.csv")

plot(imp.pi)

require(lattice)
densityplot(imp.pi)
# pi not looking great and age2 a complete failure

regs1 <- with(imp.pi, lmer(pi.jk ~ 1 + ed + marital + working + work.type +
                    age + age2 + race + eth + children + income + 
                    inc.ed + gender + victim + yrs.in.nbh + (1|SID) + (1|zip)))
est <- pool(regs1)


######################## bootstrapping #####################
# get matrix with placement of missing values
# only variables in set 1 should have missings
# all other columns should just be 0
z.mat <- matrix(rep(0, dim(noNA.df)[1]*dim(noNA.df)[2]), ncol = dim(noNA.df)[2])
colnames(z.mat) <- names(noNA.df)
z.mat[is.na(noNA.df)] <- 1 
miss.mat <- as.data.frame(z.mat)

# import imputed dataset
dat <- read.table("outputs/imputed_mice_1.csv")

# initial estimates - cold and cool very low, others 5-7%
cov.list = "+ ed + marital + working + work.type + 
            age + age2 + race + eth + children + income + 
            inc.ed + gender + victim + yrs.in.nbh"
get.icc.fn(dat, covariates = cov.list, zip.incl = TRUE)
get.icc.fn(dat, covariates = cov.list, zip.incl = TRUE, block.type = "Cold Spot")
get.icc.fn(dat, covariates = cov.list, zip.incl = TRUE, block.type = "Cool Spot")
get.icc.fn(dat, covariates = cov.list, zip.incl = TRUE, block.type = "Drug Spot")
get.icc.fn(dat, covariates = cov.list, zip.incl = TRUE, block.type = "Violent Spot")
get.icc.fn(dat, covariates = cov.list, zip.incl = TRUE, block.type = "Combined")

# bootstrapping
n = dim(dat)[1]

# get sample w replacement and make dataset
rows = sample(c(1:n), n, replace = T)
b1 <- dat[rows,]
b1[miss.mat == 1] <- NA

# impute sample dataset
ini <- mice(b1, max = 0, print = FALSE)
meth <- ini$meth
pred <- ini$pred                            

imp.b1 <- mice(b1, meth = meth, pred = pred, seed = 0, maxit = 1)
b1.imputed <- complete(imp.b1)

# get statistic of interest from imputed data
# fn in "icc reg function" file
get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE)
get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Cold Spot")
get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Cool Spot")
get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Drug Spot")
get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Violent Spot")
get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Combined")

# test10 <- boots.imputed.icc(10, noNA.df)
# this is such a mysterious bug, refuses to make a new sample after 2 or 3 iterations
