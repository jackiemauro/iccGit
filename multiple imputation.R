##################################################
#                                                #
# multiple imputation for continuous variables   #
#                                                #
##################################################

require(mice)
require(mi)
require(foreign)
require(plyr)


######################### dataset ######################
setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
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
                    yrs.in.nbh =  lived_yrs_2a, zip =  as.factor(Zip.Code)) 

detach(merge.geo)
attach(reduced)

# 4 households have >1 row, excluding these
# takes you from 3770 to 3762 rows
table.dup.hhid<-as.data.frame(table(reduced$person))
dup.hhids<-table.dup.hhid$Var1[which(table.dup.hhid$Freq>1)]
reduced<-reduced[-which(reduced$person %in% dup.hhids),]

# for now, drop 112 year old
# now 3761 individuals
reduced <- reduced[-which(reduced$age > 100),]

###################### using mice package ##################

ptm <- proc.time()
imp1 <- mice(reduced[1:400,], m=1, predictionMatrix = pred)
proc.time() - ptm
# without changing to "Blank", this throws weird errors
# it can't deal with a dataset where a category is empty
# ie if no one in the subset makes >100K, it breaks
# but even if you deal with that it gives you a 
# "too many weights" error -- which doesn't seem fixable
# without making really slow code

# non-continuous vars
# this process imputes *ONLY* the continuous variables
# we change all the missing categorical variables to
# a category called "Blank" so that the code doesn't
# try to impute them
factors <- reduced[,-c(3:14,19,20,23,28)]
conts <- reduced[,c(3:14,19,20,23,28)]
noNA <- as.matrix(factors)
noNA[is.na(noNA)] <- "Blank"
sm <- data.frame(noNA, conts)

library(Matrix)
# prediction matrix asks it to impute only the last 4 variables
pred.mat <- matrix(c(rep(1,29*29)), ncol = ncol(sm))
diag <- Diagonal(n=29, x = -1)
pred <- pred.mat + diag
pred[1:25,] <- 0

ptm <- proc.time()
imp2 <- mice(sm[500:600,], m=1, predictionMatrix = pred)
proc.time() - ptm
# cannot deal with missingness pattern in q1-q12
# even if we change all the categorical NA's to "Blank"
# but everything I've read says you need to keep all 
# the variables you'll use in analysis in your imputation


# person is useless, take a large enough sample that there are no empty
# categories. Check working and ed especially for this (DK and Ref).
# two options for datasets, first is larger, check for empties
test <- reduced[c(sample(c(1:dim(reduced)[1]), 1500, replace = F)),-c(2)] 
test <- reduced[1:500,-c(2,18)] 


md.pattern(test)

#parameters
order = c()              
method = c("", rep("norm",12), "polyreg", "polyreg", "polyreg",
           "polyreg","norm", "~age", "polyreg", "polyreg",
           "norm", rep("polyreg",3), "logreg","", "polyreg")

pred <- quickpred(test)
# pred <- quickpred(test, include = "SID")
for(ii in 1:dim(pred)[1]){                 # avoid multicollinearity
  if(ii %% 2 == 0){pred[ii, "age"] <-0 }
  else{pred[ii, "age2"] <- 0}
}
pred["age","age2"] <- 0

ptm <- proc.time()
imp2 <- mice(test, m=1, seed = 0, meth = method, 
             pred = pred, maxit = 2, MaxNWts = 2000)
proc.time() - ptm
# again, breaks if any category is empty

########### try with gelman ##############
require(mi)

#first, leave categoricals with NA
gel <- missing_data.frame(reduced[1:100,])

gel <- change(gel, y = "q8", what = "type", to = "continuous")
gel <- change(gel, y = "q10", what = "type", to = "continuous")
gel <- change(gel, y = "q11", what = "type", to = "continuous")
#gel <- change(gel, y = "income_145", what = "type", to = "ordered")


imp.mi1 <- mi(gel)
# subscript out of bounds error

#change missing categoricals to blank
gel2 <- missing_data.frame(sm[1:100,])

gel2 <- change(gel2, y = "q8", what = "type", to = "continuous")
gel2 <- change(gel2, y = "q10", what = "type", to = "continuous")
gel2 <- change(gel2, y = "q11", what = "type", to = "continuous")
#gel2 <- change(gel2, y = "income_145", what = "type", to = "ordered")


imp.mi2 <- mi(gel2) 
#still not working: says to start with different seed
#haven't had any luck setting seed to other values

############# iterative -- found a way! ################
# but changing sid and person to continuous probs a nono

n = dim(reduced)[1]
u.sid <- unique(reduced$SID)
n.sid <- c(1:length(u.sid))
sids <- data.frame(u.sid, n.sid)
red2 <- merge(reduced, sids, by.x = "SID", by.y = "u.sid")

red_dk <- data.frame(person = c(1:n), red2[1:n, c(3:17,19:30)])
# fails if you include worktype: too many missings
# fails if you add person (why?????) but not if you just number them...
# fails if you add SID but can replace with numbers
# fails if you leave DK (why?????)
red_dk$eth <- revalue(red_dk$eth, c("DK"="dontknow"))
red_dk$income <- revalue(red_dk$income, c("DK"="dontknow"))
red_dk$ed <- revalue(red_dk$ed, c("DK"="dontknow"))
red_dk$marital <- revalue(red_dk$marital, c("DK"="dontknow"))
red_dk$working <- revalue(red_dk$working, c("DK"="dontknow"))
red_dk$race <- revalue(red_dk$race, c("DK"="dontknow"))
red_dk$gender <- revalue(red_dk$gender, c("DK"="dontknow"))

gel3 <- missing_data.frame(red_dk)
imp.mi3 <- mi(gel3, max.minutes = 20)  # may have to remove time limit
imp.dfs3 <- complete(imp.mi3, 3)
View(imp.dfs3)
write.table(imp.dfs3, file = "imputed_df.csv")
write.table(imp.dfs3[1], file = "imputed_df_chain1.csv")
write.table(imp.dfs3[2], file = "imputed_df_chain2.csv")
write.table(imp.dfs3[3], file = "imputed_df_chain3.csv")

############### test to see if you can keep person & sid categorical #####
test <- reduced[1:100,-c(2,13:14,18)] 
# if you get rid of 2,3,4,18 works
# get rid of person, has no value
test$eth <- revalue(test$eth, c("DK"="dontknow"))
test$income <- revalue(test$income, c("DK"="dontknow"))
test$ed <- revalue(test$ed, c("DK"="dontknow"))
test$marital <- revalue(test$marital, c("DK"="dontknow"))
test$working <- revalue(test$working, c("DK"="dontknow"))
test$race <- revalue(test$race, c("DK"="dontknow"))
test$gender <- revalue(test$gender, c("DK"="dontknow"))

gel_cat <- missing_data.frame(test)
test_imp <- mi(gel_cat, n.chains = 2, n.iter = 10, max.minutes = 20)

################ can we make hotdecking work? ##############
# yrs.in.nbh has no missing values, don't put in DATA?


