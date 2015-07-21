##################################################
#                                                #
# multiple imputation for continuous variables   #
#                                                #
##################################################

require(mice)

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


