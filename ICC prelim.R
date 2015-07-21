library(foreign)
setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")

# choose dataset based on which way you want to code DK's and REF's
# hotspot file distinguishes between DK, REF and NA
# hotspot orig file codes all DK, REF, NA to NA

hotspot.orig<-read.spss("PostDrop09252014.sav", to.data.frame=TRUE)

hotspot<-read.spss("PostDrop09252014 - Copy.sav", to.data.frame=TRUE)
attach(hotspot)

# note: at least 1 household has more than 1 survey
# 3770 surveys in all, 3766 household id's
# 454 segments

library(lme4)
library(plyr)
library(ggplot2)

#level 1: individual
#level 2: segment
#level 3: hotspot type ("ColdSpot3")
###################### reshape variables ############

hotspot$race2<-ifelse(hotspot$race_138=="White","White",
                      ifelse(hotspot$race_138=="Black or African American", "Black",
                             "Other"))
income.binned <- factor(income_145,
                        levels = c("Less than $10,000","Between $10,001 and $25,000","Between $25,001 and $40,000",
                                   "Between $40,001 and $60,000","Between $60,001 and $80,000",
                                   "Between $80,001 and $100,000","More than $100,000"))

inc<-revalue(income.binned, c("Less than $10,000"="<10K","Between $10,001 and $25,000"="10-25K",
                              "Between $25,001 and $40,000"="25-40K",
                              "Between $40,001 and $60,000"="40-60K",
                              "Between $60,001 and $80,000"="60-80K",
                              "Between $80,001 and $100,000"="80-100K","More than $100,000"=">100K"))

safe.bi<-ifelse(safe_18a %in% c("Stongly Agree","Agree"),1,
                ifelse(safe_18a %in% c("Disagree","Strongly Disagree"),0,NA))
some.pols<-ifelse(num_police_40==0,0,
                  ifelse(num_police_40>=0,1,NA))

sid.n <-  cumsum(!duplicated(SID)) 

########### covariate recoding #######
# 
# phy_disord_21fred<-ifelse(phy_disord_21f==1,"None",
#                           ifelse(phy_disord_21f==2,"One or two",
#                                  ifelse(phy_disord_21f==3,"Many",
#                                         NA)))

#######some basic summary stats #######
tab1<-unique(hotspot[,c("SID","ColdSpot3")])
SID.hs<-as.data.frame(table(tab1$ColdSpot3))
