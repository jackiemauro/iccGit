require("mice")
require("Amelia")

require(foreign)

setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
hotspot<-read.spss("PostDrop09252014 - Copy.sav", to.data.frame=TRUE)
geo.id<-read.csv("geographic ids.csv")
mi.dat <- merge(hotspot, geo.id)

table.dup.hhid<-as.data.frame(table(mi.dat$household_ID))
dup.hhids<-table.dup.hhid$Var1[which(table.dup.hhid$Freq>1)]

data <- mi.dat[-which(mi.dat$household_ID %in% dup.hhids),]

sm.test <- data[,c(1, 7, 10, 24:35, 88, 370, 373, 393, 397, 398, 425, 460)]

pred.mat <- matrix(c(0,1,1,1,
                     0,0,0,0,
                     0,0,0,0,
                     0,0,0,0), ncol = 4, byrow = T)

sm <- data.frame(income = as.factor(data[1:2000,397]),
                 a7 = as.factor(data[1:2000,30]),
                 b7 = as.factor(data[1:2000,31]),
                 age = data[1:2000,425])

imp.sm <- mice(sm, predictorMatrix = pred.mat)
# hates it when categorical variables have missing values
# if you're using them as predictors.
# if using as predictors, change to "Blank" and it'll work

noNA <- as.matrix(sm)[,2:3] 
noNA[is.na(noNA)] <- "Blank"
sm2 <- data.frame(income = sm[,1], noNA, age = sm[,4])

imp.cat <- mice(sm2, predictorMatrix = pred.mat)


imp2 <- amelia(sm.test) #hates factors
