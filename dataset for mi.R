# dataset creation for imputation
# want to keep all the info but also have everything clean

require("mi")
require(foreign)

setwd("C:/Users/jackie/Desktop/own research/icc/iccGit")
hotspot<-read.spss("PostDrop09252014 - Copy.sav", to.data.frame=TRUE)
geo.id<-read.csv("geographic ids.csv")
mi.dat <- merge(hotspot, geo.id)

table.dup.hhid<-as.data.frame(table(mi.dat$household_ID))
dup.hhids<-table.dup.hhid$Var1[which(table.dup.hhid$Freq>1)]

data <- mi.dat[-which(mi.dat$household_ID %in% dup.hhids),]

sm.test <- data[,c(1, 7, 10, 24:35, 88, 370, 373, 393, 397, 398, 425, 460)]

sm <- missing_data.frame(sm.test)

sm <- change(sm, y = "SID", what = "type", to = "un")
sm <- change(sm, y = "cohesion_6a", what = "type", to = "continuous")
sm <- change(sm, y = "cohesion_6b", what = "type", to = "continuous")
sm <- change(sm, y = "cohesion_6c", what = "type", to = "continuous")
sm <- change(sm, y = "cohesion_6d", what = "type", to = "continuous")
sm <- change(sm, y = "cohesion_6e", what = "type", to = "continuous")
sm <- change(sm, y = "cohesion_6f", what = "type", to = "continuous")
sm <- change(sm, y = "soc_control_7a", what = "type", to = "continuous")
sm <- change(sm, y = "soc_control_7b", what = "type", to = "continuous")
sm <- change(sm, y = "soc_control_7c", what = "type", to = "continuous")
sm <- change(sm, y = "soc_control_7d", what = "type", to = "continuous")
sm <- change(sm, y = "soc_control_7e", what = "type", to = "continuous")
sm <- change(sm, y = "soc_control_7f", what = "type", to = "continuous")
sm <- change(sm, y = "marital_127", what = "type", to = "un")
sm <- change(sm, y = "income_145", what = "type", to = "ordered")
sm <- change(sm, y = "Zip.Code", what = "type", to = "un")


impt <- mi(sm)

miss <- missing_data.frame(mi.dat)
impt <- mi(miss)
