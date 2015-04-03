setwd("C:/Users/jackie/Desktop/own research/icc")
source("ICC prelim.R")
library(XLConnect)

geo.id<-read.csv("geographic ids.csv")

#when dealing with this file, make sure other things like merge.geo are not attached

attach(geo.id)
geo.id$indiv.block<-paste (Census.Tract,Block.Group, sep = ".", collapse = NULL)

hhID = data.frame(SID = hotspot$SID, hhID = hotspot$household_ID)

geo.id = merge(geo.id, hhID)

#housesholds / SID
all.SID.hhID<-table(geo.id$hhID,geo.id$SID)
SID.hhID<-as.data.frame(margin.table(all.SID.hhID,2))
hist(SID.hhID$Freq, main="households per SID",xlab="Counts", 
     labels = T, ylim = c(0, 200))
summary(SID.hhID$Freq)
stem(SID.hhID$Freq)

#segments / block group

all.blocks<-table(SID,indiv.block)
block<-as.data.frame(margin.table(all.blocks,2))
hist(block$Freq, main="SID's per block group",xlab="Counts")
summary(block$Freq)

#segments / tract

all.tracts<-table(SID,Census.Tract)
tract<-as.data.frame(margin.table(all.tracts,2))

hist(tract$Freq, main="SID's per census tract",xlab="Counts",xlim=c(0,10))
summary(tract$Freq)

#check on tracts with few SID's
# few.pertract<-tract[which(tract$Freq<=3),]
# table3.pertract<-table(few.pertract$SID,few.pertract$ColdSpot3)

#block gps / tract
tab3<-unique(geo.id[,c("Census.Tract","Block.Group")])
tr.bl<-as.data.frame(table(tab3$Census.Tract))

hist(tr.bl$Freq, main="block gps per census tract",xlab="Counts")
summary(tr.bl$Freq)

#segments / zip

all.zip<-table(SID,Zip.Code)
zip<-as.data.frame(margin.table(all.zip,2))
hist(zip$Freq, main="SID's per zip code",xlab="Counts", 
     labels = T, ylim = c(0,15))
summary(zip$Freq)
stem(zip$Freq)

all.zip.hhID<-table(geo.id$hhID,geo.id$Zip.Code)
zip.hhID<-as.data.frame(margin.table(all.zip.hhID,2))
hist(zip.hhID$Freq, main="households per zip code",xlab="Counts", 
     labels = T, ylim = c(0, 9), xlim = c(0,450))
summary(zip.hhID$Freq)
stem(zip.hhID$Freq)


detach(geo.id)
