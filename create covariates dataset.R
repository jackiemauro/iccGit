# create covariates dataset for all regression
# will merge this dataset with pi's for given
# question

setwd("C:/Users/jackie/Desktop/own research/icc")

geo.id<-read.csv("geographic ids.csv")
merge.geo<-merge(hotspot,geo.id)

attach(merge.geo)

# interaction refused income * education
refusedInc.educ = ifelse(income_145 == "Refused", school_comp_124, 0)

covs.set      <-       data.frame(SID= SID,
                                  SID.type =  ColdSpot3,
                                  person = household_ID,
                                  ed =  school_comp_124,
                                  marital =  marital_127,
                                  working =  you_working_132,
                                  work.type =  work_type_134,
                                  age =  age,
                                  age2 =  age^2,
                                  race =  race_138,
                                  eth =  ethnicity_137,
                                  children =  minors_live_142,
                                  income =  income_145,
                                  inc.ed = factor(refusedInc.educ),
                                  gender =  gender_146,
                                  victim =  crime_victim_23,
                                  #self.report =  ,
                                  #self.control =  ,
                                  yrs.in.nbh =  lived_yrs_2a,
                                  tract= Census.Tract,
                                  block.grp= Block.Group,
                                  state.fips= State.FIPS,
                                  county.fips= County.FIPS,
                                  zip =  Zip.Code) 

detach(merge.geo)
