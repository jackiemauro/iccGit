for.table <- unique(for.icc67.noNA[,c(2, 16:36)])

attach(for.table)


cov.output <- rbind(table(ed, SID.type),
      c("Marital", "", "", "", ""),
      table(marital, SID.type),
      c("Working", "", "", "", ""),
      table(working, SID.type),
      c("Work type", "", "", "", ""),
      table(work.type, SID.type),
      c("Age", "", "", "", ""),
      table(age.noNA, SID.type),
      c("Race", "", "", "", ""),
      table(race, SID.type),
      c("Ethnicity", "", "", "", ""),
      table(eth, SID.type),
      c("Children", "", "", "", ""),
      table(children.noNA, SID.type),
      c("Income", "", "", "", ""),
      table(income, SID.type),
      c("Inc*ed", "", "", "", ""),
      table(inc.ed, SID.type),
      c("Gender", "", "", "", ""),
      table(gender, SID.type),
      c("Victim", "", "", "", ""),
      table(victim, SID.type),
      c("Years in nbh", "", "", "", ""),
      table(yrs.nbh.noNA, SID.type)
      )

write.csv(cov.output, file = "covariates output noNA.csv")
