###########################################################
#                                                         #
#  this function outputs ICC's for SID and zip if it's    #
#  included. It requires the user to do each type one by  #
#  one. You also have to input your covariates in the form#
#  " + cov1 + cov2 + cov3 + ... + covn" cause I don't feel#
#  like figuring out how to parse a nice list             #
#  still, i feel like this is an improvement.             #
#                                                         #  
#  This fn essentially replaces the files named           #
#  "questions 6 and 7 - iccs no NA.R" and                 #
#  "questions 6 and 7 - iccs.R"                           #
#  except those ones will still output to excel.          #
#                                                         #
###########################################################

# usual covariates
cov.list = "+ ed + marital + working + work.type +
            age.noNA + age.noNA.sq + race + eth + children.noNA + income + 
            inc.ed + gender + victim + yrs.nbh.noNA"


get.icc.fn <- function(data, 
                       covariates = FALSE, 
                       zip.incl = FALSE,
                       block.type = FALSE){
  if(block.type != FALSE) {
    d = data[data$SID.type == block.type,]
  } else {
    d = data
  }
  reg.list = "1 + (1|SID)"
  if(covariates != FALSE){
    reg.list = paste(reg.list, covariates)
  }
  if(zip.incl == TRUE) {
    reg.list = paste(reg.list, " + (1|zip)")
    reg = eval(parse(text = paste("lmer(pi.jk ~ ", reg.list, ", data = d)")))
    df <- as.data.frame(VarCorr(reg))
    icc.sid <- df[1,4]/sum(df[1:3,4]) 
    icc.zip <- df[2,4]/sum(df[1:3,4]) 
    icc.out = list(df, c(icc.sid, icc.zip))
  }
  else{
    reg = eval(parse(text = paste("lmer(pi.jk ~ ", reg.list, ")")))
    df <- as.data.frame(VarCorr(reg))
    icc <- df[1,4]/sum(df[1:2,4]) 
    icc.out <- list(df, icc)
  }
  return(icc.out)
}