# archival. use instead icc reg function


get.icc.fn <- function(data, covariates = FALSE, zip.incl = FALSE, block.type = FALSE){
  if(block.type != FALSE){
    data = data[which(data$SID.type == block.type),]
  }
  attach(data)
  reg.list = "1 + (1|SID)"
  if(covariates != FALSE){
    reg.list = paste(reg.list, covariates)
  }
  if(zip.incl == TRUE) {
    reg.list = paste(reg.list, " + (1|zip)")
    reg = eval(parse(text = paste("lmer(pi.jk ~ ", reg.list, ")")))
    df <- as.data.frame(VarCorr(reg))
    icc.sid <- df[1,4]/sum(df[1:3,4]) 
    icc.zip <- df[2,4]/sum(df[1:3,4]) 
    icc.out = c(icc.sid, icc.zip)
  }
  else{
    reg = eval(parse(text = paste("lmer(pi.jk ~ ", reg.list, ")")))
    df <- as.data.frame(VarCorr(reg))
    icc.out <- df[1,4]/sum(df[1:2,4]) 
  }
  detach(data)
  return(icc.out)
}