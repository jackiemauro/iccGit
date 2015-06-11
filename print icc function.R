#print icc function
#dinky function that takes a regression and prints the icc

print.icc <- function(regression){
  df <- as.data.frame(VarCorr(regression))
  icc1 <- df[1,4]/sum(df[,4])
  if(dim(df)[1] == 3){
    icc2 <- df[2,4]/sum(df[,4])
    iccs <- c(icc1, icc2)
  } else {
    iccs = icc1
  }
  iccs
}
