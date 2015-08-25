####################################
# fn should follow shao process
# take imputed dataset
# create sample from that
# re-impute
# find ICC values



test <- function(its, orig){
  # list of cov's: hardcoded for now
  cov.list = "+ ed + marital + working + work.type +
            age + age2 + race + eth + children + income + 
            inc.ed + gender + victim + yrs.in.nbh"
  
  # get matrix with placement of missing values
  miss.mat <- orig[,c(3:18,31)]
  miss.mat[!is.na(miss.mat)] <- 0
  miss.mat[is.na(miss.mat)] <- 1
  miss.mat2 <- data.frame(orig[,2], miss.mat[,1:16], orig[,19:30], pi.jk = miss.mat[,17])
  miss.mat2[,c(1:2,19:30)] <- 0
  
  # import imputed dataset (also hardcoded for now)
  dat <- read.table("imputed_mice_1.csv")
  n = dim(dat)[1]
  all = NULL; 
  rows = NULL
  for(ii in 1:its){
    rows[[ii]] = sample(c(1:n), n, replace = T)
  }
#   cold = NULL; cool = NULL; drug = NULL; viol = NULL; combo = NULL
  for(it in 1:its){
    # get sample w replacement and make dataset
    b1 <- dat[rows[[it]],]
    b1[miss.mat2 == 1] <- NA
    
    # impute sample dataset
    ini <- mice(b1, max = 0, print = FALSE)
    meth <- ini$meth
    pred <- ini$pred                            # create prediction matrix
    
    imp.b1 <- mice(b1, meth = meth, pred = pred, seed = 0, maxit = 1, m = 1)
    b1.imputed <- complete(imp.b1)
    all[it] = sum(b1.imputed[,2])
  }
  return(all)
}




boots.imputed.icc <- function(its, orig){
  # list of cov's: hardcoded for now
  cov.list = "+ ed + marital + working + work.type +
            age + age2 + race + eth + children + income + 
            inc.ed + gender + victim + yrs.in.nbh"
  
  # get matrix with placement of missing values
  miss.mat <- orig[,c(3:18,31)]
  miss.mat[!is.na(miss.mat)] <- 0
  miss.mat[is.na(miss.mat)] <- 1
  miss.mat2 <- data.frame(orig[,2], miss.mat[,1:16], orig[,19:30], pi.jk = miss.mat[,17])
  miss.mat2[,c(1:2,19:30)] <- 0
  
  # import imputed dataset (also hardcoded for now)
  dat <- read.table("imputed_mice_1.csv")
  n = dim(dat)[1]
  all = NULL; cold = NULL; cool = NULL; drug = NULL; viol = NULL; combo = NULL
  rows = NULL
  for(ii in 1:its){
    rows[[ii]] = sample(c(1:n), n, replace = T)
  }
  for(it in 1:its){
    # get sample w replacement and make dataset
    b1 <- dat[rows[[it]],]
    b1[miss.mat2 == 1] <- NA
    
    # impute sample dataset
    ini <- mice(b1, max = 0, print = FALSE)
    meth <- ini$meth
    pred <- ini$pred                            # create prediction matrix
    
    imp.b1 <- mice(b1, meth = meth, pred = pred, seed = 0, maxit = 1, m = 1)
    b1.imputed <- complete(imp.b1)
    
    # get statistic of interest from imputed data
    # fn in "icc reg function" file
    all[it] = get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE)[[2]][1]
    cold[it] = get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Cold Spot")[[2]][1]
    cool[it] = get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Cool Spot")[[2]][1]
    drug[it] = get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Drug Spot")[[2]][1]
    viol[it] = get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Violent Spot")[[2]][1]
    combo[it] = get.icc.fn(b1.imputed, covariates = cov.list, zip.incl = TRUE, block.type = "Combined")[[2]][1]
    
    remove(b1, b1.imputed, imp.b1, ini, meth, pred)
   }
  results = list(all, cold, cool, drug, viol, combo)
  return(results)
}
