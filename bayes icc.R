######################################
# Can I estimate all 3 regs at once? #
######################################

source("ICC prelim.R")

#if need be, run "questions 6 and 7 - dataset creation.R" up to line 105
for.bayes <- join_all(list(for.pi67, covs.set))

# bayesian style
require(R2jags)
require(rube)
attach(for.bayes)

# get a counter for person so that you can put it in model
p.row = c(1, rep(NA, (dim(for.bayes)[1] - 1)))
for(i in 2:dim(for.bayes)[1]){
  if(person[i] == person[(i-1)]){p.row[i] = p.row[(i-1)]}  
  else(p.row[i] = p.row[(i-1)] + 1)
}

# counter for SID is harder, needs to be length of person
# need to get rid of duplicates in hotspot
u.person = unique(person)
t = hotspot$SID[-which(hotspot$household_ID %in% dup.hhids)]
s.row = c(rep(NA, length(u.person)))
for(i in 1:length(u.person)){
  s.row[i] = which(unique(SID) == t[i])
}

ans = as.numeric(Answer)
for.bayes <- data.frame(for.bayes, ans, p.row)

M1 <- "model{
  #no covs
  for(i in 1:n){
    # one answer per row
    ans[i] ~ dnorm(d[i], sig3)
    d[i] <- pi[p.row[i]]*person[i] +
      b1*Q1[i] + b2*Q2.rev[i] + b3*Q3[i] + b4*Q4.rev[i] + b5*Q5.rev[i] + 
      b6*Q6[i] + b7*Q7[i] + b8*Q8[i] + b9*Q9[i] + b10*Q10[i] + b11*Q11[i]
    }
  
  for(j in 1:m){
    # one pi per person
    pi[j] ~ dnorm(eta[s.row[j]], sig2)
  }

  for(k in 1:l){
    # one eta per street segment
    eta[k] ~ dnorm(gam, sig1)
  }
  
  sig1 ~ dgamma(.001, .001)
  sig2 ~ dgamma(.001, .001)
  sig3 ~ dgamma(.001, .001)
  gam ~ dnorm(0, .00001)
  b1 ~ dnorm(0, .00001)
  b2 ~ dnorm(0, .00001)
  b3 ~ dnorm(0, .00001)
  b4 ~ dnorm(0, .00001)
  b5 ~ dnorm(0, .00001)
  b6 ~ dnorm(0, .00001)
  b7 ~ dnorm(0, .00001)
  b8 ~ dnorm(0, .00001)
  b9 ~ dnorm(0, .00001)
  b10 ~ dnorm(0, .00001)
  b11 ~ dnorm(0, .00001)

}"


data.list1 <- list(for.bayes, 
                   n = dim(for.bayes)[1],
                   m = length(unique(for.bayes$person)),
                   l = length(unique(for.bayes$SID)))

inits.1 <- function () {
  list(gam = rnorm(1,0,1),
       sig1 = rgamma(1,1,1),
       sig2 = rgamma(1,1,1),
       sig3 = rgamma(1,1,1),
       b1 = rnorm(1,0,1),
       b2 = rnorm(1,0,1),
       b3 = rnorm(1,0,1),
       b4 = rnorm(1,0,1),
       b5 = rnorm(1,0,1),
       b6 = rnorm(1,0,1),
       b7 = rnorm(1,0,1),
       b8 = rnorm(1,0,1),
       b9 = rnorm(1,0,1),
       b10 = rnorm(1,0,1),
       b11 = rnorm(1,0,1))
}

rube(M1,data.list1,inits.1,parameters.to.save=c("gam", "sig1"))

M1.fit <- rube(M1,data.list.1,inits.1,
               parameters.to.save=c("gam", "sig1"),
               n.chains=3)

M1.fit