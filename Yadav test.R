# Tests the function Yadav against a simulated dataset
# Looks good

require(lme4)

# create simulated dataset
cluster = c(rep(1,5), rep(2,4), rep(3, 4))
response = c(rnorm(5,1), rnorm(4,2), rnorm(4,-3))
cov[1:4] = response[1:4]*1.2 + rnorm(1,0,3)
cov[5:13] = response[5:13]*1.8 + rnorm(1,0,3)
dataset = data.frame(cluster = cluster, response.var = response)

Yadav(dataset, cluster, response)

reg = lmer(response ~ 1 + (1|cluster))
icc = as.data.frame(VarCorr(reg))[1,4]/sum(as.data.frame(VarCorr(reg))[1:2,4])

reg2 = lmer(response ~ cov + (1|cluster))
icc2 = as.data.frame(VarCorr(reg2))[1,4]/sum(as.data.frame(VarCorr(reg2))[1:2,4])
