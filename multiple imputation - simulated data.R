############## imputation using simulated dataset ###########

require("mi")
require("mice")
library(plyr)

#dataset creation 
a <- rnorm(100, 1, 2)
b <- a*3 + rnorm(100, .2, 1)
b[which(b < 0)] <- 0
c <- a - .5 + rnorm(100, -1, 2)
d <- sample(c("yes", "no", "DK"), 100, replace = T, prob = c(.3, .35, .35))
e <- b^2
f <- e*rnorm(100, .002, 1)
g <- sample(c("sad", "happy", "stressed", "tired"), 100, replace = T)
h <- c(rep(111,10), rep(222,10), rep(333,10), 
       rep(444, 30), rep(555,10), rep(777,20),
       rep(888,10))     # to replicate SID, but not reproducing failure
i <- reduced$SID[1:100] # still won't fail


missb <- sample(c(1:100), 10)             # leave a with all its values
b[missb] <- NA
missc <- sample(c(1:100), 10)   
c[missc] <- NA
missd <- sample(c(1:100), 20)
d[missb] <- NA                          #give lots of vars same missingness
e[missb] <- NA                          #e just a transformation of b        
f[missb] <- NA
# missg <- sample(c(1:50), 25)          #if you give so many missings, fails
# g[missg] <- NA

df <- data.frame(a,b,c,d,e,f,h,i)

df$d <- revalue(df$d, c("DK"="dontknow"))

# mi
mdf <- missing_data.frame(df)
mdf <- change(mdf, y = "h", what = "type", to = "un")
imp.mi1 <- mi(mdf, max.minutes = 20)
imp.dfs1 <- complete(imp.mi1, 3)

# mice
require("mice")
