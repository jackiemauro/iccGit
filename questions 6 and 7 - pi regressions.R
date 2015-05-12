# pi regressions for questions 6 and 7

source("questions 6 and 7 - dataset creation.R")

# find pi's for combined
pi.reg.67 <- lm(as.numeric(Answer)~person+Q1+Q2.rev+Q3+Q4.rev+Q5.rev+Q6+Q7+Q8+Q9+Q10+Q11-1)
pi.67 <-pi.reg.67$coefficients[1:(length(pi.reg.67$coefficients)-11)] 
alphas.67 = pi.reg.67$coefficients[(length(pi.reg.67$coefficients)-10):length(pi.reg.67$coefficients)] 

# pi for just 6
drops<-c("Q7","Q8","Q9","Q10", "Q11", "Q12")
for.pi6<-for.pi67[, !names(for.pi67) %in% drops] 
pi.reg6 <- lm(as.numeric(Answer)~person+Q1+Q2.rev+Q3+Q4.rev+Q5.rev-1)
pi6 <-pi.reg6$coefficients[1:(length(pi.reg6$coefficients)-5)] 

# pi for just 7
drops<-c("Q1","Q2.rev","Q3","Q4.rev", "Q5.rev", "Q6")
for.pi7<-for.pi67[, !names(for.pi67) %in% drops] 
pi.reg7 <- lm(as.numeric(Answer)~person+Q7+Q8+Q9+Q10+Q11-1)
pi7 <-pi.reg7$coefficients[1:(length(pi.reg7$coefficients)-5)] 
