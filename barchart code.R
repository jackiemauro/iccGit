# create a bar chart with the ICC's broken out 
# by hotspot type. Compare this to the ICC when 
# not breaking out (in red for the SID level, blue
# for the Zip level). 

# tell it which file to source for the ICC regressions: 
# "questions 6 and 7 - iccs.R" for collective efficacy
# "question 19 iccs.R" for 19
# "icc reg for 20 41 42.R" for others


# not working, hates the red and blue lines

source("questions 6 and 7 - iccs.R")

icc.bar <- function(sidRegression, zipRegression,
                    redLine, blueLine,
                    ylab = "SID level ICC", 
                    mainTitle = "ICC for ZIP and SID"){
  require(ggplot2)
  require(plyr)
  
  bar.df = data.frame(Spot = names(sidRegression),
                      SID = sidRegression, ZIP = zipRegression)
  
  mm <- ddply(bar.df, "Spot", summarise, test = mean(SID))
  ggplot(mm, aes(x = factor(Spot), y = test)) + 
    geom_bar(stat = "identity")+ 
    xlab("Hotspot type") + 
    geom_hline(aes(yintercept = redLine), col = "red") + 
    geom_hline(aes(yintercept = blueLine), col = "blue") +
    ylab(ylab) + 
    ggtitle(mainTitle)
}
