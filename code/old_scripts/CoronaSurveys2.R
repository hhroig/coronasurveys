library(httr)

#url <- paste("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")
#GET(url, authenticate(":", ":", type="ntlm"), write_disk(pt <- tempfile(fileext = ".csv")))

# read and normalize column names
#poll <- read.csv("Poll #4 in Portugal  (Responses) - Form Responses 1.csv")
poll <- read.csv("Poll #6 in Spain  (Responses) - Form Responses 1.csv")
#poll <- read.csv("Poll #3 in Portugal  (Responses) - Form Responses 1.csv")
names(poll) <- c("date","region","reach","cases")

#votes=dim(poll)[1]

#adjust maxreach
# An outlier is an observation that is numerically distant from the rest of the data. 
# When reviewing a boxplot, an outlier is defined as a data point that is located outside the fences 
# (“whiskers”) of the boxplot (e.g: outside 1.5 times the interquartile range above the upper quartile 
# and bellow the lower quartile)
outup<-min(boxplot(poll$reach,plot=F)$out)

#removing entries that have reach values that classify as outliers
write.table(poll[poll$reach>=outup,], file="log.txt", append=T) #append removals to log
poll <- poll[poll$reach<outup,]

maxratio <- 0.30
ratio<-poll$cases/poll$reach
write.table(poll[ratio>=maxratio,], file="log.txt", append=T)
poll <- poll[ratio<maxratio,]

classes<-table(poll$cases) #table(poll$region,poll$cases)

cases_per_answer<-mean(poll$cases)
reach_per_answer<-mean(poll$reach)
cases_per_reach<-sum(poll$cases)/sum(poll$reach)

#population=10261075 #PT Pordata
population<-46750016 #SP https://www.worldometers.info/world-population/spain-population/ https://tradingeconomics.com/spain/urban-population-percent-of-total-wb-data.html

CorrectionFactor<-0.65 # magic factor! since not all population is reachable and susceptible
naif_cases<-population * cases_per_reach * CorrectionFactor

