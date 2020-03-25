library(httr)

#url <- paste("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")
#GET(url, authenticate(":", ":", type="ntlm"), write_disk(pt <- tempfile(fileext = ".csv")))

# read and normalize column names
poll <- read.csv("Poll #4 in Portugal  (Responses) - Form Responses 1.csv")
names(poll) <- c("date","region","reach","cases")

votes=dim(poll)[1]

#adjust maxreach
for (p in seq(votes))
{
    poll$reach[p]=min(poll$reach[p],1000)
}

#clean outliers by ratio
classes=rep(0,10)
cumreach=0
cumcases=0
done=0
ratio=poll$cases/poll$reach
for (p in seq(votes))
{
    if (ratio[p] < 0.3)
    { 
        done=done+1; 
        cumreach=cumreach+poll$reach[p]; 
        cumcases=cumcases+poll$cases[p]
        classes[poll$cases[p]+1]=classes[poll$cases[p]+1]+1
    }
}

cases_per_answer=cumcases/done
cases_per_reach=cumcases/cumreach

populationPT=10261075

naif_cases=populationPT * cases_per_reach

