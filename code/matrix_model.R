#country population
pop <- 10000
side <- floor(sqrt(pop))

m <- matrix(0,nrow = side,ncol = side)

#True ratio  of infection
P <- 0.02

inf <- floor(P*pop)

#get unique places
selection <- sample(pop,inf)

#place them all
for (i in selection)
{
    x<-((i-1) %/% side)+1
    y<-((i-1) %% side)+1
    m[x,y]<-1
}
P <- sum(m)/(side*side)

#reach of each response in survey
reach<-9
rside <- floor(sqrt(reach))

resp <- 500

answ <- rep(0,resp)
p <- rep(0,resp)
for (j in 1:resp)
{
    width<-side - rside +1
    x<-sample(width,1)
    y<-sample(width,1)
    hits<-0
    for (k in 0:(rside-1))
    {
        for(l in 0:(rside-1))
        {
            hits<-hits+m[x+k,y+l]
        }
    }
    answ[j]<-hits
    p[j]<-sum(head(answ,j))/(j*reach)
}

plot(p,ylim=c(0,P*2))
abline(h=P,lty="dotted")
