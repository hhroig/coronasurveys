#model of a 1-lattice with k=reach, as in Duncan J. Watts, Small Worlds: The Dynamics of Networks between Order and Randomness

#country population
pop <- 10000000

r <- rep(0,pop)

#True ratio  of infection
P <- 0.0075

inf <- floor(P*pop)

#get unique places
selection <- sample(pop,inf)

#place them all
for (i in selection)
{
    r[i]<-1
}
P <- sum(r)/pop

#reach of each response in survey
reach<-150

resp <- 200

answ <- rep(0,resp)
p <- rep(0,resp)
for (j in 1:resp)
{
    x<-sample(pop,1)
    hits<-0
    for (k in 0:(reach-1))
    {
        hits<-hits+r[((x-1+k) %% pop)+1]
    }
    answ[j]<-hits
    p[j]<-sum(head(answ,j))/(j*reach)
}

plot(p,ylim=c(0,P*2))
abline(h=P,lty="dotted")
