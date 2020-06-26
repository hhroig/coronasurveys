#model of a 1-lattice with k=reach, as in Duncan J. Watts, Small Worlds: The Dynamics of Networks between Order and Randomness

source("surveyDiffusionOnRing.R")







#country population
pop <- 10000000

r <- rep(0,pop)

#True ratio  of infection
P <- 0.01

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
reach<-100

#how many runs to average
runs <- 100

resp <- 100
error <- rep(0,resp) 

uniform=TRUE
noise=0 # 0 for no noise, then 0.1 for some scale free noise around each response estimated p

for (run in 1:runs)
{
    answ <- rep(0,resp)
    p <- rep(0,resp)
   # probes <- sample(pop,resp,replace=FALSE)
    probes <- selectProbes(pop,resp,replace=FALSE)
 #   print (probes)
    for (j in 1:resp)
    {
        x <- probes[j]
        hits<-0
        for (k in 0:(reach-1))
        {
            hits<-hits+r[((x-1+k) %% pop)+1]
        }
        answ[j]<-hits
        p[j]<-sum(head(answ,j))/(j*reach)
        #perturbate
        p[j]=p[j]+p[j]*rnorm(1,mean=0,sd=noise)
        error[j] <- error[j] + (p[j]-P)^2
    }
    

}
#plot(p,ylim=c(0,P*2)
# Normalized root mean square error
plot(type="l",sqrt(error/runs)/P,ylim=c(0,5))
abline(h=0,lty="dotted")






