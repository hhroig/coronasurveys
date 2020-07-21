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

uniform=FALSE

sizes=vector()
for (ansProb in seq(from=0.05, to=0.3, by=0.05)){
  for (run in 1:runs){
    resp=selectProbes(pop, 10000000, FALSE)
    sizes[run]=length(resp);
  }
  cat("ansProb = ",ansProb)
  print(summary(sizes))
}