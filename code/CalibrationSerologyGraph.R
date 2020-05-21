library(zoo)

days <- seq(as.Date("2020-04-13"), as.Date("2020-04-27"), by = "days")

baseIgG <- rep(6.33,15)
#basertpcr <- c(170099,172541,177644,184948,190839,191726,198674,200210,204178,208389,213024,202990,205905,207634,209465)/46934628*100/0.66
basertpcr <- c(170099,172541,177644,184948,190839,191726,198674,200210,204178,208389,213024,202990,205905,207634,209465)/46934628*100
basecCFRe <- c(6.22,6.23,6.39,6.48,6.57,6.66,6.55,6.54,6.59,6.63,6.69,6.72,6.76,6.76,6.68)
baseEw <- c(5.98,5.51,5.66,5.71,5.88,5.91,6.03,6.02,6.55,6.59,6.61,6.45,6.48,6.69,6.93)


IgG <- zoo(baseIgG,days)
cCFR <- zoo(basecCFRe,days)
Survey <- zoo(baseEw,days)
RTPCR <- zoo(basertpcr,days)


all.ts <- cbind(IgG,cCFR,Survey,RTPCR)

plot(all.ts,plot.type = "single",ylim=c(0,10),col = c("green","red","blue","black"),ylab="Percentage of infection in total population",xlab="Dates",lty = c(1,2,3,4), lwd=2)
title("Estimates of SARS-COV2 infections in Spain",cex=0.6)
legend("topleft", c("IgG prevalence","cCFR based estimate","Survey based estimate","RT-PCR based estimate"), lty = c(1,2,3,4), col =c("green","red","blue","black"), lwd=2, cex=0.8, bty="n")