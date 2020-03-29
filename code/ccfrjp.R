#install.packages("readxl")
library(readxl)
library(httr)

zmeanHDT <- 13
zsdHDT <- 12.7
zmedianHDT <- 9.1
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))
cCFRBaseline <- 1.38
cCFREstimateRange <- c(1.23, 1.53)
#cCFRIQRRange <- c(1.3, 1.4)


# Functions from https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html
# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x)
{
  dlnorm(x, muHDT, sigmaHDT)
}
# Function to work out correction CFR
scale_cfr <- function(data_1_in, death_incidence, delay_fun){
  case_incidence <- data_1_in$confirmados
  death_incidence <- data_1_in$obitos
  cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
  # Sum over cases up to time tt
  for(ii in 1:length(case_incidence)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
  }
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  # corrected CFR estimator
  p_tt <- sum(death_incidence)/cumulative_known_t
  data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
             cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}


#url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-28.xlsx"
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
data <- read_excel(tf)

data<-data[data$geoId=="JP",]
data<-list(confirmados=cumsum(rev(data$cases)),obitos=cumsum(rev(data$deaths)))

size=length(data$confirmados)
est_ccfr<-rep(NaN,size)

for (rr in 0:(size-1))
{
    last <- size-rr
    data2 <- list(confirmados=diff(data$confirmados[1:last]),obitos=diff(data$obitos[1:last]))
    ccfr<-scale_cfr(data2, delay_fun = hospitalisation_to_death_truncated)
    
    fraction_reported=cCFRBaseline / (ccfr$cCFR*100)
    
    est_ccfr[last]<-data$confirmados[last]*1/fraction_reported
}
#data2 <- list(confirmados=diff(data$confirmados),obitos=diff(data$obitos))
#ccfr<-scale_cfr(data2, delay_fun = hospitalisation_to_death_truncated)
#
#fraction_reported=cCFRBaseline / (ccfr$cCFR*100) 


populationJP<-1.27*10^8
survey_twitter<-rep(NaN,size)
survey_gforms<-rep(NaN,size)

#position 89 is March 28 results about March 27 cases
#survey_twitter[76]<-(374.05/(762*150))*populationSP #14 Mar
survey_gforms[89]<-337019 #27 Mar cf=1, pool 1

#est_ccfr[size]<-data$confirmados[size]*1/fraction_reported

plot(data$obitos*400,log="y",ylim=c(1,1000000),yaxt="n",xaxt="n",type="l",xlab="Days",main="Different estimates of SARS-COV2 cases in Japan",ylab="Total cases",lty=4)
lines(data$confirmados)
points(survey_twitter,pch=23)
points(survey_gforms,pch=24)
points(est_ccfr,pch=20)
axis(side = 2, at = 10^seq(0, 6),labels=c("1","10","100","1,000","10,000","100,000","1,000,000"))
abline(h=1000000,lty="dotted"); abline(h=100000,lty="dotted"); abline(h=10000,lty="dotted"); abline(h=1000,lty="dotted"); abline(h=100,lty="dotted"); abline(h=10,lty="dotted")
axis(side=1,at=c(47,57,67,77,87),labels=c("Feb 14","Feb 24","Mar 5","Mar 15","Mar 25"))
abline(v=47,lty="dotted"); abline(v=57,lty="dotted"); abline(v=67,lty="dotted"); abline(v=77,lty="dotted"); abline(v=87,lty="dotted");
