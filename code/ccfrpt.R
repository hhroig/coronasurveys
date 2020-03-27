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

url <- paste("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")
GET(url, authenticate(":", ":", type="ntlm"), write_disk(pt <- tempfile(fileext = ".csv")))
data <- read.csv(pt)

est_ccfr<-rep(NaN,size)
size<-dim(data)[1]
for (rr in 0:(length(data$obitos[data$obitos>0])-1))
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


populationPT<-10261075
survey_twitter<-rep(NaN,size)
survey_gforms<-rep(NaN,size)
#est_ccfr<-rep(NaN,size)
#survey[23]=(11/(63*276))*populationPT #18 Mar
#survey[25]=15/(45*276)*populationPT #20 Mar
survey_twitter[23]<-(11/(63*150))*populationPT #18 Mar
survey_twitter[25]<-15/(45*150)*populationPT #20 Mar
survey_gforms[28]<-20577 #23 Mar
survey_gforms[28]<-31657 #23 Mar cf=1
survey_gforms[30]<-45395 #25 Mar
survey_gforms[30]<-69839 #25 Mar cf=1
#est_ccfr[size]<-data$confirmados[size]*1/fraction_reported

plot(data$obitos*400,log="y",ylim=c(1,100000),type="l",xlab="Days",main="Different estimates of SARS-COV2 cases in Portugal",ylab="Total cases",lty=4)
lines(data$confirmados)
points(survey_twitter,pch=23)
points(survey_gforms,pch=24)
points(est_ccfr,pch=20)

