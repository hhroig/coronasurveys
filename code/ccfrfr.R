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
  case_incidence <- data_1_in$cases
  death_incidence <- data_1_in$deaths
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
url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-04-02.xlsx"
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
data <- read_excel(tf)

data<-data[data$geoId=="FR",]
data<-list(cases=c(0,cumsum(rev(data$cases))),deaths=c(0,cumsum(rev(data$deaths))))
           

size=length(data$cases)
est_ccfr<-rep(NaN,size)

for (rr in 0:(size-2))
{
    if (rr>=91) {
       cat("rr is",rr,"\n");
    }
    last <- size-rr
    data2 <- list(cases=diff(data$cases[1:last]),deaths=diff(data$deaths[1:last]))
    ccfr<-scale_cfr(data2, delay_fun = hospitalisation_to_death_truncated)
    
    fraction_reported=cCFRBaseline / (ccfr$cCFR*100)
    
    est_ccfr[last]<-data$cases[last]*1/fraction_reported
}
#data2 <- list(confirmados=diff(data$confirmados),obitos=diff(data$obitos))
#ccfr<-scale_cfr(data2, delay_fun = hospitalisation_to_death_truncated)
#
#fraction_reported=cCFRBaseline / (ccfr$cCFR*100) 


francePopulation<-66987244
#survey_twitter<-rep(NaN,size+1)
estimates_fr_poll_d<-rep(NaN,size+1)
estimates_fr_poll_e<-rep(NaN,size+1)
estimates_fr_day_d<-rep(NaN,size+1)
estimates_fr_day_e<-rep(NaN,size+1)
#position 18 is March 28 results about March 27 cases
#survey_twitter[8]<-(4/(36*150))*populationFR #17 Mar
#dunbar
#23 mar
estimates_fr_poll_d[86] <- estimate_cases(file_path = "../data/FR-01-20200322-20200323.csv", country_population = francePopulation, correction_factor = 1)$dunbar_cases
#30 mar
estimates_fr_poll_d[93] <- estimate_cases(file_path = "../data/FR-02-20200329-20200329.csv", country_population = francePopulation, correction_factor = 1)$dunbar_cases
#1 apr
estimates_fr_poll_d[95] <- estimate_cases(file_path = "../data/FR-03-20200330-20200401.csv", country_population = francePopulation, correction_factor = 1)$dunbar_cases
#23 mar
estimates_fr_poll_e[86] <- estimate_cases(file_path = "../data/FR-01-20200322-20200323.csv", country_population = francePopulation, correction_factor = 1)$estimated_cases
#30 mar
estimates_fr_poll_e[93] <- estimate_cases(file_path = "../data/FR-02-20200329-20200329.csv", country_population = francePopulation, correction_factor = 1)$estimated_cases
#1 apr
estimates_fr_poll_e[95] <- estimate_cases(file_path = "../data/FR-03-20200330-20200401.csv", country_population = francePopulation, correction_factor = 1)$estimated_cases

#22 mar
estimates_fr_day_d[85] <- estimate_cases(file_path = "../data/byDay/FR-01-20200322-20200322.csv", country_population = francePopulation, correction_factor = 1)$dunbar_cases
#23 mar
estimates_fr_day_d[86] <- estimate_cases(file_path = "../data/byDay/FR-02-20200323-20200323.csv", country_population = francePopulation, correction_factor = 1)$dunbar_cases
#29 mar
estimates_fr_day_d[92] <- estimate_cases(file_path = "../data/byDay/FR-03-20200329-20200329.csv", country_population = francePopulation, correction_factor = 1)$dunbar_cases
#30 mar
estimates_fr_day_d[93] <- estimate_cases(file_path = "../data/byDay/FR-04-20200330-20200330.csv", country_population = francePopulation, correction_factor = 1)$dunbar_cases
#31 mar
estimates_fr_day_d[94] <- estimate_cases(file_path = "../data/byDay/FR-05-20200331-20200331.csv", country_population = francePopulation, correction_factor = 1)$dunbar_cases
#1 apr
estimates_fr_day_d[95] <- estimate_cases(file_path = "../data/byDay/FR-06-20200401-20200401.csv", country_population = francePopulation, correction_factor = 1)$dunbar_cases

#22 mar
estimates_fr_day_e[85] <- estimate_cases(file_path = "../data/byDay/FR-01-20200322-20200322.csv", country_population = francePopulation, correction_factor = 1)$estimated_cases
#23 mar
estimates_fr_day_e[86] <- estimate_cases(file_path = "../data/byDay/FR-02-20200323-20200323.csv", country_population = francePopulation, correction_factor = 1)$estimated_cases
#29 mar
estimates_fr_day_e[92] <- estimate_cases(file_path = "../data/byDay/FR-03-20200329-20200329.csv", country_population = francePopulation, correction_factor = 1)$estimated_cases
#30 mar
estimates_fr_day_e[93] <- estimate_cases(file_path = "../data/byDay/FR-04-20200330-20200330.csv", country_population = francePopulation, correction_factor = 1)$estimated_cases
#31 mar
estimates_fr_day_e[94] <- estimate_cases(file_path = "../data/byDay/FR-05-20200331-20200331.csv", country_population = francePopulation, correction_factor = 1)$estimated_cases
#1 apr
estimates_fr_day_e[95] <- estimate_cases(file_path = "../data/byDay/FR-06-20200401-20200401.csv", country_population = francePopulation, correction_factor = 1)$estimated_cases

#estimated
#21 Mar Cf=1, poll 2
#survey_gforms[12]<-estimate_cases(file_path = "../data/CY-02-20200320-20200321.csv", country_population = 1189265-300000, correction_factor = 1)$estimated_cases
#25 Mar cf=1, poll 3
#survey_gforms[16]<-estimate_cases(file_path = "../data/CY-03-20200323-20200325.csv", country_population = 1189265-300000, correction_factor = 1)$estimated_cases
#28 Mar cf=1, poll 4
#survey_gforms[19]<-estimate_cases(file_path = "../data/CY-04-20200327-20200328.csv", country_population = 1189265-300000, correction_factor = 1)$estimated_cases
#30 Mar cf=1, poll 5
#survey_gforms[21]<-estimate_cases(file_path = "../data/CY-05-20200329-20200330.csv", country_population = 1189265-300000, correction_factor = 1)$estimated_cases



#est_ccfr[size]<-data$confirmados[size]*1/fraction_reported

plot(data$deaths*400,log="y", xlim=c(45,size+3), ylim=c(1,2000000),yaxt="n",xaxt="n",type="l",xlab="Days",main="Different estimates of COVID-19 cases in France",ylab="Total cases",lty=4)
lines(data$cases)
data$cases
# uncomment one of the four lines below
# points(estimates_fr_day_d,pch=21); legend("bottomright", 
#                                           legend = c("Confirmed", "Death*400", "CCFR", "FrancePolls ByDay-Dunbar"), 
#                                           lty = c(1,4,0,0), 
#                                           pch = c(NA,NA,20,21),
#                                           #bty = "n", 
#                                           text.col = "black") # by day with dunbar number
# points(estimates_fr_poll_d,pch=22) ;legend("bottomright",
#                                         legend = c("Confirmed", "Death*400", "CCFR", "FrancePolls ByPoll-Dunbar"),
#                                         lty = c(1,4,0,0),
#                                         pch = c(NA,NA,20,22),
#                                         #bty = "n",
#                                         text.col = "black") # by poll with dunbar number
# points(estimates_fr_day_e,pch=23);legend("bottomright",
#                                          legend = c("Confirmed", "Death*400", "CCFR", "FrancePolls ByDay-Reach"),
#                                          lty = c(1,4,0,0),
#                                          pch = c(NA,NA,20,23),
#                                          #bty = "n",
#                                          text.col = "black") # by day with average reach

points(estimates_fr_poll_e,pch=24) ;legend("bottomright",
                                          legend = c("Confirmed", "Death*400", "CCFR", "FrancePolls ByPoll-Reach"),
                                          lty = c(1,4,0,0),
                                          pch = c(NA,NA,20,24),
                                          #bty = "n",
                                          text.col = "black") # by poll with average reach


points(est_ccfr,pch=20)
axis(side = 2, at = 10^seq(0, 6),labels=c("1","10","100","1,000","10,000","100,000","1000,000"))
abline(h=1000000,lty="dotted");abline(h=100000,lty="dotted");abline(h=10000,lty="dotted"); abline(h=1000,lty="dotted"); abline(h=100,lty="dotted"); abline(h=10,lty="dotted")
axis(side=1,at=c(78,83,88,93),labels=c("Mar 15","Mar 20","Mar 25","Mar 30"))
abline(v=78,lty="dotted"); abline(v=83,lty="dotted"); abline(v=88,lty="dotted");abline(v=93,lty="dotted")  
#6,11,16,19



