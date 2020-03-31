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
url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-30.xlsx"
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
data <- read_excel(tf)

data<-data[data$geoId=="ES",]
dt <- data[rev(1:nrow(data)),]
dt$cum_cases <- cumsum(dt$cases)
data<-list(confirmados=cumsum(rev(data$cases)),obitos=cumsum(rev(data$deaths)))
data$obitos <- c(data$obitos, 7340 )
data$confirmados <- c(data$confirmados, 85195 )

size=length(data$confirmados)
est_ccfr<-rep(NaN,size)

for (rr in 0:(size-1)){
  cat("working on rr =, ", rr, "\n")
    last <- size-rr
    if(last == 1){
      data2 <- data.frame(confirmados = (data$confirmados[1:last]), obitos=(data$obitos[1:last]))
    }else{
      data2 <- data.frame(confirmados = diff(data$confirmados[1:last]), obitos=diff(data$obitos[1:last]))
    }
    
    ccfr<-scale_cfr(data2, delay_fun = hospitalisation_to_death_truncated)
    
    fraction_reported=cCFRBaseline / (ccfr$cCFR*100)
    
    est_ccfr[last]<-data$confirmados[last]*1/fraction_reported
}

#data2 <- list(confirmados=diff(data$confirmados),obitos=diff(data$obitos))
#ccfr<-scale_cfr(data2, delay_fun = hospitalisation_to_death_truncated)
#
#fraction_reported=cCFRBaseline / (ccfr$cCFR*100) 


populationSP<-46754778
survey_twitter<-rep(NaN,size)
survey_gforms<-rep(NaN,size)

#position 89 is March 28 results about March 27 cases
#survey[23]=(11/(63*276))*populationPT #18 Mar
#survey[25]=15/(45*276)*populationPT #20 Mar
survey_twitter[76]<-(374.05/(762*150))*populationSP #14 Mar
survey_twitter[78]<-(66.13/(85*150))*populationSP #16 Mar
survey_twitter[80]<-(116.16/(120*150))*populationSP #18 Mar
#survey_twitter[25]<-15/(45*150)*populationSP #20 Mar
survey_gforms[85]<-1408474 #23 Mar cf=1, pool 6 # Mar 23 is row 84
survey_gforms[87]<-1689103 #25 Mar cf=1, pool 7
survey_gforms[89]<-2061923 #27 Mar cf=1, pool 8
survey_gforms[90]<-2125610 #28 Mar cf=1, pool 9
survey_gforms[91]<-2400167 #29 Mar cf=1, poll 10
survey_gforms[92]<-2361650 #30 Mar cf=1, poll 11

#est_ccfr[size]<-data$confirmados[size]*1/fraction_reported

plot(data$obitos*400,log="y",xlim=c(40,size+1), ylim=c(1,10000000),yaxt="n",xaxt="n",type="l",xlab="Days",main="Different estimates of COVID-19 cases in Spain",ylab="Total cases",lty=4)
lines(data$confirmados)
points(survey_twitter,pch=23)
points(survey_gforms,pch=24)
points(est_ccfr,pch=20)

axis(side = 2, at = 10^seq(0, 7),labels=c("1","10","100","1,000","10,000","100,000","1,000,000","10,000,000"))
abline(h=10000000,lty="dotted"); abline(h=1000000,lty="dotted"); abline(h=100000,lty="dotted"); abline(h=10000,lty="dotted"); abline(h=1000,lty="dotted"); abline(h=100,lty="dotted"); abline(h=10,lty="dotted")
axis(side=1,at=c(47,57,67,77,87),labels=c("Feb 14","Feb 24","Mar 5","Mar 15","Mar 25"))
abline(v=47,lty="dotted");
abline(v=57,lty="dotted");
abline(v=67,lty="dotted");
abline(v=77,lty="dotted");
abline(v=87,lty="dotted");


write.csv(data.frame(confirmados = data$confirmados,
                     obitos_400 = data$obitos, 
                     survey_twitter = survey_twitter,
                     survey_gforms = survey_gforms,
                     est_ccfr = est_ccfr), "esp_mar30_plot.csv")


### alternative plot with legend using ggplot2  
library(ggplot2)
library(dplyr)
library(tidyr)
obitos_400 <- data$obitos*400
obitos_400[obitos_400 == 0] <- NA
est_ccfr2 <- est_ccfr
est_ccfr2[est_ccfr2 == 0 ] <- NA
dt_plot <- data.frame(obitos_400 = obitos_400,
                      confirmados = data$confirmados, 
                      est_ccfr2 = est_ccfr2,
                      days = 1:length(obitos_400), 
                      survey_twitter = survey_twitter,
                      survey_gforms = survey_gforms)

dt_plot2 <- dt_plot %>% gather(key = "series", value = "value", -days)

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}




ggplot(data = dt_plot2, mapping = aes(x = days, y = value)) +
  geom_line(data = dt_plot2 %>% filter(series == "confirmados" | series == "obitos_400"),
            aes(linetype = series)) +
  geom_point(data = dt_plot2 %>% 
               filter(series == "est_ccfr2" | series == "survey_twitter" | series == "survey_gforms"),
             aes(shape = series))+
  
  ylab("Total Cases") + 
  xlab("Days") +
  #ylim(0, 100000) +
  scale_y_continuous(trans='log10', label = scientific_10,
                     breaks = 10^seq(0, 7)) +
  scale_x_continuous(breaks = c(47,57,67,77,87),
                     labels = c("Feb 14","Feb 24","Mar 5","Mar 15","Mar 25"),
                     limits = c(40, size)) +
  ggtitle("Different estimates of COVID−19 cases in Spain") +
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, face = "bold",
                                  size = 16),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  scale_shape_manual(values = c(19,2,5),
                     name = "Estimates", 
                     breaks = c("est_ccfr2", "survey_gforms", "survey_twitter"),
                     label = c("adjusted_cfr", "gforms_survey", "twitter_survey")) +
  scale_linetype_manual(name = "Data",
                        values = c("solid", "dashed"),
                        breaks = c("confirmados", "obitos_400"),
                        label = c("infections", "deaths"))
  
  
  
