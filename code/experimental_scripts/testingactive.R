
library("dplyr")
library("data.table")
library("caTools")

ccfrdata <- read.csv("../../data/estimates-ccfr-based/PlotData/ES-estimate.csv")
casedata <- read.csv("../../data/estimates-confirmed/PlotData/ES-estimate.csv")
recentdata <- read.csv("../../data/estimates-W/PlotData/ES-estimate.csv")

casedata <- subset(casedata, select = c(X, date, p_cases_active))
nms <- c("X", "date", "p_active_confirmed")
setnames(casedata, nms)

ccfrdata <- subset(ccfrdata, select = c(date, p_cases_active))
nms <- c("date", "p_active_ccfr")
setnames(ccfrdata, nms)

recentdata <- subset(recentdata, select = c(date, p_cases_recent))

plot(casedata$p_active_confirmed*100,type="l",lty=1,ylab="% active",
     xlab="Days (cases considered active for 12 days)",main="Spain, Estimate of active COVID-19", ylim=c(0,1.5), xlim=c(114,200))

lines(ccfrdata$p_active_ccfr*100,lty=1,col="red")

recentdata <- merge(casedata, recentdata, by = "date")

recentdata <- subset(recentdata, select = c(X, p_cases_recent))

#lines(recentdata$p_cases_recent*100~recentdata$X,lty=1,col="blue")


#facebook
library(tidyverse)
library(httr)
library(jsonlite)

# adding url
path <- "https://covidmap.umd.edu/api/resources?indicator=covid&type=daily&country=Spain&daterange=20200423-20200723"

# request data from api
request <- GET(url = path)

# make sure the content is encoded with 'UTF-8'
response <- content(request, as = "text", encoding = "UTF-8")

# now we can have a dataframe for use!
coviddata <- fromJSON(response, flatten = TRUE) %>% data.frame()

facebook <- c(rep(NA,114),coviddata$data.percent_cli)
#facebook <- c(rep(NA,114),runMean(coviddata$data.percent_cli,5))
lines(facebook*100,lty=1,col="green")

facebook <- c(rep(NA,114),coviddata$data.percent_cli_unw)
lines(facebook*100,lty=1,col="orange")

facebook <- c(rep(NA,114),runmean(coviddata$data.percent_cli_unw,7))
lines(facebook*100,lty=1,col="blue")

# adding url
path <- "https://covidmap.umd.edu/api/resources?indicator=covid&type=smoothed&country=Spain&daterange=20200423-20200723"

# request data from api
request <- GET(url = path)

# make sure the content is encoded with 'UTF-8'
response <- content(request, as = "text", encoding = "UTF-8")

# now we can have a dataframe for use!
coviddata <- fromJSON(response, flatten = TRUE) %>% data.frame()

facebook <- c(rep(NA,114),coviddata$data.smoothed_cli)
lines(facebook*100,lty=1,col="brown")


