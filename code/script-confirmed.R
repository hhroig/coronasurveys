# load library
library(tidyverse)
library(readxl)
library(httr)

plot_estimates <- function(country_geoid = "AF", dts, ac_window){
  cat("::- script-confirmed: Working on", country_geoid, "::\n")
  data <- dts %>% 
    select(dateRep:popData2019, "Alpha.2.code" )
  data$geoId <- data$Alpha.2.code 
  data <- data %>% select(dateRep:popData2019)
  data <- data[data$geoId == country_geoid,]
  
  dt <- as.data.frame(data[rev(1:nrow(data)),])
  ####### fix NAs in cases and deaths #######
  dt$cases[is.na(dt$cases)] <- 0
  dt$deaths[is.na(dt$deaths)] <- 0
  ##########################################
  dt$cum_cases <- cumsum(dt$cases)
  dt$cum_deaths <- cumsum(dt$deaths)
  
  dt$date <- gsub("-", "/", as.Date(dt$dateRep, format = "%d/%m/%Y"))
  dt$active_cases <- cumsum(c(dt$cases[1:ac_window], diff(dt$cases, lag = ac_window))) # Carlo active cases
  
  dt <- dt %>% 
    select(date, cases, deaths, cum_cases, cum_deaths, active_cases, popData2019, ) %>% 
    rename(population = popData2019) %>% 
    mutate(p_cases = cum_cases/population,
           p_cases_daily = cases/population,
           p_cases_active = abs(active_cases/population)) %>% 
    select(date, cases, deaths, cum_cases, cum_deaths, p_cases, p_cases_daily, p_cases_active, population)
  
  dir.create("../data/estimates-confirmed/PlotData/", showWarnings = F)
  cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
  write.csv(dt, paste0("../data/estimates-confirmed/PlotData/", country_geoid, "-estimate.csv"))
}

generate_estimates <- function(active_cases_window = 12){
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
                 Sys.Date(), ".xlsx", sep = "")
    GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
    cat("::- script-confirmed: Checking the ECDC data for the day ::\n")
    #try( data_ecdc <- read_excel(tf), silent = T) # ECDC daily excel seems unvailable for now
    try( data_ecdc <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                               na.strings = "", fileEncoding = "UTF-8-BOM"), silent = T)
    
    if(!exists("data_ecdc")){
      cat("::- script-confirmed: Seems the ECDC data for the day is not available yet ::\n")
      cat("::- script-confirmed: Trying to get data for the previous day ::\n")
      url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
                   Sys.Date()-1, ".xlsx", sep = "")
      GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
      try( data_ecdc <- read_excel(tf), silent = T)
      if(!exists("data_ecdc")){
        stop("::- script-confirmed: Unfortunately, the ECDC data for yesterday is not availabe neither ::\n")
      }else{
        cat("::- script-confirmed: Using ECDC data for previous day ::\n")
        data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
        data_country_code <- read_excel("../data/common-data/wikipedia-iso-country-codes.xlsx")
        names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
                                      "Alpha.3.code", "Numeric.code", "ISO.3166.2")
        
        data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
        
        all_geo_ids <- unique(data_ecdc$Alpha.2.code)
        sapply(all_geo_ids, plot_estimates, dts = data_ecdc)
      }
    } else{
      cat("::- script-confirmed: ECDC data for the day available! ::\n")
      data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
      data_country_code <- read_excel("../data/common-data/wikipedia-iso-country-codes.xlsx")
      names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
                                    "Alpha.3.code", "Numeric.code", "ISO.3166.2")
      data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
      all_geo_ids <- unique(data_ecdc$Alpha.2.code) 
      go <- sapply(all_geo_ids, plot_estimates, dts =  data_ecdc, ac_window = active_cases_window)
    }
  
}
generate_estimates()
