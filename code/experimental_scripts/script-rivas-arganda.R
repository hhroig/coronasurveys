library(tidyr)
library(dplyr)

responses_path <- "../../data/aggregate/rivas-arganda/"
max_ratio <- 0.3

get_survey_countries <- function(path = "../../data/aggregate/"){
  plotdata_files <- list.files(path)
  plotdata_files <- grep("-aggregate.csv", plotdata_files, value= TRUE)
  substr(plotdata_files,start = 1, stop = 2)
}

calculate_ci <- function(p_est, level, pop_size) {
  z <- qnorm(level+(1-level)/2)
  se <- sqrt(p_est*(1-p_est))/sqrt(pop_size)
  return(list(low=p_est-z*se, upp=p_est+z*se, error=z*se))
}

survey_countries <- get_survey_countries(responses_path)

country_iso <- "ES"

if(country_iso %in% survey_countries){
  file_path <- paste0(responses_path, country_iso, "-aggregate.csv")
  dt <- read.csv(file_path, as.is = T)
  cat("::- script-rivas-arganda: Read ", nrow(dt), "responses\n")
  names(dt) <- tolower(names(dt))
  
  #list of dates
  dates <- as.character(seq.Date(as.Date(dt$timestamp[1]), as.Date(tail(dt$timestamp,1)), by = "days"))
  dates <- gsub("-","/", dates)
  
  #remove outliers of reach.
  dt <- dt[!is.na(dt$reach),]
  dt <- dt[dt$reach != 0, ]
  cat("::- script-rivas-arganda: Responses after removing reach=NA or reach=0:", nrow(dt), "\n")
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  dt <- dt[dt$reach <= reach_cutoff, ]
  cat("::- script-rivas-arganda: Responses after removing ouliers with cutoff", reach_cutoff, ":", nrow(dt), "\n")
  
  # remove outliers based on max cases/reach ratio
  dt <- dt[!is.na(dt$cases),]
  dt$ratio <- dt$cases/dt$reach
  dt <- dt[dt$ratio <= max_ratio, ]
  cat("::- script-rivas-arganda: Responses after removing ouliers with cases=NA or cases/reach ratio >", 
      max_ratio, ":", nrow(dt), "\n")
  
  regions <- unique(dt$iso.3166.2)
  for (i in 1:length(regions)){
    reg <- regions[i]
    # get data for the current region
    dt_r <- dt[dt$iso.3166.2 == reg, ]
    cat("::- script-rivas-arganda: Working on region: ", reg, "with", nrow(dt_r), 
        "responses & reach", sum(dt_r$reach), "\n")
    
    p_cases <- sum(dt_r$cases)/sum(dt_r$reach) 
    p_cases_low <- calculate_ci(p_est = p_cases, level = 0.95, pop_size = sum(dt_r$reach))$low
    p_cases_high <- calculate_ci(p_est = p_cases, level=0.95, pop_size = sum(dt_r$reach))$upp
    cat("::- script-rivas-arganda: Region: ", reg, " p_cases:", p_cases, p_cases_low, p_cases_high, "\n")
    
    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$fatalities),]
    dt_f <- dt_f[dt_f$fatalities <= dt_f$cases,]
    cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), "responses after removing ouliers with fatalities=NA or cases > fatalities\n")
    p_cfr <- sum(dt_f$fatalities)/sum(dt_f$cases)
    p_cfr_low <- calculate_ci(p_est = p_cfr, level = 0.95, pop_size = sum(dt_f$cases))$low
    p_cfr_high <- calculate_ci(p_est = p_cfr, level=0.95, pop_size = sum(dt_f$cases))$upp
    cat("::- script-rivas-arganda: Region: ", reg, " p_cfr:", p_cfr, p_cfr_low, p_cfr_high, "\n")
    
    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$recentcases),]
    dt_f <- dt_f[dt_f$recentcases <= dt_f$cases,]
    cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), 
        "responses after removing ouliers with recentcases=NA or cases > recentcases\n")
    p_recentcases <- sum(dt_f$recentcases)/sum(dt_f$cases)
    p_recentcases_low <- calculate_ci(p_est = p_recentcases, level = 0.95, pop_size = sum(dt_f$cases))$low
    p_recentcases_high <- calculate_ci(p_est = p_recentcases, level=0.95, pop_size = sum(dt_f$cases))$upp
    cat("::- script-rivas-arganda: Region: ", reg, " p_recentcases:", p_recentcases, p_recentcases_low, p_recentcases_high, "\n")
  }
}

