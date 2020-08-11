library(tidyr)
library(dplyr)

responses_path <- "../data/aggregate/rivas-arganda/"
data_path <- "../data/common-data/rivas-arganda/regions-tree-population.csv"
estimates_path <- "../data/estimates-rivas-arganda/"

# responses_path <- "../coronasurveys/data/aggregate/rivas-arganda/"
# data_path <- "../coronasurveys/data/common-data/rivas-arganda/regions-tree-population.csv"
# estimates_path <- "./PlotData/"

country_iso <- "ES"
ci_level <- 0.95
max_ratio <- 1/3
num_responses = 100


remove_outliers <- function(dt, max_ratio = 1/3) {
  #remove outliers of reach.
  dt <- dt[!is.na(dt$reach),]
  dt <- dt[dt$reach != 0, ]
  cat("Responses after removing reach=NA or reach=0 :", nrow(dt), "\n")
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  dt <- dt[dt$reach <= reach_cutoff, ]
  cat("Responses after removing ouliers with reach cutoff", reach_cutoff, ":", nrow(dt), "\n")
  
  # remove outliers based on max cases/reach ratio
  dt <- dt[!is.na(dt$cases),]
  dt$ratio <- dt$cases/dt$reach
  dt <- dt[dt$ratio <= max_ratio, ]
  cat("Responses after removing ouliers with cases=NA or cases/reach ratio >", 
      max_ratio, ":", nrow(dt), "\n")
  return(dt)
}

process_ratio <- function(dt, numerator, denominator, control){
  dta <- dt[!is.na(dt[[numerator]]),]
  dta <- dta[!is.na(dta[[denominator]]),]
  dta <- dta[dta[[numerator]] <= dta[[control]],]
  if (nrow(dta)>0){
    p_est <- sum(dta[[numerator]])/sum(dta[[denominator]])
    level <- ci_level
    z <- qnorm(level+(1-level)/2)
    se <- sqrt(p_est*(1-p_est))/sqrt(sum(dta[[denominator]]))
    return(list(val=p_est, low=max(0,p_est-z*se), upp=p_est+z*se, error=z*se, std=se))
  }
  else {
    return(list(val=NA, low=NA, upp=NA, error=NA, std=NA))
  }
}

# calculate_ci <- function(p_est, level, pop_size) {
#  z <- qnorm(level+(1-level)/2)
#  se <- sqrt(p_est*(1-p_est))/sqrt(pop_size)
#  return(list(p_est=est, low=max(0,p_est-z*se), upp=p_est+z*se)) #, error=z*se))
# }

process_region <- function(dt, reg, name, pop, dates, num_responses = 100){
  cat("Working with", nrow(dt), "responses\n"  )
  #list of dates
  # dates <- as.character(seq.Date(as.Date(dt$timestamp[1]), as.Date(tail(dt$timestamp,1)), by = "days"))
  # dates <- gsub("-","/", dates)
  
  region <- c()
  regionname <- c()
  sample_size <- c()
  reach <- c()
  p_cases <- c()
  p_cases_low <- c()
  p_cases_high <- c()
  
  cases_est <- c()
  cases_low <- c()
  cases_high <- c()
  
  p_recovered <- c()
  p_recovered_low <- c()
  p_recovered_high <- c()
  
  p_fatalities <- c()
  p_fatalities_low <- c()
  p_fatalities_high <- c()
  
  fatalities_est <- c()
  fatalities_low <- c()
  fatalities_high <- c()
  
  p_recentcases <- c()
  p_recentcases_low <- c()
  p_recentcases_high <- c()
  
  recentcases_est <- c()
  recentcases_low <- c()
  recentcases_high <- c()
  
  p_recentcasesnursing <- c()
  p_recentcasesnursing_low <- c()
  p_recentcasesnursing_high <- c()
  
  p_stillsick <- c()
  p_stillsick_low <- c()
  p_stillsick_high <- c()
  
  p_hospital <- c()
  p_hospital_low <- c()
  p_hospital_high <- c()
  
  hospital_est <- c()
  hospital_low <- c()
  hospital_high <- c()
  
  p_severe <- c()
  p_severe_low <- c()
  p_severe_high <- c()
  
  p_icu <- c()
  p_icu_low <- c()
  p_icu_high <- c()
  
  icu_est <- c()
  icu_low <- c()
  icu_high <- c()
  
  p_tested <- c()
  p_tested_low <- c()
  p_tested_high <- c()
  
  p_positive <- c()
  p_positive_low <- c()
  p_positive_high <- c()
  
  population <- c()
  
  for (j in dates){
    nr <- nrow(dt[as.Date(dt$timestamp) == as.Date(j), ])
    dt_date <- tail(dt[as.Date(dt$timestamp) <= as.Date(j), ], max(num_responses,nr))
    cat("- Working on date: ", j, "with", nrow(dt_date), "responses\n"  )
    
    region <- c(region, reg)
    regionname <- c(regionname, name)
    sample_size <- c(sample_size, nrow(dt_date))
    reach <- c(reach, sum(dt_date$reach))
    
    est <- process_ratio(dt_date, "cases", "reach", "reach")
    p_cases <- c(p_cases, est$val)
    p_cases_low <- c(p_cases_low, est$low)
    p_cases_high <- c(p_cases_high, est$upp)
    
    cases_est <- c(cases_est, pop*est$val)
    cases_low <- c(cases_low, pop*est$low)
    cases_high <- c(cases_high, pop*est$upp)
    
    est <- process_ratio(dt_date, "recovered", "cases", "cases")
    p_recovered <- c(p_recovered, est$val)
    p_recovered_low <- c(p_recovered_low, est$low)
    p_recovered_high <- c(p_recovered_high, est$upp)
    
    est <- process_ratio(dt_date, "fatalities", "cases", "cases")
    p_fatalities <- c(p_fatalities, est$val)
    p_fatalities_low <- c(p_fatalities_low, est$low)
    p_fatalities_high <- c(p_fatalities_high, est$upp)
    
    est <- process_ratio(dt_date, "fatalities", "reach", "cases")
    fatalities_est <- c(fatalities_est, pop * est$val)
    fatalities_low <- c(fatalities_low, pop * est$low)
    fatalities_high <- c(fatalities_high, pop * est$upp)
    
    est <- process_ratio(dt_date, "recentcases", "cases", "cases")
    p_recentcases <- c(p_recentcases, est$val)
    p_recentcases_low <- c(p_recentcases_low, est$low)
    p_recentcases_high <- c(p_recentcases_high, est$upp)
    
    est <- process_ratio(dt_date, "recentcases", "reach", "cases")
    recentcases_est <- c(recentcases_est, pop * est$val)
    recentcases_low <- c(recentcases_low, pop * est$low)
    recentcases_high <- c(recentcases_high, pop * est$upp)
    
    est <- process_ratio(dt_date, "recentcasesnursing", "cases", "cases")
    p_recentcasesnursing <- c(p_recentcasesnursing, est$val)
    p_recentcasesnursing_low <- c(p_recentcasesnursing_low, est$low)
    p_recentcasesnursing_high <- c(p_recentcasesnursing_high, est$upp)
    
    est <- process_ratio(dt_date, "stillsick", "cases", "cases")
    p_stillsick <- c(p_stillsick, est$val)
    p_stillsick_low <- c(p_stillsick_low, est$low)
    p_stillsick_high <- c(p_stillsick_high, est$upp)
    
    est <- process_ratio(dt_date, "hospital", "cases", "cases")
    p_hospital <- c(p_hospital, est$val)
    p_hospital_low <- c(p_hospital_low, est$low)
    p_hospital_high <- c(p_hospital_high, est$upp)
    
    est <- process_ratio(dt_date, "hospital", "reach", "cases")
    hospital_est <- c(hospital_est, pop * est$val)
    hospital_low <- c(hospital_low, pop * est$low)
    hospital_high <- c(hospital_high, pop * est$upp)
    
    est <- process_ratio(dt_date, "severe", "cases", "cases")
    p_severe <- c(p_severe, est$val)
    p_severe_low <- c(p_severe_low, est$low)
    p_severe_high <- c(p_severe_high, est$upp)
    
    est <- process_ratio(dt_date, "icu", "cases", "cases")
    p_icu <- c(p_icu, est$val)
    p_icu_low <- c(p_icu_low, est$low)
    p_icu_high <- c(p_icu_high, est$upp)
    
    est <- process_ratio(dt_date, "icu", "reach", "cases")
    icu_est <- c(icu_est, pop * est$val)
    icu_low <- c(icu_low, pop * est$low)
    icu_high <- c(icu_high, pop * est$upp)
    
    est <- process_ratio(dt_date, "tested", "reach", "reach")
    p_tested <- c(p_tested, est$val)
    p_tested_low <- c(p_tested_low, est$low)
    p_tested_high <- c(p_tested_high, est$upp)
    
    est <- process_ratio(dt_date, "positive", "tested", "tested")
    p_positive <- c(p_positive, est$val)
    p_positive_low <- c(p_positive_low, est$low)
    p_positive_high <- c(p_positive_high, est$upp)
    
    
    population <- c(population, pop)
  }
  
  dd <- data.frame(date = dates,
                   region,
                   regionname,
                   sample_size,
                   reach,
                   
                   cases_est,
                   cases_low,
                   cases_high,
                   
                   fatalities_est,
                   fatalities_low,
                   fatalities_high,
                   
                   recentcases_est,
                   recentcases_low,
                   recentcases_high,
                   
                   hospital_est,
                   hospital_low,
                   hospital_high,
                   
                   icu_est,
                   icu_low,
                   icu_high,
                   
                   p_cases,
                   p_cases_low,
                   p_cases_high,
                   
                   p_recovered,
                   p_recovered_low,
                   p_recovered_high,
                   
                   p_fatalities,
                   p_fatalities_low,
                   p_fatalities_high,
                   
                   p_recentcases,
                   p_recentcases_low,
                   p_recentcases_high,
                   
                   p_recentcasesnursing,
                   p_recentcasesnursing_low,
                   p_recentcasesnursing_high,
                   
                   p_stillsick,
                   p_stillsick_low,
                   p_stillsick_high,
                   
                   p_hospital,
                   p_hospital_low,
                   p_hospital_high,
                   
                   p_severe,
                   p_severe_low,
                   p_severe_high,
                   
                   p_icu,
                   p_icu_low,
                   p_icu_high,
                   
                   p_tested,
                   p_tested_low,
                   p_tested_high,
                   
                   p_positive,
                   p_positive_low,
                   p_positive_high,
                   
                   population,
                   stringsAsFactors = F)
  
  return(dd)
}




cat("Rivas-Arganda daily script run at ", as.character(Sys.time()), "\n\n")

file_path <- paste0(responses_path, country_iso, "-aggregate.csv")
dt <- read.csv(file_path, as.is = T)
cat("Received ", nrow(dt), " responses\n\n")
names(dt) <- tolower(names(dt))

#list of regions
region_tree <- read.csv(data_path, as.is = T)
names(region_tree) <- tolower(names(region_tree))
regions <- region_tree$provincecode
region_names <- region_tree$regionname
populations <- region_tree$population

#list of dates
dates <- as.character(seq.Date(as.Date(dt$timestamp[1]), as.Date(tail(dt$timestamp,1)), by = "days"))
dates <- gsub("-","/", dates)

#list responses per date
for (i in 1:length(regions)){
  dta <- dt[dt$iso.3166.2==regions[i],]
  cat("From ", regions[i], " received ", nrow(dta), " responses\n")
  for (j in 1:length(dates)){
    dtaa <- dta[dta$timestamp==dates[j],]
    cat("-- On day ", dates[j], "received", nrow(dtaa), " responses, \n")
  }
}
cat("\n")

dt <- remove_outliers(dt,max_ratio)

for (i in 1:length(regions)){
  reg <- regions[i]
  name <- region_names[i]
  cat("Processing", reg, "\n")
  dd <- process_region(dt[dt$iso.3166.2 == reg, ], reg, name, pop=populations[i], dates)
  cat("- Writing estimates for:", reg, "\n")
  write.csv(dd, paste0(estimates_path, reg, "-estimate.csv"))
}

