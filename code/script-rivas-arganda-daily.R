library(tidyr)
library(dplyr)

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

process_ratio <- function(dt, numerator, denominator){
  dta <- dt[!is.na(dt[[numerator]]),]
  dta <- dta[!is.na(dta[[denominator]]),]
  dta <- dta[dta[[numerator]] <= dta[[denominator]],]
  if (nrow(dta)>0){
    return(sum(dta[[numerator]])/sum(dta[[denominator]]))
  }
  else {
    return(NA)
  }
}

process_region <- function(dt, reg, pop, num_responses = 100){
  cat("reg pop", reg, pop, nrow(dt), "\n")
  #list of dates
  dates <- as.character(seq.Date(as.Date(dt$timestamp[1]), as.Date(tail(dt$timestamp,1)), by = "days"))
  dates <- gsub("-","/", dates)
  
  sample_size <- c()
  reach <- c()
  p_cases <- c()
  p_recovered <- c()
  p_fatalities <- c()
  p_recentcases <- c()
  p_recentcasesnursing <- c()
  p_stillsick <- c()
  p_hospital <- c()
  p_severe <- c()
  p_icu <- c()
  p_tested <- c()
  p_positive <- c()
  population <- c()
  
  for (j in dates){
    dt_date <- tail(dt[as.Date(dt$timestamp) <= as.Date(j), ], num_responses)
    cat("::- script-300responses: Working on date: ", j, "with", nrow(dt_date), "responses\n"  )
    
    reach_date <- sum(dt_date$reach)
    cases_date <- sum(dt_date$cases)
    
    sample_size <- c(sample_size, nrow(dt_date))
    reach <- c(reach, sum(dt_date$reach))
    p_cases <- c(p_cases, process_ratio(dt_date, "cases", "reach"))
    p_recovered <- c(p_recovered, process_ratio(dt_date, "recovered", "cases"))
    p_fatalities <- c(p_fatalities, process_ratio(dt_date, "fatalities", "cases"))
    p_recentcases <- c(p_recentcases, process_ratio(dt_date, "recentcases", "cases"))
    p_recentcasesnursing <- c(p_recentcasesnursing, process_ratio(dt_date, "recentcasesnursing", "cases"))
    p_stillsick <- c(p_stillsick, process_ratio(dt_date, "stillsick", "cases"))
    p_hospital <- c(p_hospital, process_ratio(dt_date, "hospital", "cases"))
    p_severe <- c(p_severe, process_ratio(dt_date, "severe", "cases"))
    p_icu <- c(p_icu, process_ratio(dt_date, "icu", "cases"))
    p_tested <- c(p_tested, process_ratio(dt_date, "tested", "reach"))
    p_positive <- c(p_positive, process_ratio(dt_date, "positive", "tested"))
    population <- c(population, pop)
  }
  
  dd <- data.frame(date = dates,
                                      sample_size,
                                      reach,
                                      p_cases,
                                      p_recovered,
                                      p_fatalities,
                                      p_recentcases,
                                      p_recentcasesnursing,
                                      p_stillsick,
                                      p_hospital,
                                      p_severe,
                                      p_icu,
                                      p_tested,
                                      p_positive,
                                      population,
                                      stringsAsFactors = F)
  
  return(dd)
}

calculate_ci <- function(p_est, level, pop_size) {
  z <- qnorm(level+(1-level)/2)
  se <- sqrt(p_est*(1-p_est))/sqrt(pop_size)
  return(list(low=p_est-z*se, upp=p_est+z*se)) #, error=z*se))
}





cat("Rivas-Arganda daily script run at ", as.character(Sys.time()), "\n\n")

responses_path <- "../data/aggregate/rivas-arganda/"
data_path <- "../data/common-data/rivas-arganda/regions-tree-population.csv"
estimates_path <- "../data/estimates-rivas-arganda/"

country_iso <- "ES"

file_path <- paste0(responses_path, country_iso, "-aggregate.csv")
dt <- read.csv(file_path, as.is = T)
cat("Received ", nrow(dt), " responses\n\n")
names(dt) <- tolower(names(dt))

dt <- remove_outliers(dt)

region_tree <- read.csv(data_path, as.is = T)
names(region_tree) <- tolower(names(region_tree))
regions <- region_tree$provincecode
populations <- region_tree$population

for (i in 1:length(regions)){
  reg <- regions[i]
  cat("::- script-300responses: processing", reg, "::\n")
  dd <- process_region(dt[dt$iso.3166.2 == reg, ], reg, pop=populations[i])
  cat("::- script-300responses: Writing estimates for:", reg, "::\n")
  write.csv(dd, paste0(estimates_path, reg, "-estimate.csv"))
}

