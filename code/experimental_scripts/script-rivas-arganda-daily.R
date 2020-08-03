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

responses_path <- "../../data/aggregate/rivas-arganda/"
data_path <- "../../data/common-data/rivas-arganda/regions-tree-population.csv"
estimates_path <- "../../data/estimates-rivas-arganda/"

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
  cat("::- script-300responses: processing", regions[i], "::\n")
  dd <- process_region(dt[dt$iso.3166.2 == reg, ], reg=regions[i], pop=populations[i])
  cat("::- script-300responses: Writing estimates for:", regions[i], "::\n")
  #write.csv(dd, paste0("PlotData/", regions[i], "-estimate.csv"))
}



stop(T)




  
#list of regions
#regions <- unique(dt$iso.3166.2)
  
for (i in 1:length(regions)){
  dta <- dt[dt$iso.3166.2==regions[i],]
  cat("From ", regions[i], " received ", nrow(dta), " responses\n")
  for (j in 1:length(dates)){
    dtaa <- dta[dta$timestamp==dates[j],]
    cat("-- On day ", dates[j], "received", nrow(dtaa), " responses, \n")
  }
}
cat("\n")


  
  pop_rivas <- sum(populations[1:3])
  weight_rivas <- c()
  for (i in 1:length(regions)-1){
    weight_rivas <- c(weight_rivas, populations[i]/pop_rivas)
  }
  weight_rivas <- c(weight_rivas, 0)
  
  p_cases_rivas <- 0
  p_cfr_rivas <- 0
  p_hospital_rivas <- 0
  p_icu_rivas <- 0
  p_recentcases_rivas <- 0
  
  for (i in 1:length(regions)){
    reg <- regions[i]
    pop <- populations[i]
    # get data for the current region
    dt_r <- dt[dt$iso.3166.2 == reg, ]
    cat("\n Working on region: ", reg, "with", pop, "population,", nrow(dt_r), " responses & ", sum(dt_r$reach), " reach\n")
    
    p_cases <- sum(dt_r$cases)/sum(dt_r$reach) 
    p_cases_low <- calculate_ci(p_est = p_cases, level=0.95, pop_size = sum(dt_r$reach))$low
    p_cases_high <- calculate_ci(p_est = p_cases, level=0.95, pop_size = sum(dt_r$reach))$upp
    pc <- c(p_cases, p_cases_low, p_cases_high)
    cases <- pc * pop
    cat("- Prevalencia (intervalo confianza 95%):", pc, "\n")
    cat("- Numero de casos (intervalo confianza 95%):", cases, "\n")
    
    p_cases_rivas <- p_cases_rivas + weight_rivas[i]*p_cases

    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$fatalities),]
    dt_f <- dt_f[dt_f$fatalities <= dt_f$cases,]
    #cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), "responses after removing ouliers with fatalities=NA or cases > fatalities\n")
    p_cfr <- sum(dt_f$fatalities)/sum(dt_f$cases)
    p_cfr_low <- calculate_ci(p_est = p_cfr, level = 0.95, pop_size = sum(dt_f$cases))$low
    p_cfr_high <- calculate_ci(p_est = p_cfr, level=0.95, pop_size = sum(dt_f$cases))$upp
    cfr <- c(p_cfr, p_cfr_low, p_cfr_high)
    cat("- Tasa de fallecimientos sobre casos - Case Fatality Rate CFR (intervalo confianza 95%)", cfr, "\n")
    #cat("- Letalidad IFR (intervalo confianza 95%)", cfr/asymptomatic_factor, "\n")
    
    p_cfr_rivas <- p_cfr_rivas + weight_rivas[i]*p_cfr
    
    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$hospital),]
    dt_f <- dt_f[dt_f$hospital <= dt_f$cases,]
    #cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), "responses after removing ouliers with hospital=NA or cases > hospital\n")
    p_hospital <- sum(dt_f$hospital)/sum(dt_f$cases)
    p_hospital_low <- calculate_ci(p_est = p_hospital, level = 0.95, pop_size = sum(dt_f$cases))$low
    p_hospital_high <- calculate_ci(p_est = p_hospital, level=0.95, pop_size = sum(dt_f$cases))$upp
    hospital <- c(p_hospital, p_hospital_low, p_hospital_high)
    cat("- Tasa de casos hospitalizados (intervalo confianza 95%)", hospital, "\n")
    
    p_hospital_rivas <- p_hospital_rivas + weight_rivas[i]*p_hospital
    
    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$icu),]
    dt_f <- dt_f[dt_f$icu <= dt_f$cases,]
    #cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), "responses after removing ouliers with icu=NA or cases > icu\n")
    p_icu <- sum(dt_f$icu)/sum(dt_f$cases)
    p_icu_low <- calculate_ci(p_est = p_icu, level = 0.95, pop_size = sum(dt_f$cases))$low
    p_icu_high <- calculate_ci(p_est = p_icu, level=0.95, pop_size = sum(dt_f$cases))$upp
    icu <- c(p_icu, p_icu_low, p_icu_high)
    cat("- Tasa de casos en UCI (intervalo confianza 95%)", icu, "\n")
    
    p_icu_rivas <- p_icu_rivas + weight_rivas[i]*p_icu
    
    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$recentcases),]
    dt_f <- dt_f[dt_f$recentcases <= dt_f$cases,]
    #cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), "responses after removing ouliers with recentcases=NA or cases > recentcases\n")
    p_recentcases <- sum(dt_f$recentcases)/sum(dt_f$reach)
    p_recentcases_low <- calculate_ci(p_est = p_recentcases, level = 0.95, pop_size = sum(dt_f$reach))$low
    p_recentcases_high <- calculate_ci(p_est = p_recentcases, level=0.95, pop_size = sum(dt_f$reach))$upp
    recent <- pmax(0,c(p_recentcases, p_recentcases_low, p_recentcases_high))
    cat("- Tasa de nuevos casos en la última semana (intervalo confianza 95%):", recent, "\n")
    cat("- Numero de nuevos casos (intervalo confianza 95%):", recent*pop, "\n")
    
    p_recentcases_rivas <- p_recentcases_rivas + weight_rivas[i]*p_recentcases
  }

  cat("\n Working on todo Rivas with", pop_rivas, "population\n")
  cat("- Prevalencia:", p_cases_rivas, "\n")
  cat("- Tasa de fallecimientos sobre casos - Case Fatality Rate CFR:", p_cfr_rivas, "\n")
  cat("- Tasa de casos hospitalizados:", p_hospital_rivas, "\n")
  cat("- Tasa de casos en UCI:", p_icu_rivas, "\n")
  cat("- Tasa de nuevos casos en la última semana:", p_recentcases_rivas, "\n")
  cat("- Numero de nuevos casos:", p_recentcases_rivas*pop_rivas, "\n")
