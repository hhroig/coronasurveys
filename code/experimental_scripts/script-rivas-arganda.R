library(tidyr)
library(dplyr)


calculate_ci <- function(p_est, level, pop_size) {
  z <- qnorm(level+(1-level)/2)
  se <- sqrt(p_est*(1-p_est))/sqrt(pop_size)
  return(list(low=p_est-z*se, upp=p_est+z*se, error=z*se))
}

responses_path <- "../../data/aggregate/rivas-arganda/"
data_path <- "../../data/common-data/rivas-arganda/regions-tree-population.csv"
max_ratio <- 1/3
asymptomatic_factor <- 1/0.66

country_iso <- "ES"

cat("Rivas-Arganda script run at ", Sys.time(), "\n")

region_tree <- read.csv(data_path, as.is = T)
names(region_tree) <- tolower(names(region_tree))
regions <- region_tree$provincecode
populations <- region_tree$population

file_path <- paste0(responses_path, country_iso, "-aggregate.csv")
dt <- read.csv(file_path, as.is = T)
cat("Received ", nrow(dt), " responses\n")
names(dt) <- tolower(names(dt))

#list of dates
dates <- as.character(seq.Date(as.Date(dt$timestamp[1]), as.Date(tail(dt$timestamp,1)), by = "days"))
dates <- gsub("-","/", dates)
  
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

  #remove outliers of reach.
  dt <- dt[!is.na(dt$reach),]
  dt <- dt[dt$reach != 0, ]
  cat("Responses after removing reach=NA or reach=0:", nrow(dt), "\n")
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  dt <- dt[dt$reach <= reach_cutoff, ]
  cat("Responses after removing ouliers with cutoff", reach_cutoff, ":", nrow(dt), "\n")
  
  # remove outliers based on max cases/reach ratio
  dt <- dt[!is.na(dt$cases),]
  dt$ratio <- dt$cases/dt$reach
  dt <- dt[dt$ratio <= max_ratio, ]
  cat("::- script-rivas-arganda: Responses after removing ouliers with cases=NA or cases/reach ratio >", 
      max_ratio, ":", nrow(dt), "\n")
  
  for (i in 1:length(regions)){
    reg <- regions[i]
    pop <- populations[i]
    # get data for the current region
    dt_r <- dt[dt$iso.3166.2 == reg, ]
    cat("Working on region: ", reg, "with", pop, "population,", nrow(dt_r), "responses & reach", sum(dt_r$reach), "\n")
    
    p_cases <- sum(dt_r$cases)/sum(dt_r$reach) 
    p_cases_low <- calculate_ci(p_est = p_cases, level = 0.95, pop_size = sum(dt_r$reach))$low
    p_cases_high <- calculate_ci(p_est = p_cases, level=0.95, pop_size = sum(dt_r$reach))$upp
    pc <- c(p_cases, p_cases_low, p_cases_high)
    cases <- pc * pop
    cat("- Prevalencia (intervalo confianza 95%), sintomaticos:", pc, "Total:", pc*asymptomatic_factor, "\n")
    cat("- Numero de casos (intervalo confianza 95%), sintomaticos:", cases, "Total:", cases*asymptomatic_factor, "\n")
    
    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$fatalities),]
    dt_f <- dt_f[dt_f$fatalities <= dt_f$cases,]
    #cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), "responses after removing ouliers with fatalities=NA or cases > fatalities\n")
    p_cfr <- sum(dt_f$fatalities)/sum(dt_f$cases)
    p_cfr_low <- calculate_ci(p_est = p_cfr, level = 0.95, pop_size = sum(dt_f$cases))$low
    p_cfr_high <- calculate_ci(p_est = p_cfr, level=0.95, pop_size = sum(dt_f$cases))$upp
    cfr <- c(p_cfr, p_cfr_low, p_cfr_high)
    cat("- Tasa de fallecimientos sobre sintomaticos CFR (intervalo confianza 95%)", cfr, "\n")
    cat("- Letalidad IFR (intervalo confianza 95%)", cfr/asymptomatic_factor, "\n")
    
    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$hospital),]
    dt_f <- dt_f[dt_f$hospital <= dt_f$cases,]
    #cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), "responses after removing ouliers with hospital=NA or cases > hospital\n")
    p_hospital <- sum(dt_f$hospital)/sum(dt_f$cases)
    p_hospital_low <- calculate_ci(p_est = p_hospital, level = 0.95, pop_size = sum(dt_f$cases))$low
    p_hospital_high <- calculate_ci(p_est = p_hospital, level=0.95, pop_size = sum(dt_f$cases))$upp
    hospital <- c(p_hospital, p_hospital_low, p_hospital_high)
    cat("- Tasa de casos hospitalizados (intervalo confianza 95%)", hospital, "\n")
    
    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$icu),]
    dt_f <- dt_f[dt_f$icu <= dt_f$cases,]
    #cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), "responses after removing ouliers with icu=NA or cases > icu\n")
    p_icu <- sum(dt_f$icu)/sum(dt_f$cases)
    p_icu_low <- calculate_ci(p_est = p_icu, level = 0.95, pop_size = sum(dt_f$cases))$low
    p_icu_high <- calculate_ci(p_est = p_icu, level=0.95, pop_size = sum(dt_f$cases))$upp
    icu <- c(p_icu, p_icu_low, p_icu_high)
    cat("- Tasa de casos en UCI (intervalo confianza 95%)", icu, "\n")
    
    dt_f <- dt_r
    dt_f <- dt_f[!is.na(dt_f$recentcases),]
    dt_f <- dt_f[dt_f$recentcases <= dt_f$cases,]
    #cat("::- script-rivas-arganda: Region: ", reg, nrow(dt_f), "responses after removing ouliers with recentcases=NA or cases > recentcases\n")
    p_recentcases <- sum(dt_f$recentcases)/sum(dt_f$reach)
    p_recentcases_low <- calculate_ci(p_est = p_recentcases, level = 0.95, pop_size = sum(dt_f$reach))$low
    p_recentcases_high <- calculate_ci(p_est = p_recentcases, level=0.95, pop_size = sum(dt_f$reach))$upp
    recent <- c(p_recentcases, p_recentcases_low, p_recentcases_high)
    cat("- Tasa de nuevos casos en la última semana (intervalo confianza 95%):", recent, "\n")
    cat("- Numero de nuevos casos (intervalo confianza 95%):", recent*pop, "\n")
    
  }


  
  