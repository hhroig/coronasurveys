## script needs file for country and country population.
library(tidyverse)
library(readxl)
library(httr)


## set working drirectoy here
#setwd("please put the absolute folder path of where this script is located on the server")

## 
# get data from jh csce
get_jh_data <- function(){
  ## fix iso look up file
  iso_code_source <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"    
  
  dt_iso <- read.csv(iso_code_source, as.is = T)
  dt_iso$Province_State[dt_iso$Province_State == ""] <- dt_iso$Country_Region[dt_iso$Province_State == ""] 
  
  dt_iso <- dt_iso %>% 
    group_by(iso2) %>% 
    summarise(country = Province_State[1], 
              population = Population[1]) %>% 
    arrange(country)
  # add namibia
  dt_iso$iso2[dt_iso$country == "Namibia"] <- "NA"
  
  ## work on cases file
  
  file_source_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  dt_cases <- read.csv(file = file_source_cases, as.is =T)
  
  # handle australia cases 
  dt_aus <- dt_cases[dt_cases$Country.Region == "Australia", ] %>% 
    select(-Province.State) %>% 
    group_by(Country.Region) %>% 
    summarise_all(sum)
  dt_aus$Province.State <- NA
  
  #handle canada data
  # take of the Recovered row here
  dt_can <- dt_cases[dt_cases$Country.Region == "Canada", ] %>% 
    select(-Province.State) %>% 
    group_by(Country.Region) %>% 
    summarise_all(sum)
  dt_can$Province.State <- NA
  
  # handle China Data
  dt_chn <- dt_cases[dt_cases$Country.Region == "China", ] %>% 
    select(-Province.State) %>% 
    group_by(Country.Region) %>% 
    summarise_all(sum)
  dt_chn$Province.State <- NA
  
  # remove china, canada and australia 
  dt_cases <- dt_cases[-c(which(dt_cases$Country.Region == "Australia"),
                          which(dt_cases$Country.Region == "Canada"),
                          which(dt_cases$Country.Region == "China")),  ]
  # add the new canada, china and aus data
  dt_cases <- bind_rows(dt_cases, dt_aus, dt_can, dt_chn)
  
  # search for duplicated cases
  n_occur <- data.frame(table(dt_cases$Country.Region), stringsAsFactors = F)  
  dt_cases_dup <- dt_cases[dt_cases$Country.Region %in% as.character(n_occur[n_occur$Freq > 1,]$Var1),]
  dt_cases_dup$Province.State[dt_cases_dup$Province.State == ""] <- dt_cases_dup$Country.Region[dt_cases_dup$Province.State == ""]
  dt_cases_dup$Country.Region <- dt_cases_dup$Province.State
  
  # replace duplicated entries
  dt_cases[dt_cases$Country.Region %in% as.character(n_occur[n_occur$Freq > 1,]$Var1),] <- dt_cases_dup
  
  dt_cases <- dt_cases[, -c(1,3,4)] 
  
  # deal with names
  names(dt_cases) <- c("country", names(dt_cases[-1]))
  names(dt_cases) <- gsub(patter = "X", replacement = "", names(dt_cases))
  names(dt_cases) <- gsub(patter = "[.]", replacement = "/", names(dt_cases))
  names(dt_cases) <- c("country", as.character(lubridate::mdy(names(dt_cases[-1]))))
  
  # gather the dates together
  dt_cases <- dt_cases %>% gather(key = "date", value = "cum_cases", -country)
  
  
  # deal with deaths files
  file_source_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  dt_deaths <- read.csv(file = file_source_deaths, as.is =T)
  
  # handle australia cases 
  dt_aus_d <- dt_deaths[dt_deaths$Country.Region == "Australia", ] %>% 
    select(-Province.State) %>% 
    group_by(Country.Region) %>% 
    summarise_all(sum)
  dt_aus_d$Province.State <- NA
  
  #handle canada data
  # take of the Recovered row here
  dt_can_d <- dt_deaths[dt_deaths$Country.Region == "Canada", ] %>% 
    select(-Province.State) %>% 
    group_by(Country.Region) %>% 
    summarise_all(sum)
  dt_can_d$Province.State <- NA
  
  # handle China Data
  dt_chn_d <- dt_deaths[dt_deaths$Country.Region == "China", ] %>% 
    select(-Province.State) %>% 
    group_by(Country.Region) %>% 
    summarise_all(sum)
  dt_chn_d$Province.State <- NA
  
  # remove china, canada and australia 
  dt_deaths <- dt_deaths[-c(which(dt_deaths$Country.Region == "Australia"),
                            which(dt_deaths$Country.Region == "Canada"),
                            which(dt_deaths$Country.Region == "China")),  ]
  # add the new canada, china and aus data
  dt_deaths <- bind_rows(dt_deaths, dt_aus_d, dt_can_d, dt_chn_d)
  
  # search for duplicated cases
  n_occur <- data.frame(table(dt_deaths$Country.Region), stringsAsFactors = F)  
  dt_deaths_dup <- dt_deaths[dt_deaths$Country.Region %in% as.character(n_occur[n_occur$Freq > 1,]$Var1),]
  dt_deaths_dup$Province.State[dt_deaths_dup$Province.State == ""] <- dt_deaths_dup$Country.Region[dt_deaths_dup$Province.State == ""]
  dt_deaths_dup$Country.Region <- dt_deaths_dup$Province.State
  
  
  
  
  # replace duplicated entries
  dt_deaths[dt_deaths$Country.Region %in% as.character(n_occur[n_occur$Freq > 1,]$Var1),] <- dt_deaths_dup
  
  # remove province, lat and long
  dt_deaths <- dt_deaths[, -c(1,3,4)] 
  
  # deal with names
  names(dt_deaths) <- c("country", names(dt_deaths[-1]))
  names(dt_deaths) <- gsub(patter = "X", replacement = "", names(dt_deaths))
  names(dt_deaths) <- gsub(patter = "[.]", replacement = "/", names(dt_deaths))
  names(dt_deaths) <- c("country", as.character(lubridate::mdy(names(dt_deaths[-1]))))
  
  # gather the dates together
  dt_deaths <- dt_deaths %>% gather(key = "date", value = "cum_deaths", -country)
  
  
  # create daily cases
  dt_jh <- full_join(dt_cases, dt_deaths) %>% 
    arrange(country, date) %>% 
    group_by(country) %>% 
    mutate(cases = c(0, diff(cum_cases)),
           deaths = c(0, diff(cum_deaths))) 
  
  # now add country codes and population and remove entries with no country code
  dt_jh  <- full_join(dt_jh, dt_iso, by = "country") %>% 
    filter(!is.na(iso2))
  return(dt_jh)
}

# create data twitter survey data
survey_twitter_esp <- data.frame(date = c("2020/03/14", "2020/03/16", "2020/03/18"), 
                                 survey_twitter = c((374.05/(762*150))* 46754778, (66.13/(85*150))*46754778,
                                                    (116.16/(120*150))*46754778), stringsAsFactors = F)

# create data twitter survey data
survey_twitter_pt <- data.frame(date = c("2020/03/18", "2020/03/20"), 
                                survey_twitter = c((11/(63*150))*10261075, 15/(45*150)*10261075),
                                stringsAsFactors = F)



hosp_to_death_trunc <- function(x, mu_hdt, sigma_hdt){
  dlnorm(x, mu_hdt, sigma_hdt)
}
# Functions from https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html
# Hospitalisation to death distribution

# Function to work out correction CFR
scale_cfr <- function(data_1_in, delay_fun, mu_hdt, sigma_hdt){
  case_incidence <- data_1_in$cases
  death_incidence <- data_1_in$deaths
  cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
  # Sum over cases up to time tt
  for(ii in 1:length(case_incidence)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj, mu_hdt = mu_hdt, sigma_hdt = sigma_hdt))
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
calculate_ci <- function(p_est, level, pop_size) {
  z <- qnorm(level+(1-level)/2)
  se <- sqrt(p_est*(1-p_est))/sqrt(pop_size)
  return(list(low=p_est-z*se, upp=p_est+z*se, error=z*se))
}
get_countries_with_survey <- function(path = "../data/aggregate/"){
  #get list of countries with surveys
  plotdata_files <- list.files(path)
  plotdata_files <- plotdata_files[plotdata_files != "Twitter-surveys.csv"]
  substr(plotdata_files,start = 1, stop = 2)
}

estimate_cases_aggregate <- function(file_path,
                                     country_population,
                                     batch,
                                     method = c("antonio", "carlos"),
                                     max_ratio,
                                     correction_factor) {
  #cat("file_path is ", file_path, "\n")
  #cat("country_population is", country_population, "\n")
  dt <- read.csv(file_path, as.is = T)
  names(dt) <- c("timestamp","region","reach","cases")
  dt$date <- substr(dt$timestamp, 1, 10)
  n_inital_response <- nrow(dt)
  
  # remove outliers from reach column
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  if(sum(dt$reach > reach_cutoff) > 0 ){
    # write.table(dt[dt$reach >= reach_cutoff, ],
    #             file = paste0("outliers_removed/", file_name, "_", "outliers_reach.txt"),
    #              append = T) # write out outliers from reach column to the ouliers removed folder
    n_reach_outliers <- sum(dt$reach > reach_cutoff) #number of outliers removed based on reach
    dt <- dt[dt$reach <= reach_cutoff, ]
  }else{
    n_reach_outliers <- 0
  }
  
  # remove outliers based on max ratio of   0.3
  dt$ratio <- dt$cases/dt$reach
  dt2 <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
  n_zero_reach_outliers <- sum(!is.finite(dt$ratio)) 
  if(sum(dt2$ratio > max_ratio) > 0 ){
    #write.table(dt[dt$ratio >= max_ratio, ],
    #            file = paste0("outliers_removed/", file_name, "_", "outliers_max_ratio.txt"),
    #            append = T) # write out outliers based on max_Ratio
    n_maxratio_outliers <- sum(dt2$ratio > max_ratio) 
    dt2 <- dt2[dt2$ratio <= max_ratio, ]
  }else{
    n_maxratio_outliers <- 0
  }
  
  
  method <- match.arg(method)
  
  if (method == "antonio"){
    dt_batch <- dt2 %>%
      group_by(date) %>% 
      summarise(sample_size = n())
    # generate grouping variable: if number of responses in a day is sufficient, then agg that day, if not agg multiple days
    group = 1
    group_factor = c()
    container <- c()
    
    for (i in 1:nrow(dt_batch)) {
      container <- c(container, dt_batch$sample_size[i])
      if(sum(container) < batch){
        group_factor <- c(group_factor, group)
      } else{
        group_factor <- c(group_factor, group)
        container <- c()
        group = group + 1
      }
    }
    dt_batch$group_factor <- group_factor
    
    dt_batch_s <- dt_batch %>% 
      group_by(group_factor) %>%
      summarise(n = sum(sample_size)) 
    dt_batch_s$include <- dt_batch_s$n >= batch
    
    dt_batch <- full_join(dt_batch, dt_batch_s, by = "group_factor") %>% 
      filter(include == T)
    
    dt2 <- full_join(dt2, dt_batch[,-c(2, 4,5)], by = "date")
    dt_summary <- dt2 %>%
      filter(!is.na(group_factor)) %>% 
      group_by(group_factor) %>% 
      summarise(date = last(date),
                sample_size = n(), 
                mean_cases = mean(cases),
                mean_reach = mean(reach),
                dunbar_reach = 150 * n(),
                cases_p_reach = sum(cases)/sum(reach), 
                cases_p_reach_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                                 pop_size = sum(reach))$low,
                cases_p_reach_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                  pop_size = sum(reach))$upp,
                cases_p_reach_error = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                   pop_size = sum(reach))$error,
                cases_p_reach_prop = mean(ratio), 
                cases_p_reach_prop_median = median(ratio),
                estimated_cases = country_population * sum(cases)/sum(reach) * correction_factor, 
                estimate_cases_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                                  pop_size = sum(reach))$low *  country_population * correction_factor,
                estimate_cases_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                   pop_size = sum(reach))$upp *  country_population * correction_factor,
                estimate_cases_error = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                    pop_size = sum(reach))$error *  country_population * correction_factor,
                
                prop_cases = country_population * mean(ratio) * correction_factor,
                dunbar_cases = country_population * (sum(cases)/dunbar_reach) * correction_factor, 
                prop_cases_low = calculate_ci(p_est = mean(ratio), level = 0.95,
                                             pop_size = sum(reach))$low * country_population * correction_factor,
                prop_cases_high = calculate_ci(p_est = mean(ratio), level = 0.95,
                                              pop_size = sum(reach))$upp * country_population * correction_factor,
                prop_cases_error = calculate_ci(p_est = mean(ratio), level = 0.95,
                                               pop_size = sum(reach))$error * country_population * correction_factor,
                dunbar_cases_low = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                                                pop_size = dunbar_reach)$low * country_population * correction_factor,
                dunbar_cases_high = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                                                pop_size = dunbar_reach)$upp * country_population * correction_factor,
                dunbar_cases_error = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                                                 pop_size = dunbar_reach)$error * country_population * correction_factor)
    dt_summary <- dt_summary[, -1] # remove group factor variable
    dt_summary$cases_p_reach_low[dt_summary$cases_p_reach_low < 0]   <- 0.000001
    dt_summary$estimate_cases_low[dt_summary$estimate_cases_low < 0] <- 0.000001
    dt_summary$prop_cases_low[dt_summary$prop_cases_low < 0] <- 0.000001
    dt_summary$dunbar_cases_low[dt_summary$dunbar_cases_low < 0] <- 0.000001
    
  } else if (method == "carlos"){ #deprecated
    max_group <- nrow(dt2)/batch
    group_factor <- rep(1:floor(max_group), each = batch)
    group_factor <- c(group_factor, rep(NA, times = nrow(dt2) - length(group_factor)))
    dt2$group <- group_factor 
    
    dt_summary <- dt2 %>%
      filter(!is.na(group)) %>% 
      group_by(group) %>% 
      summarise(date = last(date),
                sample_size = n(),
                mean_cases = mean(cases),
                mean_reach = mean(reach),
                dunbar_reach = 150 * n(),
                cases_p_reach = sum(cases)/sum(reach), 
                cases_p_reach_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                                 pop_size = sum(reach))$low,
                cases_p_reach_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                  pop_size = sum(reach))$upp,
                cases_p_reach_error = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                   pop_size = sum(reach))$error,
                cases_p_reach_prop = mean(ratio), 
                cases_p_reach_prop_median = median(ratio),
                estimated_cases = country_population * sum(cases)/sum(reach) * correction_factor, 
                estimate_cases_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                                  pop_size = sum(reach))$low *  country_population * correction_factor,
                estimate_cases_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                   pop_size = sum(reach))$upp *  country_population * correction_factor,
                estimate_cases_error = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                    pop_size = sum(reach))$error *  country_population * correction_factor,
                prop_cases = country_population * mean(ratio) * correction_factor,
                dunbar_cases = country_population * (sum(cases)/dunbar_reach) * correction_factor) %>% 
      group_by(date) %>% 
      summarise(sample_size = mean(sample_size),
                mean_cases = mean(mean_cases),
                mean_reach = mean(mean_reach),
                dunbar_reach = mean(dunbar_reach),
                cases_p_reach = mean(cases_p_reach), 
                cases_p_reach_low = mean(cases_p_reach_low),
                cases_p_reach_high = mean(cases_p_reach_high),
                cases_p_reach_error = mean(cases_p_reach_error),
                cases_p_reach_prop = mean(cases_p_reach_prop), 
                cases_p_reach_prop_median = mean(cases_p_reach_prop_median),
                estimated_cases = mean(estimated_cases),
                estimate_cases_low = mean(estimate_cases_low),
                estimate_cases_high = mean(estimate_cases_high),
                estimate_cases_error = mean(estimate_cases_error),
                prop_cases = mean(prop_cases),
                dunbar_cases = mean(dunbar_cases))
  } else{
    stop("method can only be antonio or carlos")
  }
  
  return(list(dt_estimates = dt_summary,
              n_inital_response = n_inital_response,
              n_reach_outliers = n_reach_outliers,
              n_maxratio_outliers = n_maxratio_outliers, 
              n_zero_reach_outliers = n_zero_reach_outliers,
              n_final_response = sum(dt_summary$sample_size)))
  
}


plot_estimates <- function(country_geoid = "ES", 
                           batch_size = 30,
                           batching_method = "antonio",
                           max_ratio = .3,
                           correction_factor = 1, 
                           z_mean_hdt = 13,
                           z_sd_hdt = 12.7,
                           z_median_hdt = 9.1,
                           c_cfr_baseline = 1.38,
                           c_cfr_estimate_range = c(1.23, 1.53), 
                           survey_countries =  get_countries_with_survey(),
                           data_srce = c("jh", "ecdc"),
                           dts){
  mu_hdt = log(z_median_hdt)
  sigma_hdt = sqrt(2*(log(z_mean_hdt) - mu_hdt))
  ## process data based on data source
  if(data_srce == "ecdc"){
    data <- dts %>% 
      select(dateRep:popData2018, "Alpha.2.code" )
    data$geoId <- data$Alpha.2.code 
    data <- data %>% select(dateRep:popData2018)
    ##############
    data <- data[data$geoId == country_geoid,]
    dt <- data[rev(1:nrow(data)),]
    dt$cum_cases <- cumsum(dt$cases)
    dt$cum_deaths <- cumsum(dt$deaths)
    dt$cum_deaths_400 <- dt$cum_deaths * 400
    dt$date <- gsub("-", "/", dt$dateRep)
    ndt <- nrow(dt)
    est_ccfr <- rep(NA, ndt)
    est_ccfr_low <- rep(NA, ndt)
    est_ccfr_high <- rep(NA, ndt)
    cat("computing ccfr estimate for ", country_geoid, "...\n")
    for (i in ndt : 1) {
      data2t <- dt[1:i, c("cases", "deaths")]
      ccfr <- scale_cfr(data2t, delay_fun = hosp_to_death_trunc, mu_hdt = mu_hdt, sigma_hdt = sigma_hdt)
      fraction_reported <- c_cfr_baseline / (ccfr$cCFR*100)
      sigma_fraction_reported <- (1/ccfr$total_deaths)-(1/ccfr$cum_known_t)+ (1/1023) - (1/74130)
      fraction_reported_high <- fraction_reported * exp(1.96*sigma_fraction_reported)
      fraction_reported_low <- fraction_reported * exp(-(1.96*sigma_fraction_reported))
      est_ccfr_low[i] <- dt$cum_cases[i]*(1/fraction_reported_high)#swich low and high here coz of inverse.
      est_ccfr_high[i] <- dt$cum_cases[i]*(1/fraction_reported_low)
      est_ccfr[i] <- dt$cum_cases[i]*(1/fraction_reported)
      
    }
    
    
    dt$est_ccfr <- est_ccfr
    dt$est_ccfr_low <- est_ccfr_low
    dt$est_ccfr_high <- est_ccfr_high
    population_value  = dt$popData2018[1]
  } else if(data_srce == "jh"){
    dt <- dts[dts$geoId == country_geoid,]
    dt$cum_deaths_400 <- dt$cum_deaths * 400
    dt$date <- gsub("-", "/", dt$date)
    ndt <- nrow(dt)
    est_ccfr <- rep(NA, ndt)
    
    for (i in ndt : 1) {
      data2t <- dt[1:i, c("cases", "deaths")]
      ccfr <- scale_cfr(data2t, delay_fun = hosp_to_death_trunc, mu_hdt = mu_hdt, sigma_hdt = sigma_hdt)
      fraction_reported <- c_cfr_baseline / (ccfr$cCFR*100)
      sigma_fraction_reported <- (1/ccfr$total_deaths)-(1/ccfr$cum_known_t)+ (1/1023) - (1/74130)
      fraction_reported_high <- fraction_reported * exp(1.96*sigma_fraction_reported)
      fraction_reported_low <- fraction_reported * exp(-(1.96*sigma_fraction_reported))
      est_ccfr_low[i] <- dt$cum_cases[i]*(1/fraction_reported_high) #swich low and high here coz of inverse.
      est_ccfr_high[i] <- dt$cum_cases[i]*(1/fraction_reported_low)
      est_ccfr[i] <- dt$cum_cases[i]*(1/fraction_reported)
    }
    
    
    dt$est_ccfr <- est_ccfr
    dt$est_ccfr_low <- est_ccfr_low
    dt$est_ccfr_high <- est_ccfr_high
    population_value  = dt$population[1]
  }else{
    stop("only jh and ecdc allowed as data sources")
  }
  
  if(country_geoid %in% survey_countries){
    cat(country_geoid, "has a survey data file..", "reading survey data...", "\n")
    file_path = paste0("../data/aggregate/", country_geoid, "-aggregate.csv")
    
    
    
    survey_gforms_estimate <- estimate_cases_aggregate(file_path = file_path,
                                                       country_population = population_value,
                                                       max_ratio = max_ratio,
                                                       correction_factor = correction_factor, 
                                                       method = batching_method,
                                                       batch = batch_size)$dt_estimates
   
    # combine dt and survey forms estimates
    dt_res <- full_join(dt, survey_gforms_estimate, by = "date")
    # combine with survey twitter
    if (country_geoid == "ES"){
      cat(country_geoid, "has twitter data...adding twitter estimates..\n")
      dt_res <- full_join(dt_res, survey_twitter_esp, by = "date") %>% 
        select(countriesAndTerritories, geoId, date, cases, deaths, cum_cases, cum_deaths, cum_deaths_400,
               est_ccfr, est_ccfr_low, est_ccfr_high, sample_size:survey_twitter)
      
    } else if(country_geoid == "PT"){
      cat(country_geoid, "has twitter data...adding twitter estimates..\n")
      dt_res <- full_join(dt_res, survey_twitter_pt, by = "date") %>% 
        select(countriesAndTerritories, geoId, date, cases, deaths, cum_cases, cum_deaths, cum_deaths_400,
               est_ccfr, est_ccfr_low, est_ccfr_high, sample_size:survey_twitter)
    } else{
      
      dt_res <- dt_res %>% 
        select(countriesAndTerritories, geoId, date, cases, deaths, cum_cases, cum_deaths, cum_deaths_400,
               est_ccfr, est_ccfr_low, est_ccfr_high, sample_size:dunbar_cases_error)
    }
    
    if(data_srce == "jh"){
      write.csv(dt_res, paste0("../data/PlotData/jh_estimates/", country_geoid, "-", "estimates.csv"))
    }else{
      write.csv(dt_res, paste0("../data/PlotData/", country_geoid, "-", "estimates.csv"))
    }
    
    cat("estimates data for ", country_geoid, "saved successfully..\n")
  } else{
    
    survey_gforms_estimate <- data.frame(date = dt$date,
                                         sample_size = NA,
                                         mean_cases = NA,
                                         mean_reach = NA,
                                         dunbar_reach = NA,
                                         cases_p_reach = NA,
                                         cases_p_reach_low = NA,
                                         cases_p_reach_high = NA,
                                         cases_p_reach_error = NA,
                                         cases_p_reach_prop = NA,
                                         cases_p_reach_prop_median = NA,
                                         estimated_cases = NA,
                                         estimate_cases_low = NA,
                                         estimate_cases_high = NA,
                                         estimate_cases_error = NA,
                                         prop_cases = NA,
                                         dunbar_cases = NA,
                                         prop_cases_low = NA,
                                         prop_cases_high = NA,
                                         prop_cases_error = NA,
                                         dunbar_cases_low = NA,
                                         dunbar_cases_high = NA,
                                         dunbar_cases_error = NA,
                                         stringsAsFactors = F)

    # combine dt and survey forms estimates
    dt_res <- full_join(dt, survey_gforms_estimate, by = "date")
    
    dt_res <- dt_res %>% 
      select(countriesAndTerritories, geoId, date, cases, deaths, cum_cases, cum_deaths, cum_deaths_400,
             est_ccfr, est_ccfr_low, est_ccfr_high, sample_size:dunbar_cases_error)
   
    if(data_srce == "jh"){
      write.csv(dt_res, paste0("../data/PlotData/jh_estimates/", country_geoid, "-", "estimates.csv"))
    }else{
      write.csv(dt_res, paste0("../data/PlotData/", country_geoid, "-", "estimates.csv"))
    }
    cat("estimates data for ", country_geoid, "saved successfully..\n")
  }
  
}


generate_estimates <- function(srce = c("ecdc", "jh")){
  srce <- match.arg(srce)
  if(srce == "ecdc"){
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
                 Sys.Date(), ".xlsx", sep = "")
    GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
    try( data_ecdc <- read_excel(tf), silent = T)
    
    if(!is.data.frame(data_ecdc)){
      cat("Seems the ECDC data for today is not available yet is not availabe yet...", "\n")
      cat("Trying to get data for the previous day...", "\n")
      url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
                   Sys.Date() - 1, ".xlsx", sep = "")
      GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
      try( data_ecdc <- read_excel(tf), silent = T)
      if(!is.data.frame(data_ecdc)){
        stop("Unfortunately, the ECDC data for yesterday is not availabe neither...\n")
      }else{
        cat("Using ECDC data for previous day...\n")
        data_country_code <- read_excel("wikipedia-iso-country-codes.xlsx")
        names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
                                      "Alpha.3.code", "Numeric.code", "ISO.3166.2")
        
        data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
        all_geo_ids <- unique(data_ecdc$Alpha.2.code)
        sapply(all_geo_ids, plot_estimates,  data_srce = "ecdc", dts = data_ecdc)
      }
    } else{
      data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
      data_country_code <- read_excel("wikipedia-iso-country-codes.xlsx")
      names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
                                    "Alpha.3.code", "Numeric.code", "ISO.3166.2")
      
      data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
      all_geo_ids <- unique(data_ecdc$Alpha.2.code) 
      sapply(all_geo_ids, plot_estimates, data_srce = "ecdc", dts =  data_ecdc)
    }
  }else if(srce == "jh"){
    data_jh <- get_jh_data() %>% 
      select(date, country, cases, deaths, cum_cases, cum_deaths, iso2, population) %>% 
      rename(countriesAndTerritories = country, geoId = iso2) %>% 
      mutate(population = as.double(population))
    all_geo_ids <- unique(data_jh$geoId)
    # remove diamond princess
    all_geo_ids <- all_geo_ids[all_geo_ids != ""]
    sapply(all_geo_ids, plot_estimates, data_srce = "jh", dts =  data_jh)
      
  }else {stop("The only data sources allowed are jh and ecdc")}
}

generate_estimates(srce = "ecdc")


get_spain_regional_estimates <- function(batch_size = 30,
                                         batching_method = "antonio",
                                         max_ratio = .3,
                                         correction_factor = 1, 
                                         z_mean_hdt = 13,
                                         z_sd_hdt = 12.7,
                                         z_median_hdt = 9.1,
                                         c_cfr_baseline = 1.38,
                                         c_cfr_estimate_range = c(1.23, 1.53)){
  mu_hdt = log(z_median_hdt)
  sigma_hdt = sqrt(2*(log(z_mean_hdt) - mu_hdt))
  
  cases_data_source <-  read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos_long.csv", as.is = T)
  deaths_data_source <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos_long.csv", as.is = T)
  
  cases_data_source <- cases_data_source %>% 
    group_by(CCAA) %>% 
    mutate(cases = c(first(total), diff(total))) %>% 
    ungroup() %>% 
    rename(cum_cases = total)
  
  cases_data_source$cases[cases_data_source$cases<0]  <- 0
  
  deaths_data_source <- deaths_data_source %>% 
    group_by(CCAA) %>%
    mutate(deaths =  c(first(total), diff(total)) ) %>% 
    ungroup() %>% 
    rename(cum_deaths = total)
  # read population
  ccaa_pop <- read.csv("ccaa_population.csv", as.is = T)
  
  dt_ds <- full_join(cases_data_source, deaths_data_source) %>% 
    full_join(ccaa_pop, by = "cod_ine") %>% 
    select(-ccaa, -ccaa_survey) %>% 
    mutate(cum_deaths_400 = cum_deaths *400) %>%
    rename(date = fecha)
    
  dt_ds$date <- gsub("-", "/", dt_ds$date)
  
  dt_ds$deaths[is.na(dt_ds$deaths)] <- 0 
  
  
  
  dt_ds_est <- lapply(unique(dt_ds$CCAA), function(x){
    cat("working on the region", x, "\n")
    dt_ccca <- dt_ds[dt_ds$CCAA == x, ]
    ndt <- nrow(dt_ccca)
    est_ccfr <- rep(NA, ndt)
    #cat("computing ccfr estimate for ", country_geoid, "...\n")
    for (i in ndt : 1) {
      data2t <- dt_ccca[1:i, c("cases", "deaths")]
      ccfr <- scale_cfr(data2t, delay_fun = hosp_to_death_trunc, mu_hdt = mu_hdt, sigma_hdt = sigma_hdt)
      fraction_reported <- c_cfr_baseline / (ccfr$cCFR*100)
      est_ccfr[i] <- dt_ccca$cum_cases[i]*1/fraction_reported
    }
    dt_ccca$est_ccfr <- est_ccfr
    survey_ccaa <- ccaa_pop$ccaa_survey[ccaa_pop$ccaa == x]
    survey_ccaa_pop <- ccaa_pop$population[ccaa_pop$ccaa == x]
    
    if(x %in% (unique(dt_ds$CCAA)[-10])){
      survey_gforms_estimate <- estimate_cases_aggregate_spain_regional(region = survey_ccaa,
                                                                        region_population = survey_ccaa_pop,
                                                                        max_ratio = max_ratio,
                                                                        correction_factor = correction_factor, 
                                                                        method = batching_method,
                                                                        batch = batch_size)$dt_estimates
    }else{
      survey_gforms_estimate <- data.frame(date = dt_ccca$date,
                                           sample_size = NA,
                                           mean_cases = NA,
                                           mean_reach = NA,
                                           dunbar_reach = NA,
                                           cases_p_reach = NA,
                                           cases_p_reach_low = NA,
                                           cases_p_reach_high = NA,
                                           cases_p_reach_error = NA,
                                           cases_p_reach_prop = NA,
                                           cases_p_reach_prop_median = NA,
                                           estimated_cases = NA,
                                           estimate_cases_low = NA,
                                           estimate_cases_high = NA,
                                           estimate_cases_error = NA,
                                           prop_cases = NA,
                                           dunbar_cases = NA,
                                           pop_cases_low = NA,
                                           pop_cases_high = NA,
                                           pop_cases_error = NA,
                                           dunbar_cases_low = NA,
                                           dunbar_cases_high = NA,
                                           dunbar_cases_error = NA,
                                           stringsAsFactors = F)
    }
      
    
      
    
    dt_ccca <- full_join(dt_ccca, survey_gforms_estimate, by = "date")
    return(dt_ccca)
  })
  
  dt_ds_est <- do.call(rbind, dt_ds_est)
  lapply(unique(dt_ds_est$CCAA), function(x){
    dt_ccca <- dt_ds_est[dt_ds_est$CCAA == x, ]
    write.csv(dt_ccca, paste0("../data/PlotData/ES_regional_estimates/", x, "-", "estimates.csv"))
  })
}




estimate_cases_aggregate_spain_regional <- function(region,
                                     region_population,
                                     batch,
                                     method = "antonio",
                                     max_ratio,
                                     correction_factor) {
  #cat("file_path is ", file_path, "\n")
  #cat("country_population is", country_population, "\n")
  
  dt <- read.csv("../data/aggregate/ES-aggregate.csv", as.is = T)
  names(dt) <- c("timestamp","region","reach","cases")
  dt$date <- substr(dt$timestamp, 1, 10)
  n_inital_response <- nrow(dt)
  
  # remove outliers from reach column
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  if(sum(dt$reach > reach_cutoff) > 0 ){
    # write.table(dt[dt$reach >= reach_cutoff, ],
    #             file = paste0("outliers_removed/", file_name, "_", "outliers_reach.txt"),
    #              append = T) # write out outliers from reach column to the ouliers removed folder
    n_reach_outliers <- sum(dt$reach > reach_cutoff) #number of outliers removed based on reach
    dt <- dt[dt$reach <= reach_cutoff, ]
  }else{
    n_reach_outliers <- 0
  }
  
  # remove outliers based on max ratio of   0.3
  dt$ratio <- dt$cases/dt$reach
  dt2 <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
  n_zero_reach_outliers <- sum(!is.finite(dt$ratio)) 
  if(sum(dt2$ratio > max_ratio) > 0 ){
    #write.table(dt[dt$ratio >= max_ratio, ],
    #            file = paste0("outliers_removed/", file_name, "_", "outliers_max_ratio.txt"),
    #            append = T) # write out outliers based on max_Ratio
    n_maxratio_outliers <- sum(dt2$ratio > max_ratio) 
    dt2 <- dt2[dt2$ratio <= max_ratio, ]
  }else{
    n_maxratio_outliers <- 0
  }
  
  dt2_r <- dt2[dt2$region == region, ]
  
  method <- match.arg(method)
  
  if (method == "antonio"){
    dt_batch <- dt2_r %>%
      group_by(date) %>% 
      summarise(sample_size = n())
    # generate grouping variable: if number of responses in a day is sufficient, then agg that day, if not agg multiple days
    group = 1
    group_factor = c()
    container <- c()
    
    for (i in 1:nrow(dt_batch)) {
      container <- c(container, dt_batch$sample_size[i])
      if(sum(container) < batch){
        group_factor <- c(group_factor, group)
      } else{
        group_factor <- c(group_factor, group)
        container <- c()
        group = group + 1
      }
    }
    
    dt_batch$group_factor <- group_factor
    
    dt_batch_s <- dt_batch %>% 
      group_by(group_factor) %>%
      summarise(n = sum(sample_size)) 
    dt_batch_s$include <- dt_batch_s$n >= batch
    
    dt_batch <- full_join(dt_batch, dt_batch_s, by = "group_factor") %>% 
      filter(include == T)
    
    dt2_r <- full_join(dt2_r, dt_batch[,-c(2, 4,5)], by = "date")
    dt_summary <- dt2_r %>%
      filter(!is.na(group_factor)) %>% 
      group_by(group_factor) %>% 
      summarise(date = last(date),
                sample_size = n(), 
                mean_cases = mean(cases),
                mean_reach = mean(reach),
                dunbar_reach = 150 * n(),
                cases_p_reach = sum(cases)/sum(reach), 
                cases_p_reach_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                                 pop_size = sum(reach))$low,
                cases_p_reach_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                  pop_size = sum(reach))$upp,
                cases_p_reach_error = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                   pop_size = sum(reach))$error,
                cases_p_reach_prop = mean(ratio), 
                cases_p_reach_prop_median = median(ratio),
                estimated_cases = region_population * sum(cases)/sum(reach) * correction_factor, 
                estimate_cases_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                                  pop_size = sum(reach))$low *  region_population * correction_factor,
                estimate_cases_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                   pop_size = sum(reach))$upp *  region_population * correction_factor,
                estimate_cases_error = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                    pop_size = sum(reach))$error *  region_population * correction_factor,
                
                prop_cases = region_population * mean(ratio) * correction_factor,
                dunbar_cases = region_population * (sum(cases)/dunbar_reach) * correction_factor, 
                pop_cases_low = calculate_ci(p_est = mean(ratio), level = 0.95,
                                             pop_size = sum(reach))$low,
                pop_cases_high = calculate_ci(p_est = mean(ratio), level = 0.95,
                                              pop_size = sum(reach))$upp,
                pop_cases_error = calculate_ci(p_est = mean(ratio), level = 0.95,
                                               pop_size = sum(reach))$error,
                dunbar_cases_low = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                                                pop_size = dunbar_reach)$low,
                dunbar_cases_high = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                                                 pop_size = dunbar_reach)$upp,
                dunbar_cases_error = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                                                  pop_size = dunbar_reach)$error)
    dt_summary <- dt_summary[, -1] # remove group factor variable
    dt_summary$cases_p_reach_low[dt_summary$cases_p_reach_low < 0]   <- 0.000001
    dt_summary$estimate_cases_low[dt_summary$estimate_cases_low < 0] <- 0.000001
    dt_summary$pop_cases_low[dt_summary$pop_cases_low < 0] <- 0.000001
    dt_summary$dunbar_cases_low[dt_summary$dunbar_cases_low < 0] <- 0.000001
    
  }  else{
    stop("method can only be antonio")
  }
  
  return(list(dt_estimates = dt_summary,
              n_inital_response = n_inital_response,
              n_reach_outliers = n_reach_outliers,
              n_maxratio_outliers = n_maxratio_outliers, 
              n_zero_reach_outliers = n_zero_reach_outliers,
              n_final_response = sum(dt_summary$sample_size)))
  
}

get_spain_regional_estimates()
