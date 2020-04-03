## script needs file for country and country population.
library(tidyverse)
library(readxl)
library(httr)
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
estimate_cases_aggregate <- function(file_path = "../data/aggregate/ES-aggregate.csv",
                                     country_population = 46754778, max_ratio = .3,
                                     correction_factor = 1) {
  cat("file_path is ", file_path, "\n")
  cat("country_population is", country_population, "\n")
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
  
  
 dt_summary <- dt2 %>%
   group_by(date) %>% 
   summarise(mean_cases = mean(cases),
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
             estimated_cases = country_population * sum(cases)/sum(reach) * correction_factor, 
             estimate_cases_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                               pop_size = sum(reach))$low *  country_population * correction_factor,
             estimate_cases_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                pop_size = sum(reach))$upp *  country_population * correction_factor,
             estimate_cases_error = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                 pop_size = sum(reach))$error *  country_population * correction_factor,
             prop_cases = country_population * mean(ratio) * correction_factor,
             dunbar_cases = country_population * (sum(cases)/dunbar_reach) * correction_factor, 
            # dunbar_prop_cases = country_population * mean(cases/150) * correction_factor,
             sample_size = n())
 
 return(list(dt_estimates = dt_summary,
             n_inital_response = n_inital_response,
             n_reach_outliers = n_reach_outliers,
             n_maxratio_outliers = n_maxratio_outliers, 
             n_zero_reach_outliers = n_zero_reach_outliers,
             n_final_response = sum(dt_summary$sample_size)))
 
}

plot_estimates <- function(country_geoid = "ES",
                           country_population = 46754778, max_ratio = .3, correction_factor = 1, 
                           z_mean_hdt = 13, z_sd_hdt = 12.7, z_median_hdt = 9.1,
                           mu_hdt = log(z_median_hdt), sigma_hdt = sqrt(2*(log(z_mean_hdt) - mu_hdt)),
                           c_cfr_baseline = 1.38, c_cfr_estimate_range = c(1.23, 1.53), 
                           est_date = format(Sys.time(), "%Y-%m-%d")){
  file_path = paste0("../data/aggregate/", country_geoid, "-aggregate.csv")
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
               est_date, ".xlsx", sep = "")
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
  data <- read_excel(tf)
  data <- data[data$geoId == country_geoid,]
  dt <- data[rev(1:nrow(data)),]
  dt$cum_cases <- cumsum(dt$cases)
  dt$cum_deaths <- cumsum(dt$deaths)
  dt$cum_deaths_400 <- dt$cum_deaths * 400
  dt$date <- gsub("-", "/", dt$dateRep)
  ndt <- nrow(dt)
  est_ccfr <- rep(NA, ndt)
  
  for (i in ndt : 1) {
    data2t <- dt[1:i, c("cases", "deaths")]
    ccfr <- scale_cfr(data2t, delay_fun = hosp_to_death_trunc, mu_hdt = mu_hdt, sigma_hdt = sigma_hdt)
    fraction_reported <- c_cfr_baseline / (ccfr$cCFR*100)
    est_ccfr[i] <- dt$cum_cases[i]*1/fraction_reported
  }
  
  survey_gforms_estimate <- estimate_cases_aggregate(file_path = file_path,
                                                     country_population = country_population,
                                                     max_ratio = max_ratio,
                                                     correction_factor = correction_factor)$dt_estimates
  
  dt$est_ccfr <- est_ccfr
  # combine dt and survey forms estimates
  dt_res <- full_join(dt, survey_gforms_estimate, by = "date")
  # combine with survey twitter
  if (country_geoid == "ES"){
    dt_res <- full_join(dt_res, survey_twitter_esp, by = "date") %>% 
      select(countriesAndTerritories, geoId, date, cases, deaths, cum_cases, cum_deaths,est_ccfr, mean_cases:survey_twitter)
    
  } else if(country_geoid == "PT"){
    dt_res <- full_join(dt_res, survey_twitter_pt, by = "date") %>% 
      select(countriesAndTerritories, geoId, date, cases, deaths, cum_cases, cum_deaths, est_ccfr, mean_cases:survey_twitter)
  } else{
    dt_res <- dt_res %>% 
      select(countriesAndTerritories, geoId, date, cases, deaths, cum_cases, cum_deaths, est_ccfr, mean_cases:sample_size, est_ccfr)
  }
  
  write.csv(dt_res, paste0("../data/PlotData/", country_geoid, "-", "estimates.csv"))
}

# usage...generate and write data to plotdata folder
# Spain
plot_estimates(est_date = "2020-04-03")

# Portugal
plot_estimates(est_date = "2020-04-03", country_geoid = "PT",
                     country_population = 10261075)

# Cyprus
plot_estimates(est_date = "2020-04-03", country_geoid = "CY",
                     country_population = 890900)

# France
plot_estimates(est_date = "2020-04-03", country_geoid = "FR",
                     country_population = 66987244)


