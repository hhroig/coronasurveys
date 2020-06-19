library(dplyr)
library(tidyr)

get_countries_with_survey <- function(path = "../../data/aggregate/"){
  #get list of countries with surveys
  plotdata_files <- list.files(path)
  plotdata_files <- plotdata_files[plotdata_files != "Twitter-surveys.csv"]
  substr(plotdata_files,start = 1, stop = 2)
}
calculate_ci <- function(p_est, level, pop_size) {
  z <- qnorm(level+(1-level)/2)
  se <- sqrt(p_est*(1-p_est))/sqrt(pop_size)
  return(list(low=p_est-z*se, upp=p_est+z*se, error=z*se))
}

estimate_cases_aggregate <- function(country_geoid = "ES",
                                     dts,
                                     batch = 30,
                                     max_ratio = .3,
                                     survey_countries = get_countries_with_survey()) {
  cat("::- script-30responses: Working on ", country_geoid, "::\n")
  country_population <- dts$population[dts$geo_id == country_geoid]
  if(country_geoid %in% survey_countries){
    ############# read data 
    file_path <- paste0("../../data/aggregate/", country_geoid, "-aggregate.csv")
    dt <- read.csv(file_path, as.is = T)
    
    if(nrow(dt)<30){
      cat("::- script-30responses: The survey data for", country_geoid, "is less than 30 responses ::\n")
      return(NULL)
    
      }
    
    names(dt) <- tolower(names(dt))
    dt <- dt[, c("timestamp","region","reach","cases")]
    dt$date <- substr(dt$timestamp, 1, 10)
    if(file_path == "../../data/aggregate/ES-aggregate.csv"){
      dt$reach[1:102] <- 150 # impute Dunbar number
    }
    dt <- dt[!is.na(dt$reach),] 
    
    ########## remove outlier
    # remove outliers from reach column
    reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
    dt <- dt[dt$reach <= reach_cutoff, ]
    
    # remove outliers based on max ratio of   0.3
    dt$ratio <- dt$cases/dt$reach
    dt <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
    dt <- dt[dt$ratio <= max_ratio, ]
    
    ########## carry out batching
    dt_batch <- dt %>%
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
    
    dt <- full_join(dt, dt_batch[,-c(2, 4,5)], by = "date")
    ####################### compute estimates
    dt_summary <- dt %>%
      filter(!is.na(group_factor)) %>% 
      group_by(group_factor) %>% 
      summarise(date = last(date),
                sample_size = n(), 
                mean_cases = mean(cases), #
                mean_reach = mean(reach), #
                p_w = sum(cases)/sum(reach), 
                p_w_low = calculate_ci(p_est = sum(cases)/sum(reach), level = 0.95,
                                                 pop_size = sum(reach))$low,
                p_w_high = calculate_ci(p_est = sum(cases)/sum(reach), level=0.95,
                                                  pop_size = sum(reach))$upp,
                p_m = mean(ratio),
                p_m_low = calculate_ci(p_est = mean(ratio), level = 0.95,
                                       pop_size = sum(reach))$low, 
                p_m_high = calculate_ci(p_est = mean(ratio), level = 0.95,
                             pop_size = sum(reach))$upp,
                population = country_population
                # p_d = sum(cases)/dunbar_reach,
                # p_d_high = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                #                         pop_size = dunbar_reach)$upp,
                # p_d_low = calculate_ci(p_est = (sum(cases)/dunbar_reach), level = 0.95,
                #                        pop_size = dunbar_reach)$low
                )
    
    dt_summary <- dt_summary[, -1] # remove group factor variable
    dt_summary$p_w_low[dt_summary$p_w_low < 0]   <- 0.000001
    dt_summary$p_m_low[dt_summary$p_m_low < 0]   <- 0.000001
    #dt_summary$p_d_low[dt_summary$p_d_low < 0]   <- 0.000001
    
    
    dir.create("../../data/estimates-30responses/PlotData/", showWarnings = F)
    cat("::- script-30responses: Writing data for ", country_geoid, " ::\n")
    write.csv(dt_summary,
              paste0("../../data/estimates-30responses/PlotData/", country_geoid, "-estimate.csv"))
    
  } else{
    cat("::- script-30responses:", country_geoid, "does not have a survey data yet ::\n")
    return(NULL)
  }
}



dtpop_ecdc <- read.csv(file = "country_populatiopn_ecdc.csv", as.is = T,
                       na.strings = "")[,-1]
all_geo_ids <- dtpop_ecdc$geo_id
go <- sapply(all_geo_ids, estimate_cases_aggregate, dts = dtpop_ecdc)


  
  
  
