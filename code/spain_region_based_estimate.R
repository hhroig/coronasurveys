#cat("file_path is ", file_path, "\n")
#cat("country_population is", country_population, "\n")
get_spain_region_based_estimate <- function(max_ratio = .3){
  cat("generating region based estimate for spain \n")
  dt <- read.csv("../data/aggregate/ES-aggregate.csv", as.is = T)
  ccaa_pop <- read.csv("ccaa_population.csv", as.is = T)
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
  
  # set "" to todo el pais
  dt2$region[dt2$region == ""] <- "Todo el país"
  
  # get all the dates
  dates <- unique(dt2$date)
  # create vector of results
  estimated_cases <- c()
  prop_cases <- c()
  
  estimate_cases_world <- c()
  estimate_cases_reg <- c()
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    dt_date <- dt2[as.Date(dt2$date) <= as.Date(j), ]
    
    # check for the date which correspond to the last 30 whole country entry
    date_w <- tail(dt_date$date[dt_date$region == "Todo el país"], 50 )[1]
    #check for the date which correspond to the last 100 entry
    date_t <- tail(dt_date$date, 300)[1]
    #dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
    if (as.Date(date_w) < as.Date(date_t)){
      dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
    } else{
      dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_w), ]
    }
    m <- sum(dt2_r$region == "Todo el país")
    n <- nrow(dt2_r) - m
    
    # compute the ratios for todo el pais
    dt2_rt <- dt2_r[dt2_r$region == "Todo el país", ]
    cases_p_reach_w <- sum(dt2_rt$cases) / sum(dt2_rt$reach)
    cases_p_reach_prop_w <- mean(dt2_rt$cases/dt2_rt$reach)
    
    estimated_cases_w <- ccaa_pop$population[ccaa_pop$ccaa_survey == ""] * cases_p_reach_w * (m/(m+n))
    prop_cases_w <- ccaa_pop$population[ccaa_pop$ccaa_survey == ""] * cases_p_reach_prop_w * (m/(m+n))
    
    # copute the ratio for the regions
    cases_p_reach_t <- c()
    cases_p_reach_prop_t <- c()
    
    estimated_cases_t <- c()
    prop_cases_t <- c()
    
    for (i in unique(dt2$region)[unique(dt2$region) != "Todo el país"]){
      # get data for all country and the current region
      dt_current <- dt2_r[dt2_r$region == i, ]
      
      cases_p_reach_current <- sum(dt_current$cases)/sum(dt_current$reach)
      cases_p_reach_prop_current <- mean(dt_current$cases/dt_current$reach)
      
      estimated_cases_current <- ccaa_pop$population[ccaa_pop$ccaa_survey == i] * cases_p_reach_current
      prop_cases_current <- ccaa_pop$population[ccaa_pop$ccaa_survey == i] * cases_p_reach_prop_current
      
      cases_p_reach_t <- c(cases_p_reach_t, cases_p_reach_current)
      cases_p_reach_prop_t <- c(cases_p_reach_prop_t, cases_p_reach_prop_current)
      estimated_cases_t <- c(estimated_cases_t, estimated_cases_current)
      prop_cases_t <- c(prop_cases_t, prop_cases_current)
    }
    
    estimated_cases_t <- sum(estimated_cases_t, na.rm = T) * (n/(m+n))
    prop_cases_t <- sum(prop_cases_t, na.rm = T) * (n/(m+n))
    
    estimated_cases <-c(estimated_cases, estimated_cases_w + estimated_cases_t)
    estimate_cases_world <- c(estimate_cases_world, estimated_cases_w)
    estimate_cases_reg <- c(estimate_cases_reg, estimated_cases_t)
    
    prop_cases <- c(prop_cases, prop_cases_w + prop_cases_t)
  }
  
  region_based_estimate <- data.frame(date = dates,
                                      estimated_cases_region_based = estimated_cases,
                                      prop_cases_region_based = prop_cases, 
                                      estimate_cases_world = estimate_cases_world,
                                      estimate_cases_reg = estimate_cases_reg)
  cat("writing the region based estimate for Spain..\n")
  write.csv(region_based_estimate, paste0("../data/PlotData/ES_regional_estimates/", "ES-region-based-estimate.csv"))
}

get_spain_region_based_estimate()


get_spain_region_based_estimate2 <- function(max_ratio = .3){
  cat("generating region based estimate for spain \n")
  dt <- read.csv("../data/aggregate/ES-aggregate.csv", as.is = T)
  ccaa_pop <- read.csv("ccaa_population.csv", as.is = T)
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
  
  # set "" to todo el pais
  dt2$region[dt2$region == ""] <- "Todo el país"
  
  # get all the dates
  dates <- unique(dt2$date)
  # create vector of results
  cases_p_reach <- c()
  cases_p_reach_prop <- c()
  # total population
  total_pop <- ccaa_pop$population[1]
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    dt_date <- dt2[as.Date(dt2$date) <= as.Date(j), ]
    
    date_t <- tail(dt_date$date, 300)[1]
    dt2_r <- dt_date[as.Date(dt_date$date) >=  as.Date(date_t), ]
    
    # copute the ratio for the regions
    cases_p_reach_t <- c()
    cases_p_reach_prop_t <- c()
    
    # estimated_cases_t <- c()
    # prop_cases_t <- c()
    
    
    
    for (i in unique(dt2$region)){
      # get data for all country and the current region
      dt_current <- dt2_r[dt2_r$region == i, ]
      
      # get current population
      pop_current <- ccaa_pop$population[ccaa_pop$ccaa_survey == i]
      weight_current <- pop_current/total_pop
      
      cases_p_reach_current <- weight_current*sum(dt_current$cases, na.rm = T)/sum(dt_current$reach, na.rm = T)
      cases_p_reach_prop_current <- weight_current*mean(dt_current$cases/dt_current$reach, na.rm = T)
      
      # estimated_cases_current <- ccaa_pop$population[ccaa_pop$ccaa_survey == i] * cases_p_reach_current
      # prop_cases_current <- ccaa_pop$population[ccaa_pop$ccaa_survey == i] * cases_p_reach_prop_current
      
      cases_p_reach_t <- c(cases_p_reach_t, cases_p_reach_current)
      cases_p_reach_prop_t <- c(cases_p_reach_prop_t, cases_p_reach_prop_current)
      # estimated_cases_t <- c(estimated_cases_t, estimated_cases_current)
      # prop_cases_t <- c(prop_cases_t, prop_cases_current)
    }
    
    cases_p_reach <- c(cases_p_reach, sum(cases_p_reach_t, na.rm = T))
    cases_p_reach_prop <- c(cases_p_reach_prop, sum(cases_p_reach_prop_t, na.rm = T))
   
  }
  
  region_based_estimate2 <- data.frame(date = dates,
                                      cases_p_reach = cases_p_reach,
                                      cases_p_reach_prop = cases_p_reach_prop, 
                                      estimate_cases_reach = cases_p_reach * total_pop,
                                      estimate_cases_reach_prop = cases_p_reach_prop * total_pop)
  cat("writing the region based estimate for Spain..\n")
  write.csv(region_based_estimate2, paste0("../data/PlotData/ES_regional_estimates/",
                                          "ES-region-based-estimate2.csv"))
}

get_spain_region_based_estimate2()