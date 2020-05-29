provincial_regional_estimate <- function(countrycode = "ES",
                                               province = T,
                                               district = F,
                                               W = 90,
                                               alpha = 0.0001,
                                               max_ratio = .3,
                                               provinces_and_codes = readxl::read_excel("regions-tree-population.xlsx"),
                                               write_summary_file = T,
                                               write_daily_file = T){
  cat(paste0("Generating region based estimates for ", countrycode, "\n"))
  dt <- read.csv(paste0("../data/aggregate/", countrycode, "-aggregate.csv"), as.is = T)
  dt_region <- provinces_and_codes[provinces_and_codes$countrycode == countrycode, ]
  names(dt) <- tolower(names(dt))
  dt <- dt[, c("timestamp","region","reach","cases", "iso.3166.1.a2", "iso.3166.2")]
  dt$date <- substr(dt$timestamp, 1, 10)
  
  # outlier detection
  dt <- dt[!is.na(dt$reach),]
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  dt <- dt[dt$reach <= reach_cutoff, ]
  
  dt$ratio <- dt$cases/dt$reach    # remove outlier based on 0.3
  dt <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
  dt <- dt[dt$ratio <= max_ratio, ]
  dt$iso.3166.2[dt$iso.3166.2 == ""] <- countrycode
  # removal of multiple responses using cookies
  
  
  # compute provincial estimates
  dates <- unique(dt$date)
  

  dt_region2 <- dt_region[, c("countrycode",  "regioncode",   "provincecode", "population")] ## bring autonomous cities code to lowest level
  r_c <-  c()
  r_r <-  c()
  I_c_estprop <- c()
  I_c_meanprop <- c()
  I_r_estprop <- c()
  I_r_meanprop <- c()
  estprop_country_agg <- c()
  meanprop_country_agg <- c()
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    # get data from the past up to W days earlier
    subcondition <- (as.Date(dt$date) >= (as.Date(j)-W)  & as.Date(dt$date) <= as.Date(j) )
    dt_date <- dt[subcondition, ]
    
    
    # compute provincia provinces
    if (province == T){
      dtprovs <- na.omit(dt_region2)
      provs <- unique(dtprovs$provincecode)
      estprop_provs <- c()
      meanprop_provs <- c()
      sumreach_provs <- c()
      for (i in provs) {
        provpop <- dtprovs$population[dtprovs$provincecode == i]
        dt_prov <- dt_date[dt_date$iso.3166.2 == i, ]
        sumreach <- sum(dt_prov$reach)
        cprov <- sumreach/provpop
        if(cprov >= alpha){
          est <- sum(dt_prov$cases)/sumreach
          meanpest <- mean(dt_prov$cases/dt_prov$reach)
          estprop_provs <- c(estprop_provs, est)
          meanprop_provs  <- c(meanprop_provs, meanpest)
          sumreach_provs <- c(sumreach_provs, sumreach)
        }else{
          estprop_provs <- c(estprop_provs, NA)
          meanprop_provs <- c(meanprop_provs, NA)
          sumreach_provs <- c(sumreach_provs, sumreach)
        }
        
      }
      dtestpropprovs <- data.frame(provincecode = provs,
                              estprop_provs = estprop_provs,
                              meanprop_provs = meanprop_provs,
                              sumreach_provs  = sumreach_provs)
      dtprovs <- merge(dtprovs, dtestpropprovs, all = T, by = "provincecode")
    }
    
    # compute regional estimates
    dtregs <- by(dt_region2,
                  list(dt_region2$countrycode, dt_region2$regioncode), # groupby country and region and compute population
                  function(x){
                    data.frame(countrycode = unique(x$countrycode),
                               regioncode = unique(x$regioncode), 
                               population_region = sum(x$population),
                               stringsAsFactors = F)
                  })
    dtregs <- do.call(rbind, dtregs)
    regions <- unique(dtregs$regioncode)
    estprop_regs <- c()
    meanprop_regs <- c()
    sumreach_regs <- c()
    for (i in regions) {
      regpop <- dtregs$population_region[dtregs$regioncode == i]
      dt_reg <- dt_date[dt_date$iso.3166.2 == i, ]
      sumreach <- sum(dt_reg$reach)
      creg <- sumreach/regpop
      if(creg >= alpha){
        est <- sum(dt_reg$cases)/sumreach
        meanpest <- mean(dt_reg$cases/dt_reg$reach)
        estprop_regs <- c(estprop_regs, est)
        meanprop_regs <- c(meanprop_regs, meanpest)
        sumreach_regs <- c(sumreach_regs, sumreach)
      }else{
        estprop_regs <- c(estprop_regs, NA)
        meanprop_regs <- c(meanprop_regs, NA)
        sumreach_regs <- c(sumreach_regs, sumreach)
      }
      
    }
    
    dtestpropregs <- data.frame(regioncode = regions,
                                estprop_regs = estprop_regs,
                                meanprop_regs = meanprop_regs,
                                sumreach_regs = sumreach_regs)
    
    dtregs <- merge(dtregs, dtestpropregs, all = T, by = "regioncode")
    
    # aggregate provincia data into regional data
    if(province == T){
      # population variable refer to population at lowest level
      dt_est_prov_reg <- merge(dtprovs, dtregs, all = T, by = c("countrycode", "regioncode")) 
      # go over regions and computed aggregated means
      estprop_regs_rhs <- c()
      meanprop_regs_rhs <- c()
      sumreach_regs_rhs <- c()
      for (i in unique(dt_est_prov_reg$regioncode)) {
        dt_est_reg <- na.omit(dt_est_prov_reg[dt_est_prov_reg$regioncode == i, ])
        weight <- dt_est_reg$population/dt_est_reg$population_region
        estprop_regs_rh <- sum(weight * dt_est_reg$estprop_provs)
        meanprop_regs_rh <- sum(weight * dt_est_reg$meanprop_provs)
        
        sumreach_regs_rhs <- c(sumreach_regs_rhs, sum(dt_est_reg$sumreach_provs))
        estprop_regs_rhs <- c(estprop_regs_rhs, estprop_regs_rh)
        meanprop_regs_rhs <- c(meanprop_regs_rhs, meanprop_regs_rh)
      }
      dt_regs_rhs <- data.frame(regioncode = unique(dt_est_prov_reg$regioncode),
                               estprop_regs_rhs = estprop_regs_rhs,
                               meanprop_regs_rhs = meanprop_regs_rhs,
                               sumreach_regs_rhs = sumreach_regs_rhs)
      
      dt_est_prov_reg <- merge(dt_est_prov_reg, dt_regs_rhs, by = c("regioncode"))
      dt_est_prov_reg$meanprop_regs_agg <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs + dt_est_prov_reg$sumreach_regs_rhs)) *
        dt_est_prov_reg$meanprop_regs) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                        dt_est_prov_reg$meanprop_regs_rhs)
      
      dt_est_prov_reg$estprop_regs_agg <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                     dt_est_prov_reg$estprop_regs) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                     dt_est_prov_reg$estprop_regs_rhs)
    }else{
      dt_est_prov_reg <- dtregs
      dt_est_prov_reg$meanprop_regs_agg <- dt_est_prov_reg$meanprop_regs
      dt_est_prov_reg$estprop_regs_agg <- dt_est_prov_reg$estprop_regs
    }
    
    
    ## aggregate regional estimates into provinvial estimates
    dt_country <- dt_date[dt_date$iso.3166.2 == countrycode, ]
    sumreach_country <- sum(dt_country$reach)
    estprop_country <- sum(dt_country$cases)/sumreach_country
    meanprop_country <- mean(dt_country$cases/dt_country$reach)

    
    dt_est_reg_count <- by(dt_est_prov_reg,
                 list(dt_est_prov_reg$countrycode, dt_est_prov_reg$regioncode), # groupby country and region and compute population
                 function(x){
                   data.frame(countrycode = unique(x$countrycode),
                              regioncode = unique(x$regioncode), 
                              population_region = unique(x$population_region),
                              estprop_regs_agg = unique(x$estprop_regs_agg),
                              meanprop_regs_agg = unique(x$meanprop_regs_agg),
                              sumreach_regs = unique(x$sumreach_regs),
                              stringsAsFactors = F)
                 })
    dt_est_reg_count <- do.call(rbind, dt_est_reg_count)
    weight <- dt_est_reg_count$population_region/sum(dt_est_reg_count$population_region)
    estprop_country_rh <- sum(weight * dt_est_reg_count$estprop_regs_agg, na.rm = T)
    meanprop_country_rh <- sum(weight * dt_est_reg_count$meanprop_regs_agg, na.rm = T)
    sumreach_country_rh <-  sum(dt_est_reg_count$sumreach_regs)
    
    
    
    meanprop_counts_agg <- ((sumreach_country/(sumreach_country + sumreach_country_rh)) * meanprop_country) + 
      ((sumreach_country_rh/(sumreach_country + sumreach_country_rh)) * meanprop_country_rh)
    
    estprop_counts_agg <- ((sumreach_country/(sumreach_country + sumreach_country_rh)) * estprop_country) + 
      ((sumreach_country_rh/(sumreach_country + sumreach_country_rh)) * estprop_country_rh)
    
    
    if (write_daily_file == T){
      dt_est_count <- data.frame(countrycode = countrycode,
                                 population_country = sum(dtregs$population_region),
                                 estprop_country = estprop_country,
                                 meanprop_country = meanprop_country, 
                                 sumreach_country = sumreach_country,
                                 estprop_country_rhs = estprop_country_rh,
                                 meanprop_country_rhs = meanprop_country_rh, 
                                 sumreach_country_rhs = sumreach_country_rh, 
                                 meanprop_country_agg = meanprop_counts_agg,
                                 estprop_country_agg = estprop_counts_agg)
     
     cat(paste0("Writing the region based daily estimate for ", countrycode, "..\n")) 
     dt_est_prov_reg_country <- merge(dt_est_prov_reg, dt_est_count, all = T, by = "countrycode")
     write.csv(x = dt_est_prov_reg_country, file = paste0("../data/PlotData/", countrycode, "_regional_estimates/daily_province_region_country_based_estimates/",
                       countrycode, "-province-region-country-based-estimate-", gsub("/", "_", j), ".csv"))
    }

    
    
    
    r_c <-  c(r_c, sumreach_country)
    r_r <-  c(r_r, sumreach_country_rh)
    I_c_estprop <- c(I_c_estprop, estprop_country)
    I_c_meanprop <- c(I_c_meanprop, meanprop_country)
    I_r_estprop <- c(I_r_estprop, estprop_country_rh)
    I_r_meanprop <- c(I_r_meanprop , meanprop_country_rh)
    estprop_country_agg <- c(estprop_country_agg, estprop_counts_agg)
    meanprop_country_agg <- c(meanprop_country_agg, meanprop_counts_agg)
    }
  
  region_based_estimate <- data.frame(date = dates,
                                      r_c = r_c,
                                      r_r = r_r,
                                      I_c_estprop = I_c_estprop,
                                      I_c_meanprop = I_c_meanprop,
                                      I_r_estprop = I_r_estprop, 
                                      I_r_meanprop = I_r_meanprop,
                                      estprop_country_agg = estprop_country_agg,
                                      meanprop_country_agg = meanprop_country_agg)
  if(write_summary_file == T){
    cat(paste0("Writing the region based estimate summary for ", countrycode, "..\n"))
    write.csv(region_based_estimate, paste0("../data/PlotData/", countrycode, "_regional_estimates/region_based_estimates/",
                                            countrycode ,"-province-region-country-based-estimate.csv"))
  }
  else{
    return(region_based_estimate)
  }

}



provincial_regional_estimate(countrycode = "EC",
                             province = F,
                             alpha = 0.00001,
                             write_summary_file = T,
                             write_daily_file = T)

provincial_regional_estimate(countrycode = "US",
                             province = F,
                             alpha = 0.00001,
                             write_summary_file = T,
                             write_daily_file = T)

provincial_regional_estimate(countrycode = "PT",
                             province = F,
                             write_summary_file = T,
                             write_daily_file = T)

provincial_regional_estimate2 <- function(countrycode = "ES",
                                         province = T,
                                         district = F,
                                         W = 90,
                                         alpha = 0.0001,
                                         max_ratio = .3,
                                         provinces_and_codes = readxl::read_excel("regions-tree-population.xlsx"),
                                         write_summary_file = T,
                                         write_daily_file = T){
  cat(paste0("Generating region based estimates for ", countrycode, "\n"))
  dt <- read.csv(paste0("../data/aggregate/", countrycode, "-aggregate.csv"), as.is = T)
  dt_region <- provinces_and_codes[provinces_and_codes$countrycode == countrycode, ]
  names(dt) <- tolower(names(dt))
  dt <- dt[, c("timestamp","region","reach","cases", "recentcases", "iso.3166.1.a2", "iso.3166.2")]
  dt$date <- substr(dt$timestamp, 1, 10)
  
  # outlier detection
  dt <- dt[!is.na(dt$reach),]
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  dt <- dt[dt$reach <= reach_cutoff, ]
  
  dt$ratio <- dt$cases/dt$reach    # remove outlier based on 0.3
  dt <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
  dt <- dt[dt$ratio <= max_ratio, ]
  dt$iso.3166.2[dt$iso.3166.2 == ""] <- countrycode
  # removal of multiple responses using cookies
  
  
  # compute provincial estimates
  dates <- unique(dt$date)
  
  
  dt_region2 <- dt_region[, c("countrycode",  "regioncode",   "provincecode", "population")] ## bring autonomous cities code to lowest level
  r_c <-  c()
  r_r <-  c()
  I_c_estprop <- c()
  I_c_meanprop <- c()
  I_c_estprop2 <- c()
  I_c_meanprop2 <- c()
  I_r_estprop <- c()
  I_r_meanprop <- c()
  I_r_estprop2 <- c()
  I_r_meanprop2 <- c()
  estprop_country_agg <- c()
  meanprop_country_agg <- c()
  estprop_country_agg2 <- c()
  meanprop_country_agg2 <- c()
  
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    # get data from the past up to W days earlier
    subcondition <- (as.Date(dt$date) >= (as.Date(j)-W)  & as.Date(dt$date) <= as.Date(j) )
    dt_date <- dt[subcondition, ]
    
    
    # compute provincia provinces
    if (province == T){
      dtprovs <- na.omit(dt_region2)
      provs <- unique(dtprovs$provincecode)
      estprop_provs <- c()
      meanprop_provs <- c()
      estprop_provs2 <- c()
      meanprop_provs2 <- c()
      sumreach_provs <- c()
      for (i in provs) {
        provpop <- dtprovs$population[dtprovs$provincecode == i]
        dt_prov <- dt_date[dt_date$iso.3166.2 == i, ]
        sumreach <- sum(dt_prov$reach)
        cprov <- sumreach/provpop
        if(cprov >= alpha){
          est <- sum(dt_prov$cases)/sumreach
          meanpest <- mean(dt_prov$cases/dt_prov$reach)
          est2 <- sum(dt_prov$recentcases, na.rm = T)/sumreach
          meanpest2 <- mean(dt_prov$recentcases/dt_prov$reach, na.rm = T)
          estprop_provs <- c(estprop_provs, est)
          meanprop_provs  <- c(meanprop_provs, meanpest)
          estprop_provs2 <- c(estprop_provs2, est2)
          meanprop_provs2  <- c(meanprop_provs2, meanpest2)
          sumreach_provs <- c(sumreach_provs, sumreach)
        }else{
          estprop_provs <- c(estprop_provs, NA)
          meanprop_provs <- c(meanprop_provs, NA)
          estprop_provs2 <- c(estprop_provs2, NA)
          meanprop_provs2 <- c(meanprop_provs2, NA)
          sumreach_provs <- c(sumreach_provs, sumreach)
        }
        
      }
      dtestpropprovs <- data.frame(provincecode = provs,
                                   estprop_provs = estprop_provs,
                                   meanprop_provs = meanprop_provs,
                                   estprop_provs2 = estprop_provs2,
                                   meanprop_provs2 = meanprop_provs2,
                                   sumreach_provs  = sumreach_provs)
      dtprovs <- merge(dtprovs, dtestpropprovs, all = T, by = "provincecode")
    }
    
    # compute regional estimates
    dtregs <- by(dt_region2,
                 list(dt_region2$countrycode, dt_region2$regioncode), # groupby country and region and compute population
                 function(x){
                   data.frame(countrycode = unique(x$countrycode),
                              regioncode = unique(x$regioncode), 
                              population_region = sum(x$population),
                              stringsAsFactors = F)
                 })
    dtregs <- do.call(rbind, dtregs)
    regions <- unique(dtregs$regioncode)
    estprop_regs <- c()
    meanprop_regs <- c()
    estprop_regs2 <- c()
    meanprop_regs2 <- c()
    sumreach_regs <- c()
    for (i in regions) {
      regpop <- dtregs$population_region[dtregs$regioncode == i]
      dt_reg <- dt_date[dt_date$iso.3166.2 == i, ]
      sumreach <- sum(dt_reg$reach)
      creg <- sumreach/regpop
      if(creg >= alpha){
        est <- sum(dt_reg$cases)/sumreach
        meanpest <- mean(dt_reg$cases/dt_reg$reach)
        est2 <- sum(dt_reg$recentcases, na.rm = T)/sumreach
        meanpest2 <- mean(dt_reg$recentcases/dt_reg$reach, na.rm = T)
        estprop_regs <- c(estprop_regs, est)
        meanprop_regs <- c(meanprop_regs, meanpest)
        estprop_regs2 <- c(estprop_regs2, est2)
        meanprop_regs2 <- c(meanprop_regs2, meanpest2)
        sumreach_regs <- c(sumreach_regs, sumreach)
      }else{
        estprop_regs <- c(estprop_regs, NA)
        meanprop_regs <- c(meanprop_regs, NA)
        estprop_regs2 <- c(estprop_regs2, NA)
        meanprop_regs2 <- c(meanprop_regs2, NA)
        sumreach_regs <- c(sumreach_regs, sumreach)
      }
      
    }
    
    dtestpropregs <- data.frame(regioncode = regions,
                                estprop_regs = estprop_regs,
                                meanprop_regs = meanprop_regs,
                                estprop_regs2 = estprop_regs2,
                                meanprop_regs2 = meanprop_regs2,
                                sumreach_regs = sumreach_regs)
    
    dtregs <- merge(dtregs, dtestpropregs, all = T, by = "regioncode")
    
    # aggregate provincia data into regional data
    if(province == T){
      # population variable refer to population at lowest level
      dt_est_prov_reg <- merge(dtprovs, dtregs, all = T, by = c("countrycode", "regioncode")) 
      # go over regions and computed aggregated means
      estprop_regs_rhs <- c()
      meanprop_regs_rhs <- c()
      estprop_regs_rhs2 <- c()
      meanprop_regs_rhs2 <- c()
      sumreach_regs_rhs <- c()
      for (i in unique(dt_est_prov_reg$regioncode)) {
        dt_est_reg <- na.omit(dt_est_prov_reg[dt_est_prov_reg$regioncode == i, ])
        weight <- dt_est_reg$population/dt_est_reg$population_region
        estprop_regs_rh <- sum(weight * dt_est_reg$estprop_provs)
        meanprop_regs_rh <- sum(weight * dt_est_reg$meanprop_provs)
        estprop_regs_rh2 <- sum(weight * dt_est_reg$estprop_provs2)
        meanprop_regs_rh2 <- sum(weight * dt_est_reg$meanprop_provs2)
        
        sumreach_regs_rhs <- c(sumreach_regs_rhs, sum(dt_est_reg$sumreach_provs))
        estprop_regs_rhs <- c(estprop_regs_rhs, estprop_regs_rh)
        meanprop_regs_rhs <- c(meanprop_regs_rhs, meanprop_regs_rh)
        estprop_regs_rhs2 <- c(estprop_regs_rhs2, estprop_regs_rh2)
        meanprop_regs_rhs2 <- c(meanprop_regs_rhs2, meanprop_regs_rh2)
      }
      dt_regs_rhs <- data.frame(regioncode = unique(dt_est_prov_reg$regioncode),
                                estprop_regs_rhs = estprop_regs_rhs,
                                meanprop_regs_rhs = meanprop_regs_rhs,
                                estprop_regs_rhs2 = estprop_regs_rhs2,
                                meanprop_regs_rhs2 = meanprop_regs_rhs2,
                                sumreach_regs_rhs = sumreach_regs_rhs)
      
      dt_est_prov_reg <- merge(dt_est_prov_reg, dt_regs_rhs, by = c("regioncode"))
      dt_est_prov_reg$meanprop_regs_agg <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs + dt_est_prov_reg$sumreach_regs_rhs)) *
                                              dt_est_prov_reg$meanprop_regs) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                                  dt_est_prov_reg$meanprop_regs_rhs)
      
      dt_est_prov_reg$estprop_regs_agg <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                             dt_est_prov_reg$estprop_regs) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                                dt_est_prov_reg$estprop_regs_rhs)
      
      dt_est_prov_reg$meanprop_regs_agg2 <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs + dt_est_prov_reg$sumreach_regs_rhs)) *
                                              dt_est_prov_reg$meanprop_regs2) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                                  dt_est_prov_reg$meanprop_regs_rhs2)
      
      dt_est_prov_reg$estprop_regs_agg2 <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                             dt_est_prov_reg$estprop_regs2) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                                dt_est_prov_reg$estprop_regs_rhs2)
    }else{
      dt_est_prov_reg <- dtregs
      dt_est_prov_reg$meanprop_regs_agg <- dt_est_prov_reg$meanprop_regs
      dt_est_prov_reg$estprop_regs_agg <- dt_est_prov_reg$estprop_regs
      dt_est_prov_reg$meanprop_regs_agg2 <- dt_est_prov_reg$meanprop_regs2
      dt_est_prov_reg$estprop_regs_agg2 <- dt_est_prov_reg$estprop_regs2
    }
    
    
    ## aggregate regional estimates into provinvial estimates
    dt_country <- dt_date[dt_date$iso.3166.2 == countrycode, ]
    sumreach_country <- sum(dt_country$reach)
    estprop_country <- sum(dt_country$cases)/sumreach_country
    meanprop_country <- mean(dt_country$cases/dt_country$reach)
    estprop_country2 <- sum(dt_country$recentcases, na.rm = T)/sumreach_country
    meanprop_country2 <- mean(dt_country$recentcases/dt_country$reach, na.rm = T)
    
    
    dt_est_reg_count <- by(dt_est_prov_reg,
                           list(dt_est_prov_reg$countrycode, dt_est_prov_reg$regioncode), # groupby country and region and compute population
                           function(x){
                             data.frame(countrycode = unique(x$countrycode),
                                        regioncode = unique(x$regioncode), 
                                        population_region = unique(x$population_region),
                                        estprop_regs_agg = unique(x$estprop_regs_agg),
                                        meanprop_regs_agg = unique(x$meanprop_regs_agg),
                                        estprop_regs_agg2 = unique(x$estprop_regs_agg2),
                                        meanprop_regs_agg2 = unique(x$meanprop_regs_agg2),
                                        sumreach_regs = unique(x$sumreach_regs),
                                        stringsAsFactors = F)
                           })
    dt_est_reg_count <- do.call(rbind, dt_est_reg_count)
    weight <- dt_est_reg_count$population_region/sum(dt_est_reg_count$population_region)
    estprop_country_rh <- sum(weight * dt_est_reg_count$estprop_regs_agg, na.rm = T)
    meanprop_country_rh <- sum(weight * dt_est_reg_count$meanprop_regs_agg, na.rm = T)
    estprop_country_rh2 <- sum(weight * dt_est_reg_count$estprop_regs_agg2, na.rm = T)
    meanprop_country_rh2 <- sum(weight * dt_est_reg_count$meanprop_regs_agg2, na.rm = T)
    sumreach_country_rh <-  sum(dt_est_reg_count$sumreach_regs)
    
    
    
    meanprop_counts_agg <- ((sumreach_country/(sumreach_country + sumreach_country_rh)) * meanprop_country) + 
      ((sumreach_country_rh/(sumreach_country + sumreach_country_rh)) * meanprop_country_rh)
    
    estprop_counts_agg <- ((sumreach_country/(sumreach_country + sumreach_country_rh)) * estprop_country) + 
      ((sumreach_country_rh/(sumreach_country + sumreach_country_rh)) * estprop_country_rh)
    
    meanprop_counts_agg2 <- ((sumreach_country/(sumreach_country + sumreach_country_rh)) * meanprop_country2) + 
      ((sumreach_country_rh/(sumreach_country + sumreach_country_rh)) * meanprop_country_rh2)
    
    estprop_counts_agg2 <- ((sumreach_country/(sumreach_country + sumreach_country_rh)) * estprop_country2) + 
      ((sumreach_country_rh/(sumreach_country + sumreach_country_rh)) * estprop_country_rh2)
    
    
    if (write_daily_file == T){
      dt_est_count <- data.frame(countrycode = countrycode,
                                 population_country = sum(dtregs$population_region),
                                 estprop_country = estprop_country,
                                 meanprop_country = meanprop_country,
                                 estprop_country2 = estprop_country2,
                                 meanprop_country2 = meanprop_country2,
                                 sumreach_country = sumreach_country,
                                 estprop_country_rhs = estprop_country_rh,
                                 meanprop_country_rhs = meanprop_country_rh, 
                                 estprop_country_rhs2 = estprop_country_rh2,
                                 meanprop_country_rhs2 = meanprop_country_rh2, 
                                 sumreach_country_rhs = sumreach_country_rh, 
                                 meanprop_country_agg = meanprop_counts_agg,
                                 estprop_country_agg = estprop_counts_agg,
                                 meanprop_country_agg2 = meanprop_counts_agg2,
                                 estprop_country_agg2 = estprop_counts_agg2)
      
      cat(paste0("Writing the region based daily estimate for ", countrycode, "..\n")) 
      dt_est_prov_reg_country <- merge(dt_est_prov_reg, dt_est_count, all = T, by = "countrycode")
      write.csv(x = dt_est_prov_reg_country, file = paste0("../data/PlotData/", countrycode, "_regional_estimates/daily_province_region_country_based_estimates/",
                                                           countrycode, "-province-region-country-based-estimate-", gsub("/", "_", j), ".csv"))
    }
    
    
    
    
    r_c <-  c(r_c, sumreach_country)
    r_r <-  c(r_r, sumreach_country_rh)
    I_c_estprop <- c(I_c_estprop, estprop_country)
    I_c_meanprop <- c(I_c_meanprop, meanprop_country)
    I_c_estprop2 <- c(I_c_estprop2, estprop_country2)
    I_c_meanprop2 <- c(I_c_meanprop2, meanprop_country2)
    
    
    I_r_estprop <- c(I_r_estprop, estprop_country_rh)
    I_r_meanprop <- c(I_r_meanprop , meanprop_country_rh)
    I_r_estprop2 <- c(I_r_estprop2, estprop_country_rh2)
    I_r_meanprop2 <- c(I_r_meanprop2 , meanprop_country_rh2)
    
    estprop_country_agg <- c(estprop_country_agg, estprop_counts_agg)
    meanprop_country_agg <- c(meanprop_country_agg, meanprop_counts_agg)
    estprop_country_agg2 <- c(estprop_country_agg2, estprop_counts_agg2)
    meanprop_country_agg2 <- c(meanprop_country_agg2, meanprop_counts_agg2)
  }
  
  region_based_estimate <- data.frame(date = dates,
                                      r_c = r_c,
                                      r_r = r_r,
                                      I_c_estprop = I_c_estprop,
                                      I_c_meanprop = I_c_meanprop,
                                      I_c_estprop2 = I_c_estprop2,
                                      I_c_meanprop2 = I_c_meanprop2,
                                      I_r_estprop = I_r_estprop, 
                                      I_r_meanprop = I_r_meanprop,
                                      I_r_estprop2 = I_r_estprop2, 
                                      I_r_meanprop2 = I_r_meanprop2,
                                      estprop_country_agg = estprop_country_agg,
                                      meanprop_country_agg = meanprop_country_agg,
                                      estprop_country_agg2 = estprop_country_agg2,
                                      meanprop_country_agg2 = meanprop_country_agg2)
  if(write_summary_file == T){
    cat(paste0("Writing the region based estimate summary for ", countrycode, "..\n"))
    write.csv(region_based_estimate, paste0("../data/PlotData/",
                                            countrycode,
                                            "_regional_estimates/region_based_estimates/",
                                            countrycode,
                                            "-province-region-country-based-estimate.csv"))
  }
  else{
    return(region_based_estimate)
  }
  
}

provincial_regional_estimate2(countrycode = "ES",
                             write_summary_file = T,
                             alpha = 0.00001,
                             write_daily_file = T)

provincial_regional_estimate2(countrycode = "BR",
                             province = F,
                             alpha = 0.00001,
                             write_summary_file = T,
                             write_daily_file = T)
