# without recent cases
provincial_regional_estimate <- function(countrycode = "ES",
                                          province = T,
                                          district = F,
                                          W = 90,
                                          alpha = 0.00001,
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
  
  r_c <- r_r <-  I_c_p_w_country <- I_c_p_m_country <- c()
  I_r_p_w_country <- I_r_p_m_country <-  c()
  p_w_country <- p_m_country <-  c()
  
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    # get data from the past up to W days earlier
    subcondition <- (as.Date(dt$date) >= (as.Date(j)-W)  & as.Date(dt$date) <= as.Date(j) )
    dt_date <- dt[subcondition, ]
    
    
    # compute provincia provinces
    if (province == T){
      dtprovs <- na.omit(dt_region2)
      provs <- unique(dtprovs$provincecode)
      p_w_provs <- p_m_provs <- sumreach_provs <- rep(NA, length(provs))
      for (i in seq_along(provs)) {
        provpop <- dtprovs$population[dtprovs$provincecode == provs[i]]
        dt_prov <- dt_date[dt_date$iso.3166.2 == provs[i], ]
        if((sum(dt_prov$reach)/provpop) >= alpha){
          p_w_provs[i] <- sum(dt_prov$cases)/sum(dt_prov$reach)
          p_m_provs[i]  <- mean(dt_prov$cases/dt_prov$reach)
          
          sumreach_provs[i] <- sum(dt_prov$reach)
        }else{
          sumreach_provs[i] <- sum(dt_prov$reach)
        }
        
      }
      dtestpropprovs <- data.frame(provincecode = provs,
                                   p_w_provs = p_w_provs,
                                   p_m_provs = p_m_provs,
                                   
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
    p_w_regs_only <- p_m_regs_only <-  sumreach_regs <- rep(NA, length(regions))
    for (k in seq_along(regions)) {
      regpop <- dtregs$population_region[dtregs$regioncode == regions[k]]
      dt_reg <- dt_date[dt_date$iso.3166.2 == regions[k], ]
      if((sum(dt_reg$reach)/regpop) >= alpha){
        p_w_regs_only[k] <- sum(dt_reg$cases)/sum(dt_reg$reach)
        p_m_regs_only[k] <- mean(dt_reg$cases/dt_reg$reach)
        
        sumreach_regs[k] <- sum(dt_reg$reach)
      }else{
        sumreach_regs[k] <- sum(dt_reg$reach)
      }
      
    }
    
    dtestpropregs <- data.frame(regioncode = regions,
                                p_w_regs_only = p_w_regs_only,
                                p_m_regs_only = p_m_regs_only,
                                
                                sumreach_regs = sumreach_regs)
    
    dtregs <- merge(dtregs, dtestpropregs, all = T, by = "regioncode")
    
    # aggregate provincia data into regional data
    if(province == T){
      # population variable refer to population at lowest level
      dt_est_prov_reg <- merge(dtprovs, dtregs, all = T, by = c("countrycode", "regioncode")) 
      # go over regions and computed aggregated means
      uregions <- unique(dt_est_prov_reg$regioncode)
      p_w_regs_rhs <-  p_m_regs_rhs <-  sumreach_regs_rhs <- rep(NA, length(uregions))
      for (l in seq_along(uregions)) {
        dt_est_reg <- na.omit(dt_est_prov_reg[dt_est_prov_reg$regioncode == uregions[l], ])
        weightreg <- dt_est_reg$population/dt_est_reg$population_region
        p_w_regs_rhs[l] <- sum(weightreg * dt_est_reg$p_w_provs)
        p_m_regs_rhs[l] <- sum(weightreg * dt_est_reg$p_m_provs)
        
        sumreach_regs_rhs[l] <- sum(dt_est_reg$sumreach_provs)
      }
      
      dt_regs_rhs <- data.frame(regioncode = unique(dt_est_prov_reg$regioncode),
                                p_w_regs_rhs = p_w_regs_rhs,
                                p_m_regs_rhs = p_m_regs_rhs,
                                
                                sumreach_regs_rhs = sumreach_regs_rhs)
      
      dt_est_prov_reg <- merge(dt_est_prov_reg, dt_regs_rhs, by = c("regioncode"))
      
      dt_est_prov_reg$p_w_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                     dt_est_prov_reg$p_w_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                         dt_est_prov_reg$p_w_regs_rhs)
      
      dt_est_prov_reg$p_m_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs + dt_est_prov_reg$sumreach_regs_rhs)) *
                                     dt_est_prov_reg$p_m_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                         dt_est_prov_reg$p_m_regs_rhs)
      
      
      
      
    }else{
      dt_est_prov_reg <- dtregs
      dt_est_prov_reg$p_w_regs <- dt_est_prov_reg$p_w_regs_only
      dt_est_prov_reg$p_m_regs <- dt_est_prov_reg$p_m_regs_only
      
      
    }
    
    
    ## aggregate regional estimates into provinvial estimates
    dt_country <- dt_date[dt_date$iso.3166.2 == countrycode, ]
    sumreach_country <- sum(dt_country$reach)
    p_w_country_only <- sum(dt_country$cases)/sumreach_country
    p_m_country_only <- mean(dt_country$cases/dt_country$reach)
    
    
    
    dt_est_reg_count <- by(dt_est_prov_reg,
                           list(dt_est_prov_reg$countrycode, dt_est_prov_reg$regioncode), # groupby country and region and compute population
                           function(x){
                             data.frame(countrycode = unique(x$countrycode),
                                        regioncode = unique(x$regioncode), 
                                        population_region = unique(x$population_region),
                                        p_w_regs = unique(x$p_w_regs),
                                        p_m_regs = unique(x$p_m_regs),
                                        sumreach_regs = unique(x$sumreach_regs),
                                        stringsAsFactors = F)
                           })
    dt_est_reg_count <- do.call(rbind, dt_est_reg_count)
    weightcountry <- dt_est_reg_count$population_region/sum(dt_est_reg_count$population_region)
    p_w_country_rhs <- sum(weightcountry * dt_est_reg_count$p_w_regs, na.rm = T)
    p_m_country_rhs <- sum(weightcountry * dt_est_reg_count$p_m_regs, na.rm = T)
    sumreach_country_rhs <-  sum(dt_est_reg_count$sumreach_regs)
    
    
    
    
    p_w_counts <- ((sumreach_country/(sumreach_country + sumreach_country_rhs)) * p_w_country_only) + 
      ((sumreach_country_rhs/(sumreach_country + sumreach_country_rhs)) * p_w_country_rhs)
    p_m_counts <- ((sumreach_country/(sumreach_country + sumreach_country_rhs)) * p_m_country_only) + 
      ((sumreach_country_rhs/(sumreach_country + sumreach_country_rhs)) * p_m_country_rhs)
    
    
    
    
    if (write_daily_file == T){
      dt_est_count <- data.frame(countrycode = countrycode,
                                 population_country = sum(dtregs$population_region),
                                 p_w_country_only = p_w_country_only,
                                 p_m_country_only = p_m_country_only,
                                 
                                 sumreach_country = sumreach_country,
                                 p_w_country_rhs = p_w_country_rhs,
                                 p_m_country_rhs = p_m_country_rhs, 
                                 
                                 sumreach_country_rhs = sumreach_country_rhs, 
                                 p_w_country =  p_w_counts,
                                 p_m_country =  p_m_counts)
      
      cat(paste0("Writing the region based daily estimate for ", countrycode, "..\n")) 
      dt_est_prov_reg_country <- merge(dt_est_prov_reg, dt_est_count, all = T, by = "countrycode")
      write.csv(x = dt_est_prov_reg_country, file = paste0("../data/PlotData/",
                                                           countrycode,
                                                           "_regional_estimates/daily_province_region_country_based_estimates/",
                                                           countrycode,
                                                           "-province-region-country-based-estimate-", gsub("/", "_", j), ".csv"))
    }
    
    
    
    
    r_c <-  c(r_c, sumreach_country)
    r_r <-  c(r_r, sumreach_country_rhs)
    I_c_p_w_country <- c(I_c_p_w_country, p_w_country_only)
    I_c_p_m_country <- c(I_c_p_m_country, p_m_country_only)
    
    
    
    I_r_p_w_country <- c(I_r_p_w_country, p_w_country_rhs)
    I_r_p_m_country <- c(I_r_p_m_country, p_m_country_rhs)
    
    
    p_w_country <- c(p_w_country, p_w_counts)
    p_m_country <- c(p_m_country, p_m_counts)
    
  }
  
  region_based_estimate <- data.frame(date = dates,
                                      r_c = r_c,
                                      r_r = r_r,
                                      I_c_p_w_country = I_c_p_w_country,
                                      I_c_p_m_country = I_c_p_m_country,
                                      
                                      I_r_p_w_country = I_r_p_w_country, 
                                      I_r_p_m_country = I_r_p_m_country,
                                      
                                      p_w_country = p_w_country,
                                      p_m_country = p_m_country)
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


#with recent cases
provincial_regional_estimate2 <- function(countrycode = "ES",
                                         province = T,
                                         district = F,
                                         W = 90,
                                         alpha = 0.00001,
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

  r_c <- r_r <-  I_c_p_w_country <- I_c_p_m_country <- I_c_recent_p_w_country <- I_c_recent_p_m_country <- c()
  I_r_p_w_country <- I_r_p_m_country <- I_r_recent_p_w_country <- I_r_recent_p_m_country <- c()
  p_w_country <- p_m_country <- recent_p_w_country <- recent_p_m_country <- c()
  
  for (j in dates){
    cat("working on date: ", j, "\n"  )
    # get data from the past up to W days earlier
    subcondition <- (as.Date(dt$date) >= (as.Date(j)-W)  & as.Date(dt$date) <= as.Date(j) )
    dt_date <- dt[subcondition, ]
    
    
    # compute provincia provinces
    if (province == T){
      dtprovs <- na.omit(dt_region2)
      provs <- unique(dtprovs$provincecode)
      p_w_provs <- p_m_provs <- recent_p_w_provs <- recent_p_m_provs <- sumreach_provs <- rep(NA, length(provs))
      for (i in seq_along(provs)) {
        provpop <- dtprovs$population[dtprovs$provincecode == provs[i]]
        dt_prov <- dt_date[dt_date$iso.3166.2 == provs[i], ]
        if((sum(dt_prov$reach)/provpop) >= alpha){
          p_w_provs[i] <- sum(dt_prov$cases)/sum(dt_prov$reach)
          p_m_provs[i]  <- mean(dt_prov$cases/dt_prov$reach)
          recent_p_w_provs[i] <- ifelse(all(is.na(dt_prov$recentcases)), 
                                      NA, sum(dt_prov$recentcases, na.rm = T)/sum(dt_prov$reach[!is.na(dt_prov$recentcases)]))  #should we use the whole sum or partial sum
          recent_p_m_provs[i] <- ifelse(all(is.na(dt_prov$recentcases)), 
                                       NA, mean(dt_prov$recentcases/dt_prov$reach, na.rm = T))
          sumreach_provs[i] <- sum(dt_prov$reach)
        }else{
          sumreach_provs[i] <- sum(dt_prov$reach)
        }
        
      }
      dtestpropprovs <- data.frame(provincecode = provs,
                                   p_w_provs = p_w_provs,
                                   p_m_provs = p_m_provs,
                                   recent_p_w_provs = recent_p_w_provs,
                                   recent_p_m_provs = recent_p_m_provs,
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
    p_w_regs_only <- p_m_regs_only <- recent_p_w_regs_only <- recent_p_m_regs_only <- sumreach_regs <- rep(NA, length(regions))
    for (k in seq_along(regions)) {
      regpop <- dtregs$population_region[dtregs$regioncode == regions[k]]
      dt_reg <- dt_date[dt_date$iso.3166.2 == regions[k], ]
      if((sum(dt_reg$reach)/regpop) >= alpha){
        p_w_regs_only[k] <- sum(dt_reg$cases)/sum(dt_reg$reach)
        p_m_regs_only[k] <- mean(dt_reg$cases/dt_reg$reach)
        recent_p_w_regs_only[k] <- ifelse(all(is.na(dt_reg$recentcases)),
                             NA, sum(dt_reg$recentcases, na.rm = T)/sum(dt_reg$reach[!is.na(dt_reg$recentcases)]))  # should we use partial sum?
        recent_p_m_regs_only[k] <- ifelse(all(is.na(dt_reg$recentcases)),
                               NA, mean(dt_reg$recentcases/dt_reg$reach, na.rm = T))
        sumreach_regs[k] <- sum(dt_reg$reach)
      }else{
        sumreach_regs[k] <- sum(dt_reg$reach)
      }
      
    }
    
    dtestpropregs <- data.frame(regioncode = regions,
                                p_w_regs_only = p_w_regs_only,
                                p_m_regs_only = p_m_regs_only,
                                recent_p_w_regs_only = recent_p_w_regs_only,
                                recent_p_m_regs_only = recent_p_m_regs_only,
                                sumreach_regs = sumreach_regs)
    
    dtregs <- merge(dtregs, dtestpropregs, all = T, by = "regioncode")
    
    # aggregate provincia data into regional data
    if(province == T){
      # population variable refer to population at lowest level
      dt_est_prov_reg <- merge(dtprovs, dtregs, all = T, by = c("countrycode", "regioncode")) 
      # go over regions and computed aggregated means
      uregions <- unique(dt_est_prov_reg$regioncode)
      p_w_regs_rhs <-  p_m_regs_rhs <- recent_p_w_regs_rhs <- recent_p_m_regs_rhs <- sumreach_regs_rhs <- rep(NA, length(uregions))
      for (l in seq_along(uregions)) {
        dt_est_reg <- na.omit(dt_est_prov_reg[dt_est_prov_reg$regioncode == uregions[l], ])
        weightreg <- dt_est_reg$population/dt_est_reg$population_region
        p_w_regs_rhs[l] <- sum(weightreg * dt_est_reg$p_w_provs)
        p_m_regs_rhs[l] <- sum(weightreg * dt_est_reg$p_m_provs)
        recent_p_w_regs_rhs[l] <- sum(weightreg * dt_est_reg$recent_p_w_provs)
        recent_p_m_regs_rhs[l] <- sum(weightreg * dt_est_reg$recent_p_m_provs)
        
        sumreach_regs_rhs[l] <- sum(dt_est_reg$sumreach_provs)
      }
      
      dt_regs_rhs <- data.frame(regioncode = unique(dt_est_prov_reg$regioncode),
                                p_w_regs_rhs = p_w_regs_rhs,
                                p_m_regs_rhs = p_m_regs_rhs,
                                recent_p_w_regs_rhs = recent_p_w_regs_rhs,
                                recent_p_m_regs_rhs = recent_p_m_regs_rhs,
                                sumreach_regs_rhs = sumreach_regs_rhs)
      
      dt_est_prov_reg <- merge(dt_est_prov_reg, dt_regs_rhs, by = c("regioncode"))
      
      dt_est_prov_reg$p_w_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                             dt_est_prov_reg$p_w_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                                dt_est_prov_reg$p_w_regs_rhs)
      
      dt_est_prov_reg$p_m_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs + dt_est_prov_reg$sumreach_regs_rhs)) *
                                              dt_est_prov_reg$p_m_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                                  dt_est_prov_reg$p_m_regs_rhs)
      
      dt_est_prov_reg$recent_p_w_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                              dt_est_prov_reg$recent_p_w_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                                  dt_est_prov_reg$recent_p_w_regs_rhs)
      
      dt_est_prov_reg$recent_p_m_regs <- ((dt_est_prov_reg$sumreach_regs/(dt_est_prov_reg$sumreach_regs + dt_est_prov_reg$sumreach_regs_rhs)) *
                                              dt_est_prov_reg$recent_p_m_regs_only) + ((dt_est_prov_reg$sumreach_regs_rhs/(dt_est_prov_reg$sumreach_regs+dt_est_prov_reg$sumreach_regs_rhs)) *
                                                                                  dt_est_prov_reg$recent_p_m_regs_rhs)
      
      
    }else{
      dt_est_prov_reg <- dtregs
      dt_est_prov_reg$p_w_regs <- dt_est_prov_reg$p_w_regs_only
      dt_est_prov_reg$p_m_regs <- dt_est_prov_reg$p_m_regs_only
      dt_est_prov_reg$recent_p_w_regs <- dt_est_prov_reg$recent_p_w_regs_only
      dt_est_prov_reg$recent_p_m_regs <- dt_est_prov_reg$recent_p_m_regs_only

    }
    
    
    ## aggregate regional estimates into provinvial estimates
    dt_country <- dt_date[dt_date$iso.3166.2 == countrycode, ]
    sumreach_country <- sum(dt_country$reach)
    p_w_country_only <- sum(dt_country$cases)/sumreach_country
    p_m_country_only <- mean(dt_country$cases/dt_country$reach)
    recent_p_w_country_only <- ifelse(all(is.na(dt_country$recentcases)),
                               NA, sum(dt_country$recentcases, na.rm = T)/sum(dt_country$reach[!is.na(dt_country$recentcases)])) # may change
    recent_p_m_country_only <- ifelse(all(is.na(dt_country$recentcases)),
                                NA, mean(dt_country$recentcases/dt_country$reach, na.rm = T))
    
    
    dt_est_reg_count <- by(dt_est_prov_reg,
                           list(dt_est_prov_reg$countrycode, dt_est_prov_reg$regioncode), # groupby country and region and compute population
                           function(x){
                             data.frame(countrycode = unique(x$countrycode),
                                        regioncode = unique(x$regioncode), 
                                        population_region = unique(x$population_region),
                                        p_w_regs = unique(x$p_w_regs),
                                        p_m_regs = unique(x$p_m_regs),
                                        recent_p_w_regs = unique(x$recent_p_w_regs),
                                        recent_p_m_regs = unique(x$recent_p_m_regs),
                                        sumreach_regs = unique(x$sumreach_regs),
                                        stringsAsFactors = F)
                           })
    dt_est_reg_count <- do.call(rbind, dt_est_reg_count)
    weightcountry <- dt_est_reg_count$population_region/sum(dt_est_reg_count$population_region)
    p_w_country_rhs <- sum(weightcountry * dt_est_reg_count$p_w_regs, na.rm = T)
    p_m_country_rhs <- sum(weightcountry * dt_est_reg_count$p_m_regs, na.rm = T)
    recent_p_w_country_rhs <- sum(weightcountry * dt_est_reg_count$recent_p_w_regs, na.rm = T)
    recent_p_m_country_rhs <- sum(weightcountry * dt_est_reg_count$recent_p_m_regs, na.rm = T)
    sumreach_country_rhs <-  sum(dt_est_reg_count$sumreach_regs)
    
    
    
    
    p_w_counts <- ((sumreach_country/(sumreach_country + sumreach_country_rhs)) * p_w_country_only) + 
      ((sumreach_country_rhs/(sumreach_country + sumreach_country_rhs)) * p_w_country_rhs)
    p_m_counts <- ((sumreach_country/(sumreach_country + sumreach_country_rhs)) * p_m_country_only) + 
      ((sumreach_country_rhs/(sumreach_country + sumreach_country_rhs)) * p_m_country_rhs)
    
    recent_p_w_counts <- ((sumreach_country/(sumreach_country + sumreach_country_rhs)) * recent_p_w_country_only) + 
      ((sumreach_country_rhs/(sumreach_country + sumreach_country_rhs)) * recent_p_w_country_rhs)
    
    recent_p_m_counts <- ((sumreach_country/(sumreach_country + sumreach_country_rhs)) * recent_p_m_country_only) + 
      ((sumreach_country_rhs/(sumreach_country + sumreach_country_rhs)) * recent_p_m_country_rhs)
    

    
    
    if (write_daily_file == T){
      dt_est_count <- data.frame(countrycode = countrycode,
                                 population_country = sum(dtregs$population_region),
                                 p_w_country_only = p_w_country_only,
                                 p_m_country_only = p_m_country_only,
                                 recent_p_w_country_only = recent_p_w_country_only,
                                 recent_p_m_country_only = recent_p_m_country_only,
                                 sumreach_country = sumreach_country,
                                 p_w_country_rhs = p_w_country_rhs,
                                 p_m_country_rhs = p_m_country_rhs, 
                                 recent_p_w_country_rhs = recent_p_w_country_rhs,
                                 recent_p_m_country_rhs = recent_p_m_country_rhs, 
                                 sumreach_country_rhs = sumreach_country_rhs, 
                                 p_w_country =  p_w_counts,
                                 p_m_country =  p_m_counts,
                                 recent_p_w_country =  recent_p_w_counts,
                                 recent_p_m_country =  recent_p_m_counts)
      
      cat(paste0("Writing the region based daily estimate for ", countrycode, "..\n")) 
      dt_est_prov_reg_country <- merge(dt_est_prov_reg, dt_est_count, all = T, by = "countrycode")
      write.csv(x = dt_est_prov_reg_country, file = paste0("../data/PlotData/",
                                                           countrycode,
                                                           "_regional_estimates/daily_province_region_country_based_estimates/",
                                                           countrycode,
                                                           "-province-region-country-based-estimate-", gsub("/", "_", j), ".csv"))
    }
    
    
    
    
    r_c <-  c(r_c, sumreach_country)
    r_r <-  c(r_r, sumreach_country_rhs)
    I_c_p_w_country <- c(I_c_p_w_country, p_w_country_only)
    I_c_p_m_country <- c(I_c_p_m_country, p_m_country_only)
    I_c_recent_p_w_country <- c(I_c_recent_p_w_country, recent_p_w_country_only)
    I_c_recent_p_m_country <- c(I_c_recent_p_m_country, recent_p_m_country_only)
    
    
    I_r_p_w_country <- c(I_r_p_w_country, p_w_country_rhs)
    I_r_p_m_country <- c(I_r_p_m_country, p_m_country_rhs)
    I_r_recent_p_w_country <- c(I_r_recent_p_w_country, recent_p_w_country_rhs)
    I_r_recent_p_m_country <- c(I_r_recent_p_m_country , recent_p_m_country_rhs)
    
    p_w_country <- c(p_w_country, p_w_counts)
    p_m_country <- c(p_m_country, p_m_counts)
    recent_p_w_country <- c(recent_p_w_country, recent_p_w_counts)
    recent_p_m_country <- c(recent_p_m_country, recent_p_m_counts)
  }
  
  region_based_estimate <- data.frame(date = dates,
                                      r_c = r_c,
                                      r_r = r_r,
                                      I_c_p_w_country = I_c_p_w_country,
                                      I_c_p_m_country = I_c_p_m_country,
                                      I_c_recent_p_w_country = I_c_recent_p_w_country,
                                      I_c_recent_p_m_country = I_c_recent_p_m_country,
                                      I_r_p_w_country = I_r_p_w_country, 
                                      I_r_p_m_country = I_r_p_m_country,
                                      I_r_recent_p_w_country = I_r_recent_p_w_country, 
                                      I_r_recent_p_m_country = I_r_recent_p_m_country,
                                      p_w_country = p_w_country,
                                      p_m_country = p_m_country,
                                      recent_p_w_country = recent_p_w_country,
                                      recent_p_m_country = recent_p_m_country)
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
