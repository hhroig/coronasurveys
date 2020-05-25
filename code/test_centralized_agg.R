provinces_and_codes <- readxl::read_excel("regions-tree-population.xlsx")

# test_brazil_regional_estimate <- function(max_ratio = .3){
#   cat("Generating region based estimates for Brazil \n")
#   dt <- read.csv("../data/aggregate/BR-aggregate.csv", as.is = T)
#   dt_region <- provinces_and_codes[provinces_and_codes$countrycode == "BR", ]
#   names(dt) <- tolower(names(dt))
#   dt <- dt[, c("timestamp","region","reach","cases", "iso.3166.1.a2", "iso.3166.2")]
#   dt$date <- substr(dt$timestamp, 1, 10)
#   # outlier detection
#   dt <- dt[!is.na(dt$reach),]
#   reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
#   dt <- dt[dt$reach <= reach_cutoff, ]
#   
#   dt$ratio <- dt$cases/dt$reach    # remove outlier based on 0.3
#   dt <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
#   dt <- dt[dt$ratio <= max_ratio, ]
#   dt$iso.3166.2[dt$iso.3166.2 == ""] <- "PT"
#   #
#   
#   
# }

test_spain_regional_estimate <- function(province = T, district = F,
                                         W = 90, alpha = 0.0001,
                                         max_ratio = .3){
  cat("Generating region based estimates for Spain \n")
  dt <- read.csv("../data/aggregate/ES-aggregate.csv", as.is = T)
  dt_region <- provinces_and_codes[provinces_and_codes$countrycode == "ES", ]
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
  dt$iso.3166.2[dt$iso.3166.2 == ""] <- "ES"
  # removal of multiple responses using cookies
  
  
  # compute provincial estimates
  dates <- unique(dt$date)
  

  dt_region2 <- dt_region[, c("countrycode",  "regioncode",   "provincecode", "population")] 
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
      dt_estimate <- merge(dtregs, dtprovs, all = T, by = c("countrycode", "regioncode")) 
      # go over regions and computed aggregated means
      estprop_regs_rhs <- c()
      meanprop_regs_rhs <- c()
      sumreach_regs_rhs <- c()
      for (i in unique(dt_estimate$regioncode)) {
        dt_est_reg <- na.omit(dt_estimate[dt_estimate$regioncode == i, ])
        weight <- dt_est_reg$population/dt_est_reg$population_region
        estprop_regs_rh <- sum(weight * dt_est_reg$estprop_provs)
        meanprop_regs_rh <- sum(weight * dt_est_reg$meanprop_provs)
        
        sumreach_regs_rhs <- c(sumreach_regs_rhs, sum(dt_est_reg$sumreach_provs))
        estprop_regs_rhs <- c(estprop_regs_rhs, estprop_regs_rh)
        meanprop_regs_rhs <- c(meanprop_regs_rhs, meanprop_regs_rh)
      }
      dt_regs_rhs <- data.frame(regioncode = unique(dt_estimate$regioncode),
                               estprop_regs_rhs = estprop_regs_rhs,
                               meanprop_regs_rhs = meanprop_regs_rhs,
                               sumreach_regs_rhs = sumreach_regs_rhs)
      
      dt_estimate <- merge(dt_estimate, dt_regs_rhs, by = c("regioncode"))
      dt_estimate$meanprop_regs_agg <- ((dt_estimate$sumreach_regs/(dt_estimate$sumreach_regs+dt_estimate$sumreach_regs_rhs)) *
        dt_estimate$meanprop_regs) + ((dt_estimate$sumreach_regs_rhs/(dt_estimate$sumreach_regs+dt_estimate$sumreach_regs_rhs)) *
                                        dt_estimate$meanprop_regs_rhs)
      
      dt_estimate$estprop_regs_agg <- ((dt_estimate$sumreach_regs/(dt_estimate$sumreach_regs+dt_estimate$sumreach_regs_rhs)) *
                                     dt_estimate$estprop_regs) + ((dt_estimate$sumreach_regs_rhs/(dt_estimate$sumreach_regs+dt_estimate$sumreach_regs_rhs)) *
                                                                     dt_estimate$estprop_regs_rhs)
    }else{
      dt_estimate <- dtregs
      dt_estimate$meanprop_regs_agg <- dt_estimate$meanprop_regs
      dt_estimate$estprop_regs_agg <- dt_estimate$estprop_regs
    }
    
    
    ## aggregate regional estimates into provinvial estimates
    dt_country <- dt_date[dt_date$iso.3166.2 == "ES", ]
    sumreach_country <- sum(dt_country$reach)
    estprop_country <- sum(dt_country$cases)/sumreach_country
    meanprop_country <- mean(dt_country$cases/dt_country$reach)
    dtcount <- data.frame(countrycode = "ES",
                          population = sum(dtregs$population_region),
                          sumreach_country = sumreach_country,
                          estprop_country = estprop_country,
                          meanprop_country = meanprop_country)
    dtcounts <- by(dt_estimate,
                 list(dt_estimate$countrycode, dt_estimate$regioncode), # groupby country and region and compute population
                 function(x){
                   data.frame(countrycode = unique(x$countrycode),
                              regioncode = unique(x$regioncode), 
                              population_region = unique(x$population_region),
                              estprop_regs_agg = unique(x$estprop_regs_agg),
                              meanprop_regs_agg = unique(x$meanprop_regs_agg),
                              sumreach_regs = unique(x$sumreach_regs),
                              stringsAsFactors = F)
                 })
    dtcounts <- do.call(rbind, dtcounts)
    weight <- dtcounts$population_region/sum(dtcounts$population_region)
    estprop_country_rh <- sum(weight * dtcounts$estprop_regs_agg)
    meanprop_country_rh <- sum(weight * dtcounts$meanprop_regs_agg)
    
    
    
    meanprop_counts_agg <- ((sumreach_country/(sumreach_country + sum(dtcounts$sumreach_regs))) *
                                          meanprop_country) + ((sum(dtcounts$sumreach_regs)/(sumreach_country + sum(dtcounts$sumreach_regs))) *
                                                                 meanprop_country_rh)
    
    estprop_counts_agg <- ((sumreach_country/(sumreach_country + sum(dtcounts$sumreach_regs))) *
                                         estprop_country) + ((sum(dtcounts$sumreach_regs)/(sumreach_country + sum(dtcounts$sumreach_regs))) *
                                                               estprop_country_rh)
    
    r_c <-  c(r_c, sumreach_country)
    r_r <-  c(r_r, sum(dtcounts$sumreach_regs))
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
                                      meanprop_country_agg = meanprop_country_agg,
                                        )
  cat("writing the region based estimate for Spain..\n")
  write.csv(region_based_estimate, paste0("../data/PlotData/ES_regional_estimates/region_based_estimates/",
                                          "ES-region-based-estimate_province_region_country_agg.csv"))
  
  
}