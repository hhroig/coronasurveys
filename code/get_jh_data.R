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