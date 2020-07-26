## script needs file for country and country population.
library(tidyverse)
library(readxl)
library(httr)
#source("get_jh_data.R") # function to get data from jh csce
#source("get_twitter_data.R") # function to get twitter data
source("spain_regional_estimate.R")
source("portugal_regional_estimates.R")
#source("spain_region_based_estimate.R")
source("portugal_region_based_estimate.R")
#source("italy_region_based_estimate.R")
source("ukraine_region_based_estimate.R")


# compute nee decentralized estimates
try(source("script-confirmed.R"), silent = T)
try(source("script-ccfr-based.R"), silent = T)
#source("script-ccfr-based.R")
try(source("script-30responses.R"), silent = T)
try(source("script-300responses.R"), silent = T)
try(source("script-W-alpha.R"), silent = T)
try(source("script-W.R"), silent = T)
source("script-smooth-300responses.R")
