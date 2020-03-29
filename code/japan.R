source("corona_surveys_estimate.R")

# test for spain

estimates_japan_pool1 <- estimate_cases(file_path = "../data/JP-01-20200326-20200328.csv", country_population = 1.27e+08)

# check results
estimates_japan_pool1$mean_cases 
estimates_japan_pool1$mean_reach
estimates_japan_pool1$cases_per_reach
estimates_japan_pool1$estimated_cases 
