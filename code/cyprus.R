source("corona_surveys_estimate.R")

# test for cyprus

estimates_cyprus_pool2 <- estimate_cases(file_path = "../data/CY-02-20200320-20200321.csv", country_population = 890900)
estimates_cyprus_pool3 <- estimate_cases(file_path = "../data/CY-03-20200323-20200325.csv", country_population = 890900)
estimates_cyprus_pool4 <- estimate_cases(file_path = "../data/CY-04-20200327-20200328.csv", country_population = 890900)
estimates_cyprus_pool5 <- estimate_cases(file_path = "../data/CY-05-20200329-20200330.csv", country_population = 890900)

# check results
estimates_cyprus_pool2$mean_cases 
estimates_cyprus_pool2$mean_reach
estimates_cyprus_pool2$cases_per_reach
estimates_cyprus_pool2$estimated_cases 

# check results
estimates_cyprus_pool3$mean_cases 
estimates_cyprus_pool3$mean_reach
estimates_cyprus_pool3$cases_per_reach
estimates_cyprus_pool3$estimated_cases 

# check results
estimates_cyprus_pool4$mean_cases 
estimates_cyprus_pool4$mean_reach
estimates_cyprus_pool4$cases_per_reach
estimates_cyprus_pool4$estimated_cases 

# check results
estimates_cyprus_pool5$mean_cases 
estimates_cyprus_pool5$mean_reach
estimates_cyprus_pool5$cases_per_reach
estimates_cyprus_pool5$estimated_cases 