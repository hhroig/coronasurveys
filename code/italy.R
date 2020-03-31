source("corona_surveys_estimate.R")


estimates_italy_pool2 <- estimate_cases(file_path = "../data/IT-02-20200323-20200324.csv", country_population = 60431283)
estimates_italy_pool3 <- estimate_cases(file_path = "../data/IT-03-20200325-20200326.csv", country_population = 60431283)
estimates_italy_pool4 <- estimate_cases(file_path = "../data/IT-04-20200327-20200328.csv", country_population = 60431283)
estimates_italy_pool5 <- estimate_cases(file_path = "../data/IT-05-20200329-20200330.csv", country_population = 60431283)

# check results
estimates_italy_pool2$mean_cases 
estimates_italy_pool2$mean_reach
estimates_italy_pool2$cases_per_reach
estimates_italy_pool2$estimated_cases 

#
estimates_italy_pool3$mean_cases 
estimates_italy_pool3$mean_reach
estimates_italy_pool3$cases_per_reach
estimates_italy_pool3$estimated_cases 

estimates_italy_pool4$mean_cases 
estimates_italy_pool4$mean_reach
estimates_italy_pool4$cases_per_reach
estimates_italy_pool4$estimated_cases 

estimates_italy_pool5$mean_cases 
estimates_italy_pool5$mean_reach
estimates_italy_pool5$cases_per_reach
estimates_italy_pool5$estimated_cases 

