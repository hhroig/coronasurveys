source("corona_surveys_estimate.R")

# test for portugal

estimates_pt_pool3 <- estimate_cases(file_path = "../data/PT-03-20200322-20200323.csv", country_population = 10261075, correction_factor = 1)
estimates_pt_pool4 <- estimate_cases(file_path = "../data/PT-04-20200324-20200325.csv", country_population = 10261075, correction_factor = 1)

# check results
estimates_pt_pool3$mean_cases 
estimates_pt_pool3$mean_reach
estimates_pt_pool3$cases_per_reach
estimates_pt_pool3$estimated_cases 

#
estimates_pt_pool4$mean_cases 
estimates_pt_pool4$mean_reach
estimates_pt_pool4$cases_per_reach
estimates_pt_pool4$estimated_cases 
