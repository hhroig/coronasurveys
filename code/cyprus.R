source("corona_surveys_estimate.R")

# test for spain

estimates_cyprus_pool3 <- estimate_cases(file_path = "../data/CY-03-20200323-20200325e.csv", country_population = 1189265)

# check results
estimates_cyprus_pool3$mean_cases 
estimates_cyprus_pool3$mean_reach
estimates_cyprus_pool3$cases_per_reach
estimates_cyprus_pool3$estimated_cases 
