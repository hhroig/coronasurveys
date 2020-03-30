source("corona_surveys_estimate.R")

# test for spain

estimates_spain_pool6 <- estimate_cases(file_path = "../data/ES-06-20200322-20200323.csv", country_population = 46754778)
estimates_spain_pool7 <- estimate_cases(file_path = "../data/ES-07-20200324-20200325.csv", country_population = 46754778)
estimates_spain_pool8 <- estimate_cases(file_path = "../data/ES-08-20200326-20200327.csv", country_population = 46754778)
estimates_spain_pool9 <- estimate_cases(file_path = "../data/ES-09-20200327-20200328.csv", country_population = 46754778)
estimates_spain_pool10 <- estimate_cases(file_path = "../data/ES-10-20200329-20200329.csv", country_population = 46754778)

# check results
estimates_spain_pool6$mean_cases 
estimates_spain_pool6$mean_reach
estimates_spain_pool6$cases_per_reach
estimates_spain_pool6$estimated_cases 
#estimates_spain_pool6$cases_breakdown # table of frequency of cases, good for bar chart
#estimates_spain_pool6$cases_breakdown_region # table of frequency of cases, by region
#estimates_spain_pool6$n_reach_outliers # number of ouliers removed based on reach column
#estimates_spain_pool6$n_maxratio_outliers # number of outliers removed based on max ratio allowed
#estimates_spain_pool6$n_inital_response # number of rows of original data
#estimates_spain_pool6$n_final_response # nrows of data after used for estimation after outliers removed.

#
estimates_spain_pool7$mean_cases 
estimates_spain_pool7$mean_reach
estimates_spain_pool7$cases_per_reach
estimates_spain_pool7$estimated_cases 

estimates_spain_pool8$mean_cases 
estimates_spain_pool8$mean_reach
estimates_spain_pool8$cases_per_reach
estimates_spain_pool8$estimated_cases 

estimates_spain_pool9$mean_cases 
estimates_spain_pool9$mean_reach
estimates_spain_pool9$cases_per_reach
estimates_spain_pool9$estimated_cases 

estimates_spain_pool10$mean_cases 
estimates_spain_pool10$mean_reach
estimates_spain_pool10$cases_per_reach
estimates_spain_pool10$estimated_cases 