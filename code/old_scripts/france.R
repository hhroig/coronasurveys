source("corona_surveys_estimate.R")

# test for France
francePopulation <- 65238294
estimates_fr_poll1 <- estimate_cases(file_path = "../data/FR-01-20200322-20200323.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_poll2 <- estimate_cases(file_path = "../data/FR-02-20200329-20200329.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_poll3 <- estimate_cases(file_path = "../data/FR-03-20200330-20200401.csv", country_population = francePopulation, correction_factor = 1)

estimates_fr_pollbd1 <- estimate_cases(file_path = "../data/byDay/FR-01-20200322-20200322.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_pollbd2 <- estimate_cases(file_path = "../data/byDay/FR-02-20200323-20200323.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_pollbd3 <- estimate_cases(file_path = "../data/byDay/FR-03-20200329-20200329.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_pollbd4 <- estimate_cases(file_path = "../data/byDay/FR-04-20200330-20200330.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_pollbd5 <- estimate_cases(file_path = "../data/byDay/FR-05-20200331-20200331.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_pollbd6 <- estimate_cases(file_path = "../data/byDay/FR-06-20200401-20200401.csv", country_population = francePopulation, correction_factor = 1)

# check results


estimates_fr_poll1$mean_cases 
estimates_fr_poll1$mean_reach
estimates_fr_poll1$cases_per_reach
estimates_fr_poll1$estimated_cases 
estimates_fr_poll1$dunbar_cases 

#
estimates_fr_poll2$mean_cases 
estimates_fr_poll2$mean_reach
estimates_fr_poll2$cases_per_reach
estimates_fr_poll2$estimated_cases 
estimates_fr_poll2$dunbar_cases 

#
estimates_fr_poll3$mean_cases 
estimates_fr_poll3$mean_reach
estimates_fr_poll3$cases_per_reach
estimates_fr_poll3$estimated_cases 
estimates_fr_poll3$dunbar_cases 

estimates_fr_pollbd1$mean_cases 
estimates_fr_pollbd1$mean_reach
estimates_fr_pollbd1$cases_per_reach
estimates_fr_pollbd1$estimated_cases 
estimates_fr_pollbd1$dunbar_cases 

#
estimates_fr_pollbd2$mean_cases 
estimates_fr_pollbd2$mean_reach
estimates_fr_pollbd2$cases_per_reach
estimates_fr_pollbd2$estimated_cases 
estimates_fr_pollbd2$dunbar_cases 

#
estimates_fr_pollbd3$mean_cases 
estimates_fr_pollbd3$mean_reach
estimates_fr_pollbd3$cases_per_reach
estimates_fr_pollbd3$estimated_cases 
estimates_fr_pollbd3$dunbar_cases 

estimates_fr_pollbd4$mean_cases 
estimates_fr_pollbd4$mean_reach
estimates_fr_pollbd4$cases_per_reach
estimates_fr_pollbd4$estimated_cases 
estimates_fr_pollbd4$dunbar_cases 

#
estimates_fr_pollbd5$mean_cases 
estimates_fr_pollbd5$mean_reach
estimates_fr_pollbd5$cases_per_reach
estimates_fr_pollbd5$estimated_cases 
estimates_fr_pollbd5$dunbar_cases 

#
estimates_fr_pollbd6$mean_cases 
estimates_fr_pollbd6$mean_reach
estimates_fr_pollbd6$cases_per_reach
estimates_fr_pollbd6$estimated_cases 
estimates_fr_pollbd6$dunbar_cases 


