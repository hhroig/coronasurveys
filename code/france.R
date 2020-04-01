source("corona_surveys_estimate.R")

# test for France
francePopulation <- 65238294
estimates_fr_pool1 <- estimate_cases(file_path = "../data/FR-01-20200322-20200323.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_pool2 <- estimate_cases(file_path = "../data/FR-02-20200329-20200329.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_pool3 <- estimate_cases(file_path = "../data/FR-03-20200330-20200401.csv", country_population = francePopulation, correction_factor = 1)

estimates_fr_poolbd1 <- estimate_cases(file_path = "../data/byDay/FR-01-20200322-20200322.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_poolbd2 <- estimate_cases(file_path = "../data/byDay/FR-02-20200323-20200323.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_poolbd3 <- estimate_cases(file_path = "../data/byDay/FR-03-20200329-20200329.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_poolbd4 <- estimate_cases(file_path = "../data/byDay/FR-04-20200330-20200330.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_poolbd5 <- estimate_cases(file_path = "../data/byDay/FR-05-20200331-20200331.csv", country_population = francePopulation, correction_factor = 1)
estimates_fr_poolbd6 <- estimate_cases(file_path = "../data/byDay/FR-06-20200401-20200401.csv", country_population = francePopulation, correction_factor = 1)

# check results


estimates_fr_pool1$mean_cases 
estimates_fr_pool1$mean_reach
estimates_fr_pool1$cases_per_reach
estimates_fr_pool1$estimated_cases 
estimates_fr_pool1$dunbar_cases 

#
estimates_fr_pool2$mean_cases 
estimates_fr_pool2$mean_reach
estimates_fr_pool2$cases_per_reach
estimates_fr_pool2$estimated_cases 
estimates_fr_pool2$dunbar_cases 

#
estimates_fr_pool3$mean_cases 
estimates_fr_pool3$mean_reach
estimates_fr_pool3$cases_per_reach
estimates_fr_pool3$estimated_cases 
estimates_fr_pool3$dunbar_cases 

estimates_fr_poolbd1$mean_cases 
estimates_fr_poolbd1$mean_reach
estimates_fr_poolbd1$cases_per_reach
estimates_fr_poolbd1$estimated_cases 
estimates_fr_poolbd1$dunbar_cases 

#
estimates_fr_poolbd2$mean_cases 
estimates_fr_poolbd2$mean_reach
estimates_fr_poolbd2$cases_per_reach
estimates_fr_poolbd2$estimated_cases 
estimates_fr_poolbd2$dunbar_cases 

#
estimates_fr_poolbd3$mean_cases 
estimates_fr_poolbd3$mean_reach
estimates_fr_poolbd3$cases_per_reach
estimates_fr_poolbd3$estimated_cases 
estimates_fr_poolbd3$dunbar_cases 

estimates_fr_poolbd4$mean_cases 
estimates_fr_poolbd4$mean_reach
estimates_fr_poolbd4$cases_per_reach
estimates_fr_poolbd4$estimated_cases 
estimates_fr_poolbd4$dunbar_cases 

#
estimates_fr_poolbd5$mean_cases 
estimates_fr_poolbd5$mean_reach
estimates_fr_poolbd5$cases_per_reach
estimates_fr_poolbd5$estimated_cases 
estimates_fr_poolbd5$dunbar_cases 

#
estimates_fr_poolbd6$mean_cases 
estimates_fr_poolbd6$mean_reach
estimates_fr_poolbd6$cases_per_reach
estimates_fr_poolbd6$estimated_cases 
estimates_fr_poolbd6$dunbar_cases 


