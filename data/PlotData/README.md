The variables in the generated files are:  
- cases: cases recorded for current day..from ecdc data
- deaths: deaths of current day..from ecdc data
- cum_cases: cumulative cases up to the current day
- cum_deaths: cumulative deaths up to the current day
- cum_deaths_400: cumulative deaths up to the current day *400
- mean_cases: daily mean of cases  
- mean_reach: dialy mean of reach
- dunbar_reach: 150 * sample_size (for current day after ouliers have
been removed)
- cases_p_reach: The % of population infected, where the confidence interval
estimates for this variable are: cases_p_reach_low and cases_p_reach_high and cases_p_reach_error.
- cases_p_reach_prop: mean(cases/reach). mean is taken over the current day.
- cases_p_reach_prop_median : median(cases/reach). median is taken over the current day.
- estimated_cases: country_population * cases_p_reach *
correction_factor. There are additional confidence interval estimates
for this variable which are:estimated_cases_low, estimated_cases_high,
and estimated_cases_error.
- prop_cases: country_population * cases_p_reach_prop * correction_factor
- dunbar_cases: country_population * (sum(cases)/dunbar_reach) *
correction_factor. Sum of cases is done over the current day.
- sample_size: the number of samples used for computation for that day
(after removing outliers)
- survey_twitter: estimates from the twitter survey if availale. I think
only the ES and PT has some estimates available for twitter survey.
