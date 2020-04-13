The variables in the generated files are:  
- countriesAndTerritories: country/territory name
- geoId: ISO2 code of country
- date: date
- cases: cases recorded for current day..from ecdc data
- deaths: deaths of current day..from ecdc data
- cum_cases: cumulative cases up to the current day
- cum_deaths: cumulative deaths up to the current day
- cum_deaths_400: cumulative deaths up to the current day *400
- est_ccfr: estimated of cases based on delay adjusted case fatality ratio where 
the confidence interval estimates for this variable are: est_ccfr_low and est_ccfr_high.
- sample_size: sample size used to compute estimates based on proportion below (after removing outliers)
- mean_cases: daily mean of cases  
- mean_reach: dialy mean of reach
- dunbar_reach: 150 * sample_size (for current day after ouliers have
been removed)
- cases_p_reach: The % of population infected, where the confidence interval
estimates for this variable are: cases_p_reach_low and cases_p_reach_high and cases_p_reach_error.
- cases_p_reach_prop: mean(cases/reach). mean is taken over the current day.
- cases_p_reach_prop_median : median(cases/reach). median is taken over the current day.
- estimated_cases: country_population * cases_p_reach * correction_factor
where the confidence interval estimates for this variable are:estimate_cases_low,
estimate_cases_high, and estimate_cases_error.
- prop_cases: country_population * cases_p_reach_prop * correction_factor where the confidence interval
estimates for this variable are: prop_cases_low, prop_cases_high, and prop_cases_error.
- dunbar_cases: country_population * (sum(cases)/dunbar_reach) * correction_factor. 
Sum of cases is done over the current day. The confidence interval estimates for this
variable are: dunbar_cases_low, dunbar_cases_high, and dunbar_cases_error.
- survey_twitter: estimates from the twitter survey if availale. I think
only the ES and PT has some estimates available for twitter survey.
