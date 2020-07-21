The variables in the generated files are:  
- countriesAndTerritories: country/territory name
- geoId: ISO2 code of country

**- population: population of the country**

- date: date
- cases: cases recorded for current day..from ecdc data
- deaths: deaths of current day..from ecdc data
- cum_cases: cumulative cases up to the current day
- cum_deaths: cumulative deaths up to the current day
- cum_deaths_400: cumulative deaths up to the current day *400

**- p_ccfr: estimated ratio of the population infected (with symptoms) based on delay adjusted case fatality ratio**

- est_ccfr: estimated number of infected cases (with symptoms) based on delay adjusted case fatality ratio where 
the confidence interval estimates for this variable are: est_ccfr_low and est_ccfr_high.
- sample_size: sample size (number of responses) used to compute estimates based on proportion below (after removing outliers)

**- sum_cases: sum of all the cases reported in the sample (responses)**

**- sum_reach: sum of all the reach values reported in the sample (responses)**

- mean_cases: daily mean of cases  
- mean_reach: dialy mean of reach
- dunbar_reach: 150 * sample_size (for current day after ouliers have
been removed)

**- p_w (aka cases_p_reach): The estimated ratio of the population infected (with symptoms), computed as sum_cases/sum_reach. The confidence interval estimates for this variable are: cases_p_reach_low and cases_p_reach_high and cases_p_reach_error.**

**- p_m (aka cases_p_reach_prop): The estimated ratio of the population infected (with symptoms), computed as mean(cases/reach). The mean is taken over the sample (set of responses).**

**- p_d: The estimated ratio of the population infected (with symptoms), computed as sum_cases/dunbar_reach.**

- cases_p_reach_prop_median : median(cases/reach). median is taken over the sample (set of responses).
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
