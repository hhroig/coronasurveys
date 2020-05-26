countrycode: 
regioncode:
provincecode:
population: population of province (or lowest level of division)
estprop_provs: estimate of p from province data only
meanprop_provs: estimate of p from province data only
sumreach_provs: sum of the reach in responses for the province data only
population_region: region population (sum or population grouped by regioncode)
estprop_regs: estimate of p from region data only (I_c)
meanprop_regs: estimate of p from region data only (I_c)
sumreach_regs: sum of the reach in responses for the region data only (r_c)
estprop_regs_rhs: estimate of p for the region obtained from province data (I_r)
meanprop_regs_rhs: estimate of p for the region obtained from province data (I_r)
sumreach_regs_rhs: sum of the reach for region; obtained from sum of sumreach_provs (r_r)
meanprop_regs_agg: estimate of p for the region obtained weighted sum of meanprop_regs and meanprop_regs_rhs
estprop_regs_agg:  estimate of p for the region obtained weighted sum of estprop_regs and estprop_regs_rhs
population_country: population of country (sum of population)
estprop_country:  estimate of p from country data only (I_c)
meanprop_country: estimate of p from country data only (I_c)
sumreach_country: sum of the reach in responses for the country data only (r_c)
estprop_country_rhs: estimate of p for the country obtained from province data using estprop_regs_agg (I_r)
meanprop_country_rhs: estimate of p for the country obtained from province data using meanprop_regs_agg (I_r)
sumreach_country_rhs: sum of the reach for country; obtained from sum of sumreach_regs (r_r)
meanprop_country_agg: estimate of p for the country obtained from the weighted sum of meanprop_country_rhs and meanprop_country (I)
estprop_country_agg: estimate of p for the country obtained from the weighted sum of estprop_country_rhs and estprop_country (I)