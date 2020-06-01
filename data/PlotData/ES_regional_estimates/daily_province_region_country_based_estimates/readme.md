
- countrycode: 
- regioncode:
- provincecode:
- population: population of province (or lowest level of division)

**- p_w_prov (was estprop_provs): estimate of p as sum_cases/sum_reach from province data only**

**- p_m_prov (was meanprop_provs): estimate of p as mean(cases/reach) from province data only**

- sumreach_provs: sum of the reach in responses for the province data only
- population_region: region population (sum or population grouped by regioncode)

**- p_w_regs_only (was estprop_regs): estimate of p as sum_cases/sum_reach from region data only (I_c)**

**- p_m_regs_only (was meanprop_regs): estimate of p as mean(cases/reach) from region data only (I_c)**

- sumreach_regs: sum of the reach in responses for the region data only (r_c)

**- p_w_regs_rhs (was estprop_regs_rhs): estimate of p for the region obtained from province data using p_w_prov (I_r)**

**- p_m_regs_rhs (was meanprop_regs_rhs): estimate of p for the region obtained from province data using p_m_prov (I_r)**

- sumreach_regs_rhs: sum of the reach for region; obtained from sum of sumreach_provs (r_r)

**- p_w_regs (was estprop_regs_agg):  estimate of p for the region obtained weighted sum of p_w_regs_only and p_w_regs_rhs**

**- p_m_regs (was meanprop_regs_agg): estimate of p for the region obtained weighted sum of p_m_regs_only and p_m_regs_rhs**

- population_country: population of country (sum of population)

**- p_w_country_only (was estprop_country):  estimate of p as sum_cases/sum_reach from country data only (I_c)**

**- p_m_country_only (was meanprop_country): estimate of p as mean(cases/reach) from country data only (I_c)**

- sumreach_country: sum of the reach in responses for the country data only (r_c)

**- p_w_country_rhs (was estprop_country_rhs): estimate of p for the country obtained from region data using p_w_regs (I_r)**

**- p_m_country_rhs (was meanprop_country_rhs): estimate of p for the country obtained from region data using p_m_regs (I_r)**

- sumreach_country_rhs: sum of the reach for country; obtained from sum of sumreach_regs (r_r)

**- p_w_country (was estprop_country_agg): estimate of p for the country obtained from the weighted sum of p_w_country_only and p_w_country_rhs (I)**

**- p_m_country (was meanprop_country_agg): estimate of p for the country obtained from the weighted sum of p_m_country_only and p_m_country_rhs (I)

**- recent_p_w_* and recent_p_m_* are the same as p_w and p_m, respectively, but computed with recent_cases instead of cases.**

