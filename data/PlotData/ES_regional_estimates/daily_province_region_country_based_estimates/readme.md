
- countrycode: 
- regioncode:
- provincecode:

**- pop_prov: population of province (or lowest level of division) (N_i)**

- p_w_prov: estimate of p as sum_cases/sum_reach from province data only
- p_m_prov: estimate of p as mean(cases/reach) from province data only

**- sr_prov: sum of the reach in responses for the province data only**

**- pop_reg: region population (sum of pop_prov grouped by regioncode) (M_k)**

**- p_w_reg_only: estimate of p as sum_cases/sum_reach from region data only**

**- p_m_reg_only: estimate of p as mean(cases/reach) from region data only**

**- sr_reg: sum of the reach in responses for the region data only**

**- p_w_reg_rhs: estimate of p for the region obtained from province data using p_w_prov**

**- p_m_reg_rhs: estimate of p for the region obtained from province data using p_m_prov**

**- sr_reg_rhs: sum of the reach for region; obtained from sum of sumreach_prov**

**- p_w_reg (was estprop_regs_agg):  estimate of p for the region obtained weighted sum of p_w_reg_only and p_w_reg_rhs**

**- p_m_reg (was meanprop_regs_agg): estimate of p for the region obtained weighted sum of p_m_reg_only and p_m_reg_rhs**

**- pop_country: population of country (sum of region population)**

**- p_w_country_only (was estprop_country):  estimate of p as sum_cases/sum_reach from country data only**

**- p_m_country_only (was meanprop_country): estimate of p as mean(cases/reach) from country data only**

**- sr_country: sum of the reach in responses for the country data only**

**- p_w_country_rhs (was estprop_country_rhs): estimate of p for the country obtained from region data using p_w_regs**

**- p_m_country_rhs (was meanprop_country_rhs): estimate of p for the country obtained from region data using p_m_regs**

**- sr_country_rhs: sum of the reach for country; obtained from sum of sumreach_regs**

**- p_w_country (was estprop_country_agg): estimate of p for the country obtained from the weighted sum of p_w_country_only and p_w_country_rhs**

**- p_m_country (was meanprop_country_agg): estimate of p for the country obtained from the weighted sum of p_m_country_only and p_m_country_rhs**

**- recent_p_w_X and recent_p_m_X are the same as p_w and p_m, respectively, but computed with recent_cases instead of cases.**

