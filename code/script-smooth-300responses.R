## OPTION 1 (if packs are installed) ----
# library(dplyr)
# library(plotly)
# library(scam)

## OPTION 2 (if packs are NOT installed) ----
## List of packages
packages = c("dplyr", "plotly", "scam")
# 
# ## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## Write TRUE for plots:
to_plot = F

## get each dataset in "estimates-300responses" ----

all.files <- list.files("../data/estimates-300responses/PlotData/")

for (k in 1:length(all.files)) {
  print(paste("smoothing", all.files[k]))
  data300 <- read.csv(paste0("../data/estimates-300responses/PlotData/",
                             all.files[k]))
  
  ## first non-zero p_cases:
  frst_n_zero <- head(data300[data300$p_cases!=0, 1], 1)
  
  ##  Set up data to smooth ----
  to.smooth <- data300[ frst_n_zero:nrow(data300) , ]
  colnames(to.smooth)[1]<- "day"
  to.smooth$date <- as.Date(to.smooth$date)
  
  # Mono-smoothing with scam ----
  b1 <- scam(p_cases ~ s(day, k = 25, bs="mpi",m=2),
             family=gaussian(link="identity"), data=to.smooth)
  
  b2 <- scam(p_cases_low ~ s(day, k = 25, bs="mpi",m=2),
             family=gaussian(link="identity"), data=to.smooth)
  
  b3 <- scam(p_cases_high ~ s(day, k = 25, bs="mpi",m=2),
             family=gaussian(link="identity"), data=to.smooth)
  
  to.smooth[, 'p_cases_smooth'] <- b1$fitted.values
  to.smooth[, 'p_cases_smooth_low'] <- b2$fitted.values
  to.smooth[, 'p_cases_smooth_high'] <- b3$fitted.values
  
  ## Savings ----
  data300 <- data300[, -1]
  data300[frst_n_zero:nrow(data300), 
          c('p_cases_smooth', 
            'p_cases_smooth_low', 
            'p_cases_smooth_high')] <- to.smooth[ ,c('p_cases_smooth', 
                                                     'p_cases_smooth_low', 
                                                     'p_cases_smooth_high')]
  write.csv(data300,
            paste0("../data/estimates-300responses/PlotData/", all.files[k]))
  
  ## Plots ----
  if (to_plot) {
    p <- to.smooth %>% 
      plot_ly(x = ~date, y = ~p_cases, type = 'scatter', mode = 'markers', 
              name = 'Estimated p') %>% 
      add_trace(x = ~date, y = ~p_cases_s, type = 'scatter', mode = 'lines', 
                name = 'Smooth p') %>% 
      add_trace( x = ~date, y = ~p_cases_s_high, type = "scatter" , mode = "lines",
                 line = list(color = 'transparent'),
                 showlegend = FALSE, name = 'High')  %>%
      add_trace(x = ~date, y = ~p_cases_s_low, type = 'scatter', mode = 'lines',
                fill = 'tonexty', line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Low') %>% 
      layout(title = substr(all.files[k], 1, 2))
    print(p)
  } # end-if-plot-smoothed-data
  
} # end-for
