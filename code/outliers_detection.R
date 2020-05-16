
clean_outliers <- function(dt, max_ratio){
  dt <- dt[!is.na(dt$reach),] # remove NAs
  # remove outliers from reach column
  reach_cutoff <- boxplot.stats(dt$reach)$stats[5] # changed cutoff to upper fence
  dt <- dt[dt$reach <= reach_cutoff, ]
  
  # remove outliers based on max ratio of   0.3
  dt$ratio <- dt$cases/dt$reach
  dt <- dt[is.finite(dt$ratio), ]  # discard cases with zero reach
  dt <- dt[dt$ratio <= max_ratio, ]
  return(dt)
}

