library(readr)
library(readxl)

## Read from Excel and write to *.csv ----
sheets <- excel_sheets("ENECOVID19-tables.xlsx")

for (i in 1:length(sheets)) {
  page_tmp <- read_excel("ENECOVID19-tables.xlsx", 
                         sheet = sheets[i], 
                         col_names = F)
  
  write_csv(page_tmp, path = paste0(sheets[i], ".csv"), na = "NA",  
            append = FALSE, col_names = F)
}

## Read any ----
all.files <- list.files(pattern = ".csv")
all.files
dataset <- read_csv(all.files[5], col_names = T, skip = 1, skip_empty_rows = T)
