
library(tidyverse)

# Combine 404 datasets
source("404code/function_read404.R")
df_404 <- combine404("data/raw") %>% clean404()
  
# Test if all codes are mapped. If not, open script, resolve
source("404code/update_groups.R")

# Create groupings
source("404code/function_group404.R")
df_404 <- group404(df_404) %>% select(-n_row)

# Add rates per 1,000 served
source("function_calc404rates.R")
df_404 <- calc404rates(df_404) 

# Add rates per 1,000 population (census)
#   source("function_calcPop.R")
#   tst <- calc404pop(census_key=census_key)
  
# Output .csv file
write_csv(df_404,"data/clean/df_404.csv")
# Output .feather file
arrow::write_feather(df_404,"data/clean/df_404.feather")


