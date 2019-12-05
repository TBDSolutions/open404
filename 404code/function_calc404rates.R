
# Calculate new variables 
# Cost per 1,000 people served by the CMHSP
# Percentage of people served receiving this service

# Function takes a df with cols "CMHSP" and "FY"

calc404rates <- function(df) {
  
  # Read in unique counts of people served from 2006-2014
  unique <- read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/TotalServedAnnual.csv")
  # unique <- read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/update-2018/data/TotalServedAnnual.csv")
  
  # Create unique keys...
  df$key <- paste(df$fy,df$cmhsp, sep = "", collapse = NULL)
  unique$key <- paste(unique$FY,unique$CMHSP, sep = "", collapse = NULL)
  
  # ...and join
  df <- 
    df %>%
    inner_join(unique %>% select(key,TotalServed), by = "key") %>%
    mutate(
      cost_1k_served = round(cost/TotalServed*1000, digits = 2),
      pct_cmh_served = round(cases/TotalServed*100, digits = 1)
    ) %>%
    select(-key,-TotalServed)
  
  return(df)
  
}
