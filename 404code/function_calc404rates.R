# Calculate new variables 
# Cost per 1,000 people served by the CMHSP
# Percentage of people served receiving this service

# Function takes a df with cols "CMHSP" and "FY"

# Read in unique counts of people served
### add current year Total Served Un-duplicated to TotalServedAnnual.csv
### review formatting of CMHSP names and confirm TotalServed does not include commas
unique <- read.csv("data/TotalServedAnnual.csv")
# unique <- read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/TotalServedAnnual.csv")
# unique <- read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/update-2018/data/TotalServedAnnual.csv")


calc404rates <- function(df) {
  
  # Create unique keys...
  df_404$key <- paste(df_404$fy,df_404$cmhsp, sep = "", collapse = NULL)
  unique$key <- paste(unique$FY,unique$CMHSP, sep = "", collapse = NULL)
  
  # ...and join
  df_404 <- 
    df_404 %>%
    inner_join(unique %>% select(key,TotalServed), by = "key") %>%
    mutate(
      cost_1k_served = round(cost/TotalServed*1000, digits = 2),
      pct_cmh_served = round(cases/TotalServed*100, digits = 1)
    ) %>%
    select(-key,-TotalServed)
  
  return(df)
  
}
