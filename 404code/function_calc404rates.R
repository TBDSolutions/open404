
# Calculate new variables 
# Cost per 1,000 people served by the CMHSP
# Percentage of people served receiving this service

# Function takes a df with cols "CMHSP" and "FY"

calc404rates <- function(df) {
  
  # Read in unique counts of people served from 2006-2014
  unique <- read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/TotalServedAnnual.csv")
  # unique <- read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/update-2018/data/TotalServedAnnual.csv")
  
  
  # Create unique keys...
  df$Key <- paste(df$FY,df$CMHSP, sep = "", collapse = NULL)
  unique$Key <- paste(unique$FY,unique$CMHSP, sep = "", collapse = NULL)
  
  # ...and join
  df <- 
    df %>%
    inner_join(unique, by = "Key") %>%
    mutate(Cost1kSvd = round(SumOfCost/TotalServed*1000, digits = 2),
           Perc_Svd = round(SumOfCases/TotalServed*100, digits = 1)
           ) %>%
    rename(FY = FY.x, CMHSP = CMHSP.x) %>%
    select(-Key, -FY.y, -CMHSP.y, -TotalServed)
  
  return(df)
  
}
