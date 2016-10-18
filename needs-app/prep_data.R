# Install required packages
  # Define required packages
    list.of.packages <- c("car","dplyr", "rcdimple","DT","tidyr",
                          "networkD3","shinydashboard")
    
  # Check for and install new packages  
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    
    rm(list.of.packages); rm(new.packages)

#### Aggregate data to allow presentation in open app

## Cleaning algorithm available at:
# source("https://raw.githubusercontent.com/j-hagedorn/open404/master/404code/function_combineNeeds_v2.R")

# Create processed datasets
  source("https://raw.githubusercontent.com/j-hagedorn/open404/needs-0-1/404code/clean_needs.R")

# Check completeness
  tst <- needs %>% group_by(FY,CMHSP) %>% summarize(n = n()) %>% group_by(FY) %>% summarize(n = n())
    
# Write to app folder for use in Shiny
  write.csv(needs, "C:/Users/joshh/Documents/GitHub/open404/needs-app/data/needs.csv", row.names = F)

# Process subsets

  needphase <-
    needs %>%
    tbl_df() %>% ungroup() %>%
    filter(Name %in% c('total_in','req_CMHsvc','assmt_sched','eligible','waiting')) %>%
    droplevels %>%
    group_by(PIHPname,CMHSP,FY,Population,Phase) %>%
    summarise(People = sum(People)) %>%
    ungroup() %>%
    spread(Phase,People) %>%
    mutate(Placement = Eligibility - Waiting) %>%
    gather(Phase,People, Start:Placement) %>%
    filter(Phase != "Waiting") %>%
    group_by(FY,PIHPname,CMHSP,Population,Phase) %>%
    summarise(People = sum(People)) %>%
    mutate(CMHpop = paste(CMHSP, Population, ""),
           Change = round((People - lag(People))
                          /(lag(People)/100), digits = 1),
           Running = round(People
                           /(first(People)/100), digits = 1))
  

 
  



