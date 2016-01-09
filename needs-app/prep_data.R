
#### Aggregate data to allow presentation in open app

## Cleaning algorithm available at:
# source("https://raw.githubusercontent.com/j-hagedorn/open404/master/404code/function_combineNeeds_v2.R")

# Create processed datasets
  source("https://raw.githubusercontent.com/j-hagedorn/open404/master/404code/clean_needs.R")

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
  

 
  



