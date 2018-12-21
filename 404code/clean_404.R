
library(dplyr)

# Combine 404 datasets
  source("function_read404.R")
  Master <- combine404("../data/raw")

# Clean CPT/HCPCS and apply groupings
  source("function_group404.R")
  Master <- group404(Master)

# Group for consistent factor levels, calc units and cost at Code_Mod level
  
  Master <-
  Master %>%
    group_by(
      FY,PIHPname,PIHP,CMHSP,Population,
      ServiceType,Service,short_description,Description,Code,Code_Mod
    ) %>%
    summarize(
      Unit_Hours = max(UnitHours),
      SumOfCases = sum(SumOfCases),
      SumOfUnits = sum(SumOfUnits),
      SumOfCost = sum(SumOfCost)
    ) %>%
    mutate(
      CostPerCase = round(SumOfCost / SumOfCases, digits = 2),
      CostPerUnit = round(SumOfCost / SumOfUnits, digits = 2),
      UnitPerCase = round(SumOfUnits / SumOfCases, digits = 1)
    ) %>%
    ungroup()
  
  
# Calculate units and costs for each row (Code_Mod) 
# as a percentage of each CMHSP's annual service use
# Note: May not add to 100 due to rounding
  
  Master <-
  Master %>%
    group_by(FY,CMHSP) %>%
    mutate(
      Unit_Perc_Tot = round(SumOfUnits/sum(SumOfUnits, na.rm = T) * 100, digits = 1), 
      Cost_Perc_Tot = round(SumOfCost/sum(SumOfCost, na.rm = T) * 100, digits = 1)
    ) %>%
    ungroup()

# Add rates per 1,000 served
  source("function_calc404rates.R")
  Master <- calc404rates(df = Master)

# Add rates per 1,000 population (census)
#   source("function_calcPop.R")
#   tst <- calc404pop(census_key=census_key)
  
# Output Master .csv file
  write.csv(Master,"../data/clean/Master.csv", row.names = F)
# Output Master .feather file
  feather::write_feather(Master,"../data/clean/Master.feather")

# Output Service Groups .csv file
  service_groups <- 
    Master %>% 
    group_by(ServiceType,Service,short_description,Description,Code,Code_Mod) %>%
    summarize(n = n())
  
  write.csv(service_groups,"../data/clean/Service_Groups.csv", row.names = F)

