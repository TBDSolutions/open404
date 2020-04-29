## Currently, the cost reports are separated based on year and population
## To have a meaningful master dataset, first have to compile and format each individual dataset

library(tidyverse)

# Function to read section 404 data from .csv format

read404 <- function(path, fy, pop = c("DD", "MIA", "MIC")) {
  
  df <- 
    read_csv(
      path, 
      col_names = c(
        "CMHSP","FirstofService.Description","FirstOfRevenue.Code","FirstOfHCPCS.Code","FirstOfModifier",
        "UnitType","SumOfCases","SumOfUnits","SumOfCost","SumOfOtherCost"
      )
    ) %>%
    slice(-1) # remove former header column
  
  df <-
    df %>%
    # Replace NAs with name CMHSP
    fill(CMHSP, .direction = "down") %>%
    mutate(
      FY = factor(fy),
      Population = factor(pop), 
      FirstOfRevenue.Code = as.character(FirstOfRevenue.Code),
      FirstOfHCPCS.Code = as.character(FirstOfHCPCS.Code),
      SumOfCost = gsub(",", "", SumOfCost),
      SumOfCost = gsub("\\..*", "", SumOfCost),
      SumOfCost = as.numeric(gsub("[[:punct:]]", "", SumOfCost)),
      SumOfCases = as.numeric(gsub(",", "", SumOfCases)),
      SumOfCases = as.numeric(gsub("\\..*", "", SumOfCases)),
      SumOfUnits = as.numeric(gsub(",", "", SumOfUnits)),
      SumOfUnits = as.numeric(gsub("\\..*", "", SumOfUnits))
    ) %>%
    filter(is.na(FirstofService.Description) == F) %>%
    mutate(
      CostPerCase = round(SumOfCost/SumOfCases, digits = 2),
      CostPerUnit = round(SumOfCost/SumOfUnits, digits = 2),
      UnitPerCase = round(SumOfUnits/SumOfCases, digits = 1)
    ) %>%
    select(
      CMHSP,FY,Population,FirstofService.Description,
      FirstOfRevenue.Code,FirstOfHCPCS.Code,FirstOfModifier,
      UnitType,SumOfCases,SumOfUnits,SumOfCost,
      CostPerCase,CostPerUnit,UnitPerCase
    )
  
  return(df)
  
} 

# Function to read and combine all .csv files in a directory
# Assumes files with naming convention "CMH-DD_FY2006.csv"

combine404 <- function(directory) {
  
  ## 'directory' is a char vector of len 1 indicating location of CSV files
  
  files <- list.files(directory, full.names = TRUE) # make list of full file names
  n <- length(files)
  df <- tibble() #create empty data frame
  
  for (i in 1:n) {
    # loop through files, rbinding them together
    
    fy <- str_sub(files[i], start = -8L, end = -5L) # extract 4 chars for FY
    pop <- str_sub(files[i], start = -14L, end = -12L) # extract 3 chars for pop
    pop <- ifelse(pop == "-DD", yes = "DD", no = pop)
    
    x <- read404(path = files[i], fy = fy, pop = pop)
    
    print(paste0(length(colnames(x)), " columns: ", fy,"_",pop))
    ## For use in diagnosing issues:
    # print(paste0("Column names: ", colnames(x)))
    
    df <- rbind(df, x)
    
  }
  
  return(df)
  
}

clean404 <- function(df) {
  
  df <- 
    df %>%
    rename_all(list(~str_to_lower(.))) %>%
    rename_all(list(~str_replace(.,"firstof",""))) %>%
    rename_all(list(~str_replace(.,"sumof",""))) %>%
    rename_all(list(~str_replace(.,"\\.","_"))) %>%
    rename_all(list(~str_replace(.,"per","_per_")))%>%
    select(-unittype) %>%
    # Identify IBNR records
    mutate(ibnr_record = if_else(cases == 0 & units == 0 & cost > 0, 1, 0)) %>%
    filter(
      cases > 0 | units > 0 | cost > 0, # Must be used # 99,175
      ibnr_record != 1 # Exclude IBNR records (Inpatient Psychiatric) # 98,555
    ) %>%
    mutate(
      hcpcs_code = str_replace(hcpcs_code,"[[:punct:]].*",""),
      hcpcs_code = case_when(
        hcpcs_code == "00104" ~ "104", 
        hcpcs_code == "ALL"   ~ "Jxxxx",
        is.na(hcpcs_code) & str_detect(service_description,"^Peer") ~ "prxxx",
        is.na(hcpcs_code) & str_detect(service_description,"^Pharmacy") ~ "phxxx",
        is.na(hcpcs_code) & service_description == "Other" ~ "xxxxx",
        TRUE ~ hcpcs_code
      ),
      modifier = case_when(
        # Make services with separate SUD reporting identifiable via modifier
        str_detect(tolower(service_description), "^substance abuse") ~ "SUD",
        TRUE ~ modifier
      ),
      revenue_code = case_when(
        revenue_code == "0114, 0124, 0134, 0154" ~ "01X4", # format Psychiatric Inpatient post-FY15
        TRUE ~ revenue_code
      ),
      revenue_code = str_replace(revenue_code,"[[:punct:]].*",""),
      revenue_code = case_when(
        # Make 4 digit revenue codes into 3 digits
        str_length(revenue_code) == 4 & str_detect(revenue_code,"^0") ~ str_sub(revenue_code,2,4),
        TRUE ~ revenue_code
      ),
      code = case_when(
        modifier == "PT22"   ~ "PT22",
        modifier == "PT65"   ~ "PT65",
        modifier == "PT68" & 
          fy %in% c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015") ~ "PT68",
        modifier == "PT73" &
          fy %in% c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015") ~ "PT73",
        !is.na(hcpcs_code)   ~ hcpcs_code,
        !is.na(revenue_code) ~ revenue_code,
        !is.na(modifier) ~ modifier
      ),
      # Remove pesky carriage returns
      code = str_replace_all(code,"\n|\r","")
    ) %>%
    ## Standardize CMHSP names
    mutate(
      cmhsp = recode(
        cmhsp,
        `LifeWays` = 'Lifeways',
        `Manistee-Benzie (Centra Wellness)` = 'Manistee-Benzie',
        `Muskegon (HealthWest)` = 'Muskegon'
      )
    ) %>%
    select(-ibnr_record)
  
  return(df)
  
}
