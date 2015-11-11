#############################################################
########             open404                        ######### 
########                                            #########
########      Formats multiple years of MDCH CMH    #########
########      Sub-Element Cost Report(404) Data     #########
########                                            #########
########             by J. Hagedorn                 #########
########                                            #########
#############################################################

## Currently, the cost reports are separated based on year and population
## To have a meaningful master dataset, first have to compile and format each individual dataset

# Function to read section 404 data from .csv format

read404 <- function(path, fy, pop = c("DD", "MIA", "MIC")) {
  
  df <- read.csv(path, sep=',', header=TRUE)
  
  df[df == ''] <- NA #change blanks to NA
  
  names(df)[1] <- "CMHSP" # change CMHSP name col
  names(df)[7] <- "SumOfCases" # change SumOfCases name col
  names(df)[8] <- "SumOfUnits" # change CMHSP name col
  names(df)[9] <- "SumOfCost" # change CMHSP name col
  
  library(dplyr)
  library(zoo)
  
  df <-
    df %>%
    mutate(CMHSP = na.locf(CMHSP), # Replace NAs with name CMHSP
           FY = factor(fy),
           Population = factor(pop), 
           FirstOfRevenue.Code = as.character(FirstOfRevenue.Code),
           FirstOfHCPCS.Code = as.character(FirstOfHCPCS.Code),
           SumOfCost = gsub(".00|,", "", SumOfCost),
           SumOfCost = as.numeric(gsub("[[:punct:]]", "", SumOfCost)),
           SumOfCases = as.numeric(gsub(".00|,", "", SumOfCases)),
           SumOfUnits = as.numeric(gsub(".00|,", "", SumOfUnits))
    ) %>%
    filter(is.na(FirstofService.Description) == F) %>%
    select(CMHSP, FY, Population, FirstofService.Description:UnitPerCase)
  
  return(df)
  
} 

# Function to read and combine all .csv files in a directory
# Assumes files with naming convention "CMH-DD_FY2006.csv"

combine404 <- function(directory) {
  
  ## 'directory' is a char vector of len 1 indicating location of CSV files
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  files <- list.files(directory, full.names = TRUE) # make list of full file names
  n <- length(files)
  df <- data.frame() #create empty data frame
  
  for (i in 1:n) {
    # loop through files, rbinding them together
    
    fy <- str_sub(files[i], start = -8L, end = -5L) # extract 4 chars for FY
    pop <- str_sub(files[i], start = -14L, end = -12L) # extract 3 chars for pop
    pop <- ifelse(pop == "-DD", yes = "DD", no = pop)
    
    x <- read404(path = files[i], fy = fy, pop = pop)
    
    df <- rbind(df, x)
    
  }
  
  return(df)
  
}

