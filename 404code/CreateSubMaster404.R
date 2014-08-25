# Load Master from GitHub (for those who don't already have it in the workspace)
  Master <- read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/Master', sep=',', header=TRUE)

# Install.packages('plyr' and 'dplyr')
  library(plyr)
  library(dplyr)

## Create subMaster dataframe, excluding services with 0 cases, units, and cost.
  subMaster <- data.frame(subset(Master, SumOfCases != 0 | SumOfUnits != 0 | SumOfCost != 0, select = c(2:19)))

# Add Cost_Perc_Tot (Annual Line Item Cost as % of Total Cost, per CMHSP/Population)
# Add Unit_Perc_Tot (Annual Line Item Units as % of Total Units, per CMHSP/Population)
  source('function_addPercentTotal.R')
  subMaster <- addPercentTotal()
  # Round them and convert to percentage (*100) so they're prettier
    subMaster$Unit_Perc_Tot <- subMaster$Unit_Perc_Tot*100
    subMaster$Cost_Perc_Tot <- subMaster$Cost_Perc_Tot*100
    subMaster$Unit_Perc_Tot <- round(subMaster$Unit_Perc_Tot, digits = 1)
    subMaster$Cost_Perc_Tot <- round(subMaster$Cost_Perc_Tot, digits = 1)
  
# Read in unique counts of people served from 2006-2013
  unique_06to13 <- read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/TotalServed_FY06-13.csv")
  # unique_06to13$TotalServed <- as.integer(unique_06to13$TotalServed)

# Create unique keys...
  subMaster$Key <- paste(subMaster$FY,subMaster$CMHSP, sep = "", collapse = NULL)
  unique_06to13$Key <- paste(unique_06to13$FY,unique_06to13$CMHSP, sep = "", collapse = NULL)

# ...and merge
  subMaster <- merge(subMaster, unique_06to13, by.x = "Key", by.y = "Key")

# Calculate new variables 
# Cost per 1,000 people served by the region
  subMaster$Cost1kSvd <- round((subMaster$SumOfCost/subMaster$TotalServed)*1000, digits = 2)

# Units per 1,000 people served by the region
  subMaster$Unit1kSvd <- round((subMaster$SumOfUnits/subMaster$TotalServed)*1000, digits = 1)

# Percentage of people served receiving this service
  subMaster$Perc_Svd <- round((subMaster$SumOfCases/subMaster$TotalServed)*100, digits = 1)

# Clean up unnecessary variables left from merge
  subMaster$FY.y <- NULL
  subMaster$FY <- subMaster$FY.x
  subMaster$FY.x <- NULL
  subMaster$CMHSP.y <- NULL
  subMaster$CMHSP <- subMaster$CMHSP.x
  subMaster$CMHSP.x <- NULL
  subMaster$TotalServed <- NULL   # rules of tidy data require excluding this
  subMaster$Key <- NULL
  
# Reordering the columns
  subMaster<-subMaster[c(22,1:2,23,3:21)]

# Output subMaster .csv file
  write.csv(subMaster,
            file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\subMaster",
            row.names = FALSE)