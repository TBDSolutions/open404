# global.R #

#### Load packages ####

library(shiny); library(shinythemes)
library(plotly)
library(tidyverse); library(magrittr)
library(shinydashboard)
library(shinythemes)
library(feather)
library(scales)

#### Read datasets ####

data404 <- read_feather("data/Master.feather")

service_groups <- unique(data404[,c('ServiceType', 'Service', 'short_description', 'Description', 'Code', 'Code_Mod')])

#### Formatting Variables ####

data404 %<>%
  mutate(
    Code = as.factor(data404$Code),
    Code_shortDesc = as.factor(paste(data404$short_description," (",(data404$Code),")")),
    Code_Desc = as.factor(paste(data404$Description," (",(data404$Code),")")),
    CodeM_shortDesc = as.factor(paste(data404$short_description," (",(data404$Code_Mod),")")),
    CodeM_Desc = as.factor(paste(data404$Description," (",(data404$Code_Mod),")"))
  )

#### Defining variable inputs ####

inputs <- 
  data.frame(SumOfCases = integer(),
             SumOfUnits = integer(),
             SumOfCost = integer(),
             CostPerCase = integer(),
             CostPerUnit = integer(),
             UnitPerCase = integer(),
             Cost_Perc_Tot = integer(),
             Cost1kSvd = integer(),
             Perc_Svd = integer()
  ) %>%
  rename(
    "Total Cases" = SumOfCases,
    "Total Units" = SumOfUnits,
    "Total Cost" = SumOfCost,
    "Cost Per Case" = CostPerCase,
    "Cost Per Unit" = CostPerUnit,
    "Total Unit Per Case" = UnitPerCase,
    "Cost per 1K Served" = Cost1kSvd,
    "Percent of Total $" = Cost_Perc_Tot,
    "Percent Served" = Perc_Svd
  )


inputs_sub <- 
  data.frame(SumOfCost = integer(),
             Cost_Perc_Tot = integer(),
             Cost1kSvd = integer()
  ) %>%
  rename(
    "Total Cost" = SumOfCost,
    "Cost per 1K Served" = Cost1kSvd,
    "Percent of Total $" = Cost_Perc_Tot
  )

# https://gist.githubusercontent.com/stla/9033053/raw/9c3ad4ce296397b923c88b40c2d43e4fbea8e4be/ShinyColumnsSelector.R
# https://groups.google.com/forum/#!topic/shiny-discuss/0V4sR4LjAuc

#### Colors ####

# library(RColorBrewer)
# n <- 47
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# cmh_palette = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

library(randomcoloR)
n <- 47
cmh_palette <- distinctColorPalette(n)

# cmh_palette <- c("#a3ab73","#4e64ed","#b6d325","#971da8","#66ad00","#fb79fd",
#                  "#009721","#f22a9b","#9ed75a","#7630a1","#6f9100","#ad7cff",
#                  "#fda812","#0071da","#c69e00","#7a93ff","#8f8e00","#b0008a",
#                  "#86d88f","#ab0066","#009266","#f62f51","#02c6ee","#c8081d",
#                  "#62d8d4","#cf4900","#334da2","#cbcb67","#ff8de3","#50571e",
#                  "#e6a5ff","#a26700","#64467f","#e0c476","#c60052","#ffaa79",
#                  "#ff94cb","#9a4400","#bc7ea0","#982b1e","#ffa6b2","#774723",
#                  "#ff7a6f","#893b26","#c1798a","#a3615b","#9844bd")
