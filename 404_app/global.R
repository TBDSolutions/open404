# global.R #








#### Load packages ####

library(shiny); library(shinythemes);library(shinydashboard)
library(plotly);library(scales)
library(tidyverse);library(magrittr);library(arrow)

#### Read datasets ####


data404 <- 
  arrow::read_feather("../data/clean/df_404.feather") %>%
  mutate(
    code_mod = paste0(code,replace_na(modifier,replace = ""))
  ) %>%
  mutate_at(
    vars(pihp_name,cmhsp,code,code_mod),
    list(~as.factor(.))
  )

service_groups <- arrow::read_feather("../data/clean/svc_grps.feather")

#### Formatting Variables ####

data404 %<>%
  mutate(
    code_shortDesc = as.factor(paste(data404$short_desc," (",(data404$code),")")),
    codeM_shortDesc = as.factor(paste(data404$short_desc," (",(data404$code_mod),")"))
  )

#### Function to summarize data 

agg_404 <- function(
  df,
  # Apply filters:
  metric_filter ,
  pop_filter = levels(data404$population),
  code_filter = levels(data404$code),
  fy_filter = levels(data404$fy),
  # Select grouping vars:
  group_org = quo(pihp_name), # Options: cmhsp, pihp_name, state
  group_svc = quo(svc_grp),   # Options: svc_grp, code, code_mod
  group_pop = quo(combined_pop)
  
) {
  
  
#  return(list(svc_grp1 = reactive({input$service_lvl})))

  df1<-df %>%
    select(fy,pihp,pihp_name,cmhsp,population,
              svc_type,svc_grp,code,short_desc,modifier,
              unit_type,unit_hrs,cases,cost,units)%>%
    filter(
      population %in% pop_filter,
      code %in% code_filter,
      fy %in% fy_filter
    ) %>%
    mutate(
      combined_pop = paste(pop_filter,collapse = ", "),
      state = "Michigan"
    )%>%
    # Rename vars for consistent output
    rename(
      org_group = !!group_org,
      svc_group = !!group_svc,
      pop_group = !!group_pop
    ) %>%
    group_by(
      # Force grouping by:
      fy,org_group,svc_group,pop_group
    ) %>%
    summarise_at(
      vars(cases,units,cost),
      list(~sum(., na.rm = T))
    ) %>%
    # Derive ratio variables at grouped level
    mutate(
      cost_per_case = round(cost/cases,digits = 2),
      cost_per_unit = round(cost/units,digits = 2),
      unit_per_case = round(units/cases,digits = 1)
    ) %>%
    # Calculate service cost as pct of org/pop/year
    group_by(fy,org_group,pop_group) %>%
    mutate(
      cost_pct_tot = round(cost / sum(cost) * 100, digits = 1)
    )%>%
    # Keep refs for selected grouping vars
    mutate(
      svc_group_var = quo_name(group_svc),
      org_group_var = quo_name(group_org),
      pop_group_var = quo_name(group_pop)
    )
  
  
  # test<-reactive({group_svc})
  # 
  # 
  # 
  # 
  # checkSel<-function(x){
  #   if( inputs)
  #     
  #     if(test == svc_grp){ 
  #       
  #       colnames<-x%>%
  #         select(-cases)%>%
  #         names()
  #     }else{
  #       
  #       colnames<-x%>%
  #         names()
  #     }
  #   
  # }    
  # 
  #  df<- df1%>%
  #      select_(.dots = checkSel(df1))

  
}

#### Defining variable inputs ####
# 
# inputs <- 
#   data.frame(
#     cases = integer(),
#     units = integer(),
#     cost = integer(),
#     cost_per_case = integer(),
#     cost_per_unit = integer(),
#     unit_per_case = integer(),
#     cost_pct_tot = integer(),
#     cost_1k_served = integer(),
#     pct_cmh_served = integer()
#   ) %>%
#   rename(
#     "Total Cases" = cases,
#     "Total Units" = units,
#     "Total Cost" = cost,
#     "Cost Per Case" = cost_per_case,
#     "Cost Per Unit" = cost_per_unit,
#     "Total Unit Per Case" = unit_per_case,
#     "Cost per 1K Served" = cost_1k_served,
#     "Percent of Total $" = cost_pct_tot,
#     "Percent Served" = pct_cmh_served
#   )
# 
# inputs_sub <- 
#   data.frame(
#     cost = integer(),
#     cost_pct_tot = integer(),
#     cost_1k_served = integer()
#   ) %>%
#   rename(
#     "Total Cost" = cost,
#     "Cost per 1K Served" = cost_1k_served,
#     "Percent of Total $" = cost_pct_tot
#   )

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
