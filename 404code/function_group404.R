# Takes as arg a df that has been processed by "function_read404.R"

library(tidyverse)
groups <- arrow::read_feather("data/clean/svc_grps.feather")
cmhsps <- read_csv("data/clean/cmh_ref.csv")

group404 <- function(df) {
  
  df <-
    df %>%
    select(-hcpcs_code,-revenue_code) %>%
    left_join(groups %>% select(-long_desc), by = "code") %>%
    # Adding PIHP regions to the dataset
    left_join(cmhsps, by = "cmhsp") %>%
    select(
      fy,cmhsp,pihp,pihp_name,
      population,code,modifier,
      svc_type:short_desc,
      orig_desc = service_description,
      cases:unit_per_case
    ) %>%
    group_by(
      fy,pihp,pihp_name,cmhsp,population,
      svc_type,svc_grp,code,short_desc, modifier,
      unit_type,unit_hrs
    ) %>%
    summarize(
      cases = sum(cases),
      units = sum(units),
      cost  = sum(cost),
      n_row = n_distinct(orig_desc)
    ) %>%
    mutate(
      cost_per_case = round(cost/cases, digits = 2),
      cost_per_unit = round(cost/units, digits = 2),
      unit_per_case = round(units/cases, digits = 1)
    ) %>%
    group_by(fy,cmhsp) %>%
    mutate(
      # Calc costs for each code/mod as percent of CMHSP annual
      # Note: May not add to 100 due to rounding
      cost_pct_tot = round(cost/sum(cost, na.rm = T) * 100, digits = 1)
    ) %>%
    ungroup()
  
  return(df)
  
}