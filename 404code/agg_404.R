
agg_404 <- function(
  df,
  # Apply filters:
  pop_filter = levels(data404$population),
  code_filter = levels(data404$code),
  fy_filter = levels(data404$fy),
  # Select grouping vars:
  group_org = quo(pihp_name), # Options: cmhsp, pihp_name, state
  group_svc = quo(svc_grp),   # Options: svc_grp, code, code_mod
  group_pop = quo(combined_pop)
  
) {
  
  df %>%
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
  
}

# tst <- 
#   data404 %>%
#   agg_404(
#     group_org = quo(state),
#     group_svc = quo(svc_grp)
#   )
  
