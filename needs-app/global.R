## global.R ##

library(dplyr)
library(tidyr)
library(rcdimple)
library(DT)
library(networkD3)
library(visNetwork)
library(shinydashboard)

# Load de-identified data

needs <- read.csv("data/needs.csv")

# Summarize to create measures

need_metrics <-
  needs %>%
  select(FY,PIHPname,CMHSP,Name,Population,People) %>%
  group_by(FY,PIHPname, CMHSP,Population) %>%
  spread(Name,People) %>%
  ungroup() %>%
  mutate(throughput = round((eligible - waiting)/total_in * 100, digits = 1),
         in_nonMH = round(out_nonMH/total_in*100, digits = 1),
         drop_out = round(no_elig_deter/assmt_sched*100, digits = 1),
         assess_elig = round(eligible/(assmt_sched - no_elig_deter)*100, digits = 1),
         in_req = round(req_CMHsvc/total_in*100, digits = 1),
         req_screenout = round(screened_out/req_CMHsvc*100, digits = 1),
         refer_MHP = round(rfr_to_MHP/(assmt_sched - no_elig_deter)*100, digits = 1),
         refer_FFS = round(rfr_to_FFS/(assmt_sched - no_elig_deter)*100, digits = 1),
         inelig_rfrMH = round(rfr_to_mh_Y/not_eligible*100, digits = 1),
         elig_urg_imm = round((urgent_crit + immed_crit) / eligible * 100, digits = 1),
         some_wait = round(some_wait / waiting * 100, digits = 1),
         all_wait = round(all_wait / waiting * 100, digits = 1),
         elig_wait = round(waiting / eligible * 100, digits = 1)
  ) %>%
  select(FY:Population, throughput:elig_wait) %>%
  group_by(FY,PIHPname,CMHSP,Population) %>%
  gather(Measure,Score, throughput:elig_wait) %>%
  mutate(MeasureDesc = recode(Measure,
                              throughput = "Overall Access",
                              in_nonMH = "% total requesting non-CMH services",
                              drop_out = "Assessment Drop-out Rate",
                              assess_elig = "% Assessed Eligible",
                              in_req = "% total requesting CMH services",
                              req_screenout = "% Screened Out",
                              refer_MHP = "% Referred to MHP",
                              refer_FFS = "% Referred to FFS",
                              inelig_rfrMH = "% Referred for External MH Svs",
                              elig_urg_imm = "% Meeting Acute Criteria",
                              some_wait = "% of waitlist with partial service",
                              all_wait = "% of waitlist with partial service",
                              elig_wait = "% of eligibles on waitlist"))
