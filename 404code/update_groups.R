
# update_groups.R

# Each year, new service codes and revenue codes may be added
# and these need to be appended to the service groupings

library(tidyverse); library(lubridate)

# So we start with the existing groupings

groups <- read_csv("data/clean/svc_grps.csv")
# groups <- arrow::read_feather("data/clean/svc_grps.feather")

# Load reference data sets to map codes to descriptions

# These are drawn from the Medicare Coverage Database
# filtering for "Current and Retired LCDs"
# and may not contain all Medicaid codes
# URL: "https://www.cms.gov/medicare-coverage-database/downloads/downloadable-databases.aspx"

# Using the 'lcd_x_hcpc_code.csv' file

hcpc_ref <- 
  read_csv("data/hcpc_code_lookup.csv") %>%
  group_by(hcpc_code_id) %>%
  filter(hcpc_code_version == max(hcpc_code_version)) %>%
  select(
    code = hcpc_code_id, 
    short_desc = short_description, 
    long_desc = long_description
  ) %>%
  distinct(code, .keep_all = T)

# Using the 'lcd_x_revenue code.csv' file

rev_ref <-
  read_csv("data/rev_code_lookup.csv") %>%
  group_by(revenue_code_id) %>%
  filter(revenue_code_version == max(revenue_code_version)) %>%
  select(
    code = revenue_code_id, 
    rev_desc = description
  ) %>%
  distinct(code, .keep_all = T) %>%
  ungroup() %>%
  mutate(code = as.character(code))
  
ref_404 <-
  df_404 %>%
  group_by(code,service_description) %>%
  summarize() %>%
  group_by(code) %>%
  filter(n_distinct(service_description) == 1) %>%
  rename(desc_404 = service_description) 

# Now test whether any codes are missing

tst <-
df_404 %>%
  group_by(fy,code) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  # Allow view across years
  spread(fy,n) %>%
  left_join(groups, by = "code") %>%
  # Missing group or code is all letters
  filter(is.na(svc_grp) | str_detect(code,"^[A-Za-z]+$")) %>%
  # Don't worry about codes previously mapped
  filter(!str_detect(code,"xxx$"))

print(paste("There are ", nrow(tst), " codes requiring review"))

# Resolve these in .csv file, test, and then write final 
# as both .csv and .feather files

arrow::write_feather(groups, "data/clean/svc_grps.feather")
write_csv(groups,"data/clean/svc_grps.csv")

rm(list = c("ref_404", "hcpc_ref", "rev_ref", "tst"))

# library(DBI); library(odbc);library(dbplyr);
# # Requires that user has a system DSN using Windows authentication for each DB names below, with same names
# ref_tables_bh_db <- DBI::dbConnect(odbc::odbc(), "ref_tables_bh")
