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

# Create ref for CMHs

cmhsps <-
  df %>%
  group_by(cmhsp) %>%
  summarize() %>% ungroup() %>%
  mutate(
    pihp = recode(
      cmhsp, 'Copper Country'='1','Network180'='3',
      'Gogebic'='1','Hiawatha'='1','Northpointe'='1', 
      'Pathways'='1','AuSable Valley'='2',
      'Manistee-Benzie'='2','North Country'='2',
      'Northeast Michigan'='2','Northern Lakes'='2',
      'Allegan'='3','Muskegon'='3','Network180'='3',
      'Ottawa'='3','West Michigan'='3','Barry'='4',
      'Berrien'='4','Kalamazoo'='4','Pines'='4',
      'St. Joseph'='4','Summit Pointe'='4','Van Buren'='4',
      'Woodlands'='4','Bay-Arenac'='5','Clinton Eaton Ingham'='5',
      'CMH for Central Michigan'='5','Gratiot'='5','Huron'='5',
      'Ionia'='5','Lifeways'='5','Montcalm'='5','Newaygo'='5',
      'Saginaw'='5','Shiawassee'='5','Tuscola'='5',
      'Lenawee'='6','Livingston'='6','Monroe'='6',
      'Washtenaw'='6','Detroit-Wayne'='7','Oakland'='8',
      'Macomb'='9','Genesee'='10','Lapeer'='10',
      'Sanilac'='10','St. Clair'='10'
    ),
    pihp_name = recode(
      pihp, '1'='Northcare','2'='NMRE',
      '3'='LRE','4'='SWMBH','5'='MSHN', 
      '6'='CMHPSM','7'='DWMHA','8'='OCCMHA',
      '9'='MCMHS','10'='Region10'
    )
  )

write_csv(cmhsps,"data/clean/cmh_ref.csv")