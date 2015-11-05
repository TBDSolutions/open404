
library(dplyr)
library(tidyr)
library(ggvis)

# Source function from GitHub
source("https://raw.githubusercontent.com/j-hagedorn/open404/master/404code/function_combineNeeds_v2.R")

needs <- combineNeeds(directory = "C:/Users/Josh/SkyDrive/Projects/NeedsAssessment/data")


# Recode years
  # levels(needs$FY)
  needs$FY <- car::recode(needs$FY, "'012*' = '2011';'2010' = '2011';'2010' = '2011'")
  needs$FY <- as.factor(needs$FY)

# Mess of regex to decrease variation in user-entered fields
  needs$CMHSP <- tolower(needs$CMHSP)
  needs$CMHSP <- gsub(" ", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("cmhsp", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("behavioral", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("health", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("cmh", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("community", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("mental", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("county", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("cmhs", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("cmh", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub(".", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("-", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("services", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("system", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("authority", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("network", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub(":", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("/", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("-", "", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("&", "and", needs$CMHSP, fixed = T)
  needs$CMHSP <- gsub("bhs", "", needs$CMHSP, fixed = T)
  
  

# Map codes

  needs$CMHSP <- car::recode(needs$CMHSP, 
                             "'allegan' = 'Allegan';
                             'ausablevalley' = 'AuSable Valley';
                             'ausablevalleycmh' = 'AuSable Valley';
                             'av' = 'AuSable Valley';
                             'ausablevalley' = 'AuSable Valley';
                             'barry' = 'Barry';
                             'bayarenac' = 'Bay-Arenac';
                             'berrien' = 'Berrien';
                             'bca' = 'Barry';
                             'cei' = 'Clinton Eaton Ingham';
                             'acei' = 'Clinton Eaton Ingham';
                             'forcentralmichigan' = 'CMH for Central Michigan';
                             'cm' = 'CMH for Central Michigan';
                             'coppercountry' = 'Copper Country';
                             'detroitwayneagencyjacquelynsummerlin' = 'Detroit-Wayne';
                             'detroitwayne' = 'Detroit-Wayne';
                             'genesee' = 'Genesee';
                             'gogebic' = 'Gogebic';
                             'gogebica' = 'Gogebic';
                             'gratiot' = 'Gratiot';
                             'hiawatha' = 'Hiawatha';
                             'huron' = 'Huron';
                             'ionia' = 'Ionia';
                             'kalamazooandsubstanceabuse' = 'Kalamazoo';
                             'kalamazooandsubabuse' = 'Kalamazoo';
                             'ksas' = 'Kalamazoo';
                             'lapeer' = 'Lapeer';
                             'lapeerco' = 'Lapeer';
                             'lenawee' = 'Lenawee';
                             'lifeways' = 'Lifeways';
                             'livingston' = 'Livingston';
                             'livingstona' = 'Livingston';
                             'macomb' = 'Macomb';
                             'macombco' = 'Macomb';
                             'mcaccesscenteronly' = 'Macomb';
                             'centrawellness' = 'Manistee-Benzie';
                             'centrawellnessmb' = 'Manistee-Benzie';
                             'centrawellnessakamanisteebenzie' = 'Manistee-Benzie';
                             'monroe' = 'Monroe';
                             'montcalmcenterfor' = 'Montcalm';
                             'muskegon' = 'Muskegon';
                             '180' = 'Network180';
                             'newaygo' = 'Newaygo';
                             'nc' = 'North Country';
                             'northcountry' = 'North Country';
                             'northcountrycmh' = 'North Country';
                             'northeastmichigan' = 'Northeast Michigan';
                             'northernlakes' = 'Northern Lakes';
                             'northpointe' = 'Northpointe';
                             'n' = 'Northpointe';
                             'oakland' = 'Oakland';
                             'ottawa' = 'Ottawa';
                             'pathways' = 'Pathways';
                             'pines' = 'Pines';
                             'saginaw' = 'Saginaw';
                             'saginawmh' = 'Saginaw';
                             'sanilac' = 'Sanilac';
                             'sanilaca' = 'Sanilac';
                             'shiawassee' = 'Shiawassee';
                             'stclair' = 'St. Clair';
                             'sassjc' = 'St. Joseph';
                             'stjoseph' = 'St. Joseph';
                             'summitpointe' = 'Summit Pointe';
                             'tuscolahelath' = 'Tuscola';
                             'tuscolas' = 'Tuscola';
                             'vanburen' = 'Van Buren';
                             'vanburena' = 'Van Buren';
                             'washtenaw' = 'Washtenaw';
                             'washtenaworganization' = 'Washtenaw';
                             'washtenawwcho' = 'Washtenaw';
                             'westmichigan' = 'West Michigan';
                             'westmichigans' = 'West Michigan';
                             'cass' = 'Woodlands';
                             'cassdbawoodlands' = 'Woodlands';
                             'cassmrntal' = 'Woodlands'")


# droplevels(needs)
# levels(as.factor(needs$CMHSP))
 
# tst <-
# needs %>%
#   group_by(CMHSP, FY) %>%
#   summarise(n = n()) %>%
#   group_by(CMHSP) %>%
#   summarise(n = n())

  needs$PIHP<-recode(needs$CMHSP, "'Copper Country'='1';
                        'Network180'='3'; 
                     'Gogebic'='1';
                     'Hiawatha'='1';
                     'Northpointe'='1'; 
                     'Pathways'='1';
                     'AuSable Valley'='2';
                     'Manistee-Benzie'='2';
                     'North Country'='2';
                     'Northeast Michigan'='2';
                     'Northern Lakes'='2';
                     'Allegan'='3';
                     'Muskegon'='3';
                     'Network180'='3';
                     'Ottawa'='3';
                     'West Michigan'='3';
                     'Barry'='4';
                     'Berrien'='4';
                     'Kalamazoo'='4';
                     'Pines'='4';
                     'St. Joseph'='4';
                     'Summit Pointe'='4';
                     'Van Buren'='4';
                     'Woodlands'='4';
                     'Bay-Arenac'='5';
                     'Clinton Eaton Ingham'='5';
                     'CMH for Central Michigan'='5';
                     'Gratiot'='5';
                     'Huron'='5';
                     'Ionia'='5';
                     'Lifeways'='5';
                     'Montcalm'='5';
                     'Newaygo'='5';
                     'Saginaw'='5';
                     'Shiawassee'='5';
                     'Tuscola'='5';
                     'Lenawee'='6';
                     'Livingston'='6';
                     'Monroe'='6';
                     'Washtenaw'='6';
                     'Detroit-Wayne'='7';
                     'Oakland'='8';
                     'Macomb'='9';
                     'Genesee'='10';
                     'Lapeer'='10';
                     'Sanilac'='10';
                     'St. Clair'='10'")
  
  needs$PIHP <- as.factor(needs$PIHP)
  
  needs$PIHPname<-recode(needs$PIHP, "'1'='Northcare';
                         '2'='NMRE';
                         '3'='LRP';
                         '4'='SWMBH';
                         '5'='MSHN'; 
                         '6'='CMHPSM';
                         '7'='DWMHA';
                         '8'='OCCMHA';
                         '9'='MCMHS';
                         '10'='Region10'")
  
  needs$PIHPname <- as.factor(needs$PIHPname)
  needs$CMHSP <- as.factor(needs$CMHSP)
  
  needs$People <- as.integer(needs$People)
  
# Order factor for phase
library(gdata)
needs$Phase <- reorder(needs$Phase, new.order=c("Start","Entry","Screening","Eligibility","Waiting"))

needs <- needs %>% select(FY,PIHP:PIHPname,CMHSP:Undup)


needphase <-
  needs %>%
  tbl_df() %>% ungroup() %>%
  filter(Name %in% c('total_in','req_CMHsvc','assmt_sched','eligible','waiting')) %>%
  droplevels %>%
  group_by(PIHPname,CMHSP,FY,Population,Phase) %>%
  summarise(People = sum(People)) %>%
  ungroup() %>%
  spread(Phase,People) %>%
  mutate(Placement = Eligibility - Waiting) %>%
  gather(Phase,People, Start:Placement) %>%
  filter(Phase != "Waiting") %>%
  group_by(FY,PIHPname,CMHSP,Population,Phase) %>%
  summarise(People = sum(People)) %>%
  mutate(CMHpop = paste(CMHSP, Population, ""),
         Change = round((People - lag(People))
                        /(lag(People)/100), digits = 1),
         Running = round(People
                         /(first(People)/100), digits = 1))

needphaseCMH <-
  needs %>%
  tbl_df() %>%
  filter(Name %in% c('total_in','req_CMHsvc','assmt_sched','eligible','waiting')
         & Population != "Other") %>%
  droplevels %>%
  group_by(FY,PIHPname,CMHSP,Population,Phase) %>%
  summarise(People = sum(People)) %>%
  ungroup() %>%
  spread(Phase,People) %>%
  mutate(Placement = Eligibility - Waiting) %>%
  gather(Phase,People, Start:Placement) %>%
  filter(Phase != "Waiting") %>%
  group_by(FY,PIHPname,CMHSP,Phase) %>%
  summarise(People = sum(People)) %>%
  mutate(Change = round((People - lag(People))
                        /(lag(People)/100), digits = 1),
         Running = round(People
                         /(first(People)/100), digits = 1))


needspread <-
  needs %>%
  select(FY,CMHSP,Name,Population,People) %>%
  group_by(FY,CMHSP,Population) %>%
  spread(Name,People) %>%
  group_by(FY,CMHSP,Population) %>%
  summarize(throughput = round((sum(eligible)-sum(waiting))/sum(total_in)*100, digits = 1),
            in_nonMH = round(sum(out_nonMH)/sum(total_in)*100, digits = 1),
            drop_out = round(sum(no_elig_deter)/sum(assmt_sched)*100, digits = 1),
            assess_elig = round(sum(eligible)
                                /(sum(assmt_sched) 
                                  - sum(no_elig_deter))*100, digits = 1),
            in_req = round(sum(req_CMHsvc)/sum(total_in)*100, digits = 1),
            req_screenout = round(sum(screened_out)/sum(req_CMHsvc)*100, digits = 1),
            refer_MHP = round(sum(rfr_to_MHP)
                              /(sum(assmt_sched) - sum(no_elig_deter))*100, 
                              digits = 1),
            refer_FFS = round(sum(rfr_to_FFS)
                              /(sum(assmt_sched) - sum(no_elig_deter))*100, 
                              digits = 1),
            inelig_rfrMH = round(sum(rfr_to_mh_Y)/sum(not_eligible)*100, digits = 1),
            elig_urg_imm = round((sum(urgent_crit)+sum(immed_crit))/sum(eligible)*100, digits = 1),
            some_wait = round(sum(some_wait)/sum(waiting)*100, digits = 1),
            all_wait = round(sum(all_wait)/sum(waiting)*100, digits = 1),
            elig_wait = round(sum(waiting)/sum(eligible)*100, digits = 1)) 

# drop_out : 

## NETWORK

need_network <- 
needs %>% 
  filter(Item!="1"&Item!="12a"&Item !="12b"&Item!="14"&Item!="15") %>%
  droplevels() %>%
  select(Phase, to = Name, People) %>%
  mutate(from = car::recode(Phase, 
                            "'Start' = 'total_in';
                            'Entry' = 'total_in';
                            'Screening' = 'req_CMHsvc';
                            'Eligibility' = 'assmt_sched';
                            'Waiting' = 'eligible'")) %>%
  group_by(from,to) %>%
  summarize(people = sum(People, na.rm = T)) %>%
  ungroup()



no_elig_deter/assmt_sched # Potential service need indicated

needprocess <-
  needs
