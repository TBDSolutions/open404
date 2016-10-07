# library(plyr)
library(dplyr)
library(tidyr)

# Source function from GitHub
source("https://raw.githubusercontent.com/j-hagedorn/open404/master/404code/function_combineNeeds_v2.R")

#needs <- combineNeeds(directory = "C:/Users/Josh/SkyDrive/Projects/NeedsAssessment/data")
needs <- combineNeeds(directory = "C:/Users/joshh/Documents/GitHub/open404/data/needs")

# Others doing this would have to download .xls from the folder at:
# "https://github.com/j-hagedorn/open404/tree/master/data/needs"

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
  needs$CMHSP <- gsub(",", "", needs$CMHSP, fixed = T)
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
                             'therightdoorforhoperecoveryandwellnessformerlyionia' = 'Ionia';
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
                             'montcalmcare' = 'Montcalm';
                             'muskegon' = 'Muskegon';
                             'west' = 'Muskegon';
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
#   group_by(CMHSP, FY) %>%
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
needs$Phase <- reorder(needs$Phase, new.order=c("Start","Entry","Screening","Eligibility","Placement","Waiting"))

needs <- needs %>% select(FY,PIHP:PIHPname,CMHSP:Undup)

needs$People[is.na(needs$People)] <- 0 # Replace NAs with zeroes

# Make a df p

# tst <- needs %>% select(FY, PIHPname, CMHSP, Population, Phase, Name, People)
# 
# tst <- ddply(tst, 
#              c("FY", "PIHPname", "CMHSP", "Population", "Phase", "Name"), 
#              summarise,
#              n = sum(People),
#              .drop = F
#              )