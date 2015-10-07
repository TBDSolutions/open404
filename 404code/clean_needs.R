
source("function_combineNeeds_v2.R")

needs <- combineNeeds(directory = "C:/Users/Josh/SkyDrive/Projects/NeedsAssessment/data")


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
                             'coppercountry' = 'Copper County';
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


droplevels(needs)
levels(as.factor(needs$CMHSP))

tst <-
needs %>%
  group_by(CMHSP, FY) %>%
  summarise(n = n())
