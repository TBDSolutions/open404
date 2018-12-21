######################################################################
## Recoding and creating new variables ###############################
######################################################################

# Takes as arg a Master df that has been processed by "function_read404.R"

library(tidyverse)

group404 <- function(Master) {
  # Removing punctuation from factor name in UnitType

  Master <-
    Master %>%
    # Clean UnitType var
    mutate(
      UnitType = str_replace_all(UnitType,"[[:punct:]]",""),
      UnitType = str_squish(UnitType),
      UnitType = str_to_lower(UnitType)
    ) %>%
    mutate(
      # Formatting UnitHours to be quantiative and standardized
      # For time ranges, use highest value; 1.00 = 1 hour
      UnitHours = recode(
        UnitType,
        `encounter facetoface generally less than 10 minutes` = '0.17',
        `15 minutes`         = '0.25',
        `each additional 15 minutes` = '0.25', 
        `up to 15 min`       = '0.25',
        `25 minutes`         = '0.42',
        `30 minutes or less` = '0.50',
        `encounter 2030 min` = '0.50',
        `35 minutes`         = '0.58',
        `encounter session at least 45 min` = '0.75',
        `encounter 4550 min` = '0.83',
        `50 minutes`         = '0.83',
        `hour`               = '1.00',
        `first hour`         = '1.00',
        `per hour`           = '1.00',
        `70 minutes`         = '1.17',
        `encounter 7580 min` = '1.33',
        `encounter trip per session one day partial day` = '8.00',
        `per session one night = one session`            = '8.00',
        `per session one daypartial day = one session`   = '8.00',
        `encounter trip per session one night = one session` = '8.00',
        `encounter trip per session one daypartial day = one session` = '8.00',
        `days`               = '24.00',
        `per diem`           = '24.00',
        `month`              = '720.00',
        `month service`      = '720.00',
        .default = NA_character_
      ),
      UnitHours = as.numeric(UnitHours),
      # Change to standard types
      UnitType = case_when(
        UnitType %in% c(
          "encounter 2030 min","encounter 4550 min","encounter 7580 min",
          "first hour","each additional 15 minutes","15 minutes","30 minutes",
          "30 minutes or less","hour","per diem","minutes","month","up to 15 min",
          "50 minutes","70 minutes","25 minutes","35 minutes","per hour",
          "encounter session at least 45 min","days","40 minutes","80 minutes",
          "20 minutes","55 minutes","110 minutes","first 30 minutes","first 60 minutes",
          "encounter facetoface generally less than 10 minutes","day","45 minutes",
          "60 minutes","first 3074 min","each additional 30 minutes","10 minutes",
          "month service","per date of service","per session one night = one session",
          "per session one daypartial day = one session",
          "encounter trip per session one night = one session",
          "encounter trip per session one daypartial day = one session"
        ) ~ "Time",
        UnitType %in% c(
          "of treatments","of visits","of units","encounter","face to face contact",                                       
          "encounter facetoface","service","per service","direct observation encounter",
          "evaluation","per screen","each procedure"
        ) ~ "Encounter",
        UnitType %in% c(
          "of tests","of items","items","amount","item","per item","per test"
        ) ~ "Item",
        UnitType %in% c(
          "per mile","per oneway trip","encounter trip"
        ) ~ "Trip",
        TRUE ~ NA_character_
      )
    ) %>%
    # Clean CPT / HCPCS codes
    mutate(
      Code = ifelse(
        is.na(FirstOfHCPCS.Code) == T,
        yes = gsub("[[:punct:]].*","",FirstOfRevenue.Code),
        no = gsub("[[:punct:]].*","",FirstOfHCPCS.Code)
      )
    ) %>%
    filter(SumOfCases > 0 | SumOfCases > 0 | SumOfCost > 0) %>%
    mutate(
      Code2 = recode(
        FirstofService.Description,
        `Other` = 'Other',
        `Peer Directed and Operated Support Services` = 'Peer',
        `Pharmacy (Drugs and Other Biologicals)` = 'Pharm'
      ),
      Code = ifelse(is.na(Code) == T,yes = Code2,no = as.character(Code)),
      Code = recode(
        Code, 
        `00104` = '0104', 
        `104` = '0104',
        `102` = '0102',
        `105` = '0105',
        `370` = '0370',
        `450` = '0450',
        `762` = '0762',
        `901` = '0901',
        `90791\n` = '90791',
        `912` = '0912',
        `913` = '0913'
      ),
      Code = as.factor(gsub("[\r\n]","",Code)), # remove line breaks
      # Make new var concat Code / Modifier. Most granular level of service
      Code_Mod = paste0(Code,FirstOfModifier, sep = "", collapse = NULL),
      Code_Mod = as.factor(gsub("NA", "", Code_Mod)) # Remove NA values 
    ) %>%
    select(
      CMHSP:Population, Description = FirstofService.Description,
      Code, Modifier = FirstOfModifier, Code_Mod, UnitType, UnitHours,
      SumOfCases:UnitPerCase
    ) %>%
    droplevels()
  
  
  # Checking to make sure there are no missing HCPCS codes
  sum(is.na(Master$Code)) #Result is 0
  
  #Grouping Service.Description into more general categories (variable named 'Service')
  Master$Service <- car::recode(Master$Code,
                          "'T2025'='Fiscal Intermediary Services';
                          'H2000'='Behavioral Treatment';
                          'H2019'='Behavioral Treatment';
                          '0104'='Ancillary Hospital Services';
                          '00104'='Ancillary Hospital Services';
                          '90870'='Ancillary Hospital Services';
                          '80100'='Laboratory';
                          '80101'='Laboratory';
                          '82075'='Assessment';
                          '90791'='Assessment';
                          '90801'='Assessment';
                          '90802'='Assessment';
                          '90887'='Assessment';
                          '96100'='Assessment';
                          '96101'='Assessment';
                          '96102'='Assessment';
                          '96103'='Assessment';
                          '96105'='Assessment';
                          '96110'='Assessment';
                          '96111'='Assessment';
                          '96115'='Assessment';
                          '96116'='Assessment';
                          '96117'='Assessment';
                          '96118'='Assessment';
                          '96119'='Assessment';
                          '96120'='Assessment';
                          '97802'='Assessment';
                          '97803'='Assessment';
                          'H0001'='Assessment';
                          'H0002'='Assessment';
                          'H0003'='Laboratory';
                          'H0031'='Assessment';
                          'H0048'='Laboratory';
                          'H0049'='Assessment';
                          'T1001'='Assessment';
                          'T1023'='Assessment';
                          'T2011'='Assessment';
                          '90772'='Medication Administration';
                          '90782'='Medication Administration';
                          '90788'='Medication Administration';
                          '90862'='Medication Management';
                          '96372'='Medication Administration';
                          '99211'='Medication Administration';
                          '99506'='Medication Administration';
                          '99605'='Medication Administration';
                          'G0351'='Medication Administration';
                          'H0020'='Pharmaceuticals';
                          'H0033'='Pharmaceuticals';
                          'M0064'='Medication Management';
                          'T1999'='Pharmaceuticals';
                          'H0045'='Respite';
                          'S5150'='Respite';
                          'S5151'='Respite';
                          'S9125'='Respite';
                          'T1005'='Health Services';
                          'T2036'='Respite';
                          'T2037'='Respite';
                          'H0019'='Residential Treatment';
                          'H2016'='Residential Treatment';
                          'S9976'='Residential Treatment';
                          'T1020'='Residential Treatment';
                          'H0022'='Substance Abuse Outpatient';
                          'H0025'='Prevention';
                          'S9482'='Prevention';
                          'T1027'='Prevention';
                          'T2024'='Prevention';
                          '92506'='OT/PT/SLT';
                          '92507'='OT/PT/SLT';
                          '92508'='OT/PT/SLT';
                          '92524'='OT/PT/SLT';
                          '92526'='OT/PT/SLT';
                          '92610'='OT/PT/SLT';
                          '97001'='OT/PT/SLT';
                          '97002'='OT/PT/SLT';
                          '97110'='OT/PT/SLT';
                          '97112'='OT/PT/SLT';
                          '97113'='OT/PT/SLT';
                          '97116'='OT/PT/SLT';
                          '97124'='OT/PT/SLT';
                          '97140'='OT/PT/SLT';
                          '97150'='OT/PT/SLT';
                          '97530'='OT/PT/SLT';
                          '97532'='OT/PT/SLT';
                          '97533'='OT/PT/SLT';
                          '97535'='OT/PT/SLT';
                          '97537'='OT/PT/SLT';
                          '97542'='OT/PT/SLT';
                          '97750'='OT/PT/SLT';
                          '97755'='OT/PT/SLT';
                          '97760'='OT/PT/SLT';
                          '97003'='OT/PT/SLT';
                          '97003/ 97004'='OT/PT/SLT';
                          '97004'='OT/PT/SLT';
                          '97504'='OT/PT/SLT';
                          '97703'='OT/PT/SLT';
                          'G0176'='OT/PT/SLT';
                          'S8990'='OT/PT/SLT';
                          '92626'='OT/PT/SLT';
                          '92627'='OT/PT/SLT';
                          '92630'='OT/PT/SLT';
                          '92633'='OT/PT/SLT';
                          '92521'='OT/PT/SLT';
                          '92522'='OT/PT/SLT';
                          '92523'='OT/PT/SLT';
                          '92607'='OT/PT/SLT';
                          '92608'='OT/PT/SLT';
                          '97161'='OT/PT/SLT';
                          '97162'='OT/PT/SLT';
                          '97163'='OT/PT/SLT';
                          '97164'='OT/PT/SLT';
                          '97165'='OT/PT/SLT';
                          '97166'='OT/PT/SLT';
                          '97167'='OT/PT/SLT';
                          '97168'='OT/PT/SLT';
                          'G0177'='Family Services';
                          'S5110'='Family Services';
                          'S5111'='Family Services';
                          'T1009'='Family Services';
                          'T1015'='Family Services';
                          'H0006'='Case Management';
                          'H0032'='Case Management';
                          'H0039'='Interdisciplinary Team';
                          'H2021'='Interdisciplinary Team';
                          'H2022'='Interdisciplinary Team';
                          'T1016'='Supports Coordination';
                          'T1017'='Case Management';
                          'T2023'='Case Management';
                          'T1007'='Case Management';
                          'H0023'='Peer Services';
                          'H0038'='Peer Services';
                          'H0046'='Peer Services';
                          '118' = 'Peer Services';
                          'Peer' = 'Peer Services';
                          'H2030'='Clubhouse';
                          'T1012'='Peer Services';
                          'A0080'='Transportation';
                          'A0090'='Transportation';
                          'A0100'='Transportation';
                          'A0110'='Transportation';
                          'A0120'='Transportation';
                          'A0130'='Transportation';
                          'A0160'='Transportation';
                          'A0170'='Transportation';
                          'A0425'='Transportation';
                          'A0427'='Transportation';
                          'A0428'='Transportation';
                          'S0209'='Transportation';
                          'S0215'='Transportation';
                          'T2001'='Transportation';
                          'T2002'='Transportation';
                          'T2003'='Transportation';
                          'T2004'='Transportation';
                          'T2005'='Transportation';
                          'A0140'='Transportation';
                          'S5160'='Equipment';
                          'S5161'='Equipment';
                          '97761'='Equipment';
                          '97762'='Equipment';
                          'E1399'='Equipment';
                          'S5165'='Equipment';
                          'S5199'='Equipment';
                          'T2028'='Equipment';
                          'T2029'='Equipment';
                          'T2039'='Equipment';
                          'H2014'='Skill-Building/Non-Vocational';
                          'H2023'='Vocational Services';
                          'T2015'='Vocational Services';
                          '90804'='Substance Abuse Outpatient';
                          '90805'='Substance Abuse Outpatient';
                          '90806'='Substance Abuse Outpatient';
                          '90807'='Substance Abuse Outpatient';
                          '90808'='Substance Abuse Outpatient';
                          '90809'='Substance Abuse Outpatient';
                          '90810'='Outpatient Therapy';
                          '90812'='Outpatient Therapy';
                          '90814'='Outpatient Therapy';
                          '90815'='Outpatient Therapy';
                          '90816'='Outpatient Therapy';
                          '90817'='Outpatient Therapy';
                          '90818'='Outpatient Therapy';
                          '90819'='Outpatient Therapy';
                          '90821'='Outpatient Therapy';
                          '90823'='Outpatient Therapy';
                          '90824'='Outpatient Therapy';
                          '90826'='Outpatient Therapy';
                          '90827'='Outpatient Therapy';
                          '90846'='Outpatient Therapy';
                          '90847'='Outpatient Therapy';
                          '90849'='Outpatient Therapy';
                          '90853'='Outpatient Therapy';
                          '90811'='Outpatient Therapy';
                          '90813'='Outpatient Therapy';
                          '90822'='Outpatient Therapy';
                          '90828'='Outpatient Therapy';
                          '90829'='Outpatient Therapy';
                          '90857'='Outpatient Therapy';
                          'H0004'='Substance Abuse Outpatient';
                          'H0005'='Substance Abuse Outpatient';
                          'H0015'='Substance Abuse Outpatient';
                          'H2027'='Outpatient Therapy';
                          'H2035'='Substance Abuse Outpatient';
                          'H2036'='Substance Abuse Outpatient';
                          'H0010'='Sub-Acute Detoxification';
                          'H0012'='Sub-Acute Detoxification';
                          'H0014'='Sub-Acute Detoxification';
                          'H0018'='Crisis Services';
                          'H2011'='Crisis Services';
                          'H2020'='Crisis Services';
                          'S9484'='Crisis Services';
                          'T2034'='Crisis Services';
                          'H0030'='Crisis Services';
                          'H0036'='Community Living Supports';
                          'H0043'='Community Living Supports';
                          'H2015'='Community Living Supports';
                          'H2033'='Community Living Supports';
                          'S5116'='Community Living Supports';
                          'S5120'='Community Living Supports';
                          'S5140'='Community Living Supports';
                          'S5145'='Community Living Supports';
                          'T2038'='Community Living Supports';
                          'D0150'='Dental';
                          'D0180'='Dental';
                          'D1110'='Dental';
                          'D2330'='Dental';
                          'D2332'='Dental';
                          'D2391'='Dental';
                          'D0220'='Dental'; 
                          'D0230'='Dental'; 
                          'D0274'='Dental';
                          'D2331'='Dental';
                          'D2392'='Dental';
                          'D2393'='Dental';
                          'D2750'='Dental';
                          'D4910'='Dental';
                          'D7210'='Dental';
                          'D7310'='Dental';
                          'D9920'='Dental';
                          'E1340'='Dental';
                          '99224'='Health Services';
                          '99225'='Health Services';
                          '99226'='Health Services';
                          '99271'='Health Services';
                          '99272'='Health Services';
                          '99273'='Health Services';
                          '99275'='Health Services';
                          '97804'='Health Services';
                          '97810'='Health Services';
                          '99203'='Health Services';
                          '99205'='Health Services';
                          '99212'='Health Services';
                          '99213'='Health Services';
                          '99214'='Health Services';
                          '99215'='Health Services';
                          '99221'='Health Services';
                          '99222'='Health Services';
                          '99223'='Health Services';
                          '99231'='Health Services';
                          '99232'='Health Services';
                          '99233'='Health Services';
                          '99241'='Health Services';
                          '99242'='Health Services';
                          '99243'='Health Services';
                          '99244'='Health Services';
                          '99245'='Health Services';
                          '99251'='Health Services';
                          '99252'='Health Services';
                          '99253'='Health Services';
                          '99254'='Health Services';
                          '99255'='Health Services';
                          '99201'='Health Services';
                          '99202'='Health Services';
                          '99204'='Health Services';
                          '99238'='Health Services';
                          '99261'='Health Services';
                          '99262'='Health Services';
                          '99263'='Health Services';
                          '99274'='Health Services';
                          '99305'='Health Services';
                          '99307'='Health Services';
                          '99308'='Health Services';
                          '99309'='Health Services';
                          '99310'='Health Services';
                          'H0034'='Health Services';
                          'S9123'='Health Services';
                          'S9124'='Health Services';
                          'S9445'='Health Services';
                          'S9446'='Health Services';
                          'S9470'='Health Services';
                          'T1000'='Health Services';
                          'T1002'='Health Services';
                          'T1003'='Health Services';
                          '97811'='Health Services';
                          '99304'='Health Services';
                          '99306'='Health Services';
                          'K0739'='Ancillary Hospital Services';
                          'ALL'='Other';
                          '98'='Other';
                          '113' = 'Other';
                          'T5999'='Equipment';
                          '0100'='Inpatient Psychiatric Hospital';
                          '0114'='Inpatient Psychiatric Hospital';
                          '100'='Inpatient Psychiatric Hospital';
                          '39'='Inpatient Psychiatric Hospital';
                          '40'='Inpatient Psychiatric Hospital';
                          '144'='Inpatient Psychiatric Hospital';
                          'Local Psychiatric Hospital/Acute Community'='Inpatient Psychiatric Hospital';
                          'Local Psychiatric Hospital/IMD'='Inpatient Psychiatric Hospital';
                          'Cost Estimates of Current Year IBNR/Accrual Local Psychiatric Hospital/Acute Community'='Inpatient Psychiatric Hospital';
                          'Cost Estimates of Current Year IBNR/Accrual Local Psychiatric Hospital/IMD'='Inpatient Psychiatric Hospital';
                          '0901'='Ancillary Hospital Services';
                          '710'='Ancillary Hospital Services';
                          '0370'='Ancillary Hospital Services';
                          '0450'='Ancillary Hospital Services';
                          '450'='Ancillary Hospital Services';
                          '0300'='Ancillary Hospital Services';
                          '0270'='Ancillary Hospital Services';
                          '0430'='Ancillary Hospital Services';
                          '0250'='Ancillary Hospital Services';
                          '0636'='Ancillary Hospital Services';
                          '0900'='Ancillary Hospital Services';
                          '0460'='Ancillary Hospital Services';
                          '0410'='Ancillary Hospital Services';
                          '0730'='Ancillary Hospital Services';
                          '0102'='Peer Services';
                          '0105'='Pharmaceuticals';
                          '121'='Pharmaceuticals';
                          'Pharm'='Pharmaceuticals';
                          '0762'='Crisis Services';
                          '762'='Crisis Services';
                          '0912'='Partial Hospitalization';
                          '0913'='Partial Hospitalization';
                          'G0409'='Peer Services';
                          'H0037'='Medication Management';
                          'H2010'='Medication Management';
                          'H0050'='Outpatient Therapy';
                          '90785'='Outpatient Therapy';
                          '90792'='Assessment';
                          '90832'='Outpatient Therapy';
                          '90833'='Outpatient Therapy';
                          '90834'='Outpatient Therapy';
                          '90836'='Outpatient Therapy';
                          '90839'='Outpatient Therapy';
                          '90840'='Outpatient Therapy';
                          '90791'='Assessment';
                          '99334'='Assessment';
                          '99335'='Assessment'; 
                          '99336'='Assessment'; 
                          '99337'='Assessment';
                          '99347'='Assessment';
                          '99348'='Assessment'; 
                          '99349'='Assessment';
                          '99350'='Assessment'; 
                          'S5108'='Behavioral Treatment';
                          '90837'='Outpatient Therapy';
                          '90838'='Assessment';
                          '99324'='Assessment';
                          '99325'='Assessment';
                          '99326'='Assessment';
                          '99327'='Assessment';
                          '99328'='Assessment';
                          '99341'='Assessment';
                          '99342'='Assessment';
                          '99343'='Assessment';
                          '99344'='Assessment';
                          '99345'='Assessment';
                          '80305'='Assessment';
                          'Q3014'='Telemedicine';
                          '0364T'='Autism Services';
                          '0365T'='Autism Services';
                          '0369T'='Autism Services';
                          '0368T'='Autism Services';
                          '0359T'='Autism Services';
                          '0363T'='Autism Services';
                          '0362T'='Autism Services';
                          '0371T'='Autism Services';
                          '0374T'='Autism Services';
                          '0373T'='Autism Services';
                          '0370T'='Autism Services';
                          '0367T'='Autism Services';
                          '0366T'='Autism Services';
                          '0372T'='Autism Services';
                          else = 'Ungrouped'")
  
  

  

  
  #Grouping Service into even more general categories (variable named 'ServiceType')
  Master$ServiceType <- car::recode(Master$Service,
                              "'Ancillary Hospital Services'='Hospital-based Services';
                              'Assessment'='Screening & Assessment';
                              'Behavioral Treatment'='Outpatient Treatment';
                              'Case Management'='Care Coordination';
                              'Supports Coordination'='Care Coordination';
                              'Interdisciplinary Team'='Care Coordination';
                              'Clubhouse'='Employment Services';
                              'Community Living Supports'='Home & Community Based Services';
                              'Crisis Services'='Crisis and Respite';
                              'Dental'='Physical Health Services';
                              'Equipment'='Equipment';
                              'Family Services'='Outpatient Treatment';
                              'Fiscal Intermediary Services'='Care Coordination';
                              'Health Services'='Physical Health Services';
                              'Laboratory'='Screening & Assessment';
                              'Inpatient Psychiatric Hospital'='Hospital-based Services';
                              'Medication Administration'='Medication';
                              'Medication Management'='Medication';
                              'OT/PT/SLT'='Physical Health Services';
                              'Other'='Other';
                              'Outpatient Therapy'='Outpatient Treatment';
                              'Partial Hospitalization'='Crisis and Respite';
                              'Peer Services'='Care Coordination';
                              'Pharmaceuticals'='Medication';
                              'Prevention'='Outpatient Treatment';
                              'Residential Treatment'='Home & Community Based Services';
                              'Respite'='Crisis and Respite';
                              'Skill-Building/Non-Vocational'='Employment Services';
                              'Sub-Acute Detoxification'='Hospital-based Services';
                              'Substance Abuse Outpatient'='Outpatient Treatment';
                              'Transportation'='Transportation';
                              'Vocational Services'='Employment Services';
                              else = 'Ungrouped'")
  
  service_groups <-
    Master %>%
    group_by(ServiceType, Service, Description, Code, Code_Mod) %>%
    summarize(n = n())
  
  
  ## Standardize CMHSP names
  Master$CMHSP <- car::recode(Master$CMHSP,
                         "'LifeWays' = 'Lifeways';
                         'Manistee-Benzie (Centra Wellness)' = 'Manistee-Benzie';
                         'Muskegon (HealthWest)' = 'Muskegon'")
  
  ## Adding PIHP regions to the dataset ##
  
  Master$PIHP <- car::recode(Master$CMHSP, "'Copper Country'='1';
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

  Master$PIHPname <- car::recode(Master$PIHP,  "'1'='Northcare';
                        '2'='NMRE';
                        '3'='LRP';
                        '4'='SWMBH';
                        '5'='MSHN'; 
                        '6'='CMHPSM';
                        '7'='DWMHA';
                        '8'='OCCMHA';
                        '9'='MCMHS';
                        '10'='Region10'")
  
  Master <- 
  Master %>%
    select(FY, PIHPname, PIHP, CMHSP, Population, ServiceType, Service,
           Description:UnitPerCase)
  
  ## Import HCPCS file
  ## In order to run this script, your 'data/' path will need 
  ## to contain the following files:
  ## /hcpc_code_lookup.csv
  ## Download option "Current LCDs" from CMS site:
  ## "https://www.cms.gov/medicare-coverage-database/downloads/downloadable-databases.aspx"
  
  hcpcs_file <- readr::read_csv("~/GitHub/open404/data/hcpc_code_lookup.csv")
  
  hcpcs_file %<>% 
    #semi_join(locus_to_svs, by = c("hcpc_code_id" = "proccode")) %>%
    group_by(hcpc_code_id) %>%
    filter(hcpc_code_version == max(hcpc_code_version)) %>%
    select(hcpc_code_id, short_description) 
  
  Master <-
    Master %>%
    left_join(hcpcs_file, by = c("Code" = "hcpc_code_id")) %>%
    mutate(short_description = ifelse(is.na(short_description) == T,as.character(Description),short_description)) %>%
    select(FY:Service,short_description,everything())
  
  return(Master)
  
}
