# Bring Census API data into rate calculations for 404

calc404pop <- function(census_key) {
  
  # Install package, define geography
  #####
  # install.packages("acs")
  library("acs")
  
  # Install API key
  api.key.install(census_key, file = "key.rda") # hide key in local environment
  
  # Define Michigan geography
  lookup_MI <- geo.lookup(state = "MI", county = "*")
  MIcounties <- as.vector(na.omit(lookup_MI$county))    #Get vector of counties from MI
  MIbyCounty <- geo.make(state="MI", county=MIcounties) #Define region for acs.fetch query
  
  #####
  
  # Get 5-year estimate data for population
  #####
  
  # Use 2012 ACS estimates for 2013 404 population rates, since 2013 ACS data not published yet.
  
  MI_2014 <- acs.fetch(endyear = 2013, 
                       span = 5, # x-year estimate
                       geography=MIbyCounty, 
                       #table.name,
                       #table.number, 
                       variable = c('B01001_001','B09001_001'), 
                       #keyword, 
                       #key, 
                       col.names = "auto")
  
  MI_2013 <- acs.fetch(endyear = 2013, span = 5, geography=MIbyCounty,  
                       variable = c('B01001_001','B09001_001'), col.names = "auto")
  MI_2012 <- acs.fetch(endyear = 2012, span = 5, geography=MIbyCounty,  
                       variable = c('B01001_001','B09001_001'), col.names = "auto")
  MI_2011 <- acs.fetch(endyear = 2011, span = 5, geography=MIbyCounty,  
                       variable = c('B01001_001','B09001_001'), col.names = "auto")
  MI_2010 <- acs.fetch(endyear = 2010, span = 5, geography=MIbyCounty,  
                       variable = c('B01001_001','B09001_001'), col.names = "auto")
  # For endyears prior to 2010, no acs package CensusAPI data
  #####
  
  # Make a dataframe
  #####
  
  # ...for 2014
  MI_df_14 <- data.frame(estimate(MI_2013))
  colnames(MI_df_14)=c("TotalPop","Under18")
  MI_df_14$Year <- 2014  #add year variable
  MI_df_14$County <- rownames(MI_df_14) #create new var using rownames
  rownames(MI_df_14) <- NULL #nullify existing rownames
  
  # ...for 2013
  MI_df_13 <- data.frame(estimate(MI_2013))
  colnames(MI_df_13)=c("TotalPop","Under18")
  MI_df_13$Year <- 2013  #add year variable
  MI_df_13$County <- rownames(MI_df_13) #create new var using rownames
  rownames(MI_df_13) <- NULL #nullify existing rownames
  
  # ...for 2012
  MI_df_12 <- data.frame(estimate(MI_2012))
  colnames(MI_df_12)=c("TotalPop","Under18")
  MI_df_12$Year <- endyear(MI_2012)  #add year variable
  MI_df_12$County <- rownames(MI_df_12) #create new var using rownames
  rownames(MI_df_12) <- NULL #nullify existing rownames
  
  # ...for 2011
  MI_df_11 <- data.frame(estimate(MI_2011))
  colnames(MI_df_11)=c("TotalPop","Under18")
  MI_df_11$Year <- endyear(MI_2011)  #add year variable
  MI_df_11$County <- rownames(MI_df_11) #create new var using rownames
  rownames(MI_df_11) <- NULL #nullify existing rownames
  
  # ...for 2010
  MI_df_10 <- data.frame(estimate(MI_2010))
  colnames(MI_df_10)=c("TotalPop","Under18")
  MI_df_10$Year <- endyear(MI_2010)  #add year variable
  MI_df_10$County <- rownames(MI_df_10) #create new var using rownames
  rownames(MI_df_10) <- NULL #nullify existing rownames
  
  MI_df <- rbind(MI_df_14,MI_df_13,MI_df_12,MI_df_11, MI_df_10) #bind the 4 years together
  MI_df <- MI_df[,c(3,4,1,2)] #reorder columns
  MI_df$Over18 <- MI_df$TotalPop-MI_df$Under18 # compute pop over 18
  MI_df$County <- as.factor(MI_df$County)
  #####
  
  # Map counties to 404 CMHs and PIHP regions
  #####
  library(car)
  
  MI_df$CMHSP<-recode(MI_df$County, "'Alcona County, Michigan'='Northeast Michigan';
                       'Alger County, Michigan'='Pathways';
                       'Allegan County, Michigan'='Allegan';       
                       'Alpena County, Michigan'='Northeast Michigan';       
                       'Antrim County, Michigan'='North Country';       
                       'Arenac County, Michigan'='Bay-Arenac';        
                       'Baraga County, Michigan'='Copper Country';         
                       'Barry County, Michigan'='Barry';     
                       'Bay County, Michigan'='Bay-Arenac';         
                       'Benzie County, Michigan'='Manistee-Benzie';        
                       'Berrien County, Michigan'='Berrien';      
                       'Branch County, Michigan'='Pines'; 
                       'Calhoun County, Michigan'='Summit Pointe';    
                       'Cass County, Michigan'='Woodlands';
                       'Charlevoix County, Michigan'='North Country';   
                       'Cheboygan County, Michigan'='North Country';   
                       'Chippewa County, Michigan'= 'Hiawatha';       
                       'Clare County, Michigan'='CMH for Central Michigan';     
                       'Clinton County, Michigan'='Clinton Eaton Ingham';       
                       'Crawford County, Michigan'='Northern Lakes';   
                       'Delta County, Michigan'='Pathways';      
                       'Dickinson County, Michigan'='Northpointe';    
                       'Eaton County, Michigan'='Clinton Eaton Ingham';      
                       'Emmet County, Michigan'='North Country';
                       'Genesee County, Michigan'='Genesee';       
                       'Gladwin County, Michigan'='CMH for Central Michigan';    
                       'Gogebic County, Michigan'='Gogebic';    
                       'Grand Traverse County, Michigan'='Northern Lakes';
                       'Gratiot County, Michigan'='Gratiot';
                       'Hillsdale County, Michigan'='Lifeways';    
                       'Houghton County, Michigan'='Copper Country';      
                       'Huron County, Michigan'='Huron';
                       'Ingham County, Michigan'='Clinton Eaton Ingham';        
                       'Ionia County, Michigan'='Ionia';    
                       'Iosco County, Michigan'='AuSable Valley';        
                       'Iron County, Michigan'='Northpointe';  
                       'Isabella County, Michigan'='CMH for Central Michigan';  
                       'Jackson County, Michigan'='Lifeways';    
                       'Kalamazoo County, Michigan'='Kalamazoo';  
                       'Kalkaska County, Michigan'='North Country'; 
                       'Kent County, Michigan'='Network180';
                       'Keweenaw County, Michigan'='Copper Country';       
                       'Lake County, Michigan'='West Michigan';
                       'Lapeer County, Michigan'='Lapeer';     
                       'Leelanau County, Michigan'='Northern Lakes';  
                       'Lenawee County, Michigan'='Lenawee';    
                       'Livingston County, Michigan'='Livingston';   
                       'Luce County, Michigan'='Pathways';
                       'Mackinac County, Michigan'='Hiawatha';       
                       'Macomb County, Michigan'='Macomb';
                       'Manistee County, Michigan'='Manistee-Benzie';     
                       'Marquette County, Michigan'='Pathways';   
                       'Mason County, Michigan'='West Michigan';
                       'Mecosta County, Michigan'='CMH for Central Michigan';     
                       'Menominee County, Michigan'='Northpointe';     
                       'Midland County, Michigan'='CMH for Central Michigan';
                       'Missaukee County, Michigan'='Northern Lakes';     
                       'Monroe County, Michigan'='Monroe';
                       'Montcalm County, Michigan'='Montcalm';
                       'Montmorency County, Michigan'='Northeast Michigan';
                       'Muskegon County, Michigan'='Muskegon';    
                       'Newaygo County, Michigan'='Newaygo'; 
                       'Oakland County, Michigan'='Oakland';    
                       'Oceana County, Michigan'='West Michigan';   
                       'Ogemaw County, Michigan'='AuSable Valley';      
                       'Ontonagon County, Michigan'='Copper Country';
                       'Osceola County, Michigan'='CMH for Central Michigan';       
                       'Oscoda County, Michigan'='AuSable Valley';      
                       'Otsego County, Michigan'='North Country';         
                       'Ottawa County, Michigan'='Ottawa';       
                       'Presque Isle County, Michigan'='Northeast Michigan';   
                       'Roscommon County, Michigan'='Northern Lakes';  
                       'Saginaw County, Michigan'='Saginaw';     
                       'Sanilac County, Michigan'='Sanilac';      
                       'Schoolcraft County, Michigan'='Hiawatha';  
                       'Shiawassee County, Michigan'='Shiawassee';   
                       'St. Clair County, Michigan'='St. Clair'; 
                       'St. Joseph County, Michigan'='St. Joseph';  
                       'Tuscola County, Michigan'='Tuscola';
                       'Van Buren County, Michigan'='Van Buren';   
                       'Washtenaw County, Michigan'='Washtenaw';      
                       'Wayne County, Michigan'='Detroit-Wayne';     
                       'Wexford County, Michigan'='Northern Lakes'") 
  
  MI_df$PIHP<-recode(MI_df$CMHSP, "'Copper Country'='1';
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
  
  MI_df$PIHPname<-recode(MI_df$PIHP, "'1'='Northcare';
                         '2'='NMRE';
                         '3'='LRP';
                         '4'='SWMBH';
                         '5'='MSHN'; 
                         '6'='CMHPSM';
                         '7'='DWMHA';
                         '8'='OCCMHA';
                         '9'='MCMHS';
                         '10'='Region10'")
  
  MI_df <- MI_df[,c(1,7,8,6,2:5)] #reorder columns
  
  # Aggregate population numbers by CMHSP regions
  library(dplyr)
  byCMHSP <- summarize(group_by(MI_df, Year, CMHSP), 
                       TotalPop = sum(TotalPop), 
                       Under18 = sum(Under18),
                       Over18 = sum(Over18))
  #####
  
  # Merge with 404 data
  #####
  # Read in 404 data
  FY10to14 <- 
    Master %>% 
    filter(FY %in% c("2010","2011","2012","2013","2014"))
  
  # Create unique "Year-CMH" key to merge
  byCMHSP$Key <- paste(byCMHSP$Year,byCMHSP$CMHSP, sep = "", collapse = NULL)
  FY10to14$Key <- paste(FY10to14$FY,FY10to14$CMHSP, sep = "", collapse = NULL)
  merged <- merge(byCMHSP, FY10to13, by.x = "Key", by.y = "Key")
  #####
  
  # Map correct age ranges to 404 disability types
  #####
  
  # Make an empty variable
  merged["subPop"] <- NA
  merged$subPop <- as.numeric(merged$subPop)
  
  # create a data.table with Population as the key
  library(data.table)
  merged <- data.table(merged, key = 'Population')
  # where the population is DD, use Total Population
  merged[.('DD'), subPop := TotalPop]
  # and where the population is MIA, use over 18 Population
  merged[.('MIA'), subPop := Over18]
  # and where the population is MIC, use under 18 Population
  merged[.('MIC'), subPop := Under18]
  #####
  
  # Use subpopulation numbers for population rate calculations
  #####
  
  # Create new variable, Cost per 1,000 population
  merged$Cost1kPop <- round(merged$SumOfCost/(merged$subPop/1000),digits = 2)
  
  # Create new variable, People served per 1,000 population
  merged$Srvd1kPop <- round(merged$SumOfCases/(merged$subPop/1000),digits = 2)
  
  #####
  
  # Clean, rearrange, and print
  #####
  #Reordering the columns
  merged$CMHSP <- merged$CMHSP.y
  merged <- subset(merged, select=c(FY,PIHPname,PIHP,CMHSP,TotalPop,
                                    Population,subPop,
                                    ServiceType,Service,FirstofService.Description,
                                    FirstOfHCPCS.Code,FirstOfModifier,Code_Mod, Unit_Hours,
                                    SumOfCases,SumOfUnits,SumOfCost,
                                    CostPerCase,CostPerUnit,UnitPerCase,
                                    Unit_Perc_Tot,Cost_Perc_Tot,CostPUPM,
                                    TotalServed,Cost1kSvd,Unit1kSvd,Perc_Svd,
                                    Cost1kPop,Srvd1kPop))
  # return the merged and cleaned dataset
  return(merged)
}