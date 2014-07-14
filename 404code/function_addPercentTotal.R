# Function to create new variables, 
# Serv_Perc_Tot (units per service description as % of total units per CMHSP/FY)
# Cost_Perc_Tot (cost per service description as % of total cost per CMHSP/FY)

addPercentTotal <- function(){
  percTot <- function(CMH) {
    # create filtered dataframes for each year; 
    # create % of total units per CMHSP/year variable; 
    # create % of total cost per CMHSP/year variable;
    # bind Serv_Perc_Tot & Cost_Perc_Tot columns to cmh_yr;
    # stack the years back up for the CMHSP;
    # return 
    library(dplyr)
    cmh_2006 <- filter(subMaster, FY==2006 & CMHSP==CMH)
    cmh_2006 <- mutate(cmh_2006, Serv_Perc_Tot = SumOfUnits/sum(SumOfUnits), Cost_Perc_Tot = SumOfCost/sum(SumOfCost))
    cmh_2007 <- filter(subMaster, FY==2007 & CMHSP==CMH)
    cmh_2007 <- mutate(cmh_2007, Serv_Perc_Tot = SumOfUnits/sum(SumOfUnits), Cost_Perc_Tot = SumOfCost/sum(SumOfCost))
    cmh_2008 <- filter(subMaster, FY==2008 & CMHSP==CMH)
    cmh_2008 <- mutate(cmh_2008, Serv_Perc_Tot = SumOfUnits/sum(SumOfUnits), Cost_Perc_Tot = SumOfCost/sum(SumOfCost))
    cmh_2009 <- filter(subMaster, FY==2009 & CMHSP==CMH)
    cmh_2009 <- mutate(cmh_2009, Serv_Perc_Tot = SumOfUnits/sum(SumOfUnits), Cost_Perc_Tot = SumOfCost/sum(SumOfCost))
    cmh_2010 <- filter(subMaster, FY==2010 & CMHSP==CMH)
    cmh_2010 <- mutate(cmh_2010, Serv_Perc_Tot = SumOfUnits/sum(SumOfUnits), Cost_Perc_Tot = SumOfCost/sum(SumOfCost))
    cmh_2011 <- filter(subMaster, FY==2011 & CMHSP==CMH)
    cmh_2011 <- mutate(cmh_2011, Serv_Perc_Tot = SumOfUnits/sum(SumOfUnits), Cost_Perc_Tot = SumOfCost/sum(SumOfCost))
    cmh_2012 <- filter(subMaster, FY==2012 & CMHSP==CMH)
    cmh_2012 <- mutate(cmh_2012, Serv_Perc_Tot = SumOfUnits/sum(SumOfUnits), Cost_Perc_Tot = SumOfCost/sum(SumOfCost))
    newCMH <- rbind(cmh_2006, cmh_2007, cmh_2008, cmh_2009, cmh_2010, cmh_2011, cmh_2012)
    return(newCMH)
  }
  # Now use percTot...
  sub_Allegan <- percTot(CMH="Allegan")
  sub_AuSable <- percTot(CMH="AuSable Valley")           
  sub_Barry <- percTot(CMH="Barry")
  sub_BayArenac <- percTot(CMH="Bay-Arenac")
  sub_Berrien <- percTot(CMH="Berrien")
  sub_CEI <- percTot(CMH="Clinton Eaton Ingham")
  sub_CentralMI <- percTot(CMH="CMH for Central Michigan")
  sub_Copper <- percTot(CMH="Copper Country")
  sub_DetroitWayne <- percTot(CMH="Detroit-Wayne")
  sub_Genesee <- percTot(CMH="Genesee")
  sub_Gogebic <- percTot(CMH="Gogebic")
  sub_Gratiot <- percTot(CMH="Gratiot")
  sub_Hiawatha <- percTot(CMH="Hiawatha")
  sub_Huron <- percTot(CMH="Huron")
  sub_Ionia <- percTot(CMH="Ionia")
  sub_Kalamazoo <- percTot(CMH="Kalamazoo")
  sub_Lapeer <- percTot(CMH="Lapeer")
  sub_Lenawee <- percTot(CMH="Lenawee")
  sub_Lifeways <- percTot(CMH="Lifeways")
  sub_Livingston <- percTot(CMH="Livingston")
  sub_Macomb <- percTot(CMH="Macomb")
  sub_Manistee <- percTot(CMH="Manistee-Benzie")
  sub_Monroe <- percTot(CMH="Monroe")
  sub_Montcalm <- percTot(CMH="Montcalm")
  sub_Muskegon <- percTot(CMH="Muskegon")
  sub_Network180 <- percTot(CMH="Network180")
  sub_Newaygo <- percTot(CMH="Newaygo")
  sub_NorthCountry <- percTot(CMH="North country")
  sub_Northeast <- percTot(CMH="Northeast Michigan")       
  sub_NorthernLakes <- percTot(CMH="Northern Lakes")           
  sub_Northpointe <- percTot(CMH="Northpointe")              
  sub_Oakland <- percTot(CMH="Oakland")                 
  sub_Ottawa <- percTot(CMH="Ottawa")                   
  sub_Pathways <- percTot(CMH="Pathways")                 
  sub_Pines <- percTot(CMH="Pines")                    
  sub_Saginaw <- percTot(CMH="Saginaw")                 
  sub_Sanilac <- percTot(CMH="Sanilac")                  
  sub_Shiawassee <- percTot(CMH="Shiawassee")               
  sub_StClair <- percTot(CMH="St. Clair")                
  sub_StJoseph <- percTot(CMH="St. Joseph")              
  sub_SummitPointe <- percTot(CMH="Summit Pointe")           
  sub_Tuscola <- percTot(CMH="Tuscola")                 
  sub_VanBuren <- percTot(CMH="Van Buren")                
  sub_Washtenaw <- percTot(CMH="Washtenaw")               
  sub_WMichigan <- percTot(CMH="West Michigan")           
  sub_Woodlands <- percTot(CMH="Woodlands")
  newSubMaster <- rbind(sub_Allegan,sub_AuSable,sub_Barry,sub_BayArenac,sub_Berrien,sub_CEI,
                        sub_CentralMI,sub_Copper,sub_DetroitWayne,sub_Genesee,sub_Gogebic,
                        sub_Gratiot,sub_Hiawatha,sub_Huron,sub_Ionia,sub_Kalamazoo,sub_Lapeer, 
                        sub_Lenawee,sub_Lifeways,sub_Livingston,sub_Macomb,sub_Manistee,sub_Monroe,
                        sub_Montcalm,sub_Muskegon,sub_Network180,sub_Newaygo,sub_NorthCountry, 
                        sub_Northeast,sub_NorthernLakes,sub_Northpointe,sub_Oakland,sub_Ottawa,
                        sub_Pathways,sub_Pines,sub_Saginaw,sub_Sanilac,sub_Shiawassee,sub_StClair,
                        sub_StJoseph,sub_SummitPointe,sub_Tuscola,sub_VanBuren,sub_Washtenaw,
                        sub_WMichigan,sub_Woodlands)
  return(newSubMaster)
} 

# This version can be used in R but doesn't translate to Rmd
subMaster <- read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/subMaster', sep=',', header=TRUE)
# Add new columns for units and cost of services as percent of total CMHSP per FY
# Be sure addPercentTotal function is defined in the working environment
subMasterPlus <- addPercentTotal()
# Save the results for easier use...
write.csv(subMasterPlus, file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\subMasterPlus")
