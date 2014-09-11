
###############
## GET DATA  ##
###############

# Load packages:
source("https://raw.githubusercontent.com/j-hagedorn/Beachbox/master/functions/function_libraries.R?token=7065685__eyJzY29wZSI6IlJhd0Jsb2I6ai1oYWdlZG9ybi9CZWFjaGJveC9tYXN0ZXIvZnVuY3Rpb25zL2Z1bmN0aW9uX2xpYnJhcmllcy5SIiwiZXhwaXJlcyI6MTQxMDc0NDExN30%3D--c9554711f3501afb9bea3b5d19d446700b96c814")
libraries(c("RCurl", "dplyr", "googleVis","devtools"))
# install_github('rCharts', 'ramnathv')

# Load 404 clean datasets
source("https://raw.githubusercontent.com/j-hagedorn/open404/master/404code/load_open404_data.R")

###############
##  WRANGLE  ##
###############

enriched10to13 <- tbl_df(enriched10to13)

hosp2 <- enriched10to13 %>%
  filter(FY==2013 & Service=="State Hospitalization"
         | Service=="Partial Hospitalization" | Service=="Local Hospitalization"
         | Service=="Crisis Services") %>%
  #mutate(CMH_Disability = as.factor(paste(CMHSP,Population, sep = "-"))) %>%
  group_by(PIHPname,Service) %>%
  select(PIHPname,Service,SumOfUnits,SumOfCases,SumOfCost,TotalServed,subPop) %>%
  summarise(#UnitPerPerson = round((sum(SumOfUnits)/sum(SumOfCases)),digits=1),
            #CostPerPerson = round((sum(SumOfCost)/sum(SumOfCases)),digits=2),
            #CostPerUnit = round((sum(SumOfCost)/sum(SumOfUnits)),digits=2),
            #CostPUPM = round((sum(SumOfCost)/sum(SumOfCases)/12),digits=2),
            Cost1kSvd = round((sum(SumOfCost)/sum(TotalServed)*1000), digits = 2)#,
            #Unit1kSvd = round((sum(SumOfUnits)/sum(TotalServed)*1000), digits = 1),
            #Perc_Svd = round((sum(SumOfCases)/sum(TotalServed)*100), digits = 1),
            #Cost1kPop = round(sum(SumOfCost)/(sum(subPop)/1000),digits = 2),
            #Srvd1kPop = round(sum(SumOfCases)/(sum(subPop)/1000),digits = 1)
            ) 



require(rCharts)

n1 <- nPlot(Cost1kSvd ~ Service, 
            group = "PIHPname", 
            data = hosp2, 
            type = "multiBarChart",
            #ylab = "Cost per 1,000 beneficiaries",
            id = "Cost per 1,000 beneficiaries")
#n1$yAxis( type = "addMeasureAxis" )
n1$print("hosp2")
require(base64enc)
n1$save('hosp2.html', standalone = TRUE)


# Line plot
hosp3 <- subMaster %>%
  filter(Service=="Local Hospitalization") %>%
  #mutate(CMH_Disability = as.factor(paste(CMHSP,Population, sep = "-"))) %>%
  group_by(FY,CMHSP) %>%
  select(FY,CMHSP,Service,SumOfUnits,SumOfCases,SumOfCost,TotalServed) %>%
  summarise(#UnitPerPerson = round((sum(SumOfUnits)/sum(SumOfCases)),digits=1),
    #CostPerPerson = round((sum(SumOfCost)/sum(SumOfCases)),digits=2),
    #CostPerUnit = round((sum(SumOfCost)/sum(SumOfUnits)),digits=2),
    #CostPUPM = round((sum(SumOfCost)/sum(SumOfCases)/12),digits=2),
    Cost1kSvd = round((sum(SumOfCost)/sum(TotalServed)*1000), digits = 2)#,
    #Unit1kSvd = round((sum(SumOfUnits)/sum(TotalServed)*1000), digits = 1),
    #Perc_Svd = round((sum(SumOfCases)/sum(TotalServed)*100), digits = 1),
    #Cost1kPop = round(sum(SumOfCost)/(sum(subPop)/1000),digits = 2),
    #Srvd1kPop = round(sum(SumOfCases)/(sum(subPop)/1000),digits = 1)
  ) 

n2 <- nPlot(Cost1kSvd ~ FY, 
            group = "CMHSP",
            data = hosp3, 
            type = 'lineChart')
n2
