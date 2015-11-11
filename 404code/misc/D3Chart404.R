
###############
## GET DATA  ##
###############

# Load packages:
source("https://raw.githubusercontent.com/j-hagedorn/Beachbox/master/functions/function_libraries.R?token=7065685__eyJzY29wZSI6IlJhd0Jsb2I6ai1oYWdlZG9ybi9CZWFjaGJveC9tYXN0ZXIvZnVuY3Rpb25zL2Z1bmN0aW9uX2xpYnJhcmllcy5SIiwiZXhwaXJlcyI6MTQxMDc0NDExN30%3D--c9554711f3501afb9bea3b5d19d446700b96c814")
libraries(c("RCurl", "dplyr", "googleVis","devtools","rCharts"))
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


# Multibar chart

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
    #Cost1kSvd = round((sum(SumOfCost)/max(TotalServed)*1000), digits = 2)#,
    #Unit1kSvd = round((sum(SumOfUnits)/max(TotalServed)*1000), digits = 1),
    Perc_Svd = round((sum(SumOfCases)/min(TotalServed)*100), digits = 1)#,
    #Cost1kPop = round(sum(SumOfCost)/(max(subPop)/1000),digits = 2),
    #Srvd1kPop = round(sum(SumOfCases)/(max(subPop)/1000),digits = 1)
  ) 

n2 <- nPlot(Perc_Svd ~ FY, 
            group = "CMHSP",
            data = hosp3, 
            type = 'lineChart') # OR 'lineWithFocusChart'
n2$yAxis(axisLabel = "% served in local psych hosp", width = 62)
n2$xAxis(axisLabel = "Year")
n2
n2$save('LocalPsychHosp06to13.html', standalone = TRUE)

# Scatter Chart

hosp4 <- enriched10to13 %>%
  filter(FY==2013 & Service=="Local Hospitalization") %>%
  mutate(CMH_Disability = as.factor(paste(CMHSP,Population, sep = "-"))) %>%
  group_by(CMH_Disability,Population,Service) %>%
  select(PIHPname,Service,SumOfUnits,SumOfCases,SumOfCost,TotalServed,subPop) %>%
  summarise(#UnitPerPerson = round((sum(SumOfUnits)/sum(SumOfCases)),digits=1),
    #CostPerPerson = round((sum(SumOfCost)/sum(SumOfCases)),digits=2),
    CostPerUnit = round((sum(SumOfCost)/sum(SumOfUnits)),digits=2),
    #CostPUPM = round((sum(SumOfCost)/sum(SumOfCases)/12),digits=2),
    #Cost1kSvd = round((sum(SumOfCost)/sum(TotalServed)*1000), digits = 2)#,
    #Unit1kSvd = round((sum(SumOfUnits)/sum(TotalServed)*1000), digits = 1),
    SumOfCases = sum(SumOfCases)
    #Perc_Svd = round((sum(SumOfCases)/sum(TotalServed)*100), digits = 1)#,
    #Cost1kPop = round(sum(SumOfCost)/(sum(subPop)/1000),digits = 2),
    #Srvd1kPop = round(sum(SumOfCases)/(sum(subPop)/1000),digits = 1)
  )

n3 <- nPlot(SumOfCases ~ CostPerUnit, group = "Population", data = hosp4, type = 'scatterChart')
n3$xAxis(axisLabel = 'Cost per day for local psych hosp, 2013', width = 62)
n3$yAxis(axisLabel = '# beneficiaries using local psych hosp, 2013', width = 62)
#n3$chart(size = '#! function(d){return d.var} !#')
#p2$chart(tooltipContent = "#! function(key, x, y, e){ 
#return e.point.screenName + ' Followers: ' + e.point.followersCount +' Friends: ' + e.point.friendsCount
#} !#")
n3$chart(tooltipContent = "#! function(key, x, y, e){ 
  return '<b>CMH-Population:</b> ' + e.point.CMH_Disability 
} !#")
n3

n3$save('UnitCostVsNumServed.html', standalone = TRUE)

##
n <- nPlot(mpg ~ wt, data = mtcars, type = 'scatterChart', group = 'gear')
n$chart(tooltipContent= "#! function(key, x, y, e){
  return '<b>Disp:</b> ' + e.point.disp + '<br/>' +
    '<b>Cyl: </b>' + e.point.cyl
} !#")
n
# Multi Chart

hosp5 <- subMaster %>%
  filter(Service=="Local Hospitalization") %>%
  mutate(CMH_Disability = as.factor(paste(CMHSP,Population, sep = "-"))) %>%
  group_by(FY,PIHPname,CMH_Disability,Population,Service) %>%
  select(PIHPname,Service,SumOfUnits,SumOfCases,SumOfCost,TotalServed) %>%
  summarise(UnitPerPerson = round((sum(SumOfUnits)/sum(SumOfCases)),digits=1),
    #CostPerPerson = round((sum(SumOfCost)/sum(SumOfCases)),digits=2),
    CostPerUnit = round((sum(SumOfCost)/sum(SumOfUnits)),digits=2),
    #CostPUPM = round((sum(SumOfCost)/sum(SumOfCases)/12),digits=2),
    #Cost1kSvd = round((sum(SumOfCost)/sum(TotalServed)*1000), digits = 2)#,
    #Unit1kSvd = round((sum(SumOfUnits)/sum(TotalServed)*1000), digits = 1),
    Perc_Svd = round((sum(SumOfCases)/sum(TotalServed)*100), digits = 1)#,
    #Cost1kPop = round(sum(SumOfCost)/(sum(subPop)/1000),digits = 2),
    #Srvd1kPop = round(sum(SumOfCases)/(sum(subPop)/1000),digits = 1)
  )


n4 <- nPlot(Perc_Svd ~ FY, group = 'PIHPname', data = hosp5, type = 'multiChart')
n4$set(multi = list(
  UnitPerPerson = list(type="area", yAxis=1),
  CostPerUnit = list(type="line", yAxis=2)
))
n4$setTemplate(script = system.file(
  "/libraries/nvd3/layouts/multiChart.html",
  package = "rCharts"
))
n4



# Line plot
pc <- subMaster %>%
  filter(FirstofService.Description=="Personal Care in Licensed Specialized Residential Setting") %>%
  #mutate(CMH_Disability = as.factor(paste(CMHSP,Population, sep = "-"))) %>%
  group_by(FY,CMHSP) %>%
  select(FY,CMHSP,Service,SumOfUnits,SumOfCases,SumOfCost,TotalServed) %>%
  summarise(#UnitPerPerson = round((sum(SumOfUnits)/sum(SumOfCases)),digits=1),
    #CostPerPerson = round((sum(SumOfCost)/sum(SumOfCases)),digits=2),
    CostPerUnit = round((sum(SumOfCost)/sum(SumOfUnits)),digits=2)#,
    #CostPUPM = round((sum(SumOfCost)/sum(SumOfCases)/12),digits=2),
    #Cost1kSvd = round((sum(SumOfCost)/max(TotalServed)*1000), digits = 2)#,
    #Unit1kSvd = round((sum(SumOfUnits)/max(TotalServed)*1000), digits = 1),
    #Perc_Svd = round((sum(SumOfCases)/min(TotalServed)*100), digits = 1)#,
    #Cost1kPop = round(sum(SumOfCost)/(max(subPop)/1000),digits = 2),
    #Srvd1kPop = round(sum(SumOfCases)/(max(subPop)/1000),digits = 1)
  ) 

p1 <- nPlot(CostPerUnit ~ FY, 
            group = "CMHSP",
            data = pc, 
            type = 'lineChart') # OR 'lineWithFocusChart'
p1$yAxis(axisLabel = "Cost per unit, personal care", width = 62)
p1$xAxis(axisLabel = "Year")
p1
p1$save('PersonalCare06to13.html', standalone = TRUE)

cls <- subMaster %>%
  filter(FirstofService.Description=="Community Living Supports (Daily)") %>%
  #mutate(CMH_Disability = as.factor(paste(CMHSP,Population, sep = "-"))) %>%
  group_by(FY,CMHSP) %>%
  select(FY,CMHSP,Service,SumOfUnits,SumOfCases,SumOfCost,TotalServed) %>%
  summarise(#UnitPerPerson = round((sum(SumOfUnits)/sum(SumOfCases)),digits=1),
    #CostPerPerson = round((sum(SumOfCost)/sum(SumOfCases)),digits=2),
    CostPerUnit = round((sum(SumOfCost)/sum(SumOfUnits)),digits=2)#,
    #CostPUPM = round((sum(SumOfCost)/sum(SumOfCases)/12),digits=2),
    #Cost1kSvd = round((sum(SumOfCost)/max(TotalServed)*1000), digits = 2)#,
    #Unit1kSvd = round((sum(SumOfUnits)/max(TotalServed)*1000), digits = 1),
    #Perc_Svd = round((sum(SumOfCases)/min(TotalServed)*100), digits = 1)#,
    #Cost1kPop = round(sum(SumOfCost)/(max(subPop)/1000),digits = 2),
    #Srvd1kPop = round(sum(SumOfCases)/(max(subPop)/1000),digits = 1)
  ) 

c1 <- nPlot(CostPerUnit ~ FY, 
            group = "CMHSP",
            data = pc, 
            type = 'lineChart') # OR 'lineWithFocusChart'
c1$yAxis(axisLabel = "Cost per unit, CLS per diem", width = 62)
c1$xAxis(axisLabel = "Year")
c1
c1$save('CLSperdiem06to13.html', standalone = TRUE)

locAcute <- subMaster %>%
  filter(FirstofService.Description=="Local Psychiatric Hospital - Acute Community PT73" ) %>%
  #mutate(CMH_Disability = as.factor(paste(CMHSP,Population, sep = "-"))) %>%
  group_by(FY,CMHSP) %>%
  select(FY,CMHSP,Service,SumOfUnits,SumOfCases,SumOfCost,TotalServed) %>%
  summarise(#UnitPerPerson = round((sum(SumOfUnits)/sum(SumOfCases)),digits=1),
    #CostPerPerson = round((sum(SumOfCost)/sum(SumOfCases)),digits=2),
    CostPerUnit = round((sum(SumOfCost)/sum(SumOfUnits)),digits=2)#,
    #CostPUPM = round((sum(SumOfCost)/sum(SumOfCases)/12),digits=2),
    #Cost1kSvd = round((sum(SumOfCost)/max(TotalServed)*1000), digits = 2)#,
    #Unit1kSvd = round((sum(SumOfUnits)/max(TotalServed)*1000), digits = 1),
    #Perc_Svd = round((sum(SumOfCases)/min(TotalServed)*100), digits = 1)#,
    #Cost1kPop = round(sum(SumOfCost)/(max(subPop)/1000),digits = 2),
    #Srvd1kPop = round(sum(SumOfCases)/(max(subPop)/1000),digits = 1)
  ) 

a1 <- nPlot(CostPerUnit ~ FY, 
            group = "CMHSP",
            data = locAcute, 
            type = 'lineChart') # OR 'lineWithFocusChart'
a1$yAxis(axisLabel = "Cost per unit, Local Acute Psych Inpt", width = 62)
a1$xAxis(axisLabel = "Year")
a1
a1$save('localacute06to13.html', standalone = TRUE)



#######
# PARAMETERS...

# interactive dropdown controls:

$addControls("x", value = "CostPerUnit", values = names(comphosp))
$addControls("y", value = "UnitPerPerson", values = names(comphosp))
$addControls("color", value = "FirstofService.Description", values = names(comphosp))
$addFilters("Population")
