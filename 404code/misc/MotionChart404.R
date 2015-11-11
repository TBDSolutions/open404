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

hosp <- enriched10to13 %>%
  filter(ServiceType=="Hospital-based Services") %>%
  mutate(CMH_Disability_Service = as.factor(paste(CMHSP,Population,Service, sep = "-"))) %>%
  group_by(FY,PIHPname,CMHSP,Population,Service,CMH_Disability_Service) %>%
  select(FY,CMH_Disability_Service,PIHPname,CMHSP,Population,Service,
         SumOfUnits,SumOfCases,SumOfCost,TotalServed,subPop) %>%
  summarise(UnitPerPerson = round((sum(SumOfUnits)/sum(SumOfCases)),digits=1),
            CostPerPerson = round((sum(SumOfCost)/sum(SumOfCases)),digits=2),
            CostPerUnit = round((sum(SumOfCost)/sum(SumOfUnits)),digits=2),
            CostPUPM = round((sum(SumOfCost)/sum(SumOfCases)/12),digits=2),
            Cost1kSvd = round((sum(SumOfCost)/sum(TotalServed)*1000), digits = 2),
            Unit1kSvd = round((sum(SumOfUnits)/sum(TotalServed)*1000), digits = 1),
            Perc_Svd = round((sum(SumOfCases)/sum(TotalServed)*100), digits = 1),
            Cost1kPop = round(sum(SumOfCost)/(sum(subPop)/1000),digits = 2),
            Srvd1kPop = round(sum(SumOfCases)/(sum(subPop)/1000),digits = 1)) 
  

####################
##  MOTION CHART: ##
##  DEFAULT STATE ##
####################

# Define default state for motion chart
defaultState <- '{"iconKeySettings":[],
"stateVersion":3,
"time":"notime",
"xAxisOption":"_NOTHING",
"playDuration":15,
"iconType":"BUBBLE",
"sizeOption":"_NOTHING",
"xZoomedDataMin":null,
"xZoomedIn":false,
"duration":{"multiplier":1,"timeUnit":"none"},
"yZoomedDataMin":null,
"xLambda":1,
"colorOption":"_NOTHING",
"nonSelectedAlpha":0.4,
"dimensions":{"iconDimensions":[]},
"yZoomedIn":false,
"yAxisOption":"_NOTHING",
"yLambda":1,
"yZoomedDataMax":null,
"showTrails":true,
"xZoomedDataMax":null}'

customState <-'
{"dimensions":{"iconDimensions":["dim0"]},"iconKeySettings":[],"yLambda":1,"orderedByX":false,"orderedByY":false,"uniColorForNonSelected":false,"colorOption":"2","yAxisOption":"5","showTrails":true,"sizeOption":"4","yZoomedDataMax":74832922,"xLambda":1,"yZoomedIn":false,"xZoomedDataMin":6,"duration":{"multiplier":1,"timeUnit":"Y"},"playDuration":15000,"iconType":"BUBBLE","time":"2012","nonSelectedAlpha":0.4,"yZoomedDataMin":293,"xZoomedIn":false,"xAxisOption":"7","xZoomedDataMax":23488}
'

####################
##  MOTION CHART: ##
##  SETTINGs      ##
####################

motionhosp <- gvisMotionChart(hosp, idvar='CMH_Disability_Service', timevar='FY', 
                              options=list(height=600,width=1000, 
                                           state=customState))

plot(motionhosp)

# cat(motionNMRE$html$chart, file="NMRE_motion.html")

hosp13 <- hosp %>%
  filter(FY==2013)

Bubble <- gvisBubbleChart(hosp13, idvar="CMH_Disability_Service", 
                          xvar="Perc_Svd", yvar="Cost1kSvd",
                          colorvar="Service", sizevar="CostPerPerson")
plot(Bubble)
