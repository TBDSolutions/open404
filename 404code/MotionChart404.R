###############
## GET DATA  ##
###############


# Load packages:
source("https://raw.githubusercontent.com/j-hagedorn/Beachbox/master/functions/function_libraries.R?token=7065685__eyJzY29wZSI6IlJhd0Jsb2I6ai1oYWdlZG9ybi9CZWFjaGJveC9tYXN0ZXIvZnVuY3Rpb25zL2Z1bmN0aW9uX2xpYnJhcmllcy5SIiwiZXhwaXJlcyI6MTQxMDc0NDExN30%3D--c9554711f3501afb9bea3b5d19d446700b96c814")
libraries(c("RCurl", "dplyr", "googleVis"))

# Load 404 clean datasets
source("https://raw.githubusercontent.com/j-hagedorn/open404/master/404code/load_open404_data.R")

###############
##  WRANGLE  ##
###############

enriched10to13 <- tbl_df(enriched10to13)

df <- 
enriched10to13 %>%
  select(FY,PIHPname,CMHSP,Population,ServiceType,Service, enriched10to13$)
  
  
summarise(group_by(NMRE_main, FY, Service, ServiceType),
                     Units = round((sum(SumOfUnits)), digits=2),
                     UnitPerPerson = round((sum(SumOfUnits)/sum(SumOfCases)),digits=2),
                     Cost = round(sum(SumOfCost), digits=2),
                     CostPerPerson = round((sum(SumOfCost)/sum(SumOfCases)),digits=2),
                     PeopleServed = round(sum(SumOfCases), digits=2))

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

library(googleVis)
motionNMRE <- gvisMotionChart(NMRE_df, idvar='Service', timevar='FY', 
                              options=list(height=600,width=1000, 
                                           state=customState))

plot(motionNMRE)

# cat(motionNMRE$html$chart, file="NMRE_motion.html")