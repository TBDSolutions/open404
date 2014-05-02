############################################
#Creating Aggregate Dataframes for Analysis#
############################################

## By creating aggregate dataframes, we can look at the totals for each CMH, PIHP, 
## population, or any combination of the three.

## Aggregating this way will allow for easier analysis

## Load Master from GitHub (for those who don't already have it in the workspace)

Master <- read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/Master', sep=',', header=TRUE)

install.packages('plyr')
library(plyr)

## Create subMaster dataframe, excluding services with 0 cases, units, and cost.
subMaster<-data.frame(subset(Master, SumOfCases != 0 | SumOfUnits != 0 | SumOfCost != 0, select = c(1:15)))

#Output subMaster .csv file
write.csv(subMaster, file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\subMaster")

##There are multiple HCPCS codes per Service, and so the calculated rates do not end up acurately reflecting the 
##totals.  Therefore, they will be re-calculated and added to the dataframes later.

#Creating a data frame that sums the variables at each level of service, region, population, unit hour and year
Agg_PIHP<-ddply(subMaster, .(FY, PIHP, Service, FirstofService.Description, Population, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Output Agg_PIHP .csv file
#write.csv(Agg_PIHP, file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\Agg_PIHP")

#Creating a data frame that sums the variables at each level of service, CMH, population, unit hour and year
Agg_CMH<-ddply(subMaster, .(FY, CMHSP, Service, FirstofService.Description, Population, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Output Agg_CMH .csv file
#write.csv(Agg_CMH, file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\Agg_CMH")

#Printing first 10 rows of dataframes just to see what they look like

head(Agg_PIHP, n=10)
head(Agg_CMH, n=10)


####################################################################################################

##Creating subset dataframes from Master so analysis can be performed for each population##
MIC <-data.frame(subset(subMaster, Population == "MIC", select = c(1:15)))
#Removing 'Population' since whole dataframe is MIC
MIC$Population<-NULL
#Output MIC .csv file
#write.csv(MIC, file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\MIC")

MIA <-data.frame(subset(subMaster, Population == "MIA", select = c(1:15)))
#Removing 'Population' since whole dataframe is MIA
MIA$Population<-NULL
#Output MIA .csv file
#write.csv(MIA, file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\MIA")

DD <-data.frame(subset(subMaster, Population == "DD", select = c(1:15)))
#Removing 'Population' since whole dataframe is DD
DD$Population<-NULL
#Output DD .csv file
#write.csv(DD, file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\DD")

################################

#Creating aggregate dataframes for each population at the level of CMH and PIHP
  #These not printed to .csv

DD_CMH<-ddply(DD, .(FY, CMHSP, Service, FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
              SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
              CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
              CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

DD_PIHP<-ddply(DD, .(FY, PIHP, Service, FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

MIC_CMH<-ddply(MIC, .(FY, CMHSP, FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

MIC_PIHP<-ddply(MIC, .(FY, PIHP, Service,FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

MIA_CMH<-ddply(MIA, .(FY, CMHSP, Service,FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

MIA_PIHP<-ddply(MIA, .(FY, PIHP, Service, FirstofService.Description,Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Printing first 10 rows to see an example of what the dataframes look like
head(MIA_PIHP, n=10)
head(MIA_CMH, n=10)

######################################
## Formatting Datasets for Analysis ##
######################################

## To be able to understand differences across services, we will 
## look only at the service groups (Service).  These services
## need to become columns instead of rows.  The below process
## creates such a dataframe

#Getting rid of Unit_Hours to get equal #s of rows
PIHP<-ddply(Master, .(FY, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
            SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
            CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
            CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Dental is not included because values only go until 2009
#DentalTest<-data.frame(subset(PIHP, Service=="Dental", select=c(1:10)))

#Creating data frames for every service and then merging into 1 df

LocalHosp<-data.frame(subset(PIHP, Service == "Local Hospitalization", select = c(1:9)))
CareCoord<-data.frame(subset(PIHP, Service == "Care Coordination", select = c(1:9)))
CLS<-data.frame(subset(PIHP, Service == "Community Living Supports", select = c(1:9)))
Employment<-data.frame(subset(PIHP, Service == "Employment Services", select = c(1:9)))
Health<-data.frame(subset(PIHP, Service == "Health Services", select = c(1:9)))
OTPTSLT<-data.frame(subset(PIHP, Service == "OT/PT/SLT", select = c(1:9)))
Prevention<-data.frame(subset(PIHP, Service == "Prevention", select = c(1:9)))
StateHosp<-data.frame(subset(PIHP, Service == "State Hospitalization", select = c(1:9)))
Crisis<-data.frame(subset(PIHP, Service == "Crisis Services", select = c(1:9)))
Residential<-data.frame(subset(PIHP, Service == "Residential Treatment", select = c(1:9)))
SUD<-data.frame(subset(PIHP, Service == "Substance Abuse Outpatient", select = c(1:9)))
Behavioral<-data.frame(subset(PIHP, Service == "Behavioral Treatment", select = c(1:9)))
Medication<-data.frame(subset(PIHP, Service == "Medication", select = c(1:9)))
Outpatient<-data.frame(subset(PIHP, Service == "Outpatient Therapy", select = c(1:9)))
Respite<-data.frame(subset(PIHP, Service == "Respite", select = c(1:9)))
Peer<-data.frame(subset(PIHP, Service == "Peer Services", select = c(1:9)))
Screening<-data.frame(subset(PIHP, Service == "Screening & Assessment", select = c(1:9)))
Ancillary<-data.frame(subset(PIHP, Service == "Ancillary Services / ECT", select = c(1:9)))
Family<-data.frame(subset(PIHP, Service == "Family Services", select = c(1:9)))
Transportation<-data.frame(subset(PIHP, Service == "Transportation", select = c(1:9)))
Monitoring<-data.frame(subset(PIHP, Service == "Monitoring", select = c(1:9)))

#Merging all of the service dataframes
Services_PIHP<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                          OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                          SUD, Behavioral, Medication, Outpatient, Respite,
                          Peer, Screening, Ancillary, Family, Transportation,
                          Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_PIHP)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_PIHP <- rename(Services_PIHP, my_changes)

#Removing FY, Service, and CMH duplicates
Services_PIHP$Service<-NULL
Services_PIHP$FY.1<-NULL
Services_PIHP$FY.2<-NULL
Services_PIHP$FY.3<-NULL
Services_PIHP$FY.4<-NULL
Services_PIHP$FY.5<-NULL
Services_PIHP$FY.6<-NULL
Services_PIHP$FY.7<-NULL
Services_PIHP$FY.8<-NULL
Services_PIHP$FY.9<-NULL
Services_PIHP$FY.10<-NULL
Services_PIHP$FY.11<-NULL
Services_PIHP$FY.12<-NULL
Services_PIHP$FY.13<-NULL
Services_PIHP$FY.14<-NULL
Services_PIHP$FY.15<-NULL
Services_PIHP$FY.16<-NULL
Services_PIHP$FY.17<-NULL
Services_PIHP$FY.18<-NULL
Services_PIHP$FY.19<-NULL
Services_PIHP$FY.20<-NULL
Services_PIHP$FY.21<-NULL
Services_PIHP$PIHP.1<-NULL
Services_PIHP$PIHP.2<-NULL
Services_PIHP$PIHP.3<-NULL
Services_PIHP$PIHP.4<-NULL
Services_PIHP$PIHP.5<-NULL
Services_PIHP$PIHP.6<-NULL
Services_PIHP$PIHP.7<-NULL
Services_PIHP$PIHP.8<-NULL
Services_PIHP$PIHP.9<-NULL
Services_PIHP$PIHP.10<-NULL
Services_PIHP$PIHP.11<-NULL
Services_PIHP$PIHP.12<-NULL
Services_PIHP$PIHP.13<-NULL
Services_PIHP$PIHP.14<-NULL
Services_PIHP$PIHP.15<-NULL
Services_PIHP$PIHP.16<-NULL
Services_PIHP$PIHP.17<-NULL
Services_PIHP$PIHP.18<-NULL
Services_PIHP$PIHP.19<-NULL
Services_PIHP$PIHP.20<-NULL
Services_PIHP$PIHP.21<-NULL
Services_PIHP$Service.1<-NULL
Services_PIHP$Service.2<-NULL
Services_PIHP$Service.3<-NULL
Services_PIHP$Service.4<-NULL
Services_PIHP$Service.5<-NULL
Services_PIHP$Service.6<-NULL
Services_PIHP$Service.7<-NULL
Services_PIHP$Service.8<-NULL
Services_PIHP$Service.9<-NULL
Services_PIHP$Service.10<-NULL
Services_PIHP$Service.11<-NULL
Services_PIHP$Service.12<-NULL
Services_PIHP$Service.13<-NULL
Services_PIHP$Service.14<-NULL
Services_PIHP$Service.15<-NULL
Services_PIHP$Service.16<-NULL
Services_PIHP$Service.17<-NULL
Services_PIHP$Service.18<-NULL
Services_PIHP$Service.19<-NULL
Services_PIHP$Service.20<-NULL
Services_PIHP$Service.21<-NULL
Services_PIHP$Population.1<-NULL
Services_PIHP$Population.2<-NULL
Services_PIHP$Population.3<-NULL
Services_PIHP$Population.4<-NULL
Services_PIHP$Population.5<-NULL
Services_PIHP$Population.6<-NULL
Services_PIHP$Population.7<-NULL
Services_PIHP$Population.8<-NULL
Services_PIHP$Population.9<-NULL
Services_PIHP$Population.10<-NULL
Services_PIHP$Population.11<-NULL
Services_PIHP$Population.12<-NULL
Services_PIHP$Population.13<-NULL
Services_PIHP$Population.14<-NULL
Services_PIHP$Population.15<-NULL
Services_PIHP$Population.16<-NULL
Services_PIHP$Population.17<-NULL
Services_PIHP$Population.18<-NULL
Services_PIHP$Population.19<-NULL
Services_PIHP$Population.20<-NULL
Services_PIHP$Population.21<-NULL

#Printing first 10 rows to see an example of the dataframe
head(Services_PIHP, n=10)

#Output Services_PIHP .csv file
#write.csv(Services_PIHP, file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\Services_PIHP")

#############################################

#Creating a data frame that sums the variables at each level of service, CMH, PIHP and year
CMH<-ddply(Master, .(FY, CMHSP, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
           SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
           CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
           CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

#Dental is not included because values only go until 2009 and are only 
#reported by a few CMHs
#DentalTest<-data.frame(subset(CMH, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(CMH, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(CMH, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(CMH, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(CMH, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(CMH, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(CMH, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(CMH, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(CMH, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(CMH, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(CMH, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(CMH, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(CMH, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(CMH, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(CMH, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(CMH, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(CMH, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(CMH, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(CMH, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(CMH, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(CMH, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(CMH, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_CMH<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                         OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                         SUD, Behavioral, Medication, Outpatient, Respite,
                         Peer, Screening, Ancillary, Family, Transportation,
                         Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_CMH)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_CMH <- rename(Services_CMH, my_changes)

#Removing FY, Service, and CMH duplicates
Services_CMH$Service<-NULL
Services_CMH$FY.1<-NULL
Services_CMH$FY.2<-NULL
Services_CMH$FY.3<-NULL
Services_CMH$FY.4<-NULL
Services_CMH$FY.5<-NULL
Services_CMH$FY.6<-NULL
Services_CMH$FY.7<-NULL
Services_CMH$FY.8<-NULL
Services_CMH$FY.9<-NULL
Services_CMH$FY.10<-NULL
Services_CMH$FY.11<-NULL
Services_CMH$FY.12<-NULL
Services_CMH$FY.13<-NULL
Services_CMH$FY.14<-NULL
Services_CMH$FY.15<-NULL
Services_CMH$FY.16<-NULL
Services_CMH$FY.17<-NULL
Services_CMH$FY.18<-NULL
Services_CMH$FY.19<-NULL
Services_CMH$FY.20<-NULL
Services_CMH$FY.21<-NULL
Services_CMH$CMHSP.1<-NULL
Services_CMH$CMHSP.2<-NULL
Services_CMH$CMHSP.3<-NULL
Services_CMH$CMHSP.4<-NULL
Services_CMH$CMHSP.5<-NULL
Services_CMH$CMHSP.6<-NULL
Services_CMH$CMHSP.7<-NULL
Services_CMH$CMHSP.8<-NULL
Services_CMH$CMHSP.9<-NULL
Services_CMH$CMHSP.10<-NULL
Services_CMH$CMHSP.11<-NULL
Services_CMH$CMHSP.12<-NULL
Services_CMH$CMHSP.13<-NULL
Services_CMH$CMHSP.14<-NULL
Services_CMH$CMHSP.15<-NULL
Services_CMH$CMHSP.16<-NULL
Services_CMH$CMHSP.17<-NULL
Services_CMH$CMHSP.18<-NULL
Services_CMH$CMHSP.19<-NULL
Services_CMH$CMHSP.20<-NULL
Services_CMH$CMHSP.21<-NULL
Services_CMH$Service.1<-NULL
Services_CMH$Service.2<-NULL
Services_CMH$Service.3<-NULL
Services_CMH$Service.4<-NULL
Services_CMH$Service.5<-NULL
Services_CMH$Service.6<-NULL
Services_CMH$Service.7<-NULL
Services_CMH$Service.8<-NULL
Services_CMH$Service.9<-NULL
Services_CMH$Service.10<-NULL
Services_CMH$Service.11<-NULL
Services_CMH$Service.12<-NULL
Services_CMH$Service.13<-NULL
Services_CMH$Service.14<-NULL
Services_CMH$Service.15<-NULL
Services_CMH$Service.16<-NULL
Services_CMH$Service.17<-NULL
Services_CMH$Service.18<-NULL
Services_CMH$Service.19<-NULL
Services_CMH$Service.20<-NULL
Services_CMH$Service.21<-NULL
Services_CMH$PIHP.1<-NULL
Services_CMH$PIHP.2<-NULL
Services_CMH$PIHP.3<-NULL
Services_CMH$PIHP.4<-NULL
Services_CMH$PIHP.5<-NULL
Services_CMH$PIHP.6<-NULL
Services_CMH$PIHP.7<-NULL
Services_CMH$PIHP.8<-NULL
Services_CMH$PIHP.9<-NULL
Services_CMH$PIHP.10<-NULL
Services_CMH$PIHP.11<-NULL
Services_CMH$PIHP.12<-NULL
Services_CMH$PIHP.13<-NULL
Services_CMH$PIHP.14<-NULL
Services_CMH$PIHP.15<-NULL
Services_CMH$PIHP.16<-NULL
Services_CMH$PIHP.17<-NULL
Services_CMH$PIHP.18<-NULL
Services_CMH$PIHP.19<-NULL
Services_CMH$PIHP.20<-NULL
Services_CMH$PIHP.21<-NULL

#Output Services_CMH .csv file
#write.csv(Services_CMH, file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\Services_CMH")

#########################################################
#Repeating above processes for each population ##########

#########
## MIC ##
#########

MIC_Test<-ddply(MIC, .(FY, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))
#Creating data frames for every service and then merging into 1 df

#Dental is not included because all values are empty
#DentalTest<-data.frame(subset(MIC_Test, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(MIC_Test, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(MIC_Test, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(MIC_Test, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(MIC_Test, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(MIC_Test, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(MIC_Test, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(MIC_Test, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(MIC_Test, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(MIC_Test, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(MIC_Test, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(MIC_Test, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(MIC_Test, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(MIC_Test, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(MIC_Test, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(MIC_Test, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(MIC_Test, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(MIC_Test, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(MIC_Test, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(MIC_Test, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(MIC_Test, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(MIC_Test, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_MIC_PIHP<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                              OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                              SUD, Behavioral, Medication, Outpatient, Respite,
                              Peer, Screening, Ancillary, Family, Transportation,
                              Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_MIC_PIHP)

#Will keep first FY and PIHP, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_MIC_PIHP <- rename(Services_MIC_PIHP, my_changes)

#Removing FY, Service, and PIHP duplicates
Services_MIC_PIHP$Service<-NULL
Services_MIC_PIHP$FY.1<-NULL
Services_MIC_PIHP$FY.2<-NULL
Services_MIC_PIHP$FY.3<-NULL
Services_MIC_PIHP$FY.4<-NULL
Services_MIC_PIHP$FY.5<-NULL
Services_MIC_PIHP$FY.6<-NULL
Services_MIC_PIHP$FY.7<-NULL
Services_MIC_PIHP$FY.8<-NULL
Services_MIC_PIHP$FY.9<-NULL
Services_MIC_PIHP$FY.10<-NULL
Services_MIC_PIHP$FY.11<-NULL
Services_MIC_PIHP$FY.12<-NULL
Services_MIC_PIHP$FY.13<-NULL
Services_MIC_PIHP$FY.14<-NULL
Services_MIC_PIHP$FY.15<-NULL
Services_MIC_PIHP$FY.16<-NULL
Services_MIC_PIHP$FY.17<-NULL
Services_MIC_PIHP$FY.18<-NULL
Services_MIC_PIHP$FY.19<-NULL
Services_MIC_PIHP$FY.20<-NULL
Services_MIC_PIHP$FY.21<-NULL
Services_MIC_PIHP$PIHP.1<-NULL
Services_MIC_PIHP$PIHP.2<-NULL
Services_MIC_PIHP$PIHP.3<-NULL
Services_MIC_PIHP$PIHP.4<-NULL
Services_MIC_PIHP$PIHP.5<-NULL
Services_MIC_PIHP$PIHP.6<-NULL
Services_MIC_PIHP$PIHP.7<-NULL
Services_MIC_PIHP$PIHP.8<-NULL
Services_MIC_PIHP$PIHP.9<-NULL
Services_MIC_PIHP$PIHP.10<-NULL
Services_MIC_PIHP$PIHP.11<-NULL
Services_MIC_PIHP$PIHP.12<-NULL
Services_MIC_PIHP$PIHP.13<-NULL
Services_MIC_PIHP$PIHP.14<-NULL
Services_MIC_PIHP$PIHP.15<-NULL
Services_MIC_PIHP$PIHP.16<-NULL
Services_MIC_PIHP$PIHP.17<-NULL
Services_MIC_PIHP$PIHP.18<-NULL
Services_MIC_PIHP$PIHP.19<-NULL
Services_MIC_PIHP$PIHP.20<-NULL
Services_MIC_PIHP$PIHP.21<-NULL
Services_MIC_PIHP$Service.1<-NULL
Services_MIC_PIHP$Service.2<-NULL
Services_MIC_PIHP$Service.3<-NULL
Services_MIC_PIHP$Service.4<-NULL
Services_MIC_PIHP$Service.5<-NULL
Services_MIC_PIHP$Service.6<-NULL
Services_MIC_PIHP$Service.7<-NULL
Services_MIC_PIHP$Service.8<-NULL
Services_MIC_PIHP$Service.9<-NULL
Services_MIC_PIHP$Service.10<-NULL
Services_MIC_PIHP$Service.11<-NULL
Services_MIC_PIHP$Service.12<-NULL
Services_MIC_PIHP$Service.13<-NULL
Services_MIC_PIHP$Service.14<-NULL
Services_MIC_PIHP$Service.15<-NULL
Services_MIC_PIHP$Service.16<-NULL
Services_MIC_PIHP$Service.17<-NULL
Services_MIC_PIHP$Service.18<-NULL
Services_MIC_PIHP$Service.19<-NULL
Services_MIC_PIHP$Service.20<-NULL
Services_MIC_PIHP$Service.21<-NULL

## MIC CMH ##

MIC_CMH2<-ddply(MIC, .(FY, CMHSP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

#Dental is not included because all values are empty
#DentalTest<-data.frame(subset(MIC_CMH2, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(MIC_CMH2, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(MIC_CMH2, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(MIC_CMH2, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(MIC_CMH2, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(MIC_CMH2, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(MIC_CMH2, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(MIC_CMH2, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(MIC_CMH2, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(MIC_CMH2, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(MIC_CMH2, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(MIC_CMH2, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(MIC_CMH2, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(MIC_CMH2, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(MIC_CMH2, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(MIC_CMH2, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(MIC_CMH2, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(MIC_CMH2, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(MIC_CMH2, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(MIC_CMH2, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(MIC_CMH2, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(MIC_CMH2, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_MIC_CMH<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                             OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                             SUD, Behavioral, Medication, Outpatient, Respite,
                             Peer, Screening, Ancillary, Family, Transportation,
                             Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_MIC_CMH)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")
Services_MIC_CMH <- rename(Services_MIC_CMH, my_changes)

#Removing FY, Service, and CMH duplicates
Services_MIC_CMH$Service<-NULL
Services_MIC_CMH$FY.1<-NULL
Services_MIC_CMH$FY.2<-NULL
Services_MIC_CMH$FY.3<-NULL
Services_MIC_CMH$FY.4<-NULL
Services_MIC_CMH$FY.5<-NULL
Services_MIC_CMH$FY.6<-NULL
Services_MIC_CMH$FY.7<-NULL
Services_MIC_CMH$FY.8<-NULL
Services_MIC_CMH$FY.9<-NULL
Services_MIC_CMH$FY.10<-NULL
Services_MIC_CMH$FY.11<-NULL
Services_MIC_CMH$FY.12<-NULL
Services_MIC_CMH$FY.13<-NULL
Services_MIC_CMH$FY.14<-NULL
Services_MIC_CMH$FY.15<-NULL
Services_MIC_CMH$FY.16<-NULL
Services_MIC_CMH$FY.17<-NULL
Services_MIC_CMH$FY.18<-NULL
Services_MIC_CMH$FY.19<-NULL
Services_MIC_CMH$FY.20<-NULL
Services_MIC_CMH$FY.21<-NULL
Services_MIC_CMH$CMHSP.1<-NULL
Services_MIC_CMH$CMHSP.2<-NULL
Services_MIC_CMH$CMHSP.3<-NULL
Services_MIC_CMH$CMHSP.4<-NULL
Services_MIC_CMH$CMHSP.5<-NULL
Services_MIC_CMH$CMHSP.6<-NULL
Services_MIC_CMH$CMHSP.7<-NULL
Services_MIC_CMH$CMHSP.8<-NULL
Services_MIC_CMH$CMHSP.9<-NULL
Services_MIC_CMH$CMHSP.10<-NULL
Services_MIC_CMH$CMHSP.11<-NULL
Services_MIC_CMH$CMHSP.12<-NULL
Services_MIC_CMH$CMHSP.13<-NULL
Services_MIC_CMH$CMHSP.14<-NULL
Services_MIC_CMH$CMHSP.15<-NULL
Services_MIC_CMH$CMHSP.16<-NULL
Services_MIC_CMH$CMHSP.17<-NULL
Services_MIC_CMH$CMHSP.18<-NULL
Services_MIC_CMH$CMHSP.19<-NULL
Services_MIC_CMH$CMHSP.20<-NULL
Services_MIC_CMH$CMHSP.21<-NULL
Services_MIC_CMH$Service.1<-NULL
Services_MIC_CMH$Service.2<-NULL
Services_MIC_CMH$Service.3<-NULL
Services_MIC_CMH$Service.4<-NULL
Services_MIC_CMH$Service.5<-NULL
Services_MIC_CMH$Service.6<-NULL
Services_MIC_CMH$Service.7<-NULL
Services_MIC_CMH$Service.8<-NULL
Services_MIC_CMH$Service.9<-NULL
Services_MIC_CMH$Service.10<-NULL
Services_MIC_CMH$Service.11<-NULL
Services_MIC_CMH$Service.12<-NULL
Services_MIC_CMH$Service.13<-NULL
Services_MIC_CMH$Service.14<-NULL
Services_MIC_CMH$Service.15<-NULL
Services_MIC_CMH$Service.16<-NULL
Services_MIC_CMH$Service.17<-NULL
Services_MIC_CMH$Service.18<-NULL
Services_MIC_CMH$Service.19<-NULL
Services_MIC_CMH$Service.20<-NULL
Services_MIC_CMH$Service.21<-NULL

#########
## MIA ##
#########

MIA_PIHP_Test<-ddply(MIA, .(FY, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                     SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                     Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                     CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Only PIHP 8 uses Dental codes, only though 2009
#MIADental<-data.frame(subset(MIA_PIHP_Test, Service == "Dental", select=c(1:9)))

#Creating data frames for every service and then merging into 1 df
LocalHosp<-data.frame(subset(MIA_PIHP_Test, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(MIA_PIHP_Test, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(MIA_PIHP_Test, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(MIA_PIHP_Test, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(MIA_PIHP_Test, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(MIA_PIHP_Test, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(MIA_PIHP_Test, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(MIA_PIHP_Test, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(MIA_PIHP_Test, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(MIA_PIHP_Test, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(MIA_PIHP_Test, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(MIA_PIHP_Test, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(MIA_PIHP_Test, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(MIA_PIHP_Test, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(MIA_PIHP_Test, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(MIA_PIHP_Test, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(MIA_PIHP_Test, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(MIA_PIHP_Test, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(MIA_PIHP_Test, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(MIA_PIHP_Test, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(MIA_PIHP_Test, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_MIA_PIHP<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                              OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                              SUD, Behavioral, Medication, Outpatient, Respite,
                              Peer, Screening, Ancillary, Family, Transportation,
                              Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_MIA_PIHP)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_MIA_PIHP <- rename(Services_MIA_PIHP, my_changes)

#Removing FY, Service, and CMH duplicates
Services_MIA_PIHP$Service<-NULL
Services_MIA_PIHP$FY.1<-NULL
Services_MIA_PIHP$FY.2<-NULL
Services_MIA_PIHP$FY.3<-NULL
Services_MIA_PIHP$FY.4<-NULL
Services_MIA_PIHP$FY.5<-NULL
Services_MIA_PIHP$FY.6<-NULL
Services_MIA_PIHP$FY.7<-NULL
Services_MIA_PIHP$FY.8<-NULL
Services_MIA_PIHP$FY.9<-NULL
Services_MIA_PIHP$FY.10<-NULL
Services_MIA_PIHP$FY.11<-NULL
Services_MIA_PIHP$FY.12<-NULL
Services_MIA_PIHP$FY.13<-NULL
Services_MIA_PIHP$FY.14<-NULL
Services_MIA_PIHP$FY.15<-NULL
Services_MIA_PIHP$FY.16<-NULL
Services_MIA_PIHP$FY.17<-NULL
Services_MIA_PIHP$FY.18<-NULL
Services_MIA_PIHP$FY.19<-NULL
Services_MIA_PIHP$FY.20<-NULL
Services_MIA_PIHP$FY.21<-NULL
Services_MIA_PIHP$PIHP.1<-NULL
Services_MIA_PIHP$PIHP.2<-NULL
Services_MIA_PIHP$PIHP.3<-NULL
Services_MIA_PIHP$PIHP.4<-NULL
Services_MIA_PIHP$PIHP.5<-NULL
Services_MIA_PIHP$PIHP.6<-NULL
Services_MIA_PIHP$PIHP.7<-NULL
Services_MIA_PIHP$PIHP.8<-NULL
Services_MIA_PIHP$PIHP.9<-NULL
Services_MIA_PIHP$PIHP.10<-NULL
Services_MIA_PIHP$PIHP.11<-NULL
Services_MIA_PIHP$PIHP.12<-NULL
Services_MIA_PIHP$PIHP.13<-NULL
Services_MIA_PIHP$PIHP.14<-NULL
Services_MIA_PIHP$PIHP.15<-NULL
Services_MIA_PIHP$PIHP.16<-NULL
Services_MIA_PIHP$PIHP.17<-NULL
Services_MIA_PIHP$PIHP.18<-NULL
Services_MIA_PIHP$PIHP.19<-NULL
Services_MIA_PIHP$PIHP.20<-NULL
Services_MIA_PIHP$PIHP.21<-NULL
Services_MIA_PIHP$Service.1<-NULL
Services_MIA_PIHP$Service.2<-NULL
Services_MIA_PIHP$Service.3<-NULL
Services_MIA_PIHP$Service.4<-NULL
Services_MIA_PIHP$Service.5<-NULL
Services_MIA_PIHP$Service.6<-NULL
Services_MIA_PIHP$Service.7<-NULL
Services_MIA_PIHP$Service.8<-NULL
Services_MIA_PIHP$Service.9<-NULL
Services_MIA_PIHP$Service.10<-NULL
Services_MIA_PIHP$Service.11<-NULL
Services_MIA_PIHP$Service.12<-NULL
Services_MIA_PIHP$Service.13<-NULL
Services_MIA_PIHP$Service.14<-NULL
Services_MIA_PIHP$Service.15<-NULL
Services_MIA_PIHP$Service.16<-NULL
Services_MIA_PIHP$Service.17<-NULL
Services_MIA_PIHP$Service.18<-NULL
Services_MIA_PIHP$Service.19<-NULL
Services_MIA_PIHP$Service.20<-NULL
Services_MIA_PIHP$Service.21<-NULL

## MIA CMH ##

MIA_CMH2<-ddply(MIA, .(FY, CMHSP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

#Dental is not included because all values are empty
#DentalTest<-data.frame(subset(MIC_CMH2, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(MIA_CMH2, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(MIA_CMH2, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(MIA_CMH2, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(MIA_CMH2, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(MIA_CMH2, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(MIA_CMH2, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(MIA_CMH2, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(MIA_CMH2, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(MIA_CMH2, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(MIA_CMH2, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(MIA_CMH2, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(MIA_CMH2, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(MIA_CMH2, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(MIA_CMH2, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(MIA_CMH2, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(MIA_CMH2, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(MIA_CMH2, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(MIA_CMH2, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(MIA_CMH2, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(MIA_CMH2, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(MIA_CMH2, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_MIA_CMH<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                             OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                             SUD, Behavioral, Medication, Outpatient, Respite,
                             Peer, Screening, Ancillary, Family, Transportation,
                             Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_MIA_CMH)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_MIA_CMH <- rename(Services_MIA_CMH, my_changes)

#Removing FY, Service, and CMH duplicates
Services_MIA_CMH$Service<-NULL
Services_MIA_CMH$FY.1<-NULL
Services_MIA_CMH$FY.2<-NULL
Services_MIA_CMH$FY.3<-NULL
Services_MIA_CMH$FY.4<-NULL
Services_MIA_CMH$FY.5<-NULL
Services_MIA_CMH$FY.6<-NULL
Services_MIA_CMH$FY.7<-NULL
Services_MIA_CMH$FY.8<-NULL
Services_MIA_CMH$FY.9<-NULL
Services_MIA_CMH$FY.10<-NULL
Services_MIA_CMH$FY.11<-NULL
Services_MIA_CMH$FY.12<-NULL
Services_MIA_CMH$FY.13<-NULL
Services_MIA_CMH$FY.14<-NULL
Services_MIA_CMH$FY.15<-NULL
Services_MIA_CMH$FY.16<-NULL
Services_MIA_CMH$FY.17<-NULL
Services_MIA_CMH$FY.18<-NULL
Services_MIA_CMH$FY.19<-NULL
Services_MIA_CMH$FY.20<-NULL
Services_MIA_CMH$FY.21<-NULL
Services_MIA_CMH$CMHSP.1<-NULL
Services_MIA_CMH$CMHSP.2<-NULL
Services_MIA_CMH$CMHSP.3<-NULL
Services_MIA_CMH$CMHSP.4<-NULL
Services_MIA_CMH$CMHSP.5<-NULL
Services_MIA_CMH$CMHSP.6<-NULL
Services_MIA_CMH$CMHSP.7<-NULL
Services_MIA_CMH$CMHSP.8<-NULL
Services_MIA_CMH$CMHSP.9<-NULL
Services_MIA_CMH$CMHSP.10<-NULL
Services_MIA_CMH$CMHSP.11<-NULL
Services_MIA_CMH$CMHSP.12<-NULL
Services_MIA_CMH$CMHSP.13<-NULL
Services_MIA_CMH$CMHSP.14<-NULL
Services_MIA_CMH$CMHSP.15<-NULL
Services_MIA_CMH$CMHSP.16<-NULL
Services_MIA_CMH$CMHSP.17<-NULL
Services_MIA_CMH$CMHSP.18<-NULL
Services_MIA_CMH$CMHSP.19<-NULL
Services_MIA_CMH$CMHSP.20<-NULL
Services_MIA_CMH$CMHSP.21<-NULL
Services_MIA_CMH$Service.1<-NULL
Services_MIA_CMH$Service.2<-NULL
Services_MIA_CMH$Service.3<-NULL
Services_MIA_CMH$Service.4<-NULL
Services_MIA_CMH$Service.5<-NULL
Services_MIA_CMH$Service.6<-NULL
Services_MIA_CMH$Service.7<-NULL
Services_MIA_CMH$Service.8<-NULL
Services_MIA_CMH$Service.9<-NULL
Services_MIA_CMH$Service.10<-NULL
Services_MIA_CMH$Service.11<-NULL
Services_MIA_CMH$Service.12<-NULL
Services_MIA_CMH$Service.13<-NULL
Services_MIA_CMH$Service.14<-NULL
Services_MIA_CMH$Service.15<-NULL
Services_MIA_CMH$Service.16<-NULL
Services_MIA_CMH$Service.17<-NULL
Services_MIA_CMH$Service.18<-NULL
Services_MIA_CMH$Service.19<-NULL
Services_MIA_CMH$Service.20<-NULL
Services_MIA_CMH$Service.21<-NULL


########
## DD ##
########

DD_CMH2<-ddply(DD, .(FY, CMHSP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

#Dental is not included because values only go through 2009
#DentalTest<-data.frame(subset(DD_CMH2, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(DD_CMH2, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(DD_CMH2, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(DD_CMH2, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(DD_CMH2, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(DD_CMH2, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(DD_CMH2, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(DD_CMH2, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(DD_CMH2, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(DD_CMH2, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(DD_CMH2, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(DD_CMH2, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(DD_CMH2, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(DD_CMH2, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(DD_CMH2, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(DD_CMH2, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(DD_CMH2, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(DD_CMH2, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(DD_CMH2, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(DD_CMH2, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(DD_CMH2, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(DD_CMH2, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_DD_CMH<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                            OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                            SUD, Behavioral, Medication, Outpatient, Respite,
                            Peer, Screening, Ancillary, Family, Transportation,
                            Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_DD_CMH)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_DD_CMH <- rename(Services_DD_CMH, my_changes)

#Removing FY, Service, and CMH duplicates
Services_DD_CMH$Service<-NULL
Services_DD_CMH$FY.1<-NULL
Services_DD_CMH$FY.2<-NULL
Services_DD_CMH$FY.3<-NULL
Services_DD_CMH$FY.4<-NULL
Services_DD_CMH$FY.5<-NULL
Services_DD_CMH$FY.6<-NULL
Services_DD_CMH$FY.7<-NULL
Services_DD_CMH$FY.8<-NULL
Services_DD_CMH$FY.9<-NULL
Services_DD_CMH$FY.10<-NULL
Services_DD_CMH$FY.11<-NULL
Services_DD_CMH$FY.12<-NULL
Services_DD_CMH$FY.13<-NULL
Services_DD_CMH$FY.14<-NULL
Services_DD_CMH$FY.15<-NULL
Services_DD_CMH$FY.16<-NULL
Services_DD_CMH$FY.17<-NULL
Services_DD_CMH$FY.18<-NULL
Services_DD_CMH$FY.19<-NULL
Services_DD_CMH$FY.20<-NULL
Services_DD_CMH$FY.21<-NULL
Services_DD_CMH$CMHSP.1<-NULL
Services_DD_CMH$CMHSP.2<-NULL
Services_DD_CMH$CMHSP.3<-NULL
Services_DD_CMH$CMHSP.4<-NULL
Services_DD_CMH$CMHSP.5<-NULL
Services_DD_CMH$CMHSP.6<-NULL
Services_DD_CMH$CMHSP.7<-NULL
Services_DD_CMH$CMHSP.8<-NULL
Services_DD_CMH$CMHSP.9<-NULL
Services_DD_CMH$CMHSP.10<-NULL
Services_DD_CMH$CMHSP.11<-NULL
Services_DD_CMH$CMHSP.12<-NULL
Services_DD_CMH$CMHSP.13<-NULL
Services_DD_CMH$CMHSP.14<-NULL
Services_DD_CMH$CMHSP.15<-NULL
Services_DD_CMH$CMHSP.16<-NULL
Services_DD_CMH$CMHSP.17<-NULL
Services_DD_CMH$CMHSP.18<-NULL
Services_DD_CMH$CMHSP.19<-NULL
Services_DD_CMH$CMHSP.20<-NULL
Services_DD_CMH$CMHSP.21<-NULL
Services_DD_CMH$Service.1<-NULL
Services_DD_CMH$Service.2<-NULL
Services_DD_CMH$Service.3<-NULL
Services_DD_CMH$Service.4<-NULL
Services_DD_CMH$Service.5<-NULL
Services_DD_CMH$Service.6<-NULL
Services_DD_CMH$Service.7<-NULL
Services_DD_CMH$Service.8<-NULL
Services_DD_CMH$Service.9<-NULL
Services_DD_CMH$Service.10<-NULL
Services_DD_CMH$Service.11<-NULL
Services_DD_CMH$Service.12<-NULL
Services_DD_CMH$Service.13<-NULL
Services_DD_CMH$Service.14<-NULL
Services_DD_CMH$Service.15<-NULL
Services_DD_CMH$Service.16<-NULL
Services_DD_CMH$Service.17<-NULL
Services_DD_CMH$Service.18<-NULL
Services_DD_CMH$Service.19<-NULL
Services_DD_CMH$Service.20<-NULL
Services_DD_CMH$Service.21<-NULL

## DD PIHP ##

DD_PIHP2<-ddply(DD, .(FY, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

LocalHosp<-data.frame(subset(DD_PIHP2, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(DD_PIHP2, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(DD_PIHP2, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(DD_PIHP2, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(DD_PIHP2, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(DD_PIHP2, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(DD_PIHP2, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(DD_PIHP2, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(DD_PIHP2, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(DD_PIHP2, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(DD_PIHP2, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(DD_PIHP2, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(DD_PIHP2, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(DD_PIHP2, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(DD_PIHP2, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(DD_PIHP2, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(DD_PIHP2, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(DD_PIHP2, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(DD_PIHP2, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(DD_PIHP2, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(DD_PIHP2, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_DD_PIHP<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                             OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                             SUD, Behavioral, Medication, Outpatient, Respite,
                             Peer, Screening, Ancillary, Family, Transportation,
                             Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_DD_PIHP)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_DD_PIHP <- rename(Services_DD_PIHP, my_changes)

#Removing FY, Service, and CMH duplicates
Services_DD_PIHP$Service<-NULL
Services_DD_PIHP$FY.1<-NULL
Services_DD_PIHP$FY.2<-NULL
Services_DD_PIHP$FY.3<-NULL
Services_DD_PIHP$FY.4<-NULL
Services_DD_PIHP$FY.5<-NULL
Services_DD_PIHP$FY.6<-NULL
Services_DD_PIHP$FY.7<-NULL
Services_DD_PIHP$FY.8<-NULL
Services_DD_PIHP$FY.9<-NULL
Services_DD_PIHP$FY.10<-NULL
Services_DD_PIHP$FY.11<-NULL
Services_DD_PIHP$FY.12<-NULL
Services_DD_PIHP$FY.13<-NULL
Services_DD_PIHP$FY.14<-NULL
Services_DD_PIHP$FY.15<-NULL
Services_DD_PIHP$FY.16<-NULL
Services_DD_PIHP$FY.17<-NULL
Services_DD_PIHP$FY.18<-NULL
Services_DD_PIHP$FY.19<-NULL
Services_DD_PIHP$FY.20<-NULL
Services_DD_PIHP$FY.21<-NULL
Services_DD_PIHP$PIHP.1<-NULL
Services_DD_PIHP$PIHP.2<-NULL
Services_DD_PIHP$PIHP.3<-NULL
Services_DD_PIHP$PIHP.4<-NULL
Services_DD_PIHP$PIHP.5<-NULL
Services_DD_PIHP$PIHP.6<-NULL
Services_DD_PIHP$PIHP.7<-NULL
Services_DD_PIHP$PIHP.8<-NULL
Services_DD_PIHP$PIHP.9<-NULL
Services_DD_PIHP$PIHP.10<-NULL
Services_DD_PIHP$PIHP.11<-NULL
Services_DD_PIHP$PIHP.12<-NULL
Services_DD_PIHP$PIHP.13<-NULL
Services_DD_PIHP$PIHP.14<-NULL
Services_DD_PIHP$PIHP.15<-NULL
Services_DD_PIHP$PIHP.16<-NULL
Services_DD_PIHP$PIHP.17<-NULL
Services_DD_PIHP$PIHP.18<-NULL
Services_DD_PIHP$PIHP.19<-NULL
Services_DD_PIHP$PIHP.20<-NULL
Services_DD_PIHP$PIHP.21<-NULL
Services_DD_PIHP$Service.1<-NULL
Services_DD_PIHP$Service.2<-NULL
Services_DD_PIHP$Service.3<-NULL
Services_DD_PIHP$Service.4<-NULL
Services_DD_PIHP$Service.5<-NULL
Services_DD_PIHP$Service.6<-NULL
Services_DD_PIHP$Service.7<-NULL
Services_DD_PIHP$Service.8<-NULL
Services_DD_PIHP$Service.9<-NULL
Services_DD_PIHP$Service.10<-NULL
Services_DD_PIHP$Service.11<-NULL
Services_DD_PIHP$Service.12<-NULL
Services_DD_PIHP$Service.13<-NULL
Services_DD_PIHP$Service.14<-NULL
Services_DD_PIHP$Service.15<-NULL
Services_DD_PIHP$Service.16<-NULL
Services_DD_PIHP$Service.17<-NULL
Services_DD_PIHP$Service.18<-NULL
Services_DD_PIHP$Service.19<-NULL
Services_DD_PIHP$Service.20<-NULL
Services_DD_PIHP$Service.21<-NULL