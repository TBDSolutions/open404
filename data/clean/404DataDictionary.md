---
title: "404DataDictionary"
author: "J. Hagedorn"
output: html_document
---

Each observation (row) pertains to a specific service (Code_Mod), broken down by CMHSP and Population.

### "FY"  
The fiscal year in which the reported services were submitted as claims.

### "PIHP" 
The number of the region corresponding to the prepaid inpatient health plan (PIHP), as organized for the 2013 AFP.  A map of the regions [can be found here](http://www.chrt.org/assets/policy-papers/community-mental-health-images/appendix-d.png).  It should be noted that not all of these PIHPs oversaw the management of services prior to CY 2014, and that this field should thus be used to group the historical data for CMHSPs that are currently in the regions.

### "PIHPname"
The name of the prepaid inpatient health plan (PIHP).

### "CMHSP" 
The name of the community mental health service program (CMHSP).

### "Population" 
The designation of disability type for which the documented service was provided.  There are 3 disability types included in this dataset: *MIA = Mentally Ill Adults, MIC = Mentally Ill Children, DD = Developmentally Disabled (Adults and Children).*

### "ServiceType"  
High level groupings of services into the following overall types: *"Care Coordination",             "Crisis and Respite", "Employment Services", "Equipment", "Home & Community Based Services", "Hospital-based Services", "Medication", "Other", "Outpatient Treatment", "Physical Health Services", "Screening & Assessment", "Transportation"*

### "Service"
Lower level groupings of services into multiple types, each falling into one of the "ServiceType" groupings.

### "Description" 
The name of the service, taken from the MDCH-generated 404 report.

### "Code"
The CPT or HCPCS code for the service described in "Description".  For services which only have revenue codes, these are used.

### "Code_Mod"
A concatenated field of "Code" and "Modifier."  Because there are multiple modifiers for a single code, "Code_Mod" is the most granular specification of the type of service provided.

### "Unit_Hours"
A multiplier to convert all time-based units into a common measurement.  For example, units which are of the unit type "hour" have a multiplier of 1, while units of the unit type "15 minutes" have a multiplier of 4.

### "SumOfCases" 
The number of unique people who received the service.

### "SumOfUnits"   
The total number of units of the service which were provided.

### "SumOfCost"    
The total cost of the service.

### "CostPerCase"  
The average cost of the service for all people who received it.

### "CostPerUnit"  
The average cost for a single unit of the service.

### "UnitPerCase"    
The average number of units for each person who received the service.

### "Unit_Perc_Tot"  
The annual number of units service as a % of the total annual units provided of all services, per CMHSP/Population.

### "Cost_Perc_Tot"  
The annual cost of the service as a % of the total annual cost of all services, per CMHSP/Population.

### "Cost1kSvd"     
Cost per 1,000 people served by the CMHSP.  Uses the general formula *(Sum Of Cost/Unique Persons Served by CMHSP) x 1000*

### "Perc_Svd"
Percentage of people served by the CMHSP who received this service (per year/population).

## 2010 to 2013 files only 

These two variables are calculated using the American Community (ACS) survey data from the Census API, which only contains 5-year estimates going back to 2010.

### "Cost1kPop"
The cost of the service per 1,000 members of the general population.

### "Srvd1kPop"
The number of people receiving the service per 1,000 members of the general population.

# Other references:
[MDCH 404 site](http://www.michigan.gov/mdch/0,4612,7-132-2941_4868_4902-256889--,00.html)
