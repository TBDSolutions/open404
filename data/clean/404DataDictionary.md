---
title: "904/404 Data Dictionary"
author: "J. Hagedorn"
output: html_document
---

Each observation (i.e. row) in the `df_404` dataset pertains to a specific service (combination of `code` and `modifier`), for each year when that service was provided by a given `cmhsp` to beneficiaries from a specific `population`.

## Definition of fields

The fields (i.e. columns) in this dataset are defined as follows:

- `fy`: The fiscal year in which the reported services were submitted as claims.
- `pihp`: The number of the region corresponding to the prepaid inpatient health plan (PIHP) to which a `cmhsp` belongs, as organized for the 2013 AFP.  A map of the regions [can be found here](http://www.chrt.org/assets/policy-papers/community-mental-health-images/appendix-d.png).
- `pihp_name`: The name of the prepaid inpatient health plan (PIHP).
- `cmhsp`: The name of the community mental health service program (CMHSP) which provided or contracted for the service reported.
- `population`: The disability type of beneficiaries who received the service.  There are 3 disability types included in this dataset: *MIA* = Mentally Ill Adults, *MIC* = Mentally Ill Children, *DD* = Developmentally Disabled (Adults and Children).
- `svc_type`: High level designation of types of services into the following overall types: *"Behavioral Treatment","Coordination and Planning","Crisis Services","Employment Services","Equipment/Supplies","Health Services","Home & Community Based Services","Hospital Based Services","Outpatient Treatment","Psychiatric and Medication Services","Respite","Screening & Assessment","Family Education and Support","Intensive Community-Based Treatment","Peer Services","Transportation","Other","Sub-acute Withdrawal Management","Telemedicine"*
- `svc_grp`: Groupings of HCPCS and/or revenue codes into multiple types, each falling into only one of the `svc_type` groupings.
- `code`: The CPT or HCPCS code for the service reported.  For services which only have revenue codes, these are used.
- `short_description`: A shortened description of the `code`, drawn from the official CMS documentation where available,^[See option "Current LCDs" from CMS site [here](https://www.cms.gov/medicare-coverage-database/downloads/downloadable-databases.aspx)] or else from 904 data descriptions.  
- `modifier`: If applicable, the modifier for the `code` field.  The combination of `code` and `modifier` is the most granular specification of the type of service provided.
- `unit_type`:  The unit of measurement for the `code`.
- `unit_hrs`: Conversion factor for all time-based units to be coerced into a common measurement.  For example, units which are of the `unit_type` "hour" have a multiplier of 1, while units of the unit type "15 minutes" have a multiplier of 0.25.
- `cases`: The number of distinct people who received the service.  *Note that this field cannot be aggregated across multiple `code`s, since beneficiaries often receive more than a single service.*
- `units`: The total number of units of the service which were provided. *Note that this field cannot be aggregated across multiple `code`s, if those `code`s do not share the same `unit_type`.*
- `cost`: The total cost of the service.
- `cost_per_case`: The average cost of the service for all people who received it.
- `cost_per_unit`: The average cost for a single unit of the service.
- `unit_per_case`: The average number of units for each person who received the service.
- `cost_pct_tot`: The annual cost of the service as a % of the total annual cost of all services, per `cmhsp` and `population`.
- `cost_1k_served`: Cost per 1,000 people served by the CMHSP.  Uses the general formula *(Sum Of Cost/Unique Persons Served by CMHSP) x 1000*
- `pct_cmh_served`: Percentage of people served by the CMHSP who received this service (per `fy`).

## Caveats

The following considerations should be kept in mind when interpreting this data:

- It should be noted that not all of the PIHPs oversaw the management of services prior to CY 2014, and that this field should thus be used to group the historical data for CMHSPs that are currently in the regions, rather than to erroneously attribute historical performance to those entities.
- The CMHSP "Clinton Eaton Ingham" did not submit their cost and service use data for fiscal year 2014.  Since services with values of zero for cost, units and cases are excluded from the Master dataset, this CMHSP does not show up in 2014.
- In some years, CMHSPs reported the same HCPCS code and modifier on multiple lines with a slightly different service description.  In these instances, `cases`, `units` and `costs` are assumed to be non-duplicative and are summed.

# Other references:
[MDHHS 904/404 site](http://www.michigan.gov/mdch/0,4612,7-132-2941_4868_4902-256889--,00.html)
