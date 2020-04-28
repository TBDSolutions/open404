The following documentation pertains to the sub-element cost report
(SECR) of the section 404/904 legislative boilerplate reports, a set of
publicly available data reporting the service use and related costs for
the public behavioral health system in Michigan.

Level of Aggregation
====================

Each observation (i.e. row) in the `df_404` dataset pertains to a
specific service (combination of `code` and `modifier`), for each year
(`fy`) when that service was provided by a given `cmhsp` to
beneficiaries from a specific `population`.

Definitions
===========

The following variables are included in this dataset:

-   `fy`: The fiscal year in which the reported services were submitted
    as claims.
-   `pihp`: The number of the region corresponding to the prepaid
    inpatient health plan (PIHP) to which a `cmhsp` belongs, as
    organized for the 2013 AFP. A map of the regions [can be found
    here](http://www.chrt.org/assets/policy-papers/community-mental-health-images/appendix-d.png).
-   `pihp_name`: The name of the prepaid inpatient health plan (PIHP).
-   `cmhsp`: The name of the community mental health service program
    (CMHSP) which provided or contracted for the service reported.
-   `population`: The disability type of beneficiaries who received the
    service. There are 3 disability types included in this dataset:
    *MIA* = Mentally Ill Adults, *MIC* = Mentally Ill Children, *DD* =
    Developmentally Disabled (Adults and Children).
-   `svc_type`: High level designation of types of services into the
    following overall types: *“Behavioral Treatment”,“Coordination and
    Planning”,“Crisis Services”,“Employment
    Services”,“Equipment/Supplies”,“Health Services”,“Home & Community
    Based Services”,“Hospital Based Services”,“Outpatient
    Treatment”,“Psychiatric and Medication
    Services”,“Respite”,“Screening & Assessment”,“Family Education and
    Support”,“Intensive Community-Based Treatment”,“Peer
    Services”,“Transportation”,“Other”,“Sub-acute Withdrawal
    Management”,“Telemedicine”*
-   `svc_grp`: Groupings of HCPCS and/or revenue codes into multiple
    types, each falling into only one of the `svc_type` groupings.
-   `code`: The CPT or HCPCS code for the service reported. For services
    which only have revenue codes, these are used.
-   `short_description`: A shortened description of the `code`, drawn
    from the official CMS documentation where available,[1] or else from
    904 data descriptions. For inpatient or other services using the
    UBREV codeset, these are included in this field.
-   `modifier`: If applicable, the modifier for the `code` field. The
    combination of `code` and `modifier` is the most granular
    specification of the type of service provided. For inpatient
    services using provider type codes, these are included in this
    field.
-   `unit_type`: The unit of measurement for the `code`.
-   `unit_hrs`: Conversion factor for all time-based units to be coerced
    into a common measurement. For example, units which are of the
    `unit_type` “hour” have a multiplier of 1, while units of the unit
    type “15 minutes” have a multiplier of 0.25.
-   `cases`: The number of distinct people who received the service.
    *Note that this field cannot be aggregated across multiple `code`s,
    since beneficiaries often receive more than a single service.*
-   `units`: The total number of units of the service which were
    provided. *Note that this field cannot be aggregated across multiple
    `code`s, if those `code`s do not share the same `unit_type`.*
-   `cost`: The total cost of the service.
-   `cost_per_case`: The average cost of the service for all people who
    received it.
-   `cost_per_unit`: The average cost for a single unit of the service.
-   `unit_per_case`: The average number of units for each person who
    received the service.
-   `cost_pct_tot`: The annual cost of the service as a % of the total
    annual cost of all services, per `cmhsp` and `population`.
-   `cost_1k_served`: Cost per 1,000 people served by the CMHSP. Uses
    the general formula *(Sum Of Cost/Unique Persons Served by CMHSP) x
    1000*
-   `pct_cmh_served`: Percentage of people served by the CMHSP who
    received this service (per `fy`).

Data Issues and Limitations
===========================

General Limitations of the Dataset
----------------------------------

As noted above, the only measures included in the sub-element cost
report (SECR) of the section 404/904 legislative boilerplate are the
following: `cases`, `units`, and `cost`. These values are reported
separately for each service code, modifier, CMHSP, population (MIA, MIC,
DD), and fiscal year (FY). A unique count of all individuals who
received services the CMHSP during each year is also provided by MDHHS,
though it is not broken out by population. The measures listed above are
then combined to produce the following derivative measures: cost per
case, cost per unit, unit per case, percent receiving service, cost per
10k served.

Assumptions and limitations for specific variables are ennumerated
below. These considerations apply generally for any use of the dataset:

**Cases:**

-   Cases are assumed to provide a distinct count of beneficiaries who
    received one or more units of the service code/modifier combination
    being reported.
-   As the number of cases is a distinct count for each service
    code/modifier combination, and beneficiaries may receive more than
    one service, it is not possible to combine the counts of multiple
    services.

**Units**

-   Units are assumed to be a count of all units of the service
    code/modifier combination being reported.
-   As units are of different types (e.g. per diem, 15 minutes, etc.),
    it is not possible to combine the units counts of multiple services.
-   While assumed to be uncommon, it is possible that a beneficiaries
    units are spread across CMHSPs or populations. If a beneficiary
    received a code/modifier combination from multiple CMHSPs (or from
    multiple populations within a single CMHSP) during a given year,
    that beneficiary’s units would be split among those CMHSPs or
    populations.

**Cost**

While cost is the only measure which might presumably be summed across
service code/modifier combinations, there are significant issues in the
data which lead us to advise caution in the interpretation of findings.
A 2019 report by the state’s actuary, Milliman, found that “…many of the
CMHSPs account for their unit service costs using different cost
accounting and allocation methods than those of their peers. Although…
consistent with generally accepted accounting principles, the
inconsistency in cost allocation methods does not provide a sound basis
for allowing comparisons to other CMHSPs…” (p. 2). It is reasonable to
assume that this finding applies retrospectively to the entire period
covered by the SECR dataset. The issues below are drawn from that
report:

-   “The accounting methods employed … to assign costs to service units
    in the MUNC and SECR are not consistent \[making it difficult\] to
    understand how much of the unit cost variation can…be attributable
    to other factors” (p. 3)
-   The use of “inconsistent definitions for direct service
    administration and other non-direct service administration” (p. 2)
    may lead different types of administrative activities which are
    included by one CMHSP to be excluded for another. Furthermore, the
    distribution of administrative or overhead costs across different
    service code/modifier combinations is likely to vary.
-   Additional factors which influence variation in unit costs cannot be
    ascertained from the data, due to the issues with accounting methods
    identified above: “(a) PIHP/CMHSP operating structures, (b)
    resources required to provide the similarly defined services (due to
    variations in client acuity or applied clinical methods), (c) costs
    of resources that can be attributed to PIHP/CMHSP and provider
    locations or regions, and (d) volume of services provided with no
    appreciable change in program costs” (p. 3)

**Cost per Unit**

-   All caveats related to both the `cost` and `units` measures
    separately should be taken into account when considering the use of
    the cost per unit measure, as this measure is derived from them.
-   In addition, users of this measure should note that this is an
    average derived by taking the sum of total cost, divided by the
    total count of units. It therefore includes a number of different
    unit costs for a given service code/modifier combination. These may
    vary within a given CMHSP’s reporting according to internal/external
    service provision, and may change based on provider contracts, and
    even by a contract for services to a specific beneficiary.
-   These differences may have a greater impact on the overall average
    for CMHSPs who serve a smaller number of cases using a given service
    code/modifier, since each case will have a stronger influence on the
    mean.

**Cost per Case**

-   All caveats related to both the `cost` and `cases` measures
    separately should be taken into account when considering the use of
    the cost per case measure, as this measure is derived from them.
-   In addition, users should bear in mind that the cost per case may be
    distributed over multiple “episodes” of service, if a person has
    left and returned to receive that particular service during the `fy`
    reporting timeframe. These episodes may have gaps between them and
    thus not reflect service use that is evenly distributed across the
    `fy`.
-   Individual cases may not receive a given service for the same amount
    of time during a reporting period. For instance, an individual who
    begins service at a CMHSP midway through the `fy` will have a longer
    period during which costs can be accumulated than a person who
    begins receiving services near the end of the `fy`, yet both may be
    included in the count of `cases`
-   While the report is an annual reporting of total costs, it is
    important to note that it is not an annualized measure, since the
    specific timeframe for services received by each beneficiary is not
    known and can therefore not be used for annualization.
-   These differences may have a greater impact on the overall average
    for CMHSPs who serve a smaller number of cases using a given service
    code/modifier, since each case will have a stronger influence on the
    mean.

**Units per Case**

-   All caveats related to both the `units` and `cases` measures
    separately should be taken into account when considering the use of
    the units per case measure, as this measure is derived from them.
-   In addition, users should keep in mind that the number of units
    counted may be distributed across multiple episodes of a person
    receiving the service, as with `cost per case` above. For instance,
    if an individual has 3 psychiatric inpatient stay during a `fy`,
    then the count of units will include the total of all three stays.
    Thus, it is not possible to use the units per case measure as
    equivalent to a ‘length of stay’ measure.  
-   In addition, only partial units from a given ‘episode’ or ‘stay’
    will be reported if it overlaps the `fy` reporting period.

**Percent Served**

-   Because the global population counts for distinct cases are not
    available per `fy`, it is necessary to add up the `percent served`
    for all populations (MIA, MIC and DD) for each service code/modifer
    in order to get a total percent of CMSHP beneficiaries who received
    that service code/modifier.
-   Individuals receiving services as a member of multiple populations
    from a single CMSHP during a single `fy` would be counted more than
    once using the approach above, but it is assumed that this is an
    infrequent occurrence.

**Service Code / Modifier**

In addition to the discrepancies in reporting of the measures identified
above, which make it unsafe to assume that variations in the values
reported by various CMHSPs are due to an actual variation in underlying
practices or resource use, it is also worth noting several limitations
of analyses which aim to compare service code/modifier use across CMHSPs
and to treat variations as indicative of over- or under-utilization.

-   Since multiple service might appropriately be used to address
    similar client needs, and because the type of service selected for
    use may be based on both beneficiary choice and provider network
    availability at a particular CMHSP, a discrepancy in service use for
    a particular service code/modifier might be balanced by the use of
    another service code and modifier. For instance, a CMHSP with
    relatively high usage of a 30-minute outpatient service code
    compared to its peers, may simultaneously have a relatively low use
    of a 15-minute outpatient code. To say that the first instance
    constitutes overuse, while the second constitutes underuse would be
    a mis-interpretation of the data, and may be more indicative of
    billing practice than clinical practice.
-   For this reason, we have used a set of service groups to allow for
    easier side-by-side comparison of service codes with similar
    clinical intent and/or unit types. While most metrics should not be
    aggregated by these service groupings due to the limitations noted
    above for `cases`, `units`, and `cost` measures, they do provide a
    way to filter and compare similar services out of the broad scope of
    the available service benefit.

Limitations Related to Specific Services
----------------------------------------

The aforementioned data limitations apply to the entirety of the SECR
dataset. There are also a number of additional issues related to
specific services. This especially pertains to services whose reporting
requirements have been modified during the timeframe covered by the
dataset. The inclusion of a specific service below is not intended to
reflect an exhaustive list of all potential caveats, but simply to note
known issues related to commonly used, high-cost services which are
often included in analyses.

### Inpatient Psychiatric Hospitalization

#### Differentiating Inpatient Psychiatric at a State Facility

-   0101 is the only revenue code that can differentiate Inpatient
    Psychiatric at a State Facility (versus Community Inpatient or IMD),
    however this code is no longer used
-   Specific provider types, present in the modifier field of the
    904/404 data, could be used to identify Inpatient Psychiatric
    services provided at a State Facility, however these provider types
    (PT22 and PT65) appear to have been discontinued starting in FY16
    suggesting that, post FY16, these services are bundled under the IMD
    (PT68) provider type
-   While we can differentiate State Facility (PT22) and ICF/MR (PT65)
    between 2006 and 2015 in the 904/404 data, all Inpatient Psychiatric
    revenue codes (0100, 0101, 0114, 0124, 0134, 0154) are reported in
    aggregate during this time frame meaning we cannot differentiate
    between episodes where the physician services were or were not
    included thus limiting the interpretation
-   All revenue codes are also reported in aggregate for Community
    Inpatient (PT68) and IMD (PT73) between 2006 and 2015, but starting
    in 2016 we are able to differentiate episodes where physician
    services were or were not included

#### Reporting of Inpatient Psychiatric Services

-   Prior to the FY16 reporting change for Inpatient Psychiatric
    services there were inconsistencies in the way inpatient episodes
    were reported (see attached for details). This will make meaningful
    comparisons across PIHPs/CMHSPs from 2004 through 2015 difficult.
    Comparing a single PIHP/CMSHP year-to-year may be meaningful if we
    are comfortable assuming PIHPs/CMHSPs used the same reporting
    practices each year.
-   Effective 10/1/2016 PIHPs/CMHSPs must report all inpatient
    encounters where a portion of the episode was paid by the
    PIHP/CMHSP, however the entire cost of the episode (if there were
    multiple payers) may not be fully reflected until the FY17 reporting
    year (although complications of receiving this data from other
    payers is noted, indicating the data is likely still incomplete);
    this suggests that the reported cost per case in the 904/404 data
    may be lower than actual
-   Incurred But Not Reported (IBNR) costs for PT68 and PT73 are
    included as separate line items in the 904/404 data – we’ll need to
    determine if/how this gets included in the overall reporting of this
    service

Additional Considerations
-------------------------

The following considerations should be kept in mind when interpreting
this data:

-   It should be noted that not all of the PIHPs oversaw the management
    of services prior to CY 2014, and that this field should thus be
    used to group the historical data for CMHSPs that are currently in
    the regions, rather than to erroneously attribute historical
    performance to those entities.
-   The CMHSP “Clinton Eaton Ingham” did not submit their cost and
    service use data for fiscal year 2014. Since services with values of
    zero for cost, units and cases are excluded from the Master dataset,
    this CMHSP does not show up in 2014.
-   In some years, CMHSPs reported the same HCPCS code and modifier on
    multiple lines with a slightly different service description. In
    these instances, `cases`, `units` and `costs` are assumed to be
    non-duplicative and are summed.

References
==========

The following resources are referenced here or may otherwise be relevant
for the interpretation of the 904/404 SECR dataset:

-   [MDHHS BHDDA Reporting
    Requirements](https://www.michigan.gov/documents/mdhhs/MHCodeChart_554443_7.pdf)
-   [MDHHS BHDDA Reporting Requirements: PIHP/CMHSP Reporting Cost Per
    Code and Code Chart; Appendix: Encounter Reporting and Financial
    Work Group, Data Integrity Effort, Psychiatric Inpatient in a Local
    Hospital. 2016.
    p. 83](https://www.michigan.gov/documents/mdhhs/MHCodeChart_October12016_EDIT_WG_Decisions_536182_7.pdf)
-   Culley, B., et al. *Milliman Client Report: Behavioral Health Fee
    Schedule Development - Project Status and Standard Cost Allocation
    Process*. State of Michigan, Department of Health and Human
    Services: August 21, 2019.
-   [MDHHS 904/404
    site](http://www.michigan.gov/mdch/0,4612,7-132-2941_4868_4902-256889--,00.html)

[1] See option “Current LCDs” from CMS site
[here](https://www.cms.gov/medicare-coverage-database/downloads/downloadable-databases.aspx)
