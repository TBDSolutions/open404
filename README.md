open404
=======
The cost and utilization data reported by Michigan’s Community Mental Health Service Providers (*also known as the Section 404 report*) has potential to be a beneficial tool in improving services for the populations served by the CMHs.  Currently, this data is officially reported to the Michigan legislature and collected by the Michigan Department of Health and Human Services (MDHHS) via reporting by the CMHs.  Given the ongoing changes to Michigan’s public health system, there is increasing utility in using data to understand current service use, cost trends and inconsistencies across the state for vulnerable populations.  

The [code provided here](https://github.com/j-hagedorn/open404/tree/master/404code) does the following:

* Compilation of [individual .csv files](https://github.com/j-hagedorn/open404/tree/master/data/raw) into one [master dataset](https://github.com/j-hagedorn/open404/blob/master/data/clean/Master.csv)
* Creation of new variables for grouping and sorting: _Fiscal Year_, _PIHP_
* Grouping of CPT/HCPCS codes into broader [Service Groups](https://github.com/j-hagedorn/open404/blob/master/data/clean/service_groups.csv). 
* Standardization of unit variable to hours, such that 1.00 = 1 hour
* Calculation of the following variables related to each distinct service code for analysis: 
_Service cost as % of CMH Total Cost_, _Cost per 1,000 persons served_, _Percent of beneficiaries receiving service_

For details on the data elements in this dataset, please refer to the [data dictionary](https://github.com/j-hagedorn/open404/blob/master/data/clean/404DataDictionary.md).

You can read the code into R with a simple:

`read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/Master.csv")`

...or [import it into Excel](https://support.office.com/en-za/article/Import-or-export-text-txt-or-csv-files-5250ac4c-663c-47ce-937b-339e391393ba) using the same URL.

This code is made available under a [MIT License](http://opensource.org/licenses/MIT)

The data is drawn from source tables collected by MDHHS, provided by the Behavioral Health and Developmental Disabilities Administration (BHDDA).  Thanks to Kathleen Haines and Kasi Hunzinger for their ongoing help in obtaining data.
