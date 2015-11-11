open404
=======
The cost and utilization data reported by Michigan’s Community Mental Health Service Providers (*also known as the Section 404 report*) has potential to be a beneficial tool in improving services for the populations served by the CMHs.  Currently, this data is officially reported to the Michigan legislature and collected by the Michigan Department of Health and Human Services (MDHHS) via reporting by the CMHs.  Given the ongoing changes to Michigan’s public health system, there is increasing utility in using data to understand current service use, cost trends and inconsistencies across the state for vulnerable populations.  

The [code provided here](https://github.com/j-hagedorn/open404/tree/master/404code) does the following:

* Compilation of [individual .csv files](https://github.com/j-hagedorn/open404/tree/master/data/raw) into one [master dataset](https://github.com/j-hagedorn/open404/blob/master/data/clean/Master.csv)
* Creation of new variables: Fiscal Year, PIHP, Units per 1,000
* Grouping of CPT/HCPCS codes into broader [Service Groups](https://github.com/j-hagedorn/open404/blob/master/data/clean/service_groups.csv). 
* Standardization of unit variable to hours, such that 1.00 = 1 hour

This code is made available under a [MIT License](http://opensource.org/licenses/MIT)

The data is drawn from source tables collected by MDHHS, provided by the Behavioral Health and Developmental Disabilities Administration (BHDDA).  Thanks to Kathleen Haines and Kasi Hunzinger for their ongoing help in obtaining data.
