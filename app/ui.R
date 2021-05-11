source('global.R')


# Define UI for application that draws a histogram
ui <- function(requests){ 
  
  
  
  
  
  
  navbarPage("Explore 404 Data",#id = 'tab',
                                     
                                     theme = shinytheme("cerulean"),
                                     navbarMenu(
                                       "About",
                                       tabPanel(
                                         "General",tags$head(includeHTML(("google-analytics.html"))),
                                         fluidRow(
                                           column(
                                             width = 5,
                                             mainPanel(
                                               tags$strong("open404", style = "font-size: 125%;"),
                                               br(),
                                               p(
                                                 "The cost and utilization data is collected by the Michigan 
Department of Health and Human Services' (MDHHS) Behavioral 
Health and Developmental Disabilities Administration (BHDDA) 
and reported annually to the Michigan legislature. Given the 
ongoing changes to Michigan’s public health system, this data 
can be used to understand service use, cost trends and 
variation across the state for vulnerable populations.  This 
application has been developed by"
                                                 
                                               ),#tags$img(src = "www.rstudio.com", width = "100px", height = "100px"),
                                               tags$img(src = 'tbd_logo.png', width = "200px", align = "left"),p(tags$sub(a(href = "https://www.tbdsolutions.com/","©2020"))),
                                               br(),
                                               br(),
                                               tags$strong("license", style = "font-size: 125%;"),
                                               br(),
                                               p(
                                                 "These data and visualizations, and the code supporting them, 
are licensed under a ",
                                                 tags$a(
                                                   href = "https://creativecommons.org/licenses/by-nc-sa/4.0/",
                                                   "Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)"
                                                 ),
                                                 " license.  You can find documentation on ",
                                                 tags$a(
                                                   href = "https://github.com/j-hagedorn/open404",
                                                   "our GitHub repository"
                                                 ),
                                                 ".  We ask that any users of this application for reporting or 
research purposes cite this source appropriately."
                                               )
                                             )
                                           ),
                                           column(
                                             width = 7,
                                             mainPanel(
                                               tags$strong("Navigation", style = "font-size: 125%;"),
                                               br(),
                                               p(
                                                 "The drop down menus above can be used to navigate the 
application. The 'About' menu includes definitions of available 
data elements and service groupings. The 'Motion Chart' menu 
includes data visualizations that can be used to explore the data."
                                               ),
                                               br(),
                                               tags$strong("Full 404 Data set: ", stype = "font-size: 125%;"),
                                               p("You can download the 404 data used in this application:"),
                                               downloadButton('data404Download', 'Download'),
                                               br(),
                                               tags$small(
                                                 tags$i(
                                                   paste("Data Updated through",max(as.character(data404$fy)
                                                   )
                                                   )
                                                 )
                                               ),
                                               br(),
                                               br(),
                                               tags$strong("Service Groupings: ", stype = "font-size: 125%;"),
                                               p("You can also download the service groupings used in this application:"),
                                               downloadButton('ServiceGroups', 'Download'),
                                             )
                                           )
                                         )
                                       ), 
                                       
                                       tabPanel(
                                         "Definitions",
                                         mainPanel(
                                           tags$strong("Definitions:", style = "font-size: 125%;"),
                                           p(
                                             tags$ul(
                                               tags$li(strong("Fiscal Year: "),
                                                       "The fiscal year in which the reported services were submitted as claims"),
                                               tags$li(strong("PIHP: "),
                                                       "The name of the prepaid inpatient health plan"),
                                               tags$li(strong("CMH: "),
                                                       "The name of the community mentail health service program"),
                                               tags$li(strong("Service Type: "),
                                                       "High level groupings of service groups into the following overall types:"),
                                               tags$span("Hospital Based Services,	Crisis Services,	Screening & Assessment,	Psychiatric and Medication Services,	Outpatient Treatment,	Health Services,	Behavioral Treatment,	Equipment/Supplies,	Transportation,	Family Education and Support,	Coordination and Planning,	Sub-acute Withdrawal Management,	Home & Community Based Services,	Peer Services,	Intensive Community-Based Treatment,	Respite,	Employment Services,	Telemedicine,	Other"
                                               ),
                                               tags$li(strong("HCPC: "),
                                                       "The code and description for a particular service"),
                                               tags$li(strong("Population: "),
                                                       "The designation of disability type for which the documented service was provided. 
There are 3 disability types included in this dataset: MIA = Mentally Ill Adults, 
MIC = MIC = Mentally Ill Children (commonly referred to as Severe Emotional Disturbance - SED), DD = Developmentally Disabled (Adults and Children)"),
                                               tags$li(strong("Total Cases: "),
                                                       "The total number of unique people who received the service"),
                                               tags$li(strong("Total Units: "),
                                                       "The total number of units of the service which were provided"),
                                               tags$li(strong("Total Cost: "),
                                                       "The total cost of the service"),
                                               tags$li(strong("Cost Per Case: "),
                                                       "The average cost of the service for all people who received it"),
                                               tags$li(strong("Cost Per Unit: "),
                                                       "The average cost for a single unit of the service"),
                                               tags$li(strong("Total Unit Per Case: "),
                                                       "The average number of units for each person who received the service"),
                                               tags$li(strong("Cost per 1K Served: "),
                                                       "Cost per 1,000 people served. Uses the general formula (Sum of Cost/Unique Persons Served) x 1000"),
                                               tags$li(strong("Percent of Total $: "),
                                                       "The annual cost of the service as a percent of the total annual cost of all services"),
                                               tags$li(strong("Percent Served: "),
                                                       "The proportion of individuals who received the selected service out of all 
the unique individuals that received any service at that orginization during 
the defined time period.  For Percent Served the numerator does change based on 
population selected; please note the denominator for this metric does not change 
based on the population selected but does change based on the organization and 
service selected")
                                             )
                                           )
                                         )
                                       )
                                     ),id = 'tab',
                                     navbarMenu("Analysis",
                                                # Application title
                                                tabPanel("Graphs and Tables",
                                                         #Hide error messages at startup
                                                         tags$style(
                                                           type = "text/css",
                                                           ".shiny-output-error { visibility: hidden; }",
                                                           ".shiny-output-error:before { visibility: hidden; }"
                                                         ),
                                                         
                                                         
                                                         fluidRow(column(12,
                                                                         
                                                                         bookmarkButton(id = "barchart_bm",
                                                                                        label = "Bookmark to Save Filters",
                                                                                        style = "color: black; 
background-color: #E1E8ED;") 
                                                                         
                                                                         
                                                                         
                                                                         
                                                                         
                                                         )),
                                                         # Sidebar with a slider input for number of bins 
                                                         fluidRow(
                                                           column(3,
                                                                  
                                                                  wellPanel(
                                                                    uiOutput("org"),
                                                                    uiOutput('prov'),
                                                                    br(),
                                                                    uiOutput("servType"),
                                                                    uiOutput("groupOrHcpcsOrMod"),
                                                                    uiOutput("compareAcross"),
                                                                    br(),
                                                                    uiOutput('metric'),
                                                                    uiOutput("popType"),
                                                                    uiOutput("fiscalYear"),
                                                                    style = 'background:#CCD6DD')
                                                           ),#---
                                                           column(9,
                                                                  tags$html('*Switching between top level tabs will change and reset filters'),
                                                                  ##########################################################################                   
                                                                  tabsetPanel(id = "tabs",
                                                                              tabPanel("Barchart",
                                                                                       fluidRow(
                                                                                         br(),
                                                                                         column(2,uiOutput("mean")),
                                                                                         column(4,uiOutput("PctChange")),
                                                                                         column(3, uiOutput("shade")),
                                                                                         column(3,uiOutput('shadeOptions'))),
                                                                                       tabsetPanel(
                                                                                         tabPanel("Plot",
                                                                                                  # Show a plot of the generated distribution
                                                                                                  #     textOutput("text"),
                                                                                                  plotOutput("barchart")),
                                                                                         tabPanel("Table",
                                                                                                  #tags$b((("Barchart Data"))),
                                                                                                  # br(),
                                                                                                  DT::dataTableOutput("barTable")),
                                                                                         fluidRow(column(8,
                                                                                                         downloadButton("Barchart", "Barchart Data"),
                                                                                                         downloadButton('plot','Barchart Image'))
                                                                                         )
                                                                                       )),
                                                                              
                                                                              tabPanel("Line Chart",id = "line_chart",
                                                                                       tabsetPanel(
                                                                                         tabPanel("Plot",
                                                                                                  plotOutput("byYearPlot"),
                                                                                                  #     ,textOutput("text")
                                                                                         ),
                                                                                         tabPanel("Trended Heatmap",
                                                                                                  plotOutput("byYearHeatmap")),
                                                                                         
                                                                                         tabPanel("Table",
                                                                                                  DT::dataTableOutput("byYear")),
                                                                                         fluidRow(column(12,
                                                                                                         column(2,uiOutput("byYearSelection_start")),
                                                                                                         column(2,uiOutput("byYearSelection_end")),
                                                                                                         column(5,uiOutput("byYearSelection_org")),
                                                                                                         column(3,p("For the Line Plot, we recommend selecting no more than 8 organizations 
to avoid line clutter and to maintain 
color distinctiveness ")))),
                                                                                         fluidRow(column(8,
                                                                                                         downloadButton("LineplotData", "Line Chart Data"),
                                                                                                         downloadButton('LineplotImage','Line Chart Image'))
                                                                                         )
                                                                                       )),
                                                                              
                                                                              tabPanel("Distribution Heatmap",id = "line_chart",
                                                                                       tabsetPanel(
                                                                                         tabPanel("Heatmap Plot",
                                                                                                  
                                                                                                  fluidRow(
                                                                                                    br(),
                                                                                                    fluidRow(column(5,offset = 9,
                                                                                                                    uiOutput('sort_heatmap'))),
                                                                                                    column(12,
                                                                                                           plotOutput('heatmap'),
                                                                                                           
                                                                                                    ))),
                                                                                         tabPanel("Table",
                                                                                                  DT::dataTableOutput('dt'),
                                                                                                  downloadButton("heatData", "Download Heat Map Table")),
                                                                                         
                                                                                         column(9,wellPanel(uiOutput("yAxisSel"))),
                                                                                         column(3,p("Each row represents a “bell curve”, comparing a particular service across different organizations. Each organizations position on the “bell curve” is indicated by a percentile score which can range between 0 and 100 with 
50 being the row average"))
                                                                                       )),
                                                                              
                                                                              # Pareto 
                                                                              
                                                                              tabPanel("Pareto Chart",id = "pareto_chart",
                                                                                       tabsetPanel(
                                                                                         tabPanel("Pareto Plot",
                                                                                                  
                                                                                                  fluidRow(
                                                                                                    br(),
                                                                                                    fluidRow(column(5,offset = 1,
                                                                                                                    uiOutput('pareto_org'))),
                                                                                                    column(12,
                                                                                                           plotlyOutput('pareto_plot',height = '500px'),
                                                                                                    )))
                                                                                         
                                                                                         # column(9,wellPanel(uiOutput("yAxitsSel"))),
                                                                                         # column(3,p("Each row represents a “bell curve”, comparing a particular service across different organizations. Each organizations position on the “bell curve” is indicated by a percentile score which can range between 0 and 100 with 50 being the row average"))
                                                                                         
                                                                                       ))
                                                                              
                                                                              
                                                                              
                                                                              
                                                                  ), 
                                                           ),
                                                         )
                                                ), # Closure for 'Graphs and Tables' tabpannel
                                                ###############################################
                                                tabPanel("CMH Cost Drivers Report",
                                                         fluidRow(
                                                           column(3,wellPanel(
                                                             uiOutput("cost_driver_fy"),
                                                             uiOutput("target_cmh"),
                                                             uiOutput("peer_cmh"),
                                                             uiOutput("target_codes"),
                                                             uiOutput("exclude_codes"),
                                                             actionButton("button","Press to Build Report",icon = icon("arrow-circle-up"),
                                                                          style =   "color: black; 
background-color: #E1E8ED; 
position: relative;")
                                                           ),
                                                           wellPanel(
                                                             uiOutput('code_highlight'),
                                                             uiOutput('cost_impact'))
                                                           
                                                           
                                                           ),
                                                           
                                                           column(9,
                                                                  tabsetPanel(
                                                                    tabPanel("DD Population",
                                                                             
                                                                             plotlyOutput("dd_pop")
                                                                             #     tableOutput("test_table")
                                                                    ),
                                                                    tabPanel("MIA Population", 
                                                                             plotlyOutput("mia_pop")),
                                                                    tabPanel("MIC Population",
                                                                             plotlyOutput("mic_pop")),
                                                                    tabPanel("About the Report",
                                                                             fluidRow(column(3,offset = 0,
                                                                                             tags$strong("Data & Metrics", style = "font-size: 125%;"))),
                                                                             br(),
                                                                             fluidRow(column(8,tags$h5("The Main Graphs")),
                                                                                      column(12,offset = 0,
                                                                                             tags$ul(tags$li(paste0(
                                                                                               "Unlike the ‘Graphs and Tables’ tab, which is more exploratory in nature, the
‘Cost Drivers’ report is presented with a particular formula at its center. The
formula brings to light codes that are different from their peers, but also gives
a rough estimate of the potential cost impact of peer alignment. Each bar on the
graph represents the Cost-Per-Case difference between the target CMH and the average
of the peer group. To get a sense of magnitude we multiply the Cost-Per-Case 
difference by the number of cases for the target CMH for that code/population. In 
other words, had the target CMH’s average Cost-Per-Case been in-line with their 
peers and that amount was applied to all cases for the year, what would've been 
the net impact of alignment. Bars above 0 would be the potential cost savings for 
the target CMH when alignment with peers is achieved. On the other hand, bars 
below 0 are what it might cost the target CMH to be in alignment with it’s peers."))
                                                                                             )),
                                                                                      column(8,tags$h5("Above Peer Svc.Type - Cost/1K cases")),
                                                                                      column(12,offset = 0,
                                                                                             tags$ul(tags$li(paste0(
                                                                                               "It’s entirely possible different codes can be used to address the same
need. How often one code gets used over another can vary between CMH’s. 
This known fact can account for cost differences in any singular code 
comparison. We attempt to control for this by looking at the parent 
service type of which the code is a part. Each HCPC code belongs to a 
service type that groups together codes with the same clinical intent. 
The highlight metric - Above Svc.Type - Cost/1K cases - takes the entire
group of codes in each service type and calculates a global cost per 1K cases
and compares the cost to its peers. When a code is 
above its peer group, it provides rough evidence against 
any claims that resource use differences can be explained away by 
different HCPC code use practices by other CMH's."))
                                                                                             )),
                                                                                      column(8,tags$h5("Drivers")),
                                                                                      column(12,offset = 0,
                                                                                             tags$ul(tags$li(paste0(
                                                                                               "The primary metric of focus is cost-per-case. The Drivers highlight 
goes one step further to categorize what’s potentially behind the cost 
differential. A higher cost-per-case may be driven by a higher average 
unit cost, more units provided per case on average or both"))
                                                                                             )),
                                                                                      column(8,tags$h5("80/20 or the Pareto Effect")),
                                                                                      column(12,offset = 0,
                                                                                             tags$ul(tags$li(paste0(
                                                                                               "The 80/20 rule, also known as the Pareto Effect, highlights those 
services for the target CMH that account for 80% of its cumulative 
resource usage. The number of services highlighted may vary, but they 
will always account for at least 80% of the target CMH’s resource usage for 
the selected year.
"))
                                                                                             )),
                                                                                      br(),br(),br()
                                                                             )
                                                                    )),
                                                                  #  tags$b("Search and Click on the row to highlight the selected code"),
                                                                  withSpinner(dataTableOutput("code_table"), type = 4, color = "#92a8d1")
                                                                  #,tableOutput("code_table_selected")
                                                                  
                                                           ))
                                                         
                                                         
                                                ), # Closure for 'CMH Cost Drivers Tab' tabpannel
                                                ###############################################         
                                                tabPanel(
                                                  "Data Limitations",
                                                  fluidRow(column(3,offset = 0,
                                                                  tags$strong("Data Limitations", style = "font-size: 125%;"))),
                                                  br(),br(),#,br(),
                                                  fluidRow(column(8,tags$h5("Calculation for Length of Episode of Treatment")),
                                                           column(12,offset = 0,
                                                                  tags$ul(tags$li(
                                                                    "The 904/404 data does not allow for an accurate calculation for length of episode of treatment. The units may have been utilized 
during multiple time periods during a given year, which may or may not be consecutive. 
Therefore, a low number of units should not be interpreted as under-utilization – it may 
be that the service was simply provided during a short period of time to the beneficiaries."
                                                                  ))),
                                                           column(5,tags$h5("CMH Reporting Limitations")),
                                                           column(12,offset = 0,
                                                                  tags$ul(tags$li("Only the CMHs that reported the identified service in FY18 appear in the charts in this report. 
Furthermore,the average units per case may be skewed if the number of recipients of the service is small 
(at a given CMH)."))),
                                                           column(5, tags$h5("Availability of Metrics for Service Groupings")),
                                                           column(12,offset = 0,
                                                                  tags$ul(tags$li(
                                                                    "Service Groups represent a set
of HCPC codes with similar clinical intent and provide a natural grouping 
intended for addressing  questions with a broader scope. And while this helps
illuminate larger trends or patterns, in terms of services with similar 
clinical intent, it necessarily does so at the expense of aggregating cases 
related to those individual HCPC services. Therefore, metrics that rely 
on the counting of distinct cases  (units per case, cost per case ) are only 
available when comparing at the HCPC group level and are not available at the 
Service Group level. "
                                                                  ))),
                                                           #             column(5,tags$h5("Summarized")),
                                                           #             column(12,offset = 0,
                                                           #                     tags$ul(tags$li("Many of the CMHSPs account for their unit service costs using different
                                                           #                                     cost accounting and allocation methods than those of their peers. Although
                                                           #                                     it is our understanding that these methods are consistent with GAAP (Generally
                                                           #                                     Accepted Accounting Principles), the inconsistency in cost allocation does
                                                           #                                     provide a sound basis for allowing comparison to other CMHSPs. The inconsistency
                                                           #                                     may be the primary cause of any reported service unit cost variability. "
                                                           #                                     ))),
                                                           column(12,tags$h5("Organizations Account for Units Costs Differently, Making Peer-to-Peer Comparisons Challenging ")),
                                                           #            column(12,offset = 0,
                                                           #                    tags$ul(tags$li("During the recently completed site visits and interviews with PIHPs and CMHSPs we
                                                           #                                  identified an overarching observation that the accounting methods employed by the PIHPs
                                                           #                                   and CMHSPs to assign costs to service units in the MUNC and SECR are not consistent.
                                                           #                                   Further, understanding that this inconsistency may be contributing to much of reported 
                                                           #                                   service unit cost variability, it is very difficult for MDHHS to understand how much of
                                                           #                                   the unit cost variation can or should be attributable to the other factors; differences
                                                           #                                   in operating structures, differences in resource requirements, and differences related to 
                                                           #                                   location. Some level of consistency in accounting and allocation methods must be established 
                                                           #                                   to understand the impacts of the other factor"))),
                                                           #            
                                                           column(12,offset = 0,
                                                                  tags$ul(tags$li("Cost allocation plans may generally follow reasonable and acceptable methods, but appear to
vary from entity to entity. Changes in an entity’ s cost allocation processes, most significantly the types of 
costs allocated to unit services, may result in significant changes from one year to the next, and make it difficult
to understand true drivers of cost increases. This variance increases the difficulty in understanding
how the CMHSPs and PIHPs actual unit costs compare to each other or to any fee schedule"
                                                                  )))
                                                  )),
                                                tabPanel("Service Groupings Search",
                                                         br(),
                                                         p("The below table allows you to quickly search to better understand service groupings"),
                                                         DT::dataTableOutput("svs_groups"))
                                                
                                                
                                     ) # analysis navbar menu
) # final closure 
}