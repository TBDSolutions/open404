source('global.R')


# Define UI for application that draws a histogram
ui <- function(requests){ navbarPage("Explore 404 Data",#id = 'tab',
                                     
  theme = shinytheme("cerulean"),
   navbarMenu(
     "About",
   tabPanel(
     "General",
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
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Analysis Tab %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### UI Components
  output$org<-renderUI({
    selectInput(
      inputId = "CMHorPIHP",
      label = "What would you like to group the analysis by?",
      choices = c("CMH" = 'cmhsp', "PIHP" = 'pihp_name'),
      selected = "cmhsp")
  })
  
  output$prov<-renderUI({
    
    req(input$tabs)
    
    if(input$tabs ==  "Line Chart"){
      
      tags$b("Use the selector on the Line Chart tab to choose orginizations")     
    }
    else{
      
    org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
    
    
    # Conditional statements to populate the list
    prov_options<- if(input$CMHorPIHP == "cmhsp"){
      levels(data404$cmhsp)}
    else if(input$CMHorPIHP == "pihp_name"){levels(data404$pihp_name)}
    else{"MI"}
    
    
    selectizeInput(
      inputId = "provider",
      label =   paste("Which ",org,"are you interested in viewing?"),
      choices =  prov_options,
      selected = prov_options[1:8],
    #   selected = c("Genesee","St. Clair","Lapeer",'Sanilac',prov_options[1:12]),
      
      multiple = TRUE,
      options =  list( placeholder = 'Search or Select'))
    
    } 
    
    
  })  
  
  output$servType<-renderUI({
    req(org_type())
    selectInput(
      inputId = 'serviceType',
      label = 'Any particular area of focus',
      choices = c("All",levels(as.factor(data404$svc_type))),
      selected = "All")
  })
  
  output$groupOrHcpcsOrMod <-renderUI({
    
    #Actual options     
    
    radioButtons(
      inputId = "groupOrHcpcsOrMod_",
      label = "Service Type, HCPCS Code or Modifier",
      choices = c("Service Type" = "svc_type", "HCPCS" = "code_shortDesc","Code Mod" = 'codeM_shortDesc' ),
      selected = c("code_shortDesc"),
      inline = TRUE)
    
  })
  
  output$compareAcross<-renderUI({
    
    req(input$tabs)
    
    if(input$tabs ==  "Distribution Heatmap"){
      
      tags$b("Please choose a bundle of services using the selector 
             on the distribution heatmap tab")
    
      }
    else{

    
    type<-as.symbol(if(input$groupOrHcpcsOrMod_ == "svc_type"){"svc_type"}
                  else if(input$groupOrHcpcsOrMod_ == "codeM_shortDesc"){'codeM_shortDesc'}
                  else{"code_shortDesc"})
    
    org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
    
    grps<-if(input$groupOrHcpcsOrMod_ == "svc_type"){"Service Type"}
             else if(input$groupOrHcpcsOrMod_ == "codeM_shortDesc"){'Code Modifier'}
             else{"HCPC Code"}
    
    
    #type<-as.symbol("svc_type")
    
    options<-data404%>%
      filter(svc_type %in% case_when('All' %in% input$serviceType ~ levels(as.factor(data404$svc_type)),
                                     TRUE ~ input$serviceType))%>%
      distinct(!!type)%>%
      pull(!!type)
    
    
    if(input$groupOrHcpcsOrMod_ == "svc_type"){
     # tags$h6("will")
      selectizeInput(
        inputId = 'compareAcross',
        label = paste('Compare ',org," across this ",grps,sep = ""),
        choices = options,
        multiple = FALSE,
        selected = "Home & Community Based Services")
      
    } else if(input$groupOrHcpcsOrMod_ == "code_shortDesc") {
      
      selectizeInput(
        inputId = 'compareAcross',
        label = paste('Compare ',org," across this ",grps,sep = ""),
        choices = options,
        multiple = FALSE,
        selected = 'Community Living Supports 15 minutes  ( H2015 )')
    }
    
    else{
      
      selectizeInput(
        inputId = 'compareAcross',
        label = paste('Compare ',org," across this ",grps,sep = ""),
        choices = options,
        multiple = TRUE,
        selected = "Nursing Facility Mental Health Monitoring ( T1017SE )")
    } 

    }    
    
  })
  
  output$metric<-renderUI({
    
    org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
    
    choices<-if(input$groupOrHcpcsOrMod_ == 'svc_type'){
      c("Cost" = "cost",'Units' = 'units',
        #   'Cases' = "cases","Cost Per Case" = 'cost_per_case',
        "Cost Per Unit" = 'cost_per_unit',
        #    "Units Per Case" = "unit_per_case",
        "Percent of Total Cost" = "cost_pct_tot",
        "Cost Per 1K Served" = "cost_per_1K_served")
    }else{
      c("Cost" = "cost",'Units' = 'units',
        'Cases' = "cases","Cost Per Case" = 'cost_per_case',
        "Cost Per Unit" = 'cost_per_unit',
        "Units Per Case" = "unit_per_case",
        "Pct. Served" = "pct._served",
        "Percent of Total Cost" = "cost_pct_tot",
        "Cost Per 1K Served" = "cost_per_1K_served")
    }
    
    selectInput(
      inputId = 'metric',
      label =  paste("Benchmarking",org,"on which metric?"),
      choices = choices,
      selected = "units")
  })
  
  output$popType<-renderUI({
    # Tag list groups the two widgets together

      selectInput(
        inputId = 'popType',
        label = 'Population Grouping',
        multiple = T,
        choices = c("",levels(as.factor(data404$population))),
        selected = levels(as.factor(data404$population)))
      

  })
  
  output$fiscalYear <-renderUI({
    
    req(input$tabs)
    
    if(input$tabs ==  "Line Chart"){
      
      tags$b("Use the selector on the Line Chart tab to select multiple years")     
    }
    else{
    
    
    selectInput(
      inputId = 'fy_filter',
      label = 'Fiscal Year',
      choices = c(levels(data404$fy)),
      selected = "2019")
}
    
    
    
  })
  
  output$shade<-renderUI({
    
    req(input$CMHorPIHP)
    
    if(input$CMHorPIHP == 'cmhsp'){
      
      radioButtons(inputId = 'shadeByPihp',
                   label = "Highlight the CMH's of a PIHP?",
                   choices = c("Yes",'No'),
                   selected = "No",
                   inline = TRUE)
      
    }else{}
    
  })  
  
  output$shadeOptions<-renderUI({
    
    req(input$shadeByPihp)
    
    if(input$CMHorPIHP == 'pihp_name'){}else{
      
      if(input$shadeByPihp== "Yes"){
        
        selectInput(inputId = 'WhichPIHP',
                    label = 'Which PIHP',
                    choices = levels(data404$pihp_name),
                    selected = "")
        
      }
    }
    
    
  })
  
  output$mean<-renderUI({
    radioButtons(inputId = 'includeMean',
                 label = "Add State Avg.",
                 choices = c("Yes",'No'),
                 selected = "No",
                 inline = TRUE)
    
  })

  output$PctChange<-renderUI({
    
    radioButtons(
      inputId = "includePctChange"
    ,label = "Add % Change LY?"
    ,choices = c('Yes',"No")
    ,inline = T
    ,selected = "No"
    )
    
    
  })
  
### Reactive Input 
  
org_type <- reactive({input$CMHorPIHP})
provider<-reactive({input$provider})
fy_filter<-reactive({input$fy_filter})
metric <-reactive({input$metric})
serviceType<-reactive({input$serviceType})
serviceGroup <-reactive({input$serviceGroup})
popType<-reactive({input$popType})
codes<-reactive({input$codes})
groupOrHcpcsOrMod_<-reactive({input$groupOrHcpcsOrMod_})
compareAcross<-reactive({input$compareAcross})
WhichPIHP<-reactive({input$WhichPIHP})

### Reactive datasets  

stateAggData<-reactive({
  
  stateAggData<-state_data%>%
    group_by(!!as.symbol(org_type()),fy)%>%
    summarise(TotalServed = sum(TotalServed,na.rm = TRUE))
  
}) 

pop_filter<-reactive({
  
  # Pre-made filter for below DPLYR manipulations for formatting multiple pop
  # code options. Makes it easier to use when pre-defined
  
  
  if('' %in% popType()){ as.character(unique(data404$population))}else{
    
    data404[which(data404$population %in% popType()),'population']%>%
      mutate(population = as.character(population))%>%
      distinct(population)%>%
      pull(population)}
})
  
stateAvg<-reactive({
    
    #Aggregated Michigan Data
    stateAggData<-as.data.frame(stateAggData())
    
    
    df<-data404%>%
      filter(
        fy %in% fy_filter(),
        #   svc_grp %in%  serviceGroup() )%>% # unless individuals chosen
        (!!as.symbol(groupOrHcpcsOrMod_())) %in%  input$compareAcross,
        population %in% pop_filter()
      )%>%
      select(
        fy,
        !!as.symbol(org_type()),
        cost,units,cases,cost_pct_tot
      )%>%
      group_by(
        fy,
        (!!as.symbol(org_type()))
      )%>%
      summarise_at(
        
        vars(cases,units,cost,cost_pct_tot),
        list(~sum(., na.rm = T))
      )%>%
      mutate(
        cost_per_case = round(cost/cases,digits = 2),
        cost_per_unit = round(cost/units,digits = 2),
        unit_per_case = round(units/cases,digits = 1)
      )%>%
      left_join(
        stateAggData,by = c(org_type() ,"fy")
      )%>%
      mutate(
        
        cost_per_1K_served = (cost/TotalServed)*1000,
        pct._served = round(((cases/TotalServed)*100),3)
      )%>%
      filter_if( #remove INF values from dividing by zero
        ~is.numeric(.), all_vars(!is.infinite(.))
      )%>%
      ungroup()%>%
    #  mutate(pct_of_total_cost = 0)%>%
      group_by(fy)%>%
      summarise(avg = mean(!!as.symbol(metric()), na.rm= TRUE))%>%
      pull(avg)
    
    
  })

selectedDS<-reactive({
  
  req(input$provider)
  
  
  # If the selection is by PIHP, I need to aggregate the data before joining 
  # This table will be used below to calulate cost per 1K ect. 
  # Aggregated Michigan Data
  stateAggData<-as.data.frame(stateAggData())
  
  df<- data404%>%
    filter(
      !!as.symbol(org_type()) %in% input$provider,
      fy %in% fy_filter(),
      (!!as.symbol(groupOrHcpcsOrMod_())) %in% input$compareAcross,
      population %in% pop_filter()
      # svc_grp %in%  serviceGroup() )%>% # unless individuals chosen
    )%>%
    select(
      !!as.symbol(org_type()),fy,
      cost,units,cases,cost_pct_tot
    )%>%
    group_by(
      !!as.symbol(org_type()),fy
    )%>%
    summarise_at(
      
      vars(cases,units,cost,cost_pct_tot),
      list(~sum(., na.rm = T))
    )%>%
    mutate(
      
      cost_per_case = round(cost/cases,digits = 2),
      cost_per_unit = round(cost/units,digits = 2),
      unit_per_case = round(units/cases,digits = 1)
    )%>%
    left_join(
      
      stateAggData,by = c(org_type() ,"fy")
    )%>%
    mutate(
      
      cost_per_1K_served = round((cost/TotalServed)*1000,0),
      pct._served = round(((cases/TotalServed)*100),3)
    )%>%
    filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  
##### Year over Year trends 
  
  start<-as.numeric(fy_filter())
  
  pct_change<- data404%>%
    filter(
      !!as.symbol(org_type()) %in% input$provider,
   #   fy %in% c('2016','2019'),
       fy %in% c(start - 1, as.numeric(fy_filter())),
      (!!as.symbol(groupOrHcpcsOrMod_())) %in% input$compareAcross,
      population %in% pop_filter()
      # svc_grp %in%  serviceGroup() )%>% # unless individuals chosen
    )%>%
    select(
      !!as.symbol(org_type()),fy,
      cost,units,cases,cost_pct_tot
    )%>%
    group_by(
      !!as.symbol(org_type()),fy
    )%>%
    summarise_at(
      
      vars(cases,units,cost,cost_pct_tot),
      list(~sum(., na.rm = T))
    )%>%
    mutate(
      
      cost_per_case = round(cost/cases,digits = 2),
      cost_per_unit = round(cost/units,digits = 2),
      unit_per_case = round(units/cases,digits = 1)
    )%>%
    left_join(
      
      stateAggData,by = c(org_type() ,"fy")
    )%>%
    mutate(
      
      cost_per_1K_served = round((cost/TotalServed)*1000,0),
      pct._served = round(((cases/TotalServed)*100),3)
    )%>%
    filter_if(
             ~is.numeric(.), all_vars(!is.infinite(.))
              )%>% # rerunning the above manipulations so 
                   # I can create a dataframe that only looks 
                   # at the year over year pct changes
    select(!!as.symbol(org_type()),fy,
           metric = !!as.symbol(metric())
           )%>% 
    
    mutate(
      prev_time = lag(metric),
      metric_pct_change = round( (metric - prev_time)/prev_time *100,0)
      ) %>%
    ungroup()%>%
    filter(is.na(prev_time) == FALSE)%>%
    select(!!as.symbol(org_type()),fy,metric_pct_change) 
  
  
   df<-df%>%
       left_join(pct_change,by = c("fy",org_type()))
    
  
})

selectedDS_download<-reactive({
  foo<-selectedDS()
  
  foo<-foo%>%
    select(
      input$CMHorPIHP,
      input$metric,
      metric_pct_change,
      fy)
  
  
})

# need to make bar chart reactive for download later
plotInput<-reactive({ 
  
  #output$barchart<-renderPlot({
  
  req(compareAcross())
  req(popType())
  
  
  # Define which dataset to use based on CMH or PIHP
  # Primarily for the left join that will attach nessesary 
  # Grouping and shading columns for the graph
  # The nesting if statement checks to see if the user also needed to 
  # highlight a particular CMH, if so, conduct a mutate based on the users 
  # input
  #########################################
  df<-if(input$CMHorPIHP == 'cmhsp'){
    
    if(input$shadeByPihp== "Yes"){
      req(input$WhichPIHP)
      
      data.frame(selectedDS())%>%
        left_join(pihpCMH_LU, by = "cmhsp")%>%
        mutate(PIHP = case_when(pihp_name %in% input$WhichPIHP ~ input$WhichPIHP,
                                TRUE ~ 'Others'))}
    else{data.frame(selectedDS())%>%
        left_join(pihpCMH_LU, by = "cmhsp")}
    
    
  }else if(input$CMHorPIHP == 'pihp_name'){ 
    
    p<-pihpCMH_LU%>%distinct(pihp,pihp_name)
    
    
    data.frame(selectedDS())%>%
      left_join(p, by = "pihp_name")
    
  } else {data.frame(selectedDS())}
  ###########################################
  
  
  # Format X-Axis labels 
  xlabs<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
  else{'PIHP'}
  ############################################
  
  # Set the axis title and ensure all selections of HCPCS codes are included
  group<-if(input$groupOrHcpcsOrMod_ == "svc_type"){ paste(compareAcross()," Service Type")}
  else{ as.data.frame(list(compareAcross()))%>%
      mutate(code = as.character(.[[1]]))%>%
      pull(code)
  }
  ##############################################
  
  # Set the State average in proper format
  text_avg<-format(round(stateAvg(),0),big.mark=",", scientific=FALSE)
  
  populations<-as.data.frame(list(popType()))%>%
    mutate(popType = as.character(.[[1]]))%>%
    pull(popType)
  
  #################################################
  
  # setting pct change variable 
  
  df<-df%>%
      mutate(`% Change LY` = as.factor( case_when(metric_pct_change < 0 ~ 'Down',
                                   metric_pct_change > 0 ~ 'Up',
                                   TRUE ~ "neutral")))
  ####################################################
  
  #Setting Y-axis height 
  y_max <-df%>%
          mutate(y_max = max(!!as.symbol(metric()),na.rm = T) * 1.05)%>%
          select(y_max)%>%
          distinct(y_max)%>%
          pull()
  
  
  barplot<- if(input$shadeByPihp == 'Yes' & input$CMHorPIHP == 'cmhsp'){
    
    
    df%>%
      ggplot(aes(x = fct_reorder(as.factor(!!as.symbol(org_type())),!!as.symbol(metric()),
                                 .desc = TRUE),
                 y = !!as.symbol(metric()),
                 fill = PIHP)) +
      geom_bar(stat="identity", position=position_dodge(), alpha = .7,
               color="black")+
      scale_y_continuous(label = number_format(big.mark = ","),
                         limits = c(0,y_max))+
      xlab(xlabs)+
      ylab(str_replace_all(input$metric,pattern = "_"," "))+
      scale_fill_manual(values=c('Others' = '#696969','LRE' = '#EA4335',"MSHN" = '#EA4335',
                                 'DWMHA' = '#EA4335',"OCCMHA" = '#EA4335',"SWMBH" = '#EA4335',
                                 'CMHPSM' = '#EA4335',"NMRE" = '#EA4335',"Region10"='#EA4335',
                                 "Northcare" = '#EA4335',"MCMHS" = '#EA4335')) +
      theme_minimal()+
      ggtitle(paste("Comparing ", str_replace_all(input$metric,pattern = "_"," ")," by ",
                    xlabs," for ",paste(group,collapse = ","),sep = ""),
              subtitle =  paste("Fiscal Year ",input$fy_filter,sep = ""))+
      labs(fill='PIHP')+
      labs(caption =paste("Populations ",paste(populations,collapse = ","),sep = ""))+
      theme_ipsum(grid = 'FALSE',
                  plot_title_size = 15,
                  axis_text_size = 11,
                  axis_title_size = 13,
                  ticks = TRUE
                  #   base_family = "IBMPlexSans"
      )+
      theme(axis.text.x=element_text(angle=45, hjust=1)
            ,plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5)
          #  text=element_text(size=14 #, family="Gill Sans MT"
           )
            
  

    
    
 
    
  }else{
    df%>%
      ggplot(aes(x = fct_reorder(as.factor(!!as.symbol(org_type())),!!as.symbol(metric()),
                                 .desc = TRUE),
                 y = !!as.symbol(metric()))) +
      geom_bar(stat="identity", position=position_dodge(), alpha = .7,
               color="black")+
      scale_y_continuous(label = number_format(big.mark = ","),
                         limits = c(0,y_max))+
      xlab(xlabs)+
      ylab(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")))+
      theme_minimal()+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      ggtitle(paste("Comparing ",stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," "))," by ",
                    xlabs," for ",paste(group,collapse = ","),sep = ""),
              subtitle =  paste("Fiscal Year ",input$fy_filter,sep = ""))+
      labs(caption =paste("Populations ",paste(populations,collapse = ","),sep = ""))+
      theme_ipsum(grid = 'FALSE',
                 plot_title_size = 15,
                  axis_text_size = 11,
                  axis_title_size = 13,
                  ticks = TRUE
                  #   base_family = "IBMPlexSans"
      )+
      theme(axis.text.x=element_text(angle=45, hjust=1)
            ,plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    #        text=element_text(size=14 #, family="Gill Sans MT"
    #        ))
            
    
  } 
  
 if(input$includeMean == 'Yes' & input$includePctChange == "Yes"){
   
    barplot +  geom_hline(yintercept = c(stateAvg()),linetype = "dashed",size = 1)+
      annotate("Text",  x=Inf, y = Inf, label = paste("State Avg. ",text_avg),
               vjust=1, hjust=1)+
     geom_text(aes(label= metric_pct_change,color = `% Change LY`), size = 3,
               position=position_dodge(width=0.5), vjust=-0.5)+
     scale_colour_manual(values=c(Down = "#006699", Up = "#EA4335", "neutral" = 'grey'))
     
     
 }else if(input$includeMean == 'Yes' & input$includePctChange == "No"){
  
   barplot +  geom_hline(yintercept = c(stateAvg()),linetype = "dashed",size = 1)+
    annotate("Text",  x=Inf, y = Inf, label = paste("State Avg. ",text_avg),
             vjust=1, hjust=1)
  
 } else if(input$includeMean == 'No' & input$includePctChange == "Yes"){
   
   barplot + geom_text(aes(label= metric_pct_change,color = `% Change LY`), size = 3,
                       position=position_dodge(width=0.5), vjust=-0.5)+
     scale_colour_manual(values=c(Down = "#006699", Up = "#EA4335", "neutral" = 'grey'))
   
 }
  else{ barplot}
  
  
})
  
### Graphs and Tables  
  
output$svs_groups<-renderDataTable({
    
    df404<-data404%>%
      distinct(svc_type,svc_grp,short_desc,code)
    
    DT::datatable(df404,rownames = FALSE,class = 'cell-border stripe',
                  colnames = c("Service Type","Serivce Group","HCPC Desc.",'HCPC Code'))
    
    
    
    
  })
  
output$barTable<-DT::renderDataTable({
    
    col1<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
    else{'PIHP'}
    
    col2<-"Fiscal Year"
    
    metric_lab = str_replace_all(input$metric,pattern = "_"," ")
    
    change <- "% Change LY"
    
    foo<-as.data.frame( selectedDS())
    
    foo<-foo%>%
         select(
           input$CMHorPIHP,
                input$metric,
                metric_pct_change,
                fy)

    DT::datatable(foo,rownames = FALSE,class = 'cell-border stripe',
                  colnames = c(col1,metric_lab,change,col2),
                  options = list(pageLength = 5))
                  #colnames = c(col1,col2,metric_lab))

  })
  
output$barchart<-renderPlot({
   plotInput()
   
 #  ggsave("plot.pdf", plotInput())
   
   
})
 
 
### Downloads and Bookmarks

{

output$Barchart <- downloadHandler(
  filename = function() {
    paste("Barchart_data", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(selectedDS_download(), file, row.names = FALSE)
  }
)
  
  
  setBookmarkExclude(c("barchart_bm"))
  
  observeEvent(input$barchart_bm, {
    session$doBookmark()
  })
  

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Line chart tab %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### UI Components 

output$byYearSelection_start <- renderUI({
  
selectInput(inputId = "byYearSelection_star",
            label = "Start Year",
            choices = seq(2006,(max(as.numeric(as.character(data404$fy)))-1), by = 1),
            selected = 2006)

})

output$byYearSelection_end <-renderUI({
  
  
  
 selectInput(inputId = "byYearSelection_end",
            label = "End Year",
            choices = seq(as.numeric(input$byYearSelection_star) + 1
                          ,(max(as.numeric(as.character(data404$fy)))),
                          by = 1),
            selected = 2019)
  

})

output$byYearSelection_org <- renderUI({
  
  # by year
  
  org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
  
  
  # Conditional statements to populate the list
  prov_options<- if(input$CMHorPIHP == "cmhsp"){
    levels(data404$cmhsp)}
  else if(input$CMHorPIHP == "pihp_name"){levels(data404$pihp_name)}
  else{"MI"}
  
  
  selectizeInput(
    inputId = "provider_byYear",
    label =   paste("Which ",org,"are you interested in viewing?"),
    choices =  prov_options,
   # selected = prov_options[1:5],
   selected = c("Genesee","St. Clair","Lapeer",'Sanilac'),
    multiple = TRUE,
    options =  list( placeholder = 'Search or Select'))
  
  
  
  
}) 

#output$text <- renderText({paste0("You are viewing tab \"", input$byYearSelection, "\"")})

### Reactive input 

provider_byYear_input<-reactive({input$provider_byYear})

### Reactive data sets 

selectedDS_byYear<-reactive({
  
  req(input$provider)
  
  
  # If the selection is by PIHP, I need to aggregate the data before joining 
  # This table will be used below to calulate cost per 1K ect. 
  # Aggregated Michigan Data
  stateAggData<-as.data.frame(stateAggData())
  
  ##### Year over Year trends 
  
  start<-as.numeric(fy_filter())
  
  pct_change<- data404%>%
    filter(
      !!as.symbol(org_type()) %in% input$provider_byYear,
         fy %in% seq(input$byYearSelection_star,input$byYearSelection_end,by = 1),
    #  fy %in% c(start - 3, as.numeric(fy_filter())),
      (!!as.symbol(groupOrHcpcsOrMod_())) %in% input$compareAcross,
      population %in% pop_filter()
      # svc_grp %in%  serviceGroup() )%>% # unless individuals chosen
    )%>%
    select(
      !!as.symbol(org_type()),fy,
      cost,units,cases,cost_pct_tot
    )%>%
    group_by(
      !!as.symbol(org_type()),fy
    )%>%
    summarise_at(
      
      vars(cases,units,cost,cost_pct_tot),
      list(~sum(., na.rm = T))
    )%>%
    mutate(
      
      cost_per_case = round(cost/cases,digits = 2),
      cost_per_unit = round(cost/units,digits = 2),
      unit_per_case = round(units/cases,digits = 1)
    )%>%
    left_join(
      
      stateAggData,by = c(org_type() ,"fy")
    )%>%
    mutate(
      
      cost_per_1K_served = round((cost/TotalServed)*1000,0),
      pct._served = round(((cases/TotalServed)*100),3)
    )%>%
    filter_if(
      ~is.numeric(.), all_vars(!is.infinite(.))
    )%>%
    select(!!as.symbol(org_type()),fy,
           !!as.symbol(metric()))
    
})

line_plot_download_ds<-reactive({
  
  df<-data.frame(selectedDS_byYear())
  
  
})

#Making plot a reactive so image can be downloaded
line_plot_image<-reactive({
  
  
  time_df<-data.frame(selectedDS_byYear())
  
  
  # Format X-Axis labels 
  org_lab<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
  else{'PIHP'}
  ############################################
  
  # Set the axis title and ensure all selections of HCPCS codes are included
  group<-if(input$groupOrHcpcsOrMod_ == "svc_type"){ paste(compareAcross()," Service Type")}
  else{ as.data.frame(list(compareAcross()))%>%
      mutate(code = as.character(.[[1]]))%>%
      pull(code)
  }
  
  
  # Adding population formatting 
  populations<-as.data.frame(list(popType()))%>%
    mutate(popType = as.character(.[[1]]))%>%
    pull(popType)
  
  
  df<-if(input$CMHorPIHP == 'cmhsp'){
    
    
    p<- time_df%>%
      mutate(org = !!as.symbol(org_type()))%>%
      rename(CMH = org)%>%
      ggplot(aes(x = fy,group = !!as.symbol(org_type()))) + 
      geom_line(aes(y = !!as.symbol(metric()),color = CMH),size=1.2)+
      theme_minimal()+
    #  scale_color_hue(l=50, c=100 ,h=c(0, 360), na.value = "black")+
      scale_color_viridis(discrete=TRUE) +
      #  scale_color_brewer(palette="Dark2")+
    #  scale_fill_manual(values = c("blue","orange","green","purple"))+
  #    paletteer_c("gameofthrones::baratheon", n = 5)+
      xlab("Fiscal Year")+
      ylab(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")))+
      ggtitle(paste("Comparing peer group ", str_replace_all(input$metric,pattern = "_"," ")," by ",
                    org_lab," for ",paste(group,collapse = ","),sep = ""),
              subtitle = paste("Fiscal Years",input$byYearSelection_star,
                               "-",input$byYearSelection_end, sep = " "))+
      labs(fill='CMH')+
      labs(caption =paste("Populations ",paste(populations,collapse = ","),sep = ""))+
      theme_ipsum(grid = 'FALSE',
                  plot_title_size = 15,
                  axis_text_size = 11,
                  axis_title_size = 13,
                  ticks = TRUE
                  #   base_family = "IBMPlexSans"
      )+
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme(axis.text.x=element_text(angle=45, hjust=1)
            ,plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
            
      )
    
    
    p
  }
  else{
    
    p<- time_df%>%
      mutate(org = !!as.symbol(org_type()))%>%
      rename(PIHP = org)%>%
      ggplot(aes(x = fy,group = !!as.symbol(org_type()))) + 
      geom_line(aes(y = !!as.symbol(metric()),color = PIHP),size=1.2)+
      scale_color_viridis(discrete=TRUE) +
      theme_minimal()+
     # scale_color_hue(l=50, c=100 ,h=c(0, 360), na.value = "black")+
      xlab("Fiscal Year")+
      # ylab(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")))+
      ggtitle(paste("Comparing ", str_replace_all(input$metric,pattern = "_"," ")," by ",
                    org_lab," for ",paste(group,collapse = ","),sep = ""),
              subtitle = paste("Fiscal Years",input$byYearSelection_star,
                               "-",input$byYearSelection_end, sep = " "))+
      labs(caption =paste("Populations ",paste(populations,collapse = ","),sep = ""))+
      theme_ipsum(grid = 'FALSE',
                  plot_title_size = 15,
                  axis_text_size = 11,
                  axis_title_size = 13,
                  ticks = TRUE
                  #   base_family = "IBMPlexSans"
      )+
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme(axis.text.x=element_text(angle=45, hjust=1)
            ,plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
            
      )
    p
    
  }
  
  
  print(df) 
  
  
})

### Graphs and Tables  

output$byYear <- renderDataTable({
  
  df<-data.frame(selectedDS_byYear())
  
  col1<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
  else{'PIHP'}
  
  col2<-"Fiscal Year"
  
  metric_lab = str_replace_all(input$metric,pattern = "_"," ")

  
  DT::datatable(df,rownames = FALSE,class = 'cell-border stripe'
                ,colnames = c(col1,col2,metric_lab),
                options = list(pageLength = 5)
                )
  #colnames = c(col1,col2,metric_lab))
  
  
  
})

output$byYearPlot <-renderPlot({
 
   line_plot_image()
  
  
})


### Downloads and Bookmarks
{
  output$LineplotData <- downloadHandler(
    filename = function() {
      paste("Linechart_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(line_plot_download_ds(), file)
    }
  )
  
  # plot download
  output$LineplotImage <- downloadHandler(
    filename = function() { paste("line_chart_image", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = line_plot_image(),
             device = "png", width = 12 ,
             height = 6,dpi = 500
      )
    }
  )
} 
  

############# By year heat map tab 

### Reactive datasets 
selectedDS_byYear<-reactive({
    
    req(input$provider)
    
    
    # If the selection is by PIHP, I need to aggregate the data before joining 
    # This table will be used below to calulate cost per 1K ect. 
    # Aggregated Michigan Data
    stateAggData<-as.data.frame(stateAggData())
    
    ##### Year over Year trends 
    
    start<-as.numeric(fy_filter())
    
    pct_change<- data404%>%
      filter(
        !!as.symbol(org_type()) %in% input$provider_byYear,
        fy %in% seq(input$byYearSelection_star,input$byYearSelection_end,by = 1),
        #  fy %in% c(start - 3, as.numeric(fy_filter())),
        (!!as.symbol(groupOrHcpcsOrMod_())) %in% input$compareAcross,
        population %in% pop_filter()
        # svc_grp %in%  serviceGroup() )%>% # unless individuals chosen
      )%>%
      select(
        !!as.symbol(org_type()),fy,
        cost,units,cases,cost_pct_tot
      )%>%
      group_by(
        !!as.symbol(org_type()),fy
      )%>%
      summarise_at(
        
        vars(cases,units,cost,cost_pct_tot),
        list(~sum(., na.rm = T))
      )%>%
      mutate(
        
        cost_per_case = round(cost/cases,digits = 2),
        cost_per_unit = round(cost/units,digits = 2),
        unit_per_case = round(units/cases,digits = 1)
      )%>%
      left_join(
        
        stateAggData,by = c(org_type() ,"fy")
      )%>%
      mutate(
        
        cost_per_1K_served = round((cost/TotalServed)*1000,0),
        pct._served = round(((cases/TotalServed)*100),3)
      )%>%
      filter_if(
        ~is.numeric(.), all_vars(!is.infinite(.))
      )%>%
      select(!!as.symbol(org_type()),fy,
             !!as.symbol(metric()))
    
  })
  
  
### Graphs and tables
  
output$byYearHeatmap <- renderPlot({
  
  # function for normalizing data 
  norm<- function(x){(x - min(x))/(max(x) - min(x))} 
  
  
  
  # Format X-Axis labels 
  org_lab<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
  else{'PIHP'}
  ############################################
  
  # Set the axis title and ensure all selections of HCPCS codes are included
  group<-if(input$groupOrHcpcsOrMod_ == "svc_type"){ paste(compareAcross()," Service Type")}
  else{ as.data.frame(list(compareAcross()))%>%
      mutate(code = as.character(.[[1]]))%>%
      pull(code)
  }
  
  
  # Adding population formatting 
  populations<-as.data.frame(list(popType()))%>%
    mutate(popType = as.character(.[[1]]))%>%
    pull(popType)
  
  

  df<-data.frame(selectedDS_byYear())%>%
            #      filter(!fy %in% c("2006","2007","2008","2009","2010"))%>%
                  group_by(!!as.symbol(org_type()))%>%
                  mutate(norm = norm( !!as.symbol(metric()) ))%>%
                  ungroup()
                

  
  
df<-ggplot(df,aes( y = !!as.symbol(org_type()) ,x = fy)) + 
    geom_tile(aes(fill =norm), colour = "white")+
    #scale_fill_viridis_c(direction = -1)
    #  scale_fill_gradientn(colours = terrain.colors(10))
    scale_fill_gradientn(colours = c("#98C4F6","#236AB9","#FE2712"),na.value = "white")+
    theme_bw() +
    xlab("Fiscal Year")+
    ylab(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")))+
    theme(panel.grid=element_blank()) +
    coord_cartesian(expand=FALSE)+
  xlab("Fiscal Year")+
  ylab(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")))+
  ggtitle(paste("Normalized ", str_replace_all(input$metric,pattern = "_"," "),
                " for ",paste(group,collapse = ","),sep = ""),
          subtitle = paste("Fiscal Years",input$byYearSelection_star,
                           "-",input$byYearSelection_end, sep = " "))+
  labs(fill= paste(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")),"Normalized",
                   sep = " "))+
  labs(caption =paste("Populations ",paste(populations,collapse = ","),sep = ""))+
  theme_ipsum(grid = 'FALSE',
              plot_title_size = 15,
              axis_text_size = 11,
              axis_title_size = 13,
              ticks = TRUE)





lab = textGrob("Normalization is the process of converting the range of values (e.g. Units) for the organization to a number between zero and one; zero being the lowest value in the selected time span and one being the highest.
              This allows for the user to see which years an organization reached their minimum and maximum usage in comparison to other organizations, independent of scale. ",
               x = unit(.5, "npc"), just = c("center"), 
               gp = gpar(fontface = "italic", fontsize = 9, col = "black"))


gp = ggplotGrob(df)

# Add a row below the 2nd from the bottom
gp = gtable_add_rows(gp, unit(2, "grobheight", lab), -2)

# Add 'lab' grob to that row, under the plot panel
gp = gtable_add_grob(gp, lab, t = -2, l = gp$layout[gp$layout$name == "panel",]$l)

grid.newpage()
grid.draw(gp)


  
})

output$trendedHeatmapTable<-renderDataTable({
  
  
  df<-data.frame(selectedDS_byYear())%>%
    filter(!fy %in% c("2006","2007","2008","2009","2010"))%>%
    group_by(!!as.symbol(org_type()))%>%
    mutate(norm = round( norm( !!as.symbol(metric()) ), 2))%>%
    ungroup()
  
  col1<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
  else{'PIHP'}
  
  col2<-"Fiscal Year"
  
  metric_lab = str_replace_all(input$metric,pattern = "_"," ")
  
  col4 <-paste("Normalized",input$metric,"per",col1,sep = " ")
  
  
  DT::datatable(df,rownames = FALSE,class = 'cell-border stripe'
                ,colnames = c(col1,col2,metric_lab,col4),
                options = list(pageLength = 5)
                )
  
  

  
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Distribution Heatmap tab %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### UI Components

output$yAxisSel<-renderUI({
  
  type<-as.name(if(input$groupOrHcpcsOrMod_ == "svc_type"){'svc_type'}
                else if(input$groupOrHcpcsOrMod_ == "codeM_shortDesc"){'codeM_shortDesc'}
                else{"code_shortDesc"})
  
  org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
  
  grps<-if(input$groupOrHcpcsOrMod_ == 'svc_type'){"Service Type"}
  else if(input$groupOrHcpcsOrMod_ == "codeM_shortDesc"){'Code Modifiers'}
  else{"HCPC Codes"}
  
  
  options<-data404%>%
    filter(svc_type %in% case_when('All' %in% input$serviceType ~ levels(as.factor(data404$svc_type)),
                                   TRUE ~ input$serviceType))%>%
    distinct(!!type)%>%
    pull(!!type)
  

  selectizeInput(
    inputId = 'yAxisSel',
    label = paste('I want to compare ',org," across these ",grps,sep = ""),
    choices = options,
    multiple = TRUE,
    selected = options[1:8])
   # selected = "Peer Services"

  
  
  
})

output$sort_heatmap<-renderUI({
  
  
  if(input$CMHorPIHP == 'cmhsp'){
    
    
    dropdownButton(
      label = paste("Sort by a CMH"),
     radioButtons(
        
                  inputId = 'sort_heatmap',
                  label = 'Sort by Which CMH?',
                  choices = input$provider,
                  selected = input$provider[1]
                  
                  
                  ), 
      
      
      
      circle = F,
      right = F,
      status = "info",
      icon = icon("sort-amount-down"),
      width = "300px",
      size = 'xs',
       tooltip = tooltipOptions(title = "Choose a CMH to Sort")
      
    )
    
  }else{
    
    dropdownButton(
      label = "Sort by a PIHP",
      
      radioButtons(
        
        inputId = 'sort_heatmap',
        label = 'Sort by Which PIHP?',
        choices = input$provider,
        selected = input$provider[1]
        
        
      ), 
      

      circle = F,
      right = F,
      status = "info",
      icon = icon("sort-amount-down"),
      width = "300px",
      size = 'xs',
      tooltip = tooltipOptions(title = "Choose a PIHP to Sort")
      
    )
    
    
    
  } 
  
  
  
  
})

### Reactive inputs
yType<-reactive({input$groupOrHcpcsOrMod_})
ySel<-reactive({input$yAxisSel})
 
### Reactive datasets
 heatmapDS<-reactive({
   
   pop_filter<-if('' %in% popType()){ as.character(unique(data404$population))}else{
     
     data404[which(data404$population %in% popType()),'population']%>%
       mutate(population = as.character(population))%>%
       distinct(population)%>%
       pull(population)
   }
   
   stateAggData<-state_data%>%
     group_by(!!as.symbol(org_type()),fy)%>%
     summarise(TotalServed = sum(TotalServed,na.rm = TRUE))
   
#   test_prov<<-input$provider_options 
#   test_org<<-org_type()
   
  
   df<-data404%>%
     filter((!!as.symbol(org_type())) %in% input$provider,
            fy %in% fy_filter(),
            population %in% pop_filter,
            (!!as.symbol(groupOrHcpcsOrMod_())) %in% input$yAxisSel
     )
#=====================   
 #  test<<-df
   df<-
     df %>%
#=====================
     select(!!as.symbol(org_type()), # Provider column 
            (!!as.symbol(groupOrHcpcsOrMod_())),
             fy,
            cost,units,cases
     )%>%
     group_by(
       
         !!as.symbol(org_type()), # Provider column 
        (!!as.symbol(groupOrHcpcsOrMod_())),
         fy
     )%>%
     summarise_at(
       vars(cases,units,cost),
       list(~sum(., na.rm = T))
     )%>%
     mutate(
       cost_per_case = round(cost/cases,digits = 2),
       cost_per_unit = round(cost/units,digits = 2),
       unit_per_case = round(units/cases,digits = 1)
    )%>%
    left_join(
    
        stateAggData,by = c(org_type() ,"fy")
    )%>%
    mutate(
    
        cost_per_1K_served = round(((cost/TotalServed)*1000)),
        pct._served = round(((cases/TotalServed)*100),3)
    )
   
   
   
   # Transform into Z scores then turn Z scores into percentiles
   df<-df%>%
     select(!!as.symbol(org_type()),(!!as.symbol(groupOrHcpcsOrMod_())),!!as.symbol(metric()))%>%
     group_by((!!as.symbol(groupOrHcpcsOrMod_())))%>%
     mutate(metric = round((pnorm(scale_fun(!!as.symbol(metric())))*100),2))
   
 }) 
 
### Graphs and tables 
output$heatmap<-renderPlot({
  
  xlabs<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
        else{'PIHP'}
  
  type<-as.name(if(input$groupOrHcpcsOrMod_ == "svc_type"){'svc type'}else{"HCPCs"})
  
  populations<-as.data.frame(list(popType()))%>%
    mutate(popType = as.character(.[[1]]))%>%
    pull(popType)
  

  df<-heatmapDS() %>%
    rename(
      code_type = !!names(.[2]),
    )
  
heat_df<<-heatmapDS()
# Sorting the heatmap  -- JOey 

# Filtering the dataframe to create a sorting dataframe for 
# ordering the heatmap based on user selection. 

  sorted <-
    df%>%
    filter(!!as.symbol(org_type()) == input$sort_heatmap)

  
# Creating 3 new columns based on the single selection column 
# of type, HCPCs or Modifier. This just makes it easier when passing 
# to the plot. 
  df<-
    df %>%
    mutate(
      code_shortDesc = 
             factor(code_type, 
                    levels = sorted$code_type[  order(sorted$metric) ]), 
      svc_type = 
        factor(code_type, 
               levels = sorted$code_type[ order(sorted$metric) ]),
      codeM_shortDesc = 
        factor(code_type, 
               levels = sorted$code_type[ order(sorted$metric) ])
) %>%
    filter(is.na(code_type)==F,
           str_detect(metric,'NaN')==F,
           is.nan(metric)==F)
  

   ggplot(df,aes( y = (!!as.symbol(groupOrHcpcsOrMod_())),x = as.factor(!!as.symbol(org_type())))) + 
    geom_tile(aes(fill = metric), colour = "white") + 
     geom_tile(data = df%>%
                 filter(!!as.symbol(org_type()) == input$sort_heatmap,
                        is.na(code_type)==F,
                        str_detect(code_type,'NaN')==F,
                        is.nan(metric)==F),
               aes(fill = metric), colour = "black", size = 1) +
    scale_fill_gradientn(colours = c("#98C4F6","#236AB9","#FE2712"),na.value = "white")+
    theme(panel.grid=element_blank()) +
    coord_cartesian(expand=FALSE)+
  
    xlab(xlabs)+
    ylab(str_replace_all(input$metric,pattern = "_"," "))+
    labs(fill=paste(type," Pctl.",sep = "")) +
    theme_bw()+
    theme_ipsum(grid = FALSE,
                plot_title_size = 15,
                axis_text_size = 11,
                axis_title_size = 13)+    
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", 
                                 size = .5, linetype = "solid"))+
    labs(caption =paste("Populations ",paste(populations,collapse = ","),sep = ""))




})

output$dt<-DT::renderDataTable({
  
  col1<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
  else{'PIHP'}
  
  col2<-as.name(if(input$groupOrHcpcsOrMod_ == "svc_type"){'Service Type'}else{"HCPCS"})
  
  metric_lab = str_replace_all(input$metric,pattern = "_"," ")
  
  foo<-data.frame(heatmapDS())
  
  DT::datatable(foo,rownames = FALSE,class = 'cell-border stripe',
                colnames = c(col1,col2,metric_lab,'Pctl.'),
                options = list(pageLength = 5)
  )
  
})


# Tab selection test
#output$text <- renderText({paste0("You are viewing tab \"", input$tabs, "\"")})

###Download handlers and bookmarks 

# Downloadable csv of selected dataset ----

# 404 Data
output$data404Download <- downloadHandler(
  filename = function() {
    paste("404Data", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(data404, file, row.names = FALSE)
  }
)

# Service Groups 
output$ServiceGroups <- downloadHandler(
  filename = function() {
    paste("ServiceGroups", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(service_groups, file, row.names = FALSE)
  }
)



# heatData

output$heatData <- downloadHandler(
  filename = function() {
    paste("heatMapData", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(heatmapDS(), file, row.names = FALSE)
  }
)

# plot download
output$plot <- downloadHandler(
  filename = function() { paste("barchart", '.png', sep='') },
  content = function(file) {
    ggsave(file, plot = plotInput(),device = "png", width = 12 , height = 6
           )
  }
)






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CMH Cost Drivers Tab %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### UI Components

output$cost_driver_fy<-renderUI({
  
  
  selectInput(
    inputId = "cost_driver_fy",
    label = "Choose year", 
    choices = levels(as.factor(data404$fy)),
    selected = "2019",
    multiple = F
  )
  
  
  
})

output$target_cmh <-renderUI({
  
  
  rand_sel<-as_tibble(levels(data404$cmhsp))%>%
    sample_n(1) %>%
    pull()
  
  
  selectizeInput(
    inputId = 'target_cmh',
    label = "Target CMH",
    choices = levels(data404$cmhsp),
    multiple = F,
    selected = rand_sel)
})

output$peer_cmh <-renderUI({
  
  
  target<-input$target_cmh
  
 # target<-"Washtenaw"
  
  options<-levels(data404$cmhsp)
  
  options<-options[!(options %in% target)]
  
  rand_sel<-as_tibble(options)%>%
    sample_n(8) %>%
    pull()
  
  selectizeInput(
    inputId = 'peer_cmh',
    label = paste("Benchmark CMH's (avg. is taken)"),
    choices = levels(as.factor(options)),
    multiple = T,
    selected = rand_sel)
  
  
})

output$exclude_codes <- renderUI({
  
  selectizeInput(
    inputId = 'exclude_codes',
    label = paste("Codes that should be removed?"),
    choices = levels(as.factor(data404$code)),
    multiple = T,
    selected = c("H0043","H2015")
    
    )
  
  
  
})

output$target_codes <-renderUI({
  
  selectizeInput(
    inputId = 'target_codes',
    label = "HCPC code bundle to be included?",
    choices = c("Only codes used by Target" = 'target', 
                "Only codes used by Target or Peers" = 'peer', 
                "Include all codes" = 'all'),
    multiple = F,
    selected = "peer")
})

##----
output$cost_impact<-renderUI({
  
  radioButtons(
    
    inputId = "cost_impact",
    label = "Cost Per Case impact based on difference between Target CMH and..",
    choices = c("Peer group Avg." = 'potential_savings_peer',"State Avg." = 'potential_savings_state'
                #,"Average of the Two" = '(potential_savings_peer + potential_savings_state)/2 '
                ),
    selected = 'potential_savings_peer',
    inline = T
  )

})

output$code_highlight<-renderUI({
  
  selectizeInput(
    inputId = 'code_highlight',
    label = "Highlight bars based on...",
    choices = c("80/20","Alignment Impact","Drivers", "Above Svc.Type - Cost/1K cases" = "peer_svc_grp"),
    multiple = F,
    selected = c("Alignment Impact"))
  
  
})

output$service_group_filter<-renderUI({
  
  radioButtons(
    inputId = "service_group_filter",
    label = "Only include HCPC codes whose parent service group
             was above thier peers in cost/10K cases?",
    choices = c("No, include all HCPC Codes" = 'no',"Yes, focus on the outliers" = 'yes'),
    select = 'no'
  )
  
  
})



### Reactive inputs 
cost_impact<-reactive({input$cost_impact})
exclude_codes<-reactive({input$exclude_codes})
code_highlight<-reactive({input$code_highlight})
service_group_filter<-reactive({input$service_group_filter})
target_codes <-reactive({input$target_codes})
cost_driver_fy<-reactive({input$cost_driver_fy})

### Reactive Datasets 
data404_pivot_compare<-eventReactive(input$button,{
  
#==========================================
# applying first set of global filters 
# to tag peer and target groups
#==========================================
  

  data404_group<-data404%>%
    filter(!code %in% input$exclude_codes,
           fy == cost_driver_fy())%>%
    mutate(cmh = cmhsp,
           group = case_when(cmh %in% input$peer_cmh ~ "peer",
                             cmh %in% input$target_cmh ~"target",
                             T ~ NA_character_),
           state = case_when(cmh %in% input$target_cmh ~ "target",
                             T ~ "state")
    )%>%
    select(fy,cmh,svc_type,population,code,code_shortDesc,code_mod,codeM_shortDesc,
           cases,units,cost,cost_pct_tot,pct_cmh_served,group,state
    )%>%
    distinct()#%>%
   # filter(cmh == input$target_cmh)
  
  #==========================================
  # creating the metrics for each group
  # then joining back together so I can see 
  # each group side-by-side
  #==========================================

#target table  
   target<-data404_group%>%
    filter(fy == cost_driver_fy(),
           group == 'target')%>%
    group_by(svc_type,population, code)%>%
    summarise_at(
      vars(cases,units,cost,cost_pct_tot),
      list(~sum(., na.rm = T))
    )%>%
    mutate(
      cost_per_case = round(cost/cases,digits = 2),
      cost_per_unit = round(cost/units,digits = 2),
      unit_per_case = round(units/cases,digits = 1),
      group = 'target'
    )%>%
    ungroup()
  
# peer group table 
   peer<-data404_group%>%
     filter(fy == cost_driver_fy(),
            group == 'peer')%>%
     group_by(population,svc_type,cmh,code)%>%
     summarise_at(
       vars(cases,units,cost,cost_pct_tot),
       list(~sum(.,na.rm = T))
     )%>%
     mutate(
       cost_per_case = round(cost/cases,digits = 2),
       cost_per_unit = round(cost/units,digits = 2),
       unit_per_case = round(units/cases,digits = 1),
       group = 'peer'
     )%>%
     mutate(cost_per_case = na_if(cost_per_case,"Inf"),
            unit_per_case = na_if(unit_per_case,"Inf"))%>%
     ungroup()%>%
     group_by(population,svc_type,code)%>%
     summarise_at(
       vars(cases,units,cost,cost_pct_tot,
            cost_per_case,cost_per_unit, unit_per_case
       ),
       list(~round(mean(., na.rm = T),2))
     )%>%
     mutate( group = 'peer'
     )%>%
     ungroup()  
  
   # State average table  
   state<-data404_group%>%
     filter(fy == cost_driver_fy()
            #,state == 'state'
     )%>%
     group_by(fy,population,svc_type,cmh, code)%>%
     summarise_at(
       vars(cases,units,cost,cost_pct_tot),
       list(~sum(., na.rm = T))
     )%>%
     mutate(
       cost_per_case = round(cost/cases,digits = 2),
       cost_per_unit = round(cost/units,digits = 2),
       unit_per_case = round(units/cases,digits = 1),
       group = 'state'
     )%>%
     mutate(cost_per_case = na_if(cost_per_case,"Inf"),
            unit_per_case = na_if(unit_per_case,"Inf"))%>%
     ungroup()%>%
     filter_if( #remove INF values from dividing by zero
       ~is.numeric(.), all_vars(!is.infinite(.))
     )%>%
     # calulating each CMH (pop/code), 
     # then averaging
     group_by(population,svc_type,code)%>%
     summarise_at(
       vars(cases,units,cost,cost_pct_tot,
            cost_per_case,cost_per_unit, unit_per_case
       ),
       list(~ round(mean(., na.rm = T),2))
     )%>%
     mutate( group = as.character('state')
     )%>%
     ungroup()
   
   
df<-rbind(target,peer,state)  



#==================================
# Creating the variables for 
# comparision 
#==================================


df<-df%>%
  pivot_wider(id_cols = c(svc_type,population,code),
              values_from = c(cases,
                              cost_pct_tot,
                              cost_per_case,cost_per_unit,
                              unit_per_case),
              names_from = group)

#replace all zeros with NA  
df <-df %>% replace(is.na(.), 0)%>%
   mutate(
    cost_per_case_diff_peer = cost_per_case_target - cost_per_case_peer,
    cost_per_case_diff_state = cost_per_case_target - cost_per_case_state,
    cost_pct_tot_diff_peer = cost_pct_tot_target - cost_pct_tot_peer,
    cost_pct_tot_diff_state = cost_pct_tot_target - cost_pct_tot_state,
    cost_per_unit_diff_peer = cost_per_unit_target - cost_per_unit_peer,
    cost_per_unit_diff_state = cost_per_unit_target - cost_per_unit_state,
    units_per_case_diff_peer = unit_per_case_target - unit_per_case_peer,
    units_per_case_diff_state = unit_per_case_target - unit_per_case_state,
    
    
    potential_savings_peer = cost_per_case_diff_peer * cases_target, 
    potential_savings_state = cost_per_case_diff_state * cases_target,
    savings_peer_and_state = case_when(potential_savings_peer > 0 & 
                                         potential_savings_state > 0 ~ 1,
                                       TRUE ~ 0),
    
    
    code_pop = as.factor(paste(code,population,sep = "-")),
    
    rule_sets_peer = case_when(   
                                  units_per_case_diff_peer > 0 &
                                  cost_per_unit_diff_peer > 0 ~ "High units/case & cost/unit",
      
                                  units_per_case_diff_peer <= 0 &
                                  cost_per_unit_diff_peer > 0 ~ "High cost per unit",
                                  
                                  units_per_case_diff_peer > 0 &
                                  cost_per_unit_diff_peer <= 0 ~ "High units per case",
                                  
                               #   cases_target > cases_peer ~ 'larger number of cases',
                                  
                                  TRUE ~ "unclassified"),
    
    rule_sets_state = case_when( units_per_case_diff_state > 0 &
                                   cost_per_unit_diff_state > 0 ~ "High units/case & cost/unit",
                                 
                                 units_per_case_diff_state <= 0 &
                                   cost_per_unit_diff_state > 0 ~ "High cost per unit",
                                 
                                 units_per_case_diff_state > 0 &
                                   cost_per_unit_diff_state <= 0 ~ "High units per case",
                                 
                               #  cases_target > cases_state ~ 'larger number of cases',
                                 
                                 TRUE ~ "unclassified"))

  

#====================================
# Attaching service type cost 
# compared to peer group as a check
#====================================


# target table 
target_svc_type<-data404_group%>%
  filter(fy == cost_driver_fy(),
         group == 'target')%>%
  left_join(
    state_data%>%mutate(cmh = cmhsp),by = c('cmh',"fy")
  )%>%
  group_by(population,svc_type)%>%
  summarise(
    cost = sum(cost,na.rm = T),
    TotalServed = max(TotalServed,na.rm = T)
  )%>%
  mutate(
    group = 'target',
    cost_per_1K_served = round((cost/TotalServed)*1000,0)
  )%>%
  ungroup()%>%
  select(-cost,- TotalServed)


# peer group table 
peer_svc_type<-data404_group%>%
  filter(fy == cost_driver_fy(),
         group == 'peer')%>%
  left_join(
    state_data%>%mutate(cmh = cmhsp),by = c('cmh',"fy")
  )%>%
  group_by(population,svc_type,cmh)%>%
  summarise(
    cost = sum(cost,na.rm = T),
    TotalServed = max(TotalServed,na.rm = T)
  )%>%
  mutate(
    group = 'peer',
    cost_per_1K_served = round((cost/TotalServed)*1000,0),
  )%>%
  ungroup()%>%
  # select(-cost,- TotalServed)%>%
  group_by(population,svc_type,group)%>%
  summarise(cost_per_1K_served = round( mean(cost_per_1K_served,na.rm = T) , 2))%>%
  ungroup()

# state average scv cost table 
state_svc_type<-data404_group%>%
  filter(fy == cost_driver_fy(),
  )%>%
  left_join(
    state_data%>%mutate(cmh = cmhsp),by = c('cmh',"fy")
  )%>%
  group_by(population,svc_type,cmh)%>%
  summarise(
    cost = sum(cost,na.rm = T),
    TotalServed = max(TotalServed,na.rm = T)
  )%>%
  mutate(
    group = 'state',
    cost_per_1K_served = round((cost/TotalServed)*1000,0),
  )%>%
  ungroup()%>%
  # select(-cost,- TotalServed)%>%
  group_by(population,svc_type,group)%>%
  summarise(cost_per_1K_served = round( mean(cost_per_1K_served,na.rm = T) , 2))%>%
  ungroup()



svc_type_costs<-rbind(target_svc_type,peer_svc_type,state_svc_type)%>%
  pivot_wider(id_cols = c(population,svc_type), 
              values_from = c(cost_per_1K_served),
              names_from = group) %>%
  mutate(cost_per_1K_served_svc_type_diff_peer = target - peer,
         cost_per_1K_served_svc_type_diff_state = target - state)%>%
  select(-target,-peer,-state)




df<-df%>%
  left_join(svc_type_costs, by = c("population","svc_type"))



#======================================
# Attaching pareto by popoulation/code 
# for Washtenaw to dataframe
#======================================


pareto_by_population<-data404_group%>%
  filter(fy %in% cost_driver_fy())%>%
  filter(group == 'target')%>%
  group_by(population,code,code_shortDesc)%>%
  summarise(cost = sum(cost,na.rm = T))%>%
  ungroup()%>%
  group_by(population)%>%
  arrange(desc(cost), .by_group = TRUE)%>%  
  mutate( total = sum(cost),
          pct_to_total = round(cost/sum(cost) * 100,2),
          running_total = cumsum(pct_to_total),
          `80/20` = case_when(running_total < 85 ~ "yes",
                              TRUE ~ 'no')
)%>%
select(population,code,`80/20`,cost_pct_to_ttl = pct_to_total)


df<-df%>%
  left_join(pareto_by_population, c("population","code"))%>%
  left_join(code_ref,by = c("code"))



df<-if(target_codes() == "peer"){
        df<-df%>%
        filter(cases_target>0 | cases_peer>0)
}else if(target_codes() == 'target'){
        df<-df%>%
          filter(cases_target>0)}

else{
  
  df<-df
 
} 
  
})

### Graphs and Tables 

output$code_table = DT::renderDataTable({
  
  df<-as.data.frame(data404_pivot_compare())%>%
    select(code,code_shortDesc,svc_type)%>%
    mutate(code = as.character(code))%>%
    distinct()%>%
    rename(`HCPC Code` = code,`Code Desc` = code_shortDesc,
           `Service Type` = svc_type)
  
  DT::datatable(df, 
                selection = 'single',
                caption = "Search and Click on the row to highlight the selected code",
                rownames = F, 
                options = list(pageLength = 4,searching = T,ordering=F,dom = 'ft' 
                              # columnDefs = list(list(className = 'dt-center', targets = 0:4))
                               ))
  
})

output$code_table_selected  = renderTable({
  
  ds<-as.data.frame(data404_pivot_compare())%>%
    filter(
      !is.infinite(potential_savings_peer),
      !is.na(cost_pct_tot_diff_peer)
    )%>%
    filter( population == 'DD',
            code == 'H0038')
  
  
})

 # Same exact graphs and functionality. The filter for population is
 # the only difference. When making a change, focus on the dd_pop. When that is working
 # simply copy and paste the plot function (renderPlotly) into MIA and MIC. The only 
 # thing you need to change in the mic/mia renderPlotly would be the 3 times the population 
 # is filtered. Simply change from DD to MIA or MIC
output$dd_pop<-renderPlotly({
  
  req(cost_impact())
  req(code_highlight())
  
  drivers_based_on_costimpact_selection<-if(cost_impact() == 'potential_savings_peer'){
                                            "rule_sets_peer"}
                                         else{"rule_sets_state"}
  
  svc_type_cost_on_costimpact_selection<-if(cost_impact() == 'potential_savings_peer'){
                                             "cost_per_1K_served_svc_type_diff_peer"}else{
                                            "cost_per_1K_served_svc_type_diff_state"
                                             }
  
  
  
  df<-as.data.frame(data404_pivot_compare())%>%
       filter(population == 'DD')
  
  


df<-df%>%
    filter(
        !is.infinite(potential_savings_peer),
        !is.na(cost_pct_tot_diff_peer)
    )%>%
    mutate( 
         savings =  round( !!as.symbol(cost_impact()) /2,2),
         code_pop = fct_reorder(code_pop,savings),
         `Cost Driver` = fct_relevel(!!as.symbol(drivers_based_on_costimpact_selection),
                                     "High units/case & cost/unit","High cost per unit",
                                            "High units per case","unclassified"),
         `Alignment Impact` = case_when(savings > 0 ~ "Positive Cost Impact",
                           TRUE ~ "Negative Cost Impact"),
         peer_svc_type = case_when(!!as.symbol(svc_type_cost_on_costimpact_selection) > 0 ~"Above Svc.Type Cost/1K Cases", 
                                   !!as.symbol(svc_type_cost_on_costimpact_selection) < 0 ~ 'Below Svc.Type Cost/1K Cases',
                                   TRUE ~ "No Comparison Available"),
                                   
                                
         `80/20` = case_when(is.na(`80/20`)==TRUE ~ 'unknown',
                             TRUE ~ `80/20`),
         `80/20` = fct_relevel(`80/20`,'yes','no','unknown')
         )


# Getting click information from table selction 
table_click<-if(is.null(input$code_table_rows_selected) == TRUE){
  
  ds<-as.data.frame(data404_pivot_compare())%>%
    filter(
      !is.infinite(potential_savings_peer),
      !is.na(cost_pct_tot_diff_peer)
    )%>%
    filter(population == 'DD')%>%
    mutate( 
      savings =  round( !!as.symbol(cost_impact()) /2,2),
      code_pop = fct_reorder(code_pop,savings),
      `Cost Driver` = fct_relevel(!!as.symbol(drivers_based_on_costimpact_selection),
                                  "High units/case & cost/unit","High cost per unit",
                                  "High units per case","unclassified"),
      `Alignment Impact` = case_when(savings > 0 ~ "Positive Cost Impact",
                                     TRUE ~ "Negative Cost Impact"),
      peer_svc_type = case_when(!!as.symbol(svc_type_cost_on_costimpact_selection) > 0 ~"Above Svc.Type Cost/1K Cases", 
                                !!as.symbol(svc_type_cost_on_costimpact_selection) < 0 ~ 'Below Svc.Type Cost/1K Cases',
                                TRUE ~ "No Comparison Available"),
      
      
      `80/20` = case_when(is.na(`80/20`)==TRUE ~ 'unknown',
                          TRUE ~ `80/20`),
      `80/20` = fct_relevel(`80/20`,'yes','no','unknown')
    )
  
  
  
  table_click<-ds%>%
               slice(1)%>%
               mutate(savings = 0)
  
  
}else{
  
     ds<-as.data.frame(data404_pivot_compare())%>%
           filter(population == 'DD')%>%
       filter(
         !is.infinite(potential_savings_peer),
         !is.na(cost_pct_tot_diff_peer)
       )

     # Selecting the specific row being clicked from the table     
     row <-input$code_table_rows_selected
     
     code_sel<-as.data.frame(data404_pivot_compare())%>%
             select(code,svc_type)%>%
             mutate(code_sel = as.character(code))%>%
             distinct()%>%
             slice(row)%>%
             select(code_sel)%>%
             pull()
     
     
    table_click<- ds%>%
                  filter(code == code_sel)%>%
      mutate( 
        savings =  round( !!as.symbol(cost_impact()) /2,2),
        code_pop = fct_reorder(code_pop,savings),
        `Cost Driver` = fct_relevel(!!as.symbol(drivers_based_on_costimpact_selection),
                                    "High units/case & cost/unit","High cost per unit",
                                    "High units per case","unclassified"),
        `Alignment Impact` = case_when(savings > 0 ~ "Positive Cost Impact",
                                       TRUE ~ "Negative Cost Impact"),
        peer_svc_type = case_when(!!as.symbol(svc_type_cost_on_costimpact_selection) > 0 ~"Above Svc.Type Cost/1K Cases", 
                                  !!as.symbol(svc_type_cost_on_costimpact_selection) < 0 ~ 'Below Svc.Type Cost/1K Cases',
                                  TRUE ~ "No Comparison Available"),
        
        
        `80/20` = case_when(is.na(`80/20`)==TRUE ~ 'unknown',
                            TRUE ~ `80/20`),
        `80/20` = fct_relevel(`80/20`,'yes','no','unknown')
      )
    
    
    

  
}


title_group<-if(cost_impact() == 'potential_savings_peer'){
              "Peer Avg."}
              else{"State Avg."}


#===============================================================
# If statement based on which color the bar highlights 
#===============================================================      
        
p<-if(code_highlight() == "Alignment Impact"){
  
  p<-  ggplot(df,aes( x = code_pop, 
                y = savings,
                text = paste(
                  "HCPC Code:",code_shortDesc,
                  "\n",
                  "Population:",population,
                  "\n",
                  "Cost Impact",savings,
                  "\n",
                  "Potential Cost Driver:",`Cost Driver`))
    )+geom_bar(stat = "identity", aes(fill = `Alignment Impact`))+
    geom_bar(data = table_click,stat ="identity",aes( y = savings),color = "#292F33")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_fill_manual(values = c("grey","#92a8d1"))+
    scale_y_continuous(label = number_format(big.mark = ","))+
    theme_minimal()+
    xlab("HCPC Code")+
    ylab("Potential Cost Impact")
  
  ggplotly(p,tooltip = 'text')%>%
    layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
           yaxis = list(showgrid = F),
           title = list(text = 
                          paste0('<br>',
                                 'Potential Cost Impact of Service Alignment with ',
                                 title_group,
                                 '<br>',
                                 '<sup>',
                                 'Cost per case difference X number of cases for target'
                                 
                                 )))
}else if(code_highlight() == "80/20"){
  
  p<-  ggplot(df,aes( x = code_pop, 
                      y = savings,
                      text = paste(
                        "HCPC Code:",code_shortDesc,
                        "\n",
                        "Population:",population,
                        "\n",
                        "Cost Impact",savings,
                        "\n",
                        "Potential Cost Driver:",`Cost Driver`))
  )+geom_bar(stat = "identity", aes(fill = `80/20`))+
    geom_bar(data = table_click,stat ="identity",aes( y = savings),color = "#292F33")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_fill_manual(values = c("#92a8d1","grey",'white'))+
   # paletteer_d("awtools::b_palette")
    scale_y_continuous(label = number_format(big.mark = ","))+
    theme_minimal()+
    xlab("HCPC Code")+
    ylab("Potential Cost Impact")
  
  ggplotly(p,tooltip = 'text')%>%
    layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
           yaxis = list(showgrid = F),
           title = list(text = 
                          paste0('<br>',
                                 'Potential Cost Impact of Service Alignment with ',
                                 title_group,
                                 '<br>',
                                 '<sup>',
                                 'Cost per case difference X number of cases for target')))



}else if(code_highlight() == "Drivers"){
  
  p<-  ggplot(df,aes( x = code_pop, 
                      y = savings,
                      text = paste(
                        "HCPC Code:",code_shortDesc,
                        "\n",
                        "Population:",population,
                        "\n",
                        "Cost Impact",savings,
                        "\n",
                        "Potential Cost Driver:",`Cost Driver`))
  )+geom_bar(stat = "identity", aes(fill =  `Cost Driver`))+
    geom_bar(data = table_click,stat ="identity",aes( y = savings),color = "#292F33")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_fill_manual(values = c("#50394c","#c94c4c",'#618685','grey'))+
    # paletteer_d("awtools::b_palette")
    scale_y_continuous(label = number_format(big.mark = ","))+
    theme_minimal()+
    xlab("HCPC Code")+
    ylab("Potential Cost Impact")
  
  ggplotly(p,tooltip = 'text')%>%
    layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
           yaxis = list(showgrid = F),
           title = list(text = 
                          paste0('<br>',
                                 'Potential Cost Impact of Service Alignment with ',
                                 title_group,
                                 '<br>',
                                 '<sup>',
                                 'Cost per case difference X number of cases for target')))

}else{
  

  
  p<-  ggplot(df%>%mutate(`Peer Svc Group` = peer_svc_type),aes( x = code_pop, 
                      y = savings,
                      text = paste(
                        "HCPC Code:",code_shortDesc,
                        "\n",
                        "Population:",population,
                        "\n",
                        "Cost Impact",savings,
                        "\n",
                        "Potential Cost Driver:",`Cost Driver`))
  )+geom_bar(stat = "identity", aes(fill = `Peer Svc Group`))+
    geom_bar(data = table_click,stat ="identity",aes( y = savings),fill = "#292F33")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_fill_manual(values = c("#92a8d1","grey","salmon"))+
    #scale_fill_discrete()+
    scale_y_continuous(label = number_format(big.mark = ","))+
    theme_minimal()+
    xlab("HCPC Code/ Population")+
    ylab("Potential Cost Impact")
  
  ggplotly(p,tooltip = 'text')%>%
    layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
           yaxis = list(showgrid = F),
           title = list(text = 
                          paste0('<br>',
                                 'Potential Cost Impact of Service Alignment with ',
                                 title_group,
                                 '<br>',
                                 '<sup>',
                                 'Cost per case difference X number of cases for target')))
                                 
  
}
  

p
  

  
  
})

output$mia_pop<-renderPlotly({
  
  req(cost_impact())
  req(code_highlight())
  
  drivers_based_on_costimpact_selection<-if(cost_impact() == 'potential_savings_peer'){
    "rule_sets_peer"}
  else{"rule_sets_state"}
  
  svc_type_cost_on_costimpact_selection<-if(cost_impact() == 'potential_savings_peer'){
    "cost_per_1K_served_svc_type_diff_peer"}else{
      "cost_per_1K_served_svc_type_diff_state"
    }
  
  
  
  df<-as.data.frame(data404_pivot_compare())%>%
    filter(population == 'MIA')
  
  
  
  
  df<-df%>%
    filter(
      !is.infinite(potential_savings_peer),
      !is.na(cost_pct_tot_diff_peer)
    )%>%
    mutate( 
      savings =  round( !!as.symbol(cost_impact()) /2,2),
      code_pop = fct_reorder(code_pop,savings),
      `Cost Driver` = fct_relevel(!!as.symbol(drivers_based_on_costimpact_selection),
                                  "High units/case & cost/unit","High cost per unit",
                                  "High units per case","unclassified"),
      `Alignment Impact` = case_when(savings > 0 ~ "Positive Cost Impact",
                                     TRUE ~ "Negative Cost Impact"),
      peer_svc_type = case_when(!!as.symbol(svc_type_cost_on_costimpact_selection) > 0 ~"Above Svc.Type Cost/1K Cases", 
                                !!as.symbol(svc_type_cost_on_costimpact_selection) < 0 ~ 'Below Svc.Type Cost/1K Cases',
                                TRUE ~ "No Comparison Available"),
      
      
      `80/20` = case_when(is.na(`80/20`)==TRUE ~ 'unknown',
                          TRUE ~ `80/20`),
      `80/20` = fct_relevel(`80/20`,'yes','no','unknown')
    )
  
  
  # Getting click information from table selction 
  table_click<-if(is.null(input$code_table_rows_selected) == TRUE){
    
    ds<-as.data.frame(data404_pivot_compare())%>%
      filter(
        !is.infinite(potential_savings_peer),
        !is.na(cost_pct_tot_diff_peer)
      )%>%
      filter(population == 'MIA')%>%
      mutate( 
        savings =  round( !!as.symbol(cost_impact()) /2,2),
        code_pop = fct_reorder(code_pop,savings),
        `Cost Driver` = fct_relevel(!!as.symbol(drivers_based_on_costimpact_selection),
                                    "High units/case & cost/unit","High cost per unit",
                                    "High units per case","unclassified"),
        `Alignment Impact` = case_when(savings > 0 ~ "Positive Cost Impact",
                                       TRUE ~ "Negative Cost Impact"),
        peer_svc_type = case_when(!!as.symbol(svc_type_cost_on_costimpact_selection) > 0 ~"Above Svc.Type Cost/1K Cases", 
                                  !!as.symbol(svc_type_cost_on_costimpact_selection) < 0 ~ 'Below Svc.Type Cost/1K Cases',
                                  TRUE ~ "No Comparison Available"),
        
        
        `80/20` = case_when(is.na(`80/20`)==TRUE ~ 'unknown',
                            TRUE ~ `80/20`),
        `80/20` = fct_relevel(`80/20`,'yes','no','unknown')
      )
    
    
    
    table_click<-ds%>%
      slice(1)%>%
      mutate(savings = 0)
    
    
  }else{
    
    ds<-as.data.frame(data404_pivot_compare())%>%
      filter(population == 'MIA')%>%
      filter(
        !is.infinite(potential_savings_peer),
        !is.na(cost_pct_tot_diff_peer)
      )
    
    # Selecting the specific row being clicked from the table     
    row <-input$code_table_rows_selected
    
    code_sel<-as.data.frame(data404_pivot_compare())%>%
      select(code,svc_type)%>%
      mutate(code_sel = as.character(code))%>%
      distinct()%>%
      slice(row)%>%
      select(code_sel)%>%
      pull()
    
    
    table_click<- ds%>%
      filter(code == code_sel)%>%
      mutate( 
        savings =  round( !!as.symbol(cost_impact()) /2,2),
        code_pop = fct_reorder(code_pop,savings),
        `Cost Driver` = fct_relevel(!!as.symbol(drivers_based_on_costimpact_selection),
                                    "High units/case & cost/unit","High cost per unit",
                                    "High units per case","unclassified"),
        `Alignment Impact` = case_when(savings > 0 ~ "Positive Cost Impact",
                                       TRUE ~ "Negative Cost Impact"),
        peer_svc_type = case_when(!!as.symbol(svc_type_cost_on_costimpact_selection) > 0 ~"Above Svc.Type Cost/1K Cases", 
                                  !!as.symbol(svc_type_cost_on_costimpact_selection) < 0 ~ 'Below Svc.Type Cost/1K Cases',
                                  TRUE ~ "No Comparison Available"),
        
        
        `80/20` = case_when(is.na(`80/20`)==TRUE ~ 'unknown',
                            TRUE ~ `80/20`),
        `80/20` = fct_relevel(`80/20`,'yes','no','unknown')
      )
    
    
    
    
    
  }
  
  
  title_group<-if(cost_impact() == 'potential_savings_peer'){
    "Peer Avg."}
  else{"State Avg."}
  
  
  #===============================================================
  # If statement based on which color the bar highlights 
  #===============================================================      
  
  p<-if(code_highlight() == "Alignment Impact"){
    
    p<-  ggplot(df,aes( x = code_pop, 
                        y = savings,
                        text = paste(
                          "HCPC Code:",code_shortDesc,
                          "\n",
                          "Population:",population,
                          "\n",
                          "Cost Impact",savings,
                          "\n",
                          "Potential Cost Driver:",`Cost Driver`))
    )+geom_bar(stat = "identity", aes(fill = `Alignment Impact`))+
      geom_bar(data = table_click,stat ="identity",aes( y = savings),color = "#292F33")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_fill_manual(values = c("grey","#92a8d1"))+
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme_minimal()+
      xlab("HCPC Code")+
      ylab("Potential Cost Impact")
    
    ggplotly(p,tooltip = 'text')%>%
      layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
             yaxis = list(showgrid = F),
             title = list(text = 
                            paste0('<br>',
                                   'Potential Cost Impact of Service Alignment with ',
                                   title_group,
                                   '<br>',
                                   '<sup>',
                                   'Cost per case difference X number of cases for target'
                                   
                            )))
  }else if(code_highlight() == "80/20"){
    
    p<-  ggplot(df,aes( x = code_pop, 
                        y = savings,
                        text = paste(
                          "HCPC Code:",code_shortDesc,
                          "\n",
                          "Population:",population,
                          "\n",
                          "Cost Impact",savings,
                          "\n",
                          "Potential Cost Driver:",`Cost Driver`))
    )+geom_bar(stat = "identity", aes(fill = `80/20`))+
      geom_bar(data = table_click,stat ="identity",aes( y = savings),color = "#292F33")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_fill_manual(values = c("#92a8d1","grey",'white'))+
      # paletteer_d("awtools::b_palette")
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme_minimal()+
      xlab("HCPC Code")+
      ylab("Potential Cost Impact")
    
    ggplotly(p,tooltip = 'text')%>%
      layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
             yaxis = list(showgrid = F),
             title = list(text = 
                            paste0('<br>',
                                   'Potential Cost Impact of Service Alignment with ',
                                   title_group,
                                   '<br>',
                                   '<sup>',
                                   'Cost per case difference X number of cases for target')))
    
    
    
  }else if(code_highlight() == "Drivers"){
    
    p<-  ggplot(df,aes( x = code_pop, 
                        y = savings,
                        text = paste(
                          "HCPC Code:",code_shortDesc,
                          "\n",
                          "Population:",population,
                          "\n",
                          "Cost Impact",savings,
                          "\n",
                          "Potential Cost Driver:",`Cost Driver`))
    )+geom_bar(stat = "identity", aes(fill =  `Cost Driver`))+
      geom_bar(data = table_click,stat ="identity",aes( y = savings),color = "#292F33")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_fill_manual(values = c("#50394c","#c94c4c",'#618685','grey'))+
      # paletteer_d("awtools::b_palette")
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme_minimal()+
      xlab("HCPC Code")+
      ylab("Potential Cost Impact")
    
    ggplotly(p,tooltip = 'text')%>%
      layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
             yaxis = list(showgrid = F),
             title = list(text = 
                            paste0('<br>',
                                   'Potential Cost Impact of Service Alignment with ',
                                   title_group,
                                   '<br>',
                                   '<sup>',
                                   'Cost per case difference X number of cases for target')))
    
  }else{
    
    
    
    p<-  ggplot(df%>%mutate(`Peer Svc Group` = peer_svc_type),aes( x = code_pop, 
                                                                   y = savings,
                                                                   text = paste(
                                                                     "HCPC Code:",code_shortDesc,
                                                                     "\n",
                                                                     "Population:",population,
                                                                     "\n",
                                                                     "Cost Impact",savings,
                                                                     "\n",
                                                                     "Potential Cost Driver:",`Cost Driver`))
    )+geom_bar(stat = "identity", aes(fill = `Peer Svc Group`))+
      geom_bar(data = table_click,stat ="identity",aes( y = savings),fill = "#292F33")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_fill_manual(values = c("#92a8d1","grey","salmon"))+
      #scale_fill_discrete()+
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme_minimal()+
      xlab("HCPC Code/ Population")+
      ylab("Potential Cost Impact")
    
    ggplotly(p,tooltip = 'text')%>%
      layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
             yaxis = list(showgrid = F),
             title = list(text = 
                            paste0('<br>',
                                   'Potential Cost Impact of Service Alignment with ',
                                   title_group,
                                   '<br>',
                                   '<sup>',
                                   'Cost per case difference X number of cases for target')))
    
    
  }
  
  
  p
  
  
  
  
})

output$mic_pop<-renderPlotly({
  
  req(cost_impact())
  req(code_highlight())
  
  drivers_based_on_costimpact_selection<-if(cost_impact() == 'potential_savings_peer'){
    "rule_sets_peer"}
  else{"rule_sets_state"}
  
  svc_type_cost_on_costimpact_selection<-if(cost_impact() == 'potential_savings_peer'){
    "cost_per_1K_served_svc_type_diff_peer"}else{
      "cost_per_1K_served_svc_type_diff_state"
    }
  
  
  
  df<-as.data.frame(data404_pivot_compare())%>%
    filter(population == 'MIC')
  
  
  
  
  df<-df%>%
    filter(
      !is.infinite(potential_savings_peer),
      !is.na(cost_pct_tot_diff_peer)
    )%>%
    mutate( 
      savings =  round( !!as.symbol(cost_impact()) /2,2),
      code_pop = fct_reorder(code_pop,savings),
      `Cost Driver` = fct_relevel(!!as.symbol(drivers_based_on_costimpact_selection),
                                  "High units/case & cost/unit","High cost per unit",
                                  "High units per case","unclassified"),
      `Alignment Impact` = case_when(savings > 0 ~ "Positive Cost Impact",
                                     TRUE ~ "Negative Cost Impact"),
      peer_svc_type = case_when(!!as.symbol(svc_type_cost_on_costimpact_selection) > 0 ~"Above Svc.Type Cost/1K Cases", 
                                !!as.symbol(svc_type_cost_on_costimpact_selection) < 0 ~ 'Below Svc.Type Cost/1K Cases',
                                TRUE ~ "No Comparison Available"),
      
      
      `80/20` = case_when(is.na(`80/20`)==TRUE ~ 'unknown',
                          TRUE ~ `80/20`),
      `80/20` = fct_relevel(`80/20`,'yes','no','unknown')
    )
  
  
  # Getting click information from table selction 
  table_click<-if(is.null(input$code_table_rows_selected) == TRUE){
    
    ds<-as.data.frame(data404_pivot_compare())%>%
      filter(
        !is.infinite(potential_savings_peer),
        !is.na(cost_pct_tot_diff_peer)
      )%>%
      filter(population == 'MIC')%>%
      mutate( 
        savings =  round( !!as.symbol(cost_impact()) /2,2),
        code_pop = fct_reorder(code_pop,savings),
        `Cost Driver` = fct_relevel(!!as.symbol(drivers_based_on_costimpact_selection),
                                    "High units/case & cost/unit","High cost per unit",
                                    "High units per case","unclassified"),
        `Alignment Impact` = case_when(savings > 0 ~ "Positive Cost Impact",
                                       TRUE ~ "Negative Cost Impact"),
        peer_svc_type = case_when(!!as.symbol(svc_type_cost_on_costimpact_selection) > 0 ~"Above Svc.Type Cost/1K Cases", 
                                  !!as.symbol(svc_type_cost_on_costimpact_selection) < 0 ~ 'Below Svc.Type Cost/1K Cases',
                                  TRUE ~ "No Comparison Available"),
        
        
        `80/20` = case_when(is.na(`80/20`)==TRUE ~ 'unknown',
                            TRUE ~ `80/20`),
        `80/20` = fct_relevel(`80/20`,'yes','no','unknown')
      )
    
    
    
    table_click<-ds%>%
      slice(1)%>%
      mutate(savings = 0)
    
    
  }else{
    
    ds<-as.data.frame(data404_pivot_compare())%>%
      filter(population == 'MIC')%>%
      filter(
        !is.infinite(potential_savings_peer),
        !is.na(cost_pct_tot_diff_peer)
      )
    
    # Selecting the specific row being clicked from the table     
    row <-input$code_table_rows_selected
    
    code_sel<-as.data.frame(data404_pivot_compare())%>%
      select(code,svc_type)%>%
      mutate(code_sel = as.character(code))%>%
      distinct()%>%
      slice(row)%>%
      select(code_sel)%>%
      pull()
    
    
    table_click<- ds%>%
      filter(code == code_sel)%>%
      mutate( 
        savings =  round( !!as.symbol(cost_impact()) /2,2),
        code_pop = fct_reorder(code_pop,savings),
        `Cost Driver` = fct_relevel(!!as.symbol(drivers_based_on_costimpact_selection),
                                    "High units/case & cost/unit","High cost per unit",
                                    "High units per case","unclassified"),
        `Alignment Impact` = case_when(savings > 0 ~ "Positive Cost Impact",
                                       TRUE ~ "Negative Cost Impact"),
        peer_svc_type = case_when(!!as.symbol(svc_type_cost_on_costimpact_selection) > 0 ~"Above Svc.Type Cost/1K Cases", 
                                  !!as.symbol(svc_type_cost_on_costimpact_selection) < 0 ~ 'Below Svc.Type Cost/1K Cases',
                                  TRUE ~ "No Comparison Available"),
        
        
        `80/20` = case_when(is.na(`80/20`)==TRUE ~ 'unknown',
                            TRUE ~ `80/20`),
        `80/20` = fct_relevel(`80/20`,'yes','no','unknown')
      )
    
    
    
    
    
  }
  
  
  title_group<-if(cost_impact() == 'potential_savings_peer'){
    "Peer Avg."}
  else{"State Avg."}
  
  
  #===============================================================
  # If statement based on which color the bar highlights 
  #===============================================================      
  
  p<-if(code_highlight() == "Alignment Impact"){
    
    p<-  ggplot(df,aes( x = code_pop, 
                        y = savings,
                        text = paste(
                          "HCPC Code:",code_shortDesc,
                          "\n",
                          "Population:",population,
                          "\n",
                          "Cost Impact",savings,
                          "\n",
                          "Potential Cost Driver:",`Cost Driver`))
    )+geom_bar(stat = "identity", aes(fill = `Alignment Impact`))+
      geom_bar(data = table_click,stat ="identity",aes( y = savings),color = "#292F33")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_fill_manual(values = c("grey","#92a8d1"))+
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme_minimal()+
      xlab("HCPC Code")+
      ylab("Potential Cost Impact")
    
    ggplotly(p,tooltip = 'text')%>%
      layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
             yaxis = list(showgrid = F),
             title = list(text = 
                            paste0('<br>',
                                   'Potential Cost Impact of Service Alignment with ',
                                   title_group,
                                   '<br>',
                                   '<sup>',
                                   'Cost per case difference X number of cases for target'
                                   
                            )))
  }else if(code_highlight() == "80/20"){
    
    p<-  ggplot(df,aes( x = code_pop, 
                        y = savings,
                        text = paste(
                          "HCPC Code:",code_shortDesc,
                          "\n",
                          "Population:",population,
                          "\n",
                          "Cost Impact",savings,
                          "\n",
                          "Potential Cost Driver:",`Cost Driver`))
    )+geom_bar(stat = "identity", aes(fill = `80/20`))+
      geom_bar(data = table_click,stat ="identity",aes( y = savings),color = "#292F33")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_fill_manual(values = c("#92a8d1","grey",'white'))+
      # paletteer_d("awtools::b_palette")
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme_minimal()+
      xlab("HCPC Code")+
      ylab("Potential Cost Impact")
    
    ggplotly(p,tooltip = 'text')%>%
      layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
             yaxis = list(showgrid = F),
             title = list(text = 
                            paste0('<br>',
                                   'Potential Cost Impact of Service Alignment with ',
                                   title_group,
                                   '<br>',
                                   '<sup>',
                                   'Cost per case difference X number of cases for target')))
    
    
    
  }else if(code_highlight() == "Drivers"){
    
    p<-  ggplot(df,aes( x = code_pop, 
                        y = savings,
                        text = paste(
                          "HCPC Code:",code_shortDesc,
                          "\n",
                          "Population:",population,
                          "\n",
                          "Cost Impact",savings,
                          "\n",
                          "Potential Cost Driver:",`Cost Driver`))
    )+geom_bar(stat = "identity", aes(fill =  `Cost Driver`))+
      geom_bar(data = table_click,stat ="identity",aes( y = savings),color = "#292F33")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_fill_manual(values = c("#50394c","#c94c4c",'#618685','grey'))+
      # paletteer_d("awtools::b_palette")
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme_minimal()+
      xlab("HCPC Code")+
      ylab("Potential Cost Impact")
    
    ggplotly(p,tooltip = 'text')%>%
      layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
             yaxis = list(showgrid = F),
             title = list(text = 
                            paste0('<br>',
                                   'Potential Cost Impact of Service Alignment with ',
                                   title_group,
                                   '<br>',
                                   '<sup>',
                                   'Cost per case difference X number of cases for target')))
    
  }else{
    
    
    
    p<-  ggplot(df%>%mutate(`Peer Svc Group` = peer_svc_type),aes( x = code_pop, 
                                                                   y = savings,
                                                                   text = paste(
                                                                     "HCPC Code:",code_shortDesc,
                                                                     "\n",
                                                                     "Population:",population,
                                                                     "\n",
                                                                     "Cost Impact",savings,
                                                                     "\n",
                                                                     "Potential Cost Driver:",`Cost Driver`))
    )+geom_bar(stat = "identity", aes(fill = `Peer Svc Group`))+
      geom_bar(data = table_click,stat ="identity",aes( y = savings),fill = "#292F33")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_fill_manual(values = c("#92a8d1","grey","salmon"))+
      #scale_fill_discrete()+
      scale_y_continuous(label = number_format(big.mark = ","))+
      theme_minimal()+
      xlab("HCPC Code/ Population")+
      ylab("Potential Cost Impact")
    
    ggplotly(p,tooltip = 'text')%>%
      layout(xaxis = list(showgrid = F, titlefont = F, showticklabels = FALSE),
             yaxis = list(showgrid = F),
             title = list(text = 
                            paste0('<br>',
                                   'Potential Cost Impact of Service Alignment with ',
                                   title_group,
                                   '<br>',
                                   '<sup>',
                                   'Cost per case difference X number of cases for target')))
    
    
  }
  
  
  p
  
  
  
  
})


}
# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")
