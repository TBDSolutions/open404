source('global.R')


# Define UI for application that draws a histogram
ui <- function(requests){ navbarPage("Explore 404 Data",
                                     
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
           tags$img(src = 'tbd_logo.png', width = "200px", align = "left"),p(tags$sub(a(href = "https://www.tbdsolutions.com/","©2019"))),
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
           p("You can download the 404 data used in this application below:"),
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
           tags$strong("Service Groups: ", stype = "font-size: 125%;"),
           p("The table below provides a detailed hierarchy of CPT/HCPCS codes into broader service groups.",
             br(), "Use the search bar on the right to find a specific code or service."),
           br(),
           p("You can download the service groupings used in this application below:"),
           downloadButton('ServiceGroups', 'Download'),
           br(),
           p("The below table allows you to quickly search to understand service groupings"),
           DT::dataTableOutput("svs_groups")
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
                   the defined time period"),
           tags$h6("For Percent Served the numerator does change based on population selected; please note the denominator for this metric does not changed based on the population selected but does change based on the organization and service selected")
         )
       )
     )
   ),
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
   ))),
   navbarMenu("Analysis",
 # Application title
 tabPanel("Bar Chart & Heatmap",
          #Hide error messages at startup
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
          ),
          
          
          fluidRow(column(3,bookmarkButton(),
                          downloadButton("Barchart", "Data"),
                          downloadButton('plot','Barchart Image'))
                   ),
          # Sidebar with a slider input for number of bins 
          fluidRow(
            column(3,
                   
                   wellPanel(
                   uiOutput("org"),
                   uiOutput('prov'),
                   uiOutput("servType"),
                   uiOutput("groupOrHcpcsOrMod"),
                   uiOutput("compareAcross"),
                   uiOutput('metric'),
                #   uiOutput('servGrp'),
                #   uiOutput('code'),
                   uiOutput("popType")
                 ,style = 'background:#CCD6DD')
            ),
            column(9,p(tags$em("*barchart options only")),fluidRow(
                              column(2,uiOutput("mean")),
                              column(4,uiOutput("PctChange")),
                              column(3, uiOutput("shade")),
                              column(3,uiOutput('shadeOptions'))),
                   
##########################################################################                   
                   tabsetPanel(
                     tabPanel("Barchart",
                              # Show a plot of the generated distribution
                              #     textOutput("text"),
                              plotOutput("barchart"),
                              tags$b((("Barchart Data"))),
                              br(),
                              DT::dataTableOutput("barTable")
                     ),
                     
                     tabPanel("Line Chart",
                              # Show a plot of the generated distribution
                              #     textOutput("text"),
                              plotOutput("byYearPlot"),
                           #   tags$b((("Barchart Data"))),
                        #      br(),
                              DT::dataTableOutput("byYear")
                     ),
                     
                     tabPanel("Trended Heatmap ",
                              # Show a plot of the generated distribution
                              #     textOutput("text"),
                              plotOutput("byYearHeatmap"),
                              #   tags$b((("Barchart Data"))),
                              #      br(),
                              DT::dataTableOutput("trendedHeatmapTable")
                     ),
 
                     tabPanel("Distribution Heatmap",
                      fluidRow(column(9,
                                     plotOutput('heatmap'),
                                     tags$b((("Heatmap Data"))),
                                     br(),
                                     DT::dataTableOutput('dt')),
                               
                               column(3,downloadButton("heatData", "Download Heat Map Table"),
                                        wellPanel(
                                       #   uiOutput("yAxisType"),
                                        uiOutput("yAxisSel")))))

                     
                     
                ), # closure for tabsetpannl
#################################################################################
            ),
        )
    ) # Tabpannel for barchart 
 
 
) # analysis navbar menu
) # final closure 
}

#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# Define server logic required to draw a histogram
server <- function(input, output) {
  
source('global.R')
  
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
    
    org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
    
    
    # Conditinal statements to populate the list
    prov_options<- if(input$CMHorPIHP == "cmhsp"){
      levels(data404$cmhsp)}
    else if(input$CMHorPIHP == "pihp_name"){levels(data404$pihp_name)}
    else{"MI"}
    
    
    
    selectizeInput(
      inputId = "provider",
      label =   paste("Which ",org,"are you interested in viewing?"),
      choices =  prov_options,
      selected = prov_options,
      multiple = TRUE,
      options =  list( placeholder = 'Search or Select'))
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
      label = "Service Group, HCPCS Code or Modifier",
      choices = c("Service Group" = "svc_grp", "HCPCS" = "code_shortDesc","Code Mod" = 'codeM_shortDesc' ),
      selected = c("code_shortDesc"),
      inline = TRUE)
    
  })
  
  output$compareAcross<-renderUI({
    
    type<-as.name(if(input$groupOrHcpcsOrMod_ == "svc_grp"){'svc_grp'}
                  else if(input$groupOrHcpcsOrMod_ == "codeM_shortDesc"){'codeM_shortDesc'}
                  else{"code_shortDesc"})
    
    org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
    
    grps<-if(input$groupOrHcpcsOrMod_ == 'svc_grp'){"Service Group"}
             else if(input$groupOrHcpcsOrMod_ == "codeM_shortDesc"){'Code Modifier'}
             else{"HCPC Code"}
    
    
    options<-data404%>%
      filter(svc_type %in% case_when('All' %in% input$serviceType ~ levels(as.factor(data404$svc_type)),
                                     TRUE ~ input$serviceType))%>%
      distinct(!!type)%>%
      pull(!!type)
    
    
    if(input$groupOrHcpcsOrMod_ == "svc_grp"){
      tags$h6("will")
      selectizeInput(
        inputId = 'compareAcross',
        label = paste('Compare ',org," across this ",grps,sep = ""),
        choices = options,
        multiple = FALSE,
        selected = "Case Management")
      
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
    
    
  })
  
  output$metric<-renderUI({
    
    org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
    
    choices<-if(input$groupOrHcpcsOrMod_ == 'svc_grp'){
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
    tagList(
      selectInput(
        inputId = 'popType',
        label = 'Population Grouping',
        multiple = T,
        choices = c("",levels(as.factor(data404$population))),
        selected = levels(as.factor(data404$population))),
      
      selectInput(
        inputId = 'fy_filter',
        label = 'Fiscal Year',
        choices = c(levels(data404$fy)),
        selected = "2018")
    )
  })
  
  output$shade<-renderUI({
    
    req(input$CMHorPIHP)
    
    if(input$CMHorPIHP == 'cmhsp'){
      
      radioButtons(inputId = 'shadeByPihp',
                   label = "Highlight CMH's of a PIHP?",
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
   #   fy %in% c('2016','2018'),
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
  group<-if(input$groupOrHcpcsOrMod_ == "svc_grp"){ paste(compareAcross()," Service Group")}
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
      geom_bar(stat="identity", position=position_dodge(), alpha = .5,
               color="black")+
      scale_y_continuous(label = number_format(accuracy = 0.1,big.mark = ","),
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
                  #    base_family = "IBMPlexSans"
      )+
      theme(axis.text.x=element_text(angle=45, hjust=1)
            ,plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
            
            )

    
    
 
    
  }else{
    df%>%
      ggplot(aes(x = fct_reorder(as.factor(!!as.symbol(org_type())),!!as.symbol(metric()),
                                 .desc = TRUE),
                 y = !!as.symbol(metric()))) +
      geom_bar(stat="identity", position=position_dodge(), alpha = .5,
               color="black")+
      scale_y_continuous(label = number_format(accuracy = 0.1,big.mark = ","),
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
             plot.subtitle = element_text(hjust = 0.5)
            
      )
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
  
output$dt<-DT::renderDataTable({
    
       col1<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
       else{'PIHP'}
       
       col2<-as.name(if(input$groupOrHcpcsOrMod_ == "svc_grp"){'Service Group'}else{"HCPCS"})
       
       metric_lab = str_replace_all(input$metric,pattern = "_"," ")

       foo<-data.frame(heatmapDS())
  
       DT::datatable(foo,rownames = FALSE,class = 'cell-border stripe',
                     colnames = c(col1,col2,metric_lab,'Pctl.'))
    
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

    DT::datatable(foo,rownames = FALSE,class = 'cell-border stripe'
                  ,colnames = c(col1,metric_lab,change,col2))
                  #colnames = c(col1,col2,metric_lab))

  })
  
output$barchart<-renderPlot({
   plotInput()
   
 #  ggsave("plot.pdf", plotInput())
   
   
})
 
 
#plotInput<-function(){output$barchart}
  
#ggsave("barchart.pdf", output$barchart())


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Line chart tab %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Reatctive datasets 

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
      !!as.symbol(org_type()) %in% input$provider,
         fy %in% c("2014","2015",'2016',"2017",'2018'),
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

### Graphs and Tables  

output$byYearPlot<-renderPlot({
  
  
  time_df<-data.frame(selectedDS_byYear())
  
  
  # Format X-Axis labels 
 org_lab<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
  else{'PIHP'}
  ############################################
  
  # Set the axis title and ensure all selections of HCPCS codes are included
  group<-if(input$groupOrHcpcsOrMod_ == "svc_grp"){ paste(compareAcross()," Service Group")}
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
    scale_color_hue(l=50, c=100 ,h=c(0, 360), na.value = "black")+
#  scale_color_brewer(palette="Dark2")+
    xlab("Fiscal Year")+
  ylab(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")))+
  ggtitle(paste("Comparing ", str_replace_all(input$metric,pattern = "_"," ")," by ",
                org_lab," for ",paste(group,collapse = ","),sep = ""),
          subtitle = "Fiscal Years 2012-2018")+
  labs(fill='CMH')+
  labs(caption =paste("Populations ",paste(populations,collapse = ","),sep = ""))+
  theme_ipsum(grid = 'FALSE',
              plot_title_size = 15,
              axis_text_size = 11,
              axis_title_size = 13,
              ticks = TRUE
              #   base_family = "IBMPlexSans"
  )+
  scale_y_continuous(label = number_format(accuracy = 0.1,big.mark = ","))+
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
      theme_minimal()+
      scale_color_hue(l=50, c=100 ,h=c(0, 360), na.value = "black")+
      xlab("Fiscal Year")+
     # ylab(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")))+
      ggtitle(paste("Comparing ", str_replace_all(input$metric,pattern = "_"," ")," by ",
                    org_lab," for ",paste(group,collapse = ","),sep = ""),
              subtitle = "Fiscal Years 2012-2018")+
      labs(caption =paste("Populations ",paste(populations,collapse = ","),sep = ""))+
      theme_ipsum(grid = 'FALSE',
                  plot_title_size = 15,
                  axis_text_size = 11,
                  axis_title_size = 13,
                  ticks = TRUE
                  #   base_family = "IBMPlexSans"
      )+
      scale_y_continuous(label = number_format(accuracy = 0.1,big.mark = ","))+
      theme(axis.text.x=element_text(angle=45, hjust=1)
            ,plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
            
      )
     p
    
    }
  

 print(df) 
  
  
})

output$byYear <- renderDataTable({
  
  df<-data.frame(selectedDS_byYear())
  
  col1<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
  else{'PIHP'}
  
  col2<-"Fiscal Year"
  
  metric_lab = str_replace_all(input$metric,pattern = "_"," ")

  
  DT::datatable(df,rownames = FALSE,class = 'cell-border stripe'
                ,colnames = c(col1,col2,metric_lab))
  #colnames = c(col1,col2,metric_lab))
  
  
  
})


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% By year heatmap tab %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output$byYearHeatmap <- renderPlot({
  
  # function for normalizing data 
  norm<- function(x){(x - min(x))/(max(x) - min(x))} 
  
  df<-data.frame(selectedDS_byYear())%>%
                  filter(!fy %in% c("2006","2007","2008","2009","2010"))%>%
                  group_by(!!as.symbol(org_type()))%>%
                  mutate(norm = norm( !!as.symbol(metric()) ))%>%
                  ungroup()
                

  ggplot(df,aes( y = !!as.symbol(org_type()) ,x = fy)) + 
    geom_tile(aes(fill =norm), colour = "white")+
    #scale_fill_viridis_c(direction = -1)
    #  scale_fill_gradientn(colours = terrain.colors(10))
    scale_fill_gradientn(colours = c("#98C4F6","#236AB9","#FE2712"),na.value = "white")+
    theme_bw() +
    xlab("Fiscal Year")+
    ylab(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")))+
    theme(panel.grid=element_blank()) +
    coord_cartesian(expand=FALSE)
 
  
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
                ,colnames = c(col1,col2,metric_lab,col4))
  
  

  
})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Heatmap tab %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### UI Components
output$yAxisType <-renderUI({
  
  #Actual options     
  
  radioButtons(
    inputId = "groupOrHcpcsOrMod",
    label = "Service Group or HCPCS",
    choices = c("Service Group" = "svc_grp", "HCPCS Code" = "code_shortDesc", "Code Modifier" = 'codeM_shortDesc'),
    selected = c("svc_grp"),
    inline = TRUE)
  
})
output$yAxisSel<-renderUI({
  
  type<-as.name(if(input$groupOrHcpcsOrMod_ == "svc_grp"){'svc_grp'}
                else if(input$groupOrHcpcsOrMod_ == "codeM_shortDesc"){'codeM_shortDesc'}
                else{"code_shortDesc"})
  
  org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
  
  grps<-if(input$groupOrHcpcsOrMod_ == 'svc_grp'){"Service Groups"}
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
    selected = "")
  
  
  
  
})

### Reactive imputs
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
   
  
   df<-data404%>%
     filter((!!as.symbol(org_type())) %in% input$provider,
            fy %in% fy_filter(),
            population %in% pop_filter,
            (!!as.symbol(groupOrHcpcsOrMod_())) %in% input$yAxisSel
     )%>%
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
  
  type<-as.name(if(input$groupOrHcpcsOrMod_ == "svc_grp"){'svc grp'}else{"HCPCs"})
  
  populations<-as.data.frame(list(popType()))%>%
    mutate(popType = as.character(.[[1]]))%>%
    pull(popType)
  
  
  df<-heatmapDS()
  
  ggplot(df,aes( y = (!!as.symbol(groupOrHcpcsOrMod_())),x = as.factor(!!as.symbol(org_type())))) + 
    geom_tile(aes(fill = metric), colour = "white") + 
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

# Barchart DS

output$Barchart <- downloadHandler(
  filename = function() {
    paste("Barchart", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(selectedDS(), file, row.names = FALSE)
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



}
# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")
