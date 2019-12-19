source('global.R')

library(shiny)
library(rlang)
library(DT)
library(ggiraph)
library(shinythemes)
library(shiny)

# Define UI for application that draws a histogram
ui <- function(requests){ navbarPage("Explore 404 Data",
                                     
  theme = shinytheme("cerulean"),
   navbarMenu(
     "About",
   tabPanel(
     "General",
     fluidRow(
       column(
         width = 6,
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
           ),
           img(src = 'tbdSolutions-logo.png', width = "200px", align = "left"),p(tags$sub(a(href = "https://www.tbdsolutions.com/","©2019"))),
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
         width = 6,
         mainPanel(
           tags$strong("navigation", style = "font-size: 125%;"),
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
           br(),
           dataTableOutput("svs_groups")
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
                   "High level groupings of services into the following overall types:
                      'Care Coordination', 'Crisis and Respite', 'Employment Services', 'Equipment', 
                      'Home & Community Based Services', 'Hospital-based Services', 'Medication', 'Other', 
                      'Outpatient Treatment', 'Physical Health Services', 'Screening & Assessment', 'Transportation'"),
           tags$li(strong("Code: "),
                   "The CPT or HCPCS code and description for a particular service"),
           tags$li(strong("Population: "),
                   "The designation of disability type for which the documented service was provided. 
                      There are 3 disability types included in this dataset: MIA = Mentally Ill Adults, 
                      MIC = Mentally Ill Children, DD = Developmentally Disabled (Adults and Children)"),
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
                   "The annual cost of the service as a % of the total annual cost of all services"),
           tags$li(strong("Percent Served: "),
                   "Percentage of people served who received this service (per year)")
         )
       )
     )
   )),
   navbarMenu("Analysis",
 # Application title
 tabPanel("Bar Charts",
          fluidRow(column(3,bookmarkButton(),
                          downloadButton("Barchart", "Download"))
                   ),
          # Sidebar with a slider input for number of bins 
          fluidRow(
            column(3,
                   
                   wellPanel(
                   uiOutput('metric'),
                   uiOutput("org"),
                   uiOutput('prov'),
                   uiOutput("yAxisType"),
                    uiOutput("yAxisSel"),
                   uiOutput("servType"),
                   uiOutput('servGrp'),
                   uiOutput('code'),
                   uiOutput("addOptions")
                 ,style = 'background:#CCD6DD')
            ),
            column(9,fluidRow(column(3, uiOutput("shade")),column(3,uiOutput("mean"))),
                   tabsetPanel(
                     tabPanel("Barchart",
                              # Show a plot of the generated distribution
                              #     textOutput("text"),
                              plotOutput("barchart"),
                     ),
                     tabPanel("HeatMap",
                      fluidRow(column(9,
                                     plotOutput('heatmap'),
                                     DT::dataTableOutput('dt')),
                               
                               column(3,downloadButton("heatData", "Download Heat Map Table"))
                                       # wellPanel(uiOutput("yAxisType"),
                                       # uiOutput("yAxisSel")))
                      )
                    )
                ), # Tabsets for bar
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
  
####################################
# Bar Chart tabset and main choices  
####################################

  
############# REACTIVITY  
    
# Define Reactive inputs 
  org_type <- reactive({input$CMHorPIHP})
  provider<-reactive({input$provider})
  fy_filter<-reactive({input$fy_filter})
  metric <-reactive({input$metric})
  serviceType<-reactive({input$serviceType})
  serviceGroup <-reactive({input$serviceGroup})
  popType<-reactive({input$popType})
  codes<-reactive({input$codes})
  

# Define Reactive dataset for barchart 
  
  stateAvg<-reactive({
    
    group<-if('All' %in% codes()){ serviceGroup() }else{as.data.frame(list(codes()))%>%
        mutate(code = as.character(.[[1]]))%>%
        pull(code)}
    

    df<-data404%>%
      filter(
        fy %in% fy_filter(),
        svc_grp %in%  serviceGroup() )%>% # unless individuals chosen
      select(
         fy,svc_grp,
         !!as.symbol(org_type()),
         cost,units,cases
      )%>%
      group_by(
          fy,svc_grp,
         (!!as.symbol(org_type()))
      )%>%
      summarise_at(
        
        vars(cases,units,cost),
        list(~sum(., na.rm = T))
      )%>%
      mutate(
        cost_per_case = round(cost/cases,digits = 2)*100,
        cost_per_unit = round(cost/units,digits = 2)*100,
        unit_per_case = round(units/cases,digits = 1)*100
      )%>%
      group_by(fy,svc_grp)%>%
      summarise(avg = mean(!!as.symbol(metric()), na.rm= TRUE))%>%
      pull(avg)
  
    
  })

  selectedDS<-reactive({
    
    req(input$provider)
    
    # Pre-made filter for below DPLYR manipulations for formatting multiple HCPC
    # code options. Makes it easier to use when pre-defined
    code_filter<-if('All' %in% codes()){ as.character(unique(data404$code_shortDesc))}else{
      
                data404[which(data404$code_shortDesc %in% codes()),'code_shortDesc']%>%
                mutate(code_shortDesc = as.character(code_shortDesc))%>%
                distinct(code_shortDesc)%>%
                pull(code_shortDesc)
    }
    # Pre-made population filter for same reasons as code_filter. Simply usidin
    pop_filter<-if('' %in% popType()){ as.character(unique(data404$population))}else{
      
      data404[which(data404$population %in% popType()),'population']%>%
        mutate(population = as.character(population))%>%
        distinct(population)%>%
        pull(population)
    }
    
    # If the selection is by PIHP, I need to aggregate the data before joining 
    # This table will be used below to calulate cost per 1K ect. 
    stateAggData<-state_data%>%
      group_by(!!as.symbol(org_type()),fy)%>%
      summarise(TotalServed = sum(TotalServed,na.rm = TRUE))
    
    # Michigan only 
    michTtl<-state_data%>%
      mutate(state = 'MI')%>%
      group_by(state,fy)%>%
      summarise(TotalServed = sum(TotalServed,na.rm = TRUE))

df<-if(!input$CMHorPIHP == 'MI'){
 
df<- data404%>%
    filter(
              !!as.symbol(org_type()) %in% input$provider,
              fy %in% fy_filter(),
              svc_grp %in%  serviceGroup() )%>% # unless individuals chosen
    filter(
            code_shortDesc %in% code_filter, #using code filters defined above
            population %in% pop_filter
           )%>%
    select(
            !!as.symbol(org_type()),fy,svc_grp,
            cost,units,cases
           )%>%
    group_by(
              !!as.symbol(org_type()),fy,svc_grp
           )%>%
    summarise_at(
      
            vars(cases,units,cost),
            list(~sum(., na.rm = T))
          )%>%
    mutate(
      
          cost_per_case = round(cost/cases,digits = 2)*100,
          cost_per_unit = round(cost/units,digits = 2)*100,
          unit_per_case = round(units/cases,digits = 1)*100
         )%>%
    left_join(
      
          stateAggData,by = c(org_type() ,"fy")
          )%>%
    mutate(
      
        cost_per_1K_served = round(((cost/TotalServed)*1000)),
        percent_served = round(((cases/TotalServed)*100),3)
        )
}
else{

  df<- data404%>%
    filter(state %in% "MI",
           fy %in% fy_filter(),
           svc_grp %in%  serviceGroup())%>%
    select(state,svc_grp,fy,
           cost,units,cases)%>%
    group_by(state,svc_grp,fy)%>%
    summarise_at(
      vars(cases,units,cost),
      list(~sum(., na.rm = T))
    )%>%
    mutate(
      cost_per_case = round(cost/cases,digits = 2),
      cost_per_unit = round(cost/units,digits = 2),
      unit_per_case = round(units/cases,digits = 1))%>%
    left_join(michTtl,by = c("state","fy"))%>%
    mutate(cost_per_1K_served = round(((cost/TotalServed)*1000)),
           percent_served = round(((cases/TotalServed)*100),3))
  
}
    
  })
  

######### UI OUTPUTS FOR BARCHART  
  
  output$metric<-renderUI({
    selectInput(
      inputId = 'metric',
      label = 'Benchmark Measure',
      choices = c("Cost" = "cost",'Units' = 'units',
                  'Cases' = "cases","Cost Per Case" = 'cost_per_case',
                  "Cost Per Unit" = 'cost_per_unit',
                  "Units Per Case" = "unit_per_case",
                  "Cost Per 1K Served" = "cost_per_1K_served"),
      selected = "units")
  })
  
  output$org<-renderUI({
        selectInput(
          inputId = "CMHorPIHP",
          label = "I would like to compare accross..",
          choices = c("CMH" = 'cmhsp', "PIHP" = 'pihp_name',
                      "State" = "state"),
          selected = "pihp_name")
    })
  
  output$prov<-renderUI({
      # Conditinal statements to populate the list
      prov_options<- if(input$CMHorPIHP == "cmhsp"){
                         levels(data404$cmhsp)}
                      else if(input$CMHorPIHP == "pihp_name"){levels(data404$pihp_name)}
                      else{"MI"}
    
      selectizeInput(
        inputId = "provider",
        label = "Specifically, I would like to compare..",
        choices =  prov_options,
        selected = levels(data404$pihp_name),
        multiple = TRUE,
        options =  list( placeholder = 'Search or Select'))
  })
  
  output$servType<-renderUI({
    req(org_type())
    selectInput(
      inputId = 'serviceType',
      label = 'Focusing on this service type',
      choices = c("All",levels(as.factor(data404$svc_type))),
      selected = "Coordination and Planning")
  })
  
  output$servGrp<-renderUI({
    req(org_type())
      # Conditional for service group options 
      svc_grp_options<-data404%>%
      filter(svc_type %in% case_when('All' %in% input$serviceType ~ levels(as.factor(data404$svc_type)),
                                     TRUE ~ input$serviceType))%>%
      distinct(svc_grp)%>%
      pull(svc_grp)
     
      selectInput(
        inputId = "serviceGroup",
        label = "For this service group",
        selected = "",
        choices = c(levels(as.factor(svc_grp_options)))
        )

  })
  
  output$code<-renderUI({
  # More contionals for HCPC codes because it depends on both service type and group 
  # chosen
    req(org_type())
  svc_code_options<-data404%>%
    filter(svc_type %in% case_when('All' %in% input$serviceType ~ levels(as.factor(data404$svc_type)),
                                   TRUE ~ input$serviceType),
           svc_grp %in%  case_when('' %in% input$serviceGroup ~ levels(as.factor(data404$svc_grp)),
                                   TRUE ~ input$serviceGroup))%>%
    mutate(code_shortDesc = as.character(code_shortDesc))%>%
    distinct(code_shortDesc)%>%
    pull(code_shortDesc)
    
    selectizeInput(
      inputId = "codes",
      label = "Using all or a subset of the HCPC codes associated with this group",
      choices =  c("All",levels(as.factor(svc_code_options))),
      selected = "All",
      multiple = TRUE, 
      size = 5,
      options =  list(placeholder = 'Search or Select'))
    
  }) 

  output$addOptions<-renderUI({
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

      radioButtons(inputId = 'shadeByPihp',
                   label = 'Shade Barchart by PIHP',
                   choices = c("Yes",'No'),
                   selected = "No",
                   inline = TRUE)

  })  
  
  output$mean<-renderUI({
    radioButtons(inputId = 'includeMean',
                 label = "State Svc.Group Average?",
                 choices = c("Yes",'No'),
                 selected = "No",
                 inline = TRUE)
    
  })
  

######## PLOT & TABLE OUTPUTS     
  
  # Reactive for tab to include datatable 
  
  output$dt<-DT::renderDataTable({
    
       col1<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
       else{'PIHP'}
       
       col2<-as.name(if(input$groupOrHcpcs == "svc_grp"){'Service Group'}else{"HCPCS"})
       
       metric_lab = str_replace_all(input$metric,pattern = "_"," ")

       foo<-data.frame(heatmapDS())
  
       DT::datatable(foo,rownames = FALSE,class = 'cell-border stripe',
                     colnames = c(col1,col2,metric_lab,'Pctl.'))
    
  })
  

  output$barchart<-renderPlot({
       req(codes())
       req(popType())
      #Define which dataset to use based on CMH or PIHP
      #Primarily for the left join that will attache nessesary 
      # Grouping and shading columns for the graph
      df<-if(input$CMHorPIHP == 'cmhsp'){
        
          data.frame(selectedDS())%>%
          left_join(pihpCMH_LU, by = "cmhsp")
        
      }else if(input$CMHorPIHP == 'pihp_name'){ 
        
        p<-pihpCMH_LU%>%distinct(pihp,pihp_name)
        
        data.frame(selectedDS())%>%
        left_join(p, by = "pihp_name")
        
        } else {data.frame(selectedDS())}
      
      # Format X-Axis labels 
      xlabs<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
      else{'PIHP'}
    
      # Defning average line, if chosen
      average<-df%>%
      select(!!as.symbol(metric()))%>%
      summarise(mean = mean(!!as.symbol(metric()), na.rm= TRUE))%>%
      pull(mean)

      group<-if('All' %in% codes()){ serviceGroup() }else{as.data.frame(list(codes()))%>%
          mutate(code = as.character(.[[1]]))%>%
          pull(code)}
      
      populations<-as.data.frame(list(popType()))%>%
          mutate(popType = as.character(.[[1]]))%>%
          pull(popType)
      

   barplot<- if(input$shadeByPihp == 'Yes' ){
         
     df%>%
       ggplot(aes(x = fct_reorder(as.factor(!!as.symbol(org_type())),pihp),
                  y = !!as.symbol(metric()),
                  fill = pihp_name)) +
       geom_bar(stat="identity", position=position_dodge(), alpha = .6,
                color="black")+
       scale_y_continuous(label = number_format(big.mark = ","))+
       xlab(xlabs)+
       ylab(str_replace_all(input$metric,pattern = "_"," "))+
       # scale_fill_manual(values=c('#EA4335','#34A853'))+
       theme_minimal()+
       ggtitle(paste("Comparing ", str_replace_all(input$metric,pattern = "_"," ")," by ",
                     xlabs," for ",paste(group,collapse = ","),sep = ""),
               subtitle =  paste("Fiscal Year ",input$fy_filter,sep = ""))+
       labs(fill='PIHP') 
      
      
    }else{
      df%>%
        ggplot(aes(x = as.factor(!!as.symbol(org_type())), y = !!as.symbol(metric()))) +
        geom_bar(stat="identity", position=position_dodge(), alpha = .6,
                 color="black")+
        scale_y_continuous(label = number_format(big.mark = ","))+
        xlab(xlabs)+
        ylab(stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," ")))+
    #    scale_fill_manual(values=c('#EA4335','#34A853'))+
        theme_minimal()+
        ggtitle(paste("Comparing ",stri_trans_totitle(str_replace_all(input$metric,pattern = "_"," "))," by ",
                      xlabs," for ",paste(group,collapse = ","),sep = ""),
                subtitle =  paste("Fiscal Year ",input$fy_filter,sep = ""))+
        labs(caption =paste("Populations ",paste(populations,collapse = ","),sep = ""))+
        theme_ipsum(grid = 'Y',
                    plot_title_size = 15,
                    axis_text_size = 11,
                    axis_title_size = 13
                    )
    } 
   
   if(input$includeMean == 'Yes'){
     barplot +  geom_hline(yintercept = c(stateAvg()),linetype = "dashed",size = 1)
   }else{ barplot}
   
    
 
  })
  
################## 
# Heatmap tabset
################## 
  
  
################ REACTIVITY
  
 yType<-reactive({input$groupOrHcpcs})
 ySel<-reactive({input$yAxisSel})
 
 
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
            (!!as.symbol(yType())) %in% input$yAxisSel
     )%>%
     select(!!as.symbol(org_type()), # Provider column 
            (!!as.symbol(yType())),
             fy,
            cost,units,cases
     )%>%
     group_by(
       
         !!as.symbol(org_type()), # Provider column 
        (!!as.symbol(yType())),
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
        percent_served = round(((cases/TotalServed)*100),3)
    )
   
   
   
   
   # Transform into Z scores then turn Z scores into percentiles
   df<-df%>%
     select(!!as.symbol(org_type()),(!!as.symbol(yType())),!!as.symbol(metric()))%>%
     group_by((!!as.symbol(yType())))%>%
     mutate(metric = round((pnorm(scale_fun(!!as.symbol(metric())))*100),2))
   
 }) 
 
 
 
 
  
############### UI OUPUTS FOR HEATMAP TABSET
 
 
output$yAxisType <-renderUI({
  
#Actual options     

    radioButtons(
      inputId = "groupOrHcpcs",
      label = "Service Group or HCPCS",
      choices = c("Service Group" = "svc_grp", "HCPCS" = "code_shortDesc"),
      selected = c("svc_grp"),
      inline = TRUE)
    
  })


output$yAxisSel<-renderUI({
  
  type<-as.name(if(input$groupOrHcpcs == "svc_grp"){'svc_grp'}else{"code_shortDesc"})
  
  org<-if(input$CMHorPIHP == 'cmhsp'){'CMHs'}else{"PIHPs"}
  
  grps<-if(input$groupOrHcpcs == 'svc_grp'){"Service Groups"}else{"HCPC Codes"}
  
  
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


############## VISUALS AND TABLES FOR HEATMAP 


output$heatmap<-renderPlot({
  
  xlabs<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
        else{'PIHP'}
  
  type<-as.name(if(input$groupOrHcpcs == "svc_grp"){'svc grp'}else{"HCPCs"})
  
  
  df<-heatmapDS()
  
  ggplot(df,aes( y = (!!as.symbol(yType())),x = as.factor(!!as.symbol(org_type())))) + 
    geom_tile(aes(fill = metric), colour = "white") + 
    #   scale_fill_manual(values=c("#FB8604", "#DB4133", "#A3A7A8","#2B80A1"))+
    xlab(xlabs)+
    ylab(str_replace_all(input$metric,pattern = "_"," "))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(color = "black", 
                                   size = .5, linetype = "solid"))+
    labs(fill=paste(type," Pctl.",sep = "")) 
  
 
})

################## Download handlers and bookmarks 
### bookmarks 

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

heatTable<-reactive({
  
  col1<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
  else{'PIHP'}
  
  col2<-as.name(if(input$groupOrHcpcs == "svc_grp"){'Service Group'}else{"HCPCS"})
  
  metric_lab = str_replace_all(input$metric,pattern = "_"," ")
  
  
  foo<-data.frame(heatmapDS())
  
})

output$heatData <- downloadHandler(
  filename = function() {
    paste("heatMapData", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(heatTable(), file, row.names = FALSE)
  }
)


}
# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")
