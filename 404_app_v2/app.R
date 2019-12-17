source('global.R')

library(shiny)
library(rlang)
library(DT)
library(ggiraph)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- function(requests){ navbarPage("Explore 404 Data",
                                     
                                     
                                     
                                     theme = shinytheme("cerulean"),
tabPanel("About the App",
         fluidRow(column(5,offset = 5,tags$strong("open404", style = "font-size: 125%;"))),
         br(),
                  column(4,offset = 4,
                         "The cost and utilization data is collected by the Michigan 
                Department of Health and Human Services' (MDHHS) Behavioral 
                Health and Developmental Disabilities Administration (BHDDA) 
                and reported annually to the Michigan legislature. Given the 
                ongoing changes to Michigan’s public health system, this data 
                can be used to understand service use, cost trends and 
                variation across the state for vulnerable populations.  This 
                application has been developed by"),
         br(),
         br(),
               column(10,offset = 4, "TBD Solutions")),
 
 # Application title
 tabPanel("Bar Charts",
          fluidRow(column(3,bookmarkButton(),
                          downloadButton("downloadData", "Download"))),
          # Sidebar with a slider input for number of bins 
          fluidRow(
            column(3,
                   wellPanel(
                   uiOutput('metric')
                   ,style = 'background:#E1E8ED'),
                   wellPanel(
                   uiOutput("org"),
                   uiOutput('prov')
                    ,style = 'background:#CCD6DD'),
                   wellPanel(
                   uiOutput("servType"),
                   uiOutput('servGrp'),
                   uiOutput('code'),
                    style = 'background:#CCD6DD')
                 #  uiOutput('code'),
                #   uiOutput('metric')),
                #   br(),
                   # bookmarkButton(id = "bookmark1"),
                   #  downloadButton("downloadData", "Download")
            ),
            column(9,
                   tabsetPanel(
                     tabPanel("Barchart",
                              # Show a plot of the generated distribution
                              #     textOutput("text"),
                              plotOutput("barchart"),
                     ),
                     tabPanel("Adv. Barchart",
                              fluidRow(column(9,
                                             dataTableOutput('dt'),
                                              plotOutput('heatmap')),
                                       column(3,uiOutput("tab2varOptions"))
                              )
                                
                                
                              )),
                     
                     ),
                column(3,offset = 6,
                         wellPanel(
                         uiOutput("addOptions")
                        # style = 'background:#CCD6DD')
                       )),
                column(4,offset = 1, 
                       wellPanel(
                         uiOutput("addOptions2")
                       ))
              
               #    column(8,dataTableOutput('dt'))
            
            )
          ),
 tabPanel("Heat Map",
          fluidRow(
            column(9,
                     uiOutput("tab2axisOptions"),
                   #  uiOutput("tab2varOptions"),
                     uiOutput("tab2Metric")),
            column(9, 
                 #  plotOutput('heatmap'),
                     dataTableOutput("heatDT"))
            
            
          ))
)
}

#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



# Define server logic required to draw a histogram
server <- function(input, output) {
  
#############################################################################
  
######################### TAB 1 SERVER #######################################
  
##############################################################################
  
    
  
  # Reactive inputs 
  org_type <- reactive({input$CMHorPIHP})
  provider<-reactive({input$provider})
  fy_filter<-reactive({input$fy_filter})
  metric <-reactive({input$metric})
  serviceType<-reactive({input$serviceType})
  serviceGroup <-reactive({input$serviceGroup})
  popType<-reactive({input$popType})
  codes<-reactive({input$codes})
  
  selectedDS<-reactive({
    
    req(input$provider)
    
    # Pre-made code filter to be DPLYR complient 
    code_filter<-if('All' %in% codes()){ as.character(unique(data404$code_shortDesc))}else{
      
      data404[which(data404$code_shortDesc %in% codes()),'code_shortDesc']%>%
        mutate(code_shortDesc = as.character(code_shortDesc))%>%
        distinct(code_shortDesc)%>%
        pull(code_shortDesc)
    }
    # Pre-made population filter to be DPLYR complient 
    pop_filter<-if('' %in% popType()){ as.character(unique(data404$population))}else{
      
      data404[which(data404$population %in% popType()),'population']%>%
        mutate(population = as.character(population))%>%
        distinct(population)%>%
        pull(population)
    }
    
    
    
    
    
    
    df<- data404%>%
      filter((!!as.symbol(org_type())) %in% input$provider,
             fy %in% fy_filter(),
             svc_grp %in%  case_when('All' %in% serviceGroup() ~ levels(as.factor(data404$svc_grp)),
                                     TRUE ~ serviceGroup())
           #  population %in%  case_when('All' %in% popType() ~ levels(as.factor(data404$population)),
            #                            TRUE ~ popType())
             
             )%>%
      filter(
            code_shortDesc %in% code_filter,
            population %in% pop_filter
             
             )%>%
      select(
            !!as.symbol(org_type()),
             cost,units,cases
            )%>%
      group_by( !!as.symbol(org_type()))%>%
      summarise_at(
        vars(cases,units,cost),
        list(~sum(., na.rm = T))
      )%>%
      mutate(
        cost_per_case = round(cost/cases,digits = 2),
        cost_per_unit = round(cost/units,digits = 2),
        unit_per_case = round(units/cases,digits = 1))
    
  })
  

  
  
  ######### UI OUTPUTS 
  
  
  output$metric<-renderUI({
   
    selectInput(
      inputId = 'metric',
      label = 'Benchmark Measure',
      choices = c("Cost" = "cost",'Units' = 'units',
                  'Cases' = "cases","Cost per Case" = 'cost_per_case',
                  "Cost per Unit" = 'cost_per_unit',
                  "Units per Case"),
      selected = "units")
    
    
  })
  

  
  output$org<-renderUI({
      
      selectInput(
        inputId = "CMHorPIHP",
        label = "I would like to compare accross..",
        choices = c("CMH" = 'cmhsp', "PIHP" = 'pihp_name'),
        selected = "pihp_name")})
  
  output$prov<-renderUI({
    
      prov_options<- if(input$CMHorPIHP == "cmhsp"){
      levels(data404$cmhsp)}else{ levels(data404$pihp_name)}
    
      selectizeInput(
        inputId = "provider",
        label = "Specifically, I would like to compare..",
        choices =  prov_options,
        selected = levels(data404$pihp_name),
        multiple = TRUE,
        options =  list(maxItems = 12, 
                        placeholder = 'Search or Select'))
    
  })
  
  output$servType<-renderUI({
    
    selectInput(
      inputId = 'serviceType',
      label = 'Focusing on this service type',
      choices = c("All",levels(as.factor(data404$svc_type))),
      selected = "Coordination and Planning")

  })
  
  output$servGrp<-renderUI({
    
    
    svc_grp_options<-data404%>%
      filter(svc_type %in% case_when('All' %in% input$serviceType ~ levels(as.factor(data404$svc_type)),
                                     TRUE ~ input$serviceType))%>%
      #  filter(svc_type %in% input$serviceType)%>%
      distinct(svc_grp)%>%
      pull(svc_grp)
    
      
      selectInput(
        inputId = "serviceGroup",
        label = "For this service group",
        selected = "",
        choices = c(levels(as.factor(svc_grp_options))),
        width = '550px'
        
      )

  })
  
  output$code<-renderUI({
    
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
      #   selectize = TRUE,
      size = 5,
      options =  list(maxItems = 12, 
                      placeholder = 'Search or Select'))
    
  }) 
  
  

  
  
  output$addOptions<-renderUI({
    tagList(
      selectInput(
        inputId = 'popType',
        label = 'Population',
        multiple = T,
        choices = c("",levels(as.factor(data404$population))),
        selected = levels(as.factor(data404$population))),
      
      selectInput(
        inputId = 'fy_filter',
        label = 'Year',
        choices = c(levels(data404$fy)),
        selected = "2018")
      
                   
)
      })
  
  output$addOptions2<-renderUI({
    
    tagList(
      
      radioButtons(inputId = 'shadeByPihp',
                   label = 'Shade by PIHP',
                   choices = c("Yes",'No'),
                   selected = "No"),
      radioButtons(inputId = 'includeMean',
                   label = "Include Mean Line",
                   choices = c("Yes",'No'),
                   selected = "No")
      
    )
  })   
    
  

  
  ### Different service groups based on types 
  
  
  ###### PLOT OUTPUTS     
  
  # Reactive for tab to include datatable 
  
  output$text<-renderText({ A<- org_type()
  #B<- provider() 
  })
  
  output$dt<-DT::renderDataTable({
    
    foo<-data.frame(selectedDS())%>%
      select(!!as.symbol(org_type()),
             !!as.symbol(metric()))
       DT::datatable(foo)
    
  })
  
  
  output$barchart<-renderPlot({
    
    xlabs<-if(input$CMHorPIHP == 'cmhsp'){'CMH'}
    else{'PIHP'}
    
    
    # Do I need to attach PIHP names to the mapping dataset
    df<-if(input$CMHorPIHP == 'cmhsp'){
           data.frame(selectedDS())%>%
           left_join(pihpCMH_LU, by = "cmhsp")}
        else{ selectedDS() }
    
    average<-df%>%
             select(!!as.symbol(metric()))%>%
             summarise(mean = mean(!!as.symbol(metric()), na.rm= TRUE))%>%
             pull(mean)
    
   # anythingSelected<-input$

    
   barplot<- if(input$shadeByPihp == 'Yes'){
         
        
      df%>%
      ggplot(aes(x = as.factor(!!as.symbol(org_type())), y = !!as.symbol(metric()),
             fill = pihp_name)) +
      geom_bar(stat="identity", position=position_dodge(), alpha = .6,
               color="black")+
      scale_y_continuous(label = number_format(accuracy = 1, scale = 1e-3,
                                               big.mark = ","))+
      xlab(xlabs)+
      ylab(str_replace_all(input$metric,pattern = "_"," "))+
     # scale_fill_manual(values=c('#EA4335','#34A853'))+
      theme_minimal()+
      title()
      
      
    }else{
      df%>%
        ggplot(aes(x = as.factor(!!as.symbol(org_type())), y = !!as.symbol(metric()))) +
        geom_bar(stat="identity", position=position_dodge(), alpha = .6,
                 color="black")+
        scale_y_continuous(label = number_format(accuracy = 1, scale = 1e-3,
                                                 big.mark = ","))+
        xlab(xlabs)+
        ylab(str_replace_all(input$metric,pattern = "_"," "))+
    #    scale_fill_manual(values=c('#EA4335','#34A853'))+
        theme_minimal()+
        title()
    } 
   
   if(input$includeMean == 'Yes'){
     barplot +  geom_hline(yintercept = average,linetype = "dashed")
      
   }else{ barplot}
   
    
    
      
      
      
  })
  

  
  ### bookmarks 
  
  
  #Download handler 
  output$table <- renderTable({
    
    datasetInput()
    
  })
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("404CustomDataset", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selectedDS(), file, row.names = FALSE)
    }
  )
  
  #############################################################################
  
  ######################### TAB 2 SERVER #######################################
  
  ##############################################################################
  
  # Reactive inputs 
 tab2org_type <- reactive({input$tab2CMHorPIHP})
 tab2provider<-reactive({input$tab2provider})
 tab2Service<-reactive({input$tab2Service})
  
  tab2fy_filter<-reactive({input$tab2fy_filter})
  tab2metric <-reactive({input$tab2metric})
  #serviceType<-reactive({input$serviceType})
 tab2group <-reactive({input$tab2group})
  #popType<-reactive({input$popType})
  
  
  
  
  
  
  
  output$tab2axisOptions<-renderUI({
    wellPanel(

      selectInput(
        inputId = "tab2CMHorPIHP",
        label = "Y-Axis:CMH or PIHP",
        selected = "CMH",
        choices = c("CMH" = 'cmhsp', "PIHP" = 'pihp_name'),
        width = width_px
      ),
      
      selectInput(
        inputId = 'tab2Service',
        label = 'X-Axis:Service',
        choices = c("Service Group" = "svc_grp", "HCPCS" = "code_shortDesc"),
        selected = c("Service Group")
      )
      

      
      
    ) })
  
  
  
output$tab2varOptions <-renderUI({
  
#Actual options     
    wellPanel(
    
    radioButtons(
      inputId = "groupOrHcpcs",
      label = "Service Group or HCPCS",
      choices = c("Service Group" = "svc_grp", "HCPCS" = "code_shortDesc"),
      selected = c("Service Group")))
    
  })


# START HERE TO POPULATE WITH HCPCS or SERVICE GROUP OPTIONS for HEATMAP




 
  
heatmapDS<-reactive({
  
  
  df<- data404%>%
    filter((!!as.symbol(tab2org_type())) %in% input$tab2provider,
           (!!as.symbol(tab2Service())) %in% input$tab2group,
            fy %in% tab2fy_filter()
          #   svc_type %in% 'Behavioral Treatment'

  )%>%
    select(!!as.symbol(tab2org_type()), # Provider column 
       #    !!as.symbol(tab2Service()),
           !!as.symbol(tab2Service()), # Group column (code_shortDesc or svc_grp)
            #  svc_grp,
           cost,units,cases)%>%
    group_by( !!as.symbol(tab2org_type()),
              !!as.symbol(tab2Service())
              #!!as.symbol(tab2Service())
       #       svc_grp
              )%>%
    summarise_at(
      vars(cases,units,cost),
      list(~sum(., na.rm = T))
    )%>%
    mutate(
      cost_per_case = round(cost/cases,digits = 2),
      cost_per_unit = round(cost/units,digits = 2),
      unit_per_case = round(units/cases,digits = 1))
  
  

  
}) 
############## Render some visuals 

output$heatmap<-renderPlot({
  
  xlabs<-if(input$tab2CMHorPIHP == 'cmhsp'){'CMH'}
        else{'PIHP'}
  
  df<-heatmapDS()
  
  ggplot(df,aes( y = !!as.symbol(tab2Service()),x = as.factor(!!as.symbol(tab2org_type())))) + 
    geom_tile(aes(fill = !!as.symbol(tab2metric())), colour = "white") + 
    #   scale_fill_manual(values=c("#FB8604", "#DB4133", "#A3A7A8","#2B80A1"))+
    xlab(xlabs)+
    ylab(str_replace_all(input$tab2metric,pattern = "_"," "))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(color = "black", 
                                   size = .5, linetype = "solid"))
  
  
  
  
})
  
  output$heatDT <-  DT::renderDataTable({
    
    foo<-data.frame(heatmapDS())
    
    DT::datatable(foo)
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")
