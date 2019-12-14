source('global.R')

library(shiny)
library(rlang)
library(DT)
library(ggiraph)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- function(requests){ navbarPage("Explore 404 Data",
                                     theme = shinytheme("cerulean"),
 
 # Application title
 tabPanel("Bar Charts",
          fluidRow(column(12,bookmarkButton(),
                          downloadButton("downloadData", "Download"))),
          # Sidebar with a slider input for number of bins 
          fluidRow(
            column(3,
                   uiOutput("Org"),
                   uiOutput("sidebarUI"),
                   uiOutput('code'),
                   uiOutput('metric'),
                   br(),
                   # bookmarkButton(id = "bookmark1"),
                   #  downloadButton("downloadData", "Download")
            ),
            column(9,
                   tabsetPanel(
                     tabPanel("Barchart",
                              # Show a plot of the generated distribution
                              #     textOutput("text"),
                              plotOutput("barchart"),
                     )),
                   column(8,dataTableOutput('dt')))
          )),
 tabPanel("Line Charts",
          fluidRow(
            column(3,
                   #  uiOutput("Org"),
                   #  uiOutput("sidebarUI"),
                   br()
            )
            
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
  
  
  
  # Reactive inputs 
  org_type <- reactive({input$CMHorPIHP})
  provider<-reactive({input$provider})
  fy_filter<-reactive({input$fy_filter})
  metric <-reactive({input$metric})
  serviceType<-reactive({input$serviceType})
  serviceGroup <-reactive({input$serviceGroup})
  popType<-reactive({input$popType})
  
  selectedDS<-reactive({
    df<- data404%>%
      filter((!!as.symbol(org_type())) %in% input$provider,
             fy %in% fy_filter(),
             svc_grp %in%  case_when('All' %in% serviceGroup() ~ levels(as.factor(data404$svc_grp)),
                                     TRUE ~ serviceGroup()),
             population %in%  case_when('All' %in% popType() ~ levels(as.factor(data404$population)),
                                        TRUE ~ popType()))%>%
      select(!!as.symbol(org_type()),
             cost,units,cases)%>%
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
  
  lineDS<-reactive({
    
    df<- data404%>%
      filter((!!as.symbol(org_type())) %in% input$provider)%>%
      select(!!as.symbol(org_type()),
             cost,units,cases,fy)%>%
      group_by( !!as.symbol(org_type()),fy)%>%
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
  output$Org<-renderUI({
    wellPanel(
      
      
      selectInput(
        inputId = "CMHorPIHP",
        label = "CMH or PIHP",
        selected = "CMH",
        choices = c("CMH" = 'cmhsp', "PIHP" = 'pihp_name'),
        width = width_px
      ),
      
      selectInput(
        inputId = 'serviceType',
        label = 'Service Type',
        choices = c("All",levels(as.factor(data404$svc_type))),
        selected = "All"),
      
      selectInput(
        inputId = 'popType',
        label = 'Population',
        choices = c("All",levels(as.factor(data404$population))),
        selected = "All"),
      
      selectInput(
        inputId = 'fy_filter',
        label = 'Year',
        choices = c(levels(data404$fy)),
        selected = "2018")
      
      
    ) })
  
  output$sidebarUI<-renderUI({
    
    bar_options<- if(input$CMHorPIHP == "cmhsp"){
      levels(data404$cmhsp)}else{ levels(data404$pihp_name)}
    
    svc_grp_options<-data404%>%
      filter(svc_type %in% input$serviceType)%>%
      distinct(svc_grp)%>%
      pull(svc_grp)
    
    wellPanel(
      selectizeInput(
        inputId = "provider",
        label = "provider (CMH or PIHP)",
        choices =  bar_options,
        selected = "",
        multiple = TRUE, 
        #   selectize = TRUE,
        size = 5,
        options =  list(maxItems = 12, 
                        placeholder = 'Search or Select')),
      
      selectInput(
        inputId = "serviceGroup",
        label = "Service Group",
        selected = "All",
        choices = c("All",levels(as.factor(svc_grp_options))),
        width = '550px'
        
      ))})
  
  output$code<-renderUI({
    
    svc_code_options<-data404%>%
      filter(svc_type %in% case_when('All' %in% input$serviceType ~ levels(as.factor(data404$svc_type)),
                                     TRUE ~ input$serviceType),
             
             svc_grp %in%  case_when('All' %in% input$serviceGroup ~ levels(as.factor(data404$svc_grp)),
                                     TRUE ~ input$serviceGroup))%>%
      
      mutate(code_shortDesc = as.character(code_shortDesc))%>%
      distinct(code_shortDesc)%>%
      pull(code_shortDesc)
    
    selectizeInput(
      inputId = "Codes",
      label = "Codes Desc",
      choices =  c("All",levels(as.factor(svc_code_options))),
      selected = "All",
      multiple = TRUE, 
      #   selectize = TRUE,
      size = 5,
      options =  list(maxItems = 12, 
                      placeholder = 'Search or Select'))
    
  })
  
  ### Different service groups based on types 
  
  output$metric<-renderUI({
    
    # svc_grp_options <-if(input$CMHorPIHP == "All"){
    #     levels(data404$svc_grp)
    # }else{ levels(data404$pihp_name)}
    selectInput(
      inputId = 'metric',
      label = 'Metric',
      choices = c("Cost" = "cost",'Units' = 'units',
                  'Cases' = "cases","Cost per Case" = 'cost_per_case',
                  "Cost per Unit" = 'cost_per_unit',
                  "Units per Case"),
      selected = "units")
    
    
  })
  
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
    
    data.frame(selectedDS())%>%
      ggplot(aes(x = as.factor(!!as.symbol(org_type())), y = !!as.symbol(metric()))) +
      geom_bar(stat="identity", position=position_dodge(), alpha = .6,
               color="black")+
      scale_y_continuous(label = number_format(accuracy = 1, scale = 1e-3,
                                               big.mark = ","))+
      xlab(as.symbol(org_type()))+
      ylab(str_replace_all(input$metric,pattern = "_"," "))+
      scale_fill_manual(values=c('#EA4335','#34A853'))+
      theme_minimal()+
      title()
  })
  
  ######### Line Chart 
  output$lineChart<- renderPlot({
    
    data.frame(lineDS())
    ggplot(foo,aes(x = fy, y = !!as.symbol(metric()), group = 1))+ geom_line()
    
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
  
}  

# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")
