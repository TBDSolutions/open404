# ui.R #

shinyUI(
  fluidPage(
    titlePanel("explore 404"),
    theme = shinythemes::shinytheme("yeti"),
    #suppress errors from waiting data to be built.
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    sidebarLayout(
      sidebarPanel(
        tags$strong("Display Options:"),
        br(),
        br(),
        uiOutput("x"),
        uiOutput("y"),
        uiOutput("z"),
        tags$strong("Note: The same variable cannot be selected more than once.",
               style = "font-size: 80%;"),
        br(),
        br(),
        tags$strong("Customize the Display:"),
        br(),
        br(),
        selectInput(
          inputId = "org_type",
          label = tags$p("At which organizational level would you like to view the data:"
                         , style = "font-size: 115%;"),
          choices = c("PIHP","CMH"),
          selected = "PIHP"
        ),
        # uiOutput(
        #   "select_org"
        #   ),
        selectInput(
          inputId = "select_ServiceType",
          label = tags$p("Specify a Service Type:", style = "font-size: 115%;"),
          choices = c("All",levels(unique(data404$ServiceType))),
          selected = "Care Coordination"
        ),
        uiOutput(
          "select_code"
        ),
        radioButtons(
          inputId = "select_Population",
          label = tags$p("Select a Population:", style = "font-size: 115%;"),
          choices = c("All", levels(unique(data404$Population))),
          selected = "All",
          inline = T
        ),
        tags$strong("Note: When Population: 'All' is selected individuals may be represented
                    more than once if they received DD and MI services within the same year.",
                    style = "font-size: 80%;")
      ),
      mainPanel(
        plotlyOutput("bubble"),
        sliderInput(
          inputId = "sliderFY",
          label = tags$p("Fiscal Year:", style = "font-size: 125%;"),
          min = 2006,
          max = 2016,
          value = 2006,
          sep = "",
          animate = animationOptions(loop = FALSE, interval = 1000)
        ),
        tags$strong("Note: Graph will not display for years during which a particular 
                    service code was not used.", style = "font-size: 80%;")
      )
    )
  )
)

