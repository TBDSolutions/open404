# ui.R #

shinyUI(
  fluidPage(
    titlePanel("explore 404"),
    theme = shinythemes::shinytheme("yeti"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "org_type",
          label = tags$p("Select Organization Level:", style = "font-size: 115%;"),
          choices = c("PIHP","CMH"),
          selected = "PIHP"
        ),
        uiOutput(
          "select_org"
          ),
        selectInput(
          inputId = "select_ServiceType",
          label = tags$p("Select a Service Type:", style = "font-size: 115%;"),
          choices = c("All",levels(unique(data404$ServiceType))),
          selected = "Care Coordination"
        ),
        uiOutput(
          "select_code"
        ),
        radioButtons(
          inputId = "select_Population",
          label = tags$p("Select Population:", style = "font-size: 115%;"),
          choices = c("All", levels(unique(data404$Population))),
          selected = "All",
          inline = T
        ),
        selectInput(
          inputId = "x",
          label = tags$p("Select Variable for X axis:", style = "font-size: 115%;"),
          choices = c("Total Cases","Total Units","Total Cost",
                      "Cost Per Case","Cost Per Unit","Unit Per Case",
                      "Cost per 1K Served","% of Total Cost","Percent Served"),
          selected = "Total Cases"
        ),
        selectInput(
          inputId = "y",
          label = tags$p("Select Variable for Y axis:", style = "font-size: 115%;"),
          choices = c("Total Cases","Total Units","Total Cost",
                      "Cost Per Case","Cost Per Unit","Unit Per Case",
                      "Cost per 1K Served","% of Total Cost","Percent Served"),
          selected = "Total Units"
        ),
        selectInput(
          inputId = "z",
          label = tags$p("Select Sizing Variable:", style = "font-size: 115%;"),
          choices = c("Total Cases","Total Units","Total Cost",
                      "Cost Per Case","Cost Per Unit","Unit Per Case",
                      "Cost per 1K Served","% of Total Cost","Percent Served"),
          selected = "Percent Served"
        ),
        tags$p("Note: The same variable cannot be selected more than once.",  style = "font-size: 80%;")
      ),
      mainPanel(
        plotlyOutput("bubble"),
        sliderInput(
          inputId = "sliderFY",
          label = tags$p("Fiscal Year:", style = "font-size: 125%;"),
          min = 2006,
          max = 2016,
          value = tags$p(2006, style = "font-size: 150%;"),
          sep = "",
          animate = animationOptions(loop = FALSE, interval = 1000)
        )
      )
    )
  )
)

