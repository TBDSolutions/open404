# ui.R #

shinyUI(
  fluidPage(
    titlePanel("explore 404"),
    theme = shinythemes::shinytheme("yeti"),
    sidebarLayout(
      sidebarPanel(
        tags$strong("Display Options:"),
        br(),
        br(),
        selectInput(
          inputId = "x",
          label = tags$p("Select a variable for the x-axis (horizontal):", style = "font-size: 115%;"),
          choices = c("Total Cases","Total Units","Total Cost",
                      "Cost Per Case","Cost Per Unit","Total Unit Per Case",
                      "Cost per 1K Served","Percent of Total $","Percent Served"),
          selected = "Total Cases"
        ),
        selectInput(
          inputId = "y",
          label = tags$p("Select a variable for the y-axis (vertical):", style = "font-size: 115%;"),
          choices = c("Total Cases","Total Units","Total Cost",
                      "Cost Per Case","Cost Per Unit","Total Unit Per Case",
                      "Cost per 1K Served","Percent of Total $","Percent Served"),
          selected = "Total Units"
        ),
        selectInput(
          inputId = "z",
          label = tags$p("Select a variable to scale the size of each bubble:"
                         , style = "font-size: 115%;"),
          choices = c("Total Cases","Total Units","Total Cost",
                      "Cost Per Case","Cost Per Unit","Total Unit Per Case",
                      "Cost per 1K Served","Percent of Total $","Percent Served"),
          selected = "Percent Served"
        ),
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
        uiOutput(
          "select_org"
          ),
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
        )
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
        )
      )
    )
  )
)

