# ui.R #

dashboardPage(skin = "blue",
  dashboardHeader(title = "explore 404"),
  dashboardSidebar(
    sidebarMenu(
      selectInput(
        "org_type",
          label = "Select Organization Level:",
          choices = c("PIHP","CMH"),
          selected = "PIHP"
      ),
      uiOutput(
        "select_org"
      ),
      selectInput(
        "select_ServiceType",
        label = "Select a Service Type:",
        choices = c("All",levels(unique(data404$ServiceType))),
        selected = "Care Coordination"
      ),
      uiOutput(
        "select_code"
      ),
      radioButtons(
        "select_Population",
        label = "Select Population:",
        choices = c("All", levels(unique(data404$Population))),
        selected = "All",
        inline = T
      ),
      menuItem(
        "Display Options",
        icon = icon("plus-square-o"),
        menuSubItem(
          icon = NULL,
          selectInput(
            "x",
            label = "Select Variable for X axis:",
            choices = c("Total Cases","Total Units","Total Cost",
                        "Cost Per Case","Cost Per Unit","Unit Per Case",
                        "Cost per 1K Served","% of Total Cost","Percent Served"),
              selected = "Total Cases"
          )
        ),
        menuSubItem(
          icon = NULL,
          selectInput(
            "y",
            label = "Select Variable for Y axis:",
            choices = c("Total Cases","Total Units","Total Cost",
                        "Cost Per Case","Cost Per Unit","Unit Per Case",
                        "Cost per 1K Served","% of Total Cost","Percent Served"),
            selected = "Total Units"
          )
        ),
        menuSubItem(
          icon = NULL,
          selectInput(
            "z",
            label = "Select Sizing Variable:",
            choices = c("Total Cases","Total Units","Total Cost",
                        "Cost Per Case","Cost Per Unit","Unit Per Case",
                        "Cost per 1K Served","% of Total Cost","Percent Served"),
            selected = "Percent Served"
          )
        )
      )
    )
  ),
  dashboardBody(
    plotlyOutput("bubble"),
    sliderInput(
      "sliderFY",
      "Fiscal Year:",
      min = 2006,
      max = 2016,
      value = 2006,
      sep = "",
      animate = animationOptions(loop = FALSE, interval = 1000)
    )
  )
)
