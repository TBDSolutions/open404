# ui.R #

dashboardPage(
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
      menuItem(
        "Filters",
        icon = icon("filter"),
        menuSubItem(
          icon = NULL,
          selectInput(
            "select_PIHP",
            label = "Select a PIHP:",
            choices = c("All", levels(unique(data404$PIHPname))), 
            selected = "All"
            )
          ),
        menuSubItem(
          icon = NULL,
          selectInput(
            "select_CMHSP",
            label = "Select a CMH:",
            choices = c("All", levels(unique(data404$CMHSP))),
            selected = "All"
          )
        ),
        menuSubItem(
          icon = NULL,
          selectInput(
            "select_Population",
            label = "Select Population:",
            choices = c("All", levels(unique(data404$Population))),
            selected = "All"
          )
        )
      ),
      menuItem(
        "Display Options",
        icon = icon("plus-square-o"),
        menuSubItem(
          icon = NULL,
          selectInput(
            "x",
            label = "Select Variable for X axis:",
              choices = c("SumOfCases","SumOfUnits","SumOfCost",
                          "CostPerCase","CostPerUnit","UnitPerCase",
                          "Cost1kSvd","Cost_Perc_Tot","Perc_Svd"),
              selected = "SumOfCases"
          )
        ),
        menuSubItem(
          icon = NULL,
          selectInput(
            "y",
            label = "Select Variable for Y axis:",
            choices = c("SumOfCases","SumOfUnits","SumOfCost",
                        "CostPerCase","CostPerUnit","UnitPerCase",
                        "Cost1kSvd","Cost_Perc_Tot","Perc_Svd"),
            selected = "SumOfUnits"
          )
        ),
        menuSubItem(
          icon = NULL,
          selectInput(
            "z",
            label = "Select Sizing Variable:",
            choices = c("SumOfCases","SumOfUnits","SumOfCost",
                        "CostPerCase","CostPerUnit","UnitPerCase",
                        "Cost1kSvd","Cost_Perc_Tot","Perc_Svd"),
            selected = "Perc_Svd"
          )
        )
        # menuSubItem(
        #   icon = NULL,
        #   selectInput(
        #     "w",
        #     label = "Select Grouping Variable:",
        #     choices = c("PIHPname","CMHSP","Population","ServiceType","Service"),
        #     selected = "PIHPname"
        #   )
        # )
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
