# ui.R #

dashboardPage(
  dashboardHeader(title = "explore 404"),
  dashboardSidebar(
    sidebarMenu(
        menuItem(
          icon = NULL,
          selectInput(
            "org_type",
            label = "Select Organization Level:",
            choices = c("PIHPname","CMHSP"),
            selected = "PIHPname"
          )
        ),
        menuItem(
          icon = NULL,
          selectInput(
            "select_ServiceType",
            label = "Select a Service Type:",
            choices = c(levels(unique(data404$ServiceType))),
            selected = "Care Coordination"
          )
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
              choices = c("SumCases","SumUnits","SumCost",
                          "CostPerCase","CostPerUnit","UnitPerCase",
                          "Cost1kSvd","Cost_Perc_Tot","Perc_Svd"),
              selected = "SumCases"
          )
        ),
        menuSubItem(
          icon = NULL,
          selectInput(
            "y",
            label = "Select Variable for Y axis:",
            choices = c("SumCases","SumUnits","SumCost",
                        "CostPerCase","CostPerUnit","UnitPerCase",
                        "Cost1kSvd","Cost_Perc_Tot","Perc_Svd"),
            selected = "SumUnits"
          )
        ),
        menuSubItem(
          icon = NULL,
          selectInput(
            "z",
            label = "Select Sizing Variable:",
            choices = c("SumCases","SumUnits","SumCost",
                        "CostPerCase","CostPerUnit","UnitPerCase",
                        "Cost1kSvd","Cost_Perc_Tot","Perc_Svd"),
            selected = "SumUnits"
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
