# ui.R #

shinyUI(
  navbarPage(
    "Explore 404",
    theme = shinythemes::shinytheme("yeti"),
    navbarMenu(
      "Motion Chart",
      tabPanel(
        "Compare Organizations",
        sidebarLayout(
          sidebarPanel(
            tags$strong("Select Metrics:"),
            br(),
            br(),
            uiOutput("x"),
            uiOutput("y"),
            uiOutput("z"),
            checkboxInput(
              inputId = "ignore_z",
              label = tags$p("Keep size of bubbles constant?"),
              value = FALSE,
              width = NULL
            ),
            tags$strong("Note: The same variable cannot be selected more than once.",
                   style = "font-size: 80%;"),
            br(),
            br(),
            tags$strong("Group by Type:"),
            br(),
            br(),
            selectInput(
              inputId = "org_type",
              label = tags$p("At which organizational level would you like to view the data:"
                             , style = "font-size: 115%;"),
              choices = c("PIHP","CMH"),
              selected = "PIHP"
            ),
            selectInput(
              inputId = "filter_pihp",
              label = tags$p("View a Specific PIHP:"
                             , style = "font-size: 115%;"),
              choices = c("All",levels(unique(data404$PIHPname))),
              selected = "All"
            ),
            checkboxInput(
              inputId = "state_avg",
              label = tags$p("Include State Average?"),
              value = FALSE,
              width = NULL
            ),
            selectInput(
              inputId = "select_ServiceType",
              label = tags$p("Specify a Service Type:", style = "font-size: 115%;"),
              choices = c("All",levels(unique(data404$ServiceType))),
              selected = "Home & Community Based Services"
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
          # suppress errors from waiting data to be built.
          tags$style(type = "text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"
          ),
          # content
          plotlyOutput("bubble1"),
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
    ),
    tabPanel(
      "Compare Services",
      sidebarLayout(
        sidebarPanel(
          tags$strong("Select Metrics:"),
          br(),
          br(),
          uiOutput("a"),
          uiOutput("b"),
          uiOutput("c"),
          tags$strong("Note: The same variable cannot be selected more than once.",
                      style = "font-size: 80%;"),
          br(),
          br(),
          tags$strong("Filter by Organization:"),
          br(),
          br(),
          selectInput(
            inputId = "org_type2",
            label = tags$p("Which organization(s) would you like to see data for? "
                           , style = "font-size: 115%;"),
            choices = c("PIHP","CMH"),
            selected = "PIHP"
          ),
          uiOutput("org_filt"),
          selectInput(
            inputId = "select_ServiceType2",
            label = tags$p("Specify a Service Type:", style = "font-size: 115%;"),
            choices = levels(unique(data404$ServiceType)),
            selected = "Home & Community Based Services"
          ),
          uiOutput("select_code2"),
          radioButtons(
            inputId = "select_Population2",
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
          # suppress errors from waiting data to be built.
          tags$style(type = "text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"
          ),
          # content
          plotlyOutput("bubble2"),
          sliderInput(
            inputId = "sliderFY1",
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
  ),
    navbarMenu(
    "About",
    tabPanel(
      "General",
        mainPanel(
          tags$strong("404 Data:", style = "font-size: 125%;"),
          p("The cost and utilization data reported by Michigan’s Community Mental Health Service Providers 
            (also known as the Section 404 report) has potential to be a beneficial tool in improving services 
            for the populations served by the CMHs. Currently, this data is officially reported to the Michigan 
            legislature and collected by the Michigan Department of Health and Human Services (MDHHS) via reporting 
            by the CMHs. Given the ongoing changes to Michigan’s public health system, there is increasing utility 
            in using data to understand current service use, cost trends and inconsistencies across the state for 
            vulnerable populations."
          ),
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
          ),
          tags$strong("Motion Chart:", style = "font-size: 125%;"),
          p(
            tags$ul(
              tags$li(strong("Chart (Aggregate): "),
                      "This chart can be used to view a single Service Type and/or HCPCS/CPT code across all PIHP's,
                      or across all CMH's within a specified PIHP.", style = "font-size: 90%;"),
              tags$li(strong("Chart (Detailed): "),
                      "This chart can be used to compare multiple HCPCS/CPT codes within a Service Type for multiple
                      PIHP's or CMH's.", style = "font-size: 90%;")
            )
          ),
          tags$strong("Chart Options:", style = "font-size: 100%;"),
          p(
            tags$ul(
              tags$li(strong("Zoom: "),
                      "Interested in a a particular cluster of bubbles? Highlight a specific area of the chart to zoom in.",
                      br(), p("Note: This functionality only works when the chart is static.", style = "font-size: 90%;"))
            )
          )
        )
      ),
    tabPanel(
      "Service Groupings",
      mainPanel(
        tags$strong("Service Groups: ", stype = "font-size: 125%;"),
        p("The table below provides a detailed hierarchy of CPT/HCPCS codes into broader service groups.",
        br(), "Use the search bar on the right to find a specific code or service."),
        dataTableOutput("svs_groups")
      )
    )
    )
  )
)

        




