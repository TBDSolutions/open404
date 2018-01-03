# ui.R #

shinyUI(
  navbarPage(
    "Explore 404",
    theme = shinythemes::shinytheme("yeti"),
    navbarMenu(
      "About",
      tabPanel(
        "General",
        mainPanel(
          tags$strong("open404", style = "font-size: 125%;"),
          br(),
          p("The cost and utilization data reported by Michigan’s Community Mental Health Service Providers 
            (also known as the Section 404 report) has potential to be a beneficial tool in improving services 
            for the populations served by the CMHs. Currently, this data is officially reported to the Michigan 
            legislature and collected by the Michigan Department of Health and Human Services (MDHHS) via reporting 
            by the CMHs. Given the ongoing changes to Michigan’s public health system, there is increasing utility 
            in using data to understand current service use, cost trends and inconsistencies across the state for 
            vulnerable populations. The data is drawn from source tables collected by MDHHS, provided by the 
            Behavioral Health and Developmental Disabilities Administration (BHDDA)."
          ),
          tags$strong("navigation", style = "font-size: 125%;"),
          br(),
          p("The two drop down menus above can be used to navigate the application. The 'About' menu includes a general
            description of the application, including definitions of each available data element and an overview of the
            service groupings used to aggregate similar services. The 'Motion Chart' menu includes two separate data
            visualizations that can be used to expore the 404 data in varying levels of detail (see additional descriptions below)."
          ),
          br(),
          tags$strong("motion chart", style = "font-size: 125%;"),
          p(
            tags$ul(
              tags$li(strong("Compare Organizations: "),
                      "This chart can be used to compare a single Service Type and/or HCPCS/CPT code across all PIHP's,
                      or across all CMH's within a specified PIHP. This chart is helpful in answering the question 'How does total cost
                      compare to total units per case for all CMH's within Mid-State Health Network?'", style = "font-size: 100%;"),
              tags$li(strong("Compare Services: "),
                      "This chart can be used to compare multiple HCPCS/CPT codes within a Service Type for a single
                      PIHP or CMH. This chart is helpful in answering the question 'How does total cost compare total units per
                      case for each CLS code (H0043, H2015, H2016) at Mid-State Health Network?'", style = "font-size: 100%;")
            )
          ),
          br(),
          uiOutput("download")
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
    ),
    navbarMenu(
      "Motion Chart",
      tabPanel(
        "Compare Organizations",
        sidebarLayout(
          sidebarPanel(
            tags$strong("Make selections below to start comparing organizations"
                        , style = "font-size: 120%;"),
            br(),
            br(),
            selectInput(
              inputId = "org_type",
              label = tags$strong("Which organization(s) would you like to compare?"
                             , style = "font-size: 115%;"),
              choices = c("","PIHP","CMH"),
              selected = ""
            ),
            selectInput(
              inputId = "filter_pihp",
              label = tags$p("View a Specific PIHP:"
                             , style = "font-size: 115%;"),
              choices = c("","All",levels(unique(data404$PIHPname))),
              selected = ""
            ),
            checkboxInput(
              inputId = "state_avg",
              label = tags$p("Include a bubble for the State Average?"),
              value = FALSE,
              width = NULL
            ),
            tags$strong("Which service(s) would you like to learn about?"),
            br(),
            selectInput(
              inputId = "select_ServiceType",
              label = tags$p("Specify a Service Type:", style = "font-size: 115%;"),
              choices = c("","All",levels(unique(data404$ServiceType))),
              selected = ""
            ),
            uiOutput(
              "select_code"
            ),
            br(),
            tags$strong("Which metrics would you like to view?"),
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
            radioButtons(
              inputId = "select_Population",
              label = tags$strong("Are you interested in a specific population?"
                                  , style = "font-size: 125%;"),
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
            label = tags$strong("Press the play button below to see how the \ndata changes year over year"
                           , style = "font-size: 90%;"),
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
          tags$strong("Make selections below to start comparing services"
                      , style = "font-size: 120%;"),
          br(),
          br(),
          selectInput(
            inputId = "org_type2",
            label = tags$strong("Which type of organization are you interested in?"
                           , style = "font-size: 115%;"),
            choices = c("","State of MI","PIHP","CMH"),
            selected = ""
          ),
          uiOutput("org_filt"),
          tags$strong("Which service(s) would you like to learn about?"),
          selectInput(
            "select_service",
            label = tags$p("At which level would you like to select services?"
                           , style = "font-size: 115%;"),
            choices = c("","Service Type","Service","HCPCS Code","Code Modifier"),
            selected = ""
          ),
          uiOutput("svslvl_filt"),
          br(),
          tags$strong("Which metrics would you like to view?"),
          br(),
          uiOutput("a"),
          uiOutput("b"),
          uiOutput("c"),
          checkboxInput(
            inputId = "ignore_c",
            label = tags$p("Keep size of bubbles constant?"),
            value = FALSE,
            width = NULL
          ),
          tags$strong("Note: The same variable cannot be selected more than once.",
                      style = "font-size: 80%;"),
          br(),
          br(),
          radioButtons(
            inputId = "select_Population2",
            label = tags$strong("Are you interested in a specific population?"
                                , style = "font-size: 125%;"),
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
            label = tags$strong("Press the play button below to see how the \ndata changes year over year"
                                , style = "font-size: 90%;"),
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
  )
)

        




