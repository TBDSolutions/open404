# ui.R #

shinyUI(
  navbarPage(
    "Explore 404",
    theme = shinythemes::shinytheme("yeti"),
    navbarMenu(
      "About",
      tabPanel(
        "General",
        fluidRow(
          column(
            width = 6,
            mainPanel(
              tags$strong("open404", style = "font-size: 125%;"),
              br(),
              p(
                "The cost and utilization data is collected by the Michigan 
                Department of Health and Human Services' (MDHHS) Behavioral 
                Health and Developmental Disabilities Administration (BHDDA) 
                and reported annually to the Michigan legislature. Given the 
                ongoing changes to Michigan’s public health system, this data 
                can be used to understand service use, cost trends and 
                variation across the state for vulnerable populations.  This 
                application has been developed by"
              ),
              img(src = 'tbdSolutions-logo.png', width = "200px", align = "left"),p(tags$sub(a(href = "https://www.tbdsolutions.com/","©2019"))),
              br(),
              br(),
              tags$strong("license", style = "font-size: 125%;"),
              br(),
              p(
                "These data and visualizations, and the code supporting them, 
                are licensed under a ",
                tags$a(
                  href = "https://creativecommons.org/licenses/by-nc-sa/4.0/",
                  "Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)"
                ),
                " license.  You can find documentation on ",
                tags$a(
                  href = "https://github.com/j-hagedorn/open404",
                  "our GitHub repository"
                ),
                ".  We ask that any users of this application for reporting or 
                research purposes cite this source appropriately."
              )
            )
          ),
          column(
            width = 6,
            mainPanel(
              tags$strong("navigation", style = "font-size: 125%;"),
              br(),
              p(
                "The drop down menus above can be used to navigate the 
                application. The 'About' menu includes definitions of available 
                data elements and service groupings. The 'Motion Chart' menu 
                includes data visualizations that can be used to explore the data."
              ),
              tags$strong("motion chart", style = "font-size: 125%;"),
              br(),
              p(
                tags$ul(
                  tags$li(
                    strong("Compare Organizations: "),
                    "This chart compares a single Service Type or Service across 
                    PIHPs or CMHs. This chart answers questions such as: 
                    'How does total cost compare to units per case for all CMHs 
                    within my PIHP?'", 
                    style = "font-size: 100%;"
                  ),
                  tags$li(
                    strong("Compare Services: "),
                    "This chart compares multiple services within a single PIHP 
                    or CMH. This chart answers questions such as: 
                    'How does total cost compare to total units per case for 
                    community living services within my PIHP?'", 
                    style = "font-size: 100%;"
                  )
                )
              ),
              br(),
              p("You can download the 404 data used in this application below:"),
              downloadButton('downloadData', 'Download'),
              br(),
              tags$small(
                tags$i(
                  paste("Data Updated through",max(as.character(data404$FY)
                                                   )
                        )
                  )
                )
            )
          )
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
          br(),
          p("You can download the service groupings used in this application below:"),
          downloadButton('downloadData2', 'Download'),
          br(),
          br(),
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
            max = 2017,
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
            max = 2017,
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

        




