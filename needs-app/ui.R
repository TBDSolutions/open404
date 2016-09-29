## ui.R ##

dashboardPage(skin = "green",
  dashboardHeader(title = "Access to Service"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Access Flow", tabName = "flow", icon = icon("random")),
      menuItem("About", tabName = "about", icon = icon("file-text")),
      selectInput("pihp",
                  label = "Pick a PIHP:",
                  choices = c("All", levels(unique(needs$PIHPname))), 
                  selected = "All"),
      uiOutput("cmh"),
      sliderInput("fy", 
                  label = "Select FY(s):", 
                  min = min(as.numeric(needs$FY)), 
                  max = max(as.numeric(needs$FY)), 
                  value = c(min(as.numeric(needs$FY)), 
                            max(as.numeric(needs$FY)))
                  )
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "flow",
              fluidRow(
                tabBox(
                  title = "Flow",
                  tabPanel("Network",
                           visNetworkOutput("network")),
                  tabPanel("Sankey", 
                           sankeyNetworkOutput("sankey",
                                               width = "100%", 
                                               height = 500 #"100%"
                                               )),
                  tabPanel("Table", 
                           dataTableOutput("flow_df")
                           ),
                  tabPanel("About", 
                           br(),
                           strong("Visualizing flow through a network"),
                           p("The visualization you see here is called a", 
                             a(href = "https://en.wikipedia.org/wiki/Sankey_diagram","Sankey diagram"), 
                             ". It provides a visual summary of the disposition 
                             for all new service requests by CMHSP. The width of 
                             the arrow is shown proportionately to the flow 
                             quantity (in this example, the wider the arrow the 
                             more consumers given that disposition are represented). 
                             The following is a description of data labels found 
                             within the diagram:",
                             br(),
                             strong("All seeking service:"), 
                             "Total number of new service requests or inquiries 
                             a CMSHP received.",
                             br(),
                             strong("Non MH needs:"), 
                             "Those persons that called the CMHSP and were referred 
                             out due to requesting a non MH service (food bank, 
                             housing shelter, legal services).", 
                             br(),
                             strong("Seek CMH service:"), 
                             "Those persons that called the CMSHP and requested 
                             a service the CMSHP provides",
                             br(),
                             strong("Assessment:"), 
                             "Those persons scheduled for a psychosocial intake 
                             assessment at the CMHSP.",
                             br(),
                             strong("Eligible:"), 
                             "Those persons determined eligible for CMH services 
                             based on results of intake assessment",
                             br(),
                             strong("To SUD:"), 
                             "Those persons that called the CMSHP that were seeking 
                             SUD primary services (not co-occurring treatment at 
                             the CMHSP)",
                             br(),
                             strong("Screened out:"),
                             "Those persons that did not meet CMHSP criteria and 
                             were screened out.", 
                             br(),
                             strong("Screened other:"), 
                             "Screened out for other reasons (e.g. eligibility 
                             could not be determined, withdrew services, declined 
                             services)",
                             br(),
                             strong("No show:"), 
                             "Those consumers that were scheduled for an assessment 
                             but never showed for this service or withdrew from 
                             services prior to assessment being completed.",
                             br(),
                             strong("To MHP:"), 
                             "Those consumers referred to Medicaid health plan 
                             for services.",
                             br(),
                             strong("To FFS:"), 
                             "Those consumers referred to Medicaid fee for service 
                             provider.",
                             br(),
                             strong("Waitlist all:"), 
                             "Those consumers placed on wait list for all services.", 
                             br(),
                             strong("Waitlist some:"), 
                             "Those consumers placed on wait list for some services, 
                             but authorized for some CMHSP services."
                             )
                           ) 
                ),
                tabBox(
                  title = "Access",
                  tabPanel("Compare", 
                           selectInput("measure",
                                       label = "Select a measure:",
                                       choices = levels(unique(need_metrics$MeasureDesc))), 
                           dimpleOutput("bar")
                  ),
                  tabPanel("About", 
                           br(),
                           strong("The access measure you selected is..."),
                           br(),
                           h3(textOutput("metric_nm")),
                           br(),
                           h5(textOutput("define"))
                  )
                )
              )
            ),
      
            # Third tab content
            tabItem(tabName = "about",
                    fluidRow(
                      box(
                        title = "Needs Assessment", 
                        status = "primary",
                        collapsible = T,
                        br(),
                        strong("About the Data"),
                        p("Data related to service requests and access come from 
                          the ",
                          a(href = "http://www.michigan.gov/documents/mdch/Attachment_B_Request_for_Service_and_Disposition_of_Requests_475120_7.xls",
                            "Request for Service and Disposition of Requests"),
                          "form, a common data format submitted annually as part 
                          of the annual written assessment of community need 
                          completed by Community Mental Health Service Programs 
                          (CMHSPs) in compliance with the Michigan Mental Health 
                          Code.  This element of the ", 
                          a(href = "http://www.michigan.gov/mdhhs/0,5885,7-339-71550_2941_38765_47281-268914--,00.html", 
                            "Needs Assessment"), 
                          " contains data related to requests for service and 
                          waiting list information consistent with other CMHSP 
                          contractual requirements."),
                        p("You can download the combined and cleaned data by 
                          clicking the link below:"),
                        downloadButton('downloadData', 'Download')
                        )
                      )
                    
            )

    )
  )
)