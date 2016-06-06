## ui.R ##

  library(car)
  library(dplyr)
  library(tidyr)
  library(rcdimple)
  library(DT)
  library(networkD3)
  library(shinydashboard)

#####
# Load de-identified data
  needs <- read.csv("data/needs.csv")

# Summarize to create measures

need_metrics <-
  needs %>%
  select(FY,PIHPname,CMHSP,Name,Population,People) %>%
  group_by(FY,PIHPname, CMHSP,Population) %>%
  spread(Name,People) %>%
  ungroup() %>%
  mutate(throughput = round((eligible - waiting)/total_in * 100, digits = 1),
         in_nonMH = round(out_nonMH/total_in*100, digits = 1),
         drop_out = round(no_elig_deter/assmt_sched*100, digits = 1),
         assess_elig = round(eligible/(assmt_sched - no_elig_deter)*100, digits = 1),
         in_req = round(req_CMHsvc/total_in*100, digits = 1),
         req_screenout = round(screened_out/req_CMHsvc*100, digits = 1),
         refer_MHP = round(rfr_to_MHP/(assmt_sched - no_elig_deter)*100, digits = 1),
         refer_FFS = round(rfr_to_FFS/(assmt_sched - no_elig_deter)*100, digits = 1),
         inelig_rfrMH = round(rfr_to_mh_Y/not_eligible*100, digits = 1),
         elig_urg_imm = round((urgent_crit + immed_crit) / eligible * 100, digits = 1),
         some_wait = round(some_wait / waiting * 100, digits = 1),
         all_wait = round(all_wait / waiting * 100, digits = 1),
         elig_wait = round(waiting / eligible * 100, digits = 1)
         ) %>%
  select(FY:Population, throughput:elig_wait) %>%
  group_by(FY,PIHPname,CMHSP,Population) %>%
  gather(Measure,Score, throughput:elig_wait) %>%
  mutate(MeasureDesc = recode(Measure,
                              "'throughput' = 'Overall Access';
                                'in_nonMH' = '% total requesting non-CMH services';
                                'drop_out' = 'Assessment Drop-out Rate';
                                'assess_elig' = '% Assessed Eligible';
                                'in_req' = '% total requesting CMH services';
                                'req_screenout' = '% Screened Out';
                                'refer_MHP' = '% Referred to MHP';
                                'refer_FFS' = '% Referred to FFS';
                                'inelig_rfrMH' = '% Referred for External MH Svs';
                                'elig_urg_imm' = '% Meeting Acute Criteria';
                                'some_wait' = '% of waitlist with partial service';
                                'all_wait' = '% of waitlist with partial service';
                                'elig_wait' = '% of eligibles on waitlist'"))

## DEFINE UI ##

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
      selectInput("cmh",
                  label = "Select a CMH:",
                  choices = c("All", levels(unique(needs$CMHSP))), 
                  selected = "All"),
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
                  tabPanel("Chart", 
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