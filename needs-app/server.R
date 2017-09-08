## server.R ##

shinyServer(
  function(input, output) { 
    
    # REACTIVE DATASETS
    
    need_network <- reactive({
      
      pihp_filt <- if ( input$pihp == "All" ) {
        levels(needs$PIHPname)
      } else if ( input$pihp != "All") {
        input$pihp
      } else print(paste0("Error!  Error!"))
      
      cmh_filt <- if ( input$cmh == "All" ) {
        levels(needs$CMHSP)
      } else if ( input$cmh != "All") {
        input$cmh
      } else print(paste0("Error!  Error!"))
      
      needs %>% 
        filter(Name != "total_in" 
               & Name !="rfr_to_mh_Y"
               & Name !="rfr_to_mh_N"
               & Name != "urgent_crit" 
               & Name !="immed_crit") %>%
        filter(FY >= input$fy[1] 
               & FY <= input$fy[2]
               & CMHSP %in% cmh_filt
               & PIHPname %in% pihp_filt) %>%
        droplevels() %>%
        select(Phase, to = Name, People) %>%
        mutate(from = recode(Phase, 
                             Start = "total_in",
                             Entry = "total_in",
                             Screening = "req_CMHsvc",
                             Eligibility = "assmt_sched",
                             Waiting = "eligible")) %>%
        group_by(from,to) %>%
        summarize(people = sum(People, na.rm = T)) %>%
        ungroup()
      
    })
    
    nodes <- reactive({
      
      unique(c(unique(as.character(need_network()$from)),
               unique(as.character(need_network()$to)))) %>%
        data.frame("name_id" = .) %>%
        # Alphabetize
        arrange(name_id) %>%
        # Assign ids starting at 0
        mutate(id = row_number(name_id)-1) %>%
        mutate(name = recode(name_id, 
                             assmt_sched = "Assessment",
                             eligible = "Eligible",
                             req_CMHsvc = "Seek CMH service",
                             total_in = "All seeking service",
                             all_wait = "Waitlist all",
                             no_elig_deter = "No show",
                             not_eligible = "Other",
                             out_nonMH = "Non MH needs",
                             rfr_to_FFS = "To FFS",
                             rfr_to_MHP = "To MHP",
                             screened_out = "Screened out",
                             screened_out_other = "Screened other",
                             seeking_SUD = "To SUD",
                             some_wait = "Waitlist some",
                             waiting = "Waitlist"),
               title = recode(name_id,
                              assmt_sched = "Scheduled for a psychosocial <br>intake assessment.",
                              total_in = "Total service requests or <br>inquiries a CMSHP received.",
                              req_CMHsvc = "People who requested a <br>service the CMSHP provides",
                              eligible = "Determined eligible for <br>services based on assessment.",
                              all_wait = "Placed on wait list for all <br>services.",
                              no_elig_deter = "Scheduled for assessment but <br>never showed or withdrew from services <br>prior to assessment.",
                              not_eligible = "Determined ineligible for <br>other reasons.",
                              out_nonMH = "Requested a non-behavioral <br>health service (food bank, housing shelter).",
                              rfr_to_FFS = "Referred to Medicaid fee-for- <br>service provider.",
                              rfr_to_MHP = "Referred to Medicaid health <br>plan for services.",
                              screened_out = "Did not meet CMHSP criteria <br>and were screened out.",
                              screened_out_other = "Screened out for other reasons <br>(e.g. eligibility could not be determined, <br>withdrew services, declined services)",
                              seeking_SUD = "Called seeking to access <br>SUD primary services.",
                              some_wait = "Placed on wait list for some <br>services, but authorized for other services.",
                              waiting = "Either waitlist option."))
      
    })
    
    edges <- reactive({
      
      # Join link IDs to need_network df
      need_network() %>%
        left_join(nodes(), by = c("from" = "name_id")) %>%
        select(-name) %>%
        rename(from_id  = id) %>%
        left_join(nodes(), by = c("to" = "name_id")) %>%
        select(-name) %>%
        rename(to_id = id) %>%
        droplevels()
      
    })
    
    need_metrics <- reactive({
      
      pihp_filt <- if ( input$pihp == "All" ) {
        levels(needs$PIHPname)
      } else if ( input$pihp != "All") {
        input$pihp
      } else print(paste0("Error!  Error!"))
      
      cmh_filt <- if ( input$cmh == "All" ) {
        levels(needs$CMHSP)
      } else if ( input$cmh != "All") {
        input$cmh
      } else print(paste0("Error!  Error!"))
      
      # Summarize to create measures
      
      needs %>%
        filter(FY >= input$fy[1] 
               & FY <= input$fy[2]
               & CMHSP %in% cmh_filt
               & PIHPname %in% pihp_filt) %>%
        droplevels() %>%
        select(FY,PIHPname,CMHSP,Name,Population,People) %>%
        group_by(FY,PIHPname, CMHSP,Population) %>%
        spread(Name,People) %>%
        ungroup() %>%
        mutate(throughput_num = eligible - waiting,
               throughput_den = total_in,
               in_nonMH_num = out_nonMH,
               in_nonMH_den = total_in,
               drop_out_num = no_elig_deter,
               drop_out_den = assmt_sched,
               assess_elig_num = eligible,
               assess_elig_den = assmt_sched - no_elig_deter,
               in_req_num = req_CMHsvc,
               in_req_den = total_in,
               req_screenout_num = screened_out,
               req_screenout_den = req_CMHsvc,
               refer_MHP_num = rfr_to_MHP,
               refer_MHP_den = assmt_sched - no_elig_deter,
               refer_FFS_num = rfr_to_FFS,
               refer_FFS_den = assmt_sched - no_elig_deter,
               inelig_rfrMH_num = rfr_to_mh_Y,
               inelig_rfrMH_den = not_eligible,
               elig_urg_imm_num = urgent_crit + immed_crit,
               elig_urg_imm_den = eligible,
               some_wait_num = some_wait,
               some_wait_den = waiting,
               all_wait_num = all_wait,
               all_wait_den = waiting,
               elig_wait_num = waiting,
               elig_wait_den = eligible
        ) %>%
        select(FY:Population, throughput_num:elig_wait_den) %>%
        group_by(FY,PIHPname,CMHSP,Population) %>%
        gather(Measure,Score, throughput_num:elig_wait_den) %>%
        separate(Measure,
                 into = c("Measure","numtype"),
                 sep = "_\\s*(?=[^_]+$)") %>% # Split at last '_' character
        group_by(FY,PIHPname,CMHSP,Population,Measure) %>%
        spread(numtype,Score) %>%
        mutate(Score = round(num/den * 100, digits = 1),
               Score = ifelse(is.nan(Score) == T,0,Score), # Replace NaN with 0
               MeasureDesc = recode(Measure,
                                    throughput = "Overall Access",
                                    in_nonMH = "% total requesting non-CMH services",
                                    drop_out = "Assessment Drop-out Rate",
                                    assess_elig = "% Assessed Eligible",
                                    in_req = "% total requesting CMH services",
                                    req_screenout = "% Screened Out",
                                    refer_MHP = "% Referred to MHP",
                                    refer_FFS = "% Referred to FFS",
                                    inelig_rfrMH = "% Referred for External MH Svs",
                                    elig_urg_imm = "% Meeting Acute Criteria",
                                    some_wait = "% of waitlist with partial service",
                                    all_wait = "% of waitlist with partial service",
                                    elig_wait = "% of eligibles on waitlist")) 
      
    })
    
    # REACTIVE FILTERS
    
    output$cmh <- renderUI({
      
      filtre <- if (input$pihp == "All") {
        levels(needs$CMHSP)
      } else 
        levels(droplevels(needs$CMHSP[needs$PIHPname == input$pihp]))
      
      selectInput(
        "cmh",
        label = "Select CMH:",
        choices = c("All", filtre), 
        selected = "All"
      )
      
    })
    
    output$fy <- renderUI({
      
      sliderInput(
        "fy", 
        label = "Select FY(s):", 
        min = min(as.numeric(needs$FY)), 
        max = max(as.numeric(needs$FY)), 
        value = c(min(as.numeric(needs$FY)), 
                  max(as.numeric(needs$FY)))
      )
      
    })
    
    output$measure <- renderUI({
      
      selectInput(
        "measure",
        label = "Select a measure:",
        choices = unique(need_metrics()$MeasureDesc),
        selected = "Overall Access"
      )
      
    })
    
    # VIZ
    
    output$network <- renderVisNetwork({
      
      nodes <- nodes() %>% rename(label = name)
      
      edges <- 
        edges() %>% 
        rename(from_desc = from, to_desc = to, 
               from = from_id, to = to_id, value = people) %>%
        mutate(title = paste0(value," people"))
      
      visNetwork(nodes, edges, height = "600px", width = "100%") %>% 
        visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>%
        visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 3)),
                 color = list(color = "#78B7C5", highlight = "#E1AF00")) %>%
        visLayout(randomSeed = 123) 
      
    })
    
    output$sankey <- renderSankeyNetwork({
      
      sankeyNetwork(Links = edges(), 
                    Nodes = nodes(), 
                    Source = "from_id",
                    Target = "to_id", 
                    Value = "people", 
                    NodeID = "name",
                    units = "people",
                    fontSize = 20, 
                    nodeWidth = 20, 
                    nodePadding = 20,
                    height = 500, 
                    width = 500)
      
    })
    
    output$flow_df_preamble <- renderText({
      
      pihp_filt <- if ( input$pihp == "All" ) {
        levels(needs$PIHPname)
      } else if ( input$pihp != "All") {
        input$pihp
      } else print(paste0("Error!  Error!"))
      
      cmh_filt <- if ( input$cmh == "All" ) {
        unique(needs$CMHSP[needs$PIHPname %in% pihp_filt])
      } else if ( input$cmh != "All") {
        input$cmh
      } else print(paste0("Error!  Error!"))
      
      paste0(
        "In FY ",input$fy[1],"-",input$fy[2],", the selected CMHSPs (", 
        paste(cmh_filt,sep = '',collapse = ', '),") reported that ",
        sum(need_network()$people[need_network()$from == "total_in"]),
        " people sought services. The table below shows how those individuals 
        moved through the process of accessing services, and what percentage of 
        people moved from a given step in the access process to the next step."
      )
      
    })
    
    output$flow_df <- renderDataTable({
      
      edges() %>%
        mutate(from_desc = as.factor(recode(from, 
                                       assmt_sched = "Assessment",
                                       eligible = "Eligible",
                                       req_CMHsvc = "Seek CMH service",
                                       total_in = "All seeking service",
                                       all_wait = "Waitlist all",
                                       no_elig_deter = "No show",
                                       not_eligible = "Other",
                                       out_nonMH = "Non MH needs",
                                       rfr_to_FFS = "To FFS",
                                       rfr_to_MHP = "To MHP",
                                       screened_out = "Screened out",
                                       screened_out_other = "Screened other",
                                       seeking_SUD = "To SUD",
                                       some_wait = "Waitlist some",
                                       waiting = "Waitlist")),
               to_desc = as.factor(recode(to, 
                                     assmt_sched = "Assessment",
                                     eligible = "Eligible",
                                     req_CMHsvc = "Seek CMH service",
                                     total_in = "All seeking service",
                                     all_wait = "Waitlist all",
                                     no_elig_deter = "No show",
                                     not_eligible = "Other",
                                     out_nonMH = "Non MH needs",
                                     rfr_to_FFS = "To FFS",
                                     rfr_to_MHP = "To MHP",
                                     screened_out = "Screened out",
                                     screened_out_other = "Screened other",
                                     seeking_SUD = "To SUD",
                                     some_wait = "Waitlist some",
                                     waiting = "Waitlist")),
               pct = round( people 
                            / sum(people[from == "total_in"]) * 100, 
                            digits = 1)
        ) %>%
        select(from_desc,to_desc,people,pct) %>%
        datatable(caption = 'Summary of Flow through Access Process',
                  rownames = FALSE,
                  colnames = c('From step...','To step...',
                               '# of people','From as % of To'),
                  options = list(pageLength = 15, lengthMenu = c(5, 15))) 
      
    })
    
    output$bar <- renderPlotly({
      
      need_metrics() %>%
        filter(is.na(Score) == F
               & Score >= 0
               & Score <= 100
               & MeasureDesc == input$measure
        ) %>%
        ungroup() %>% droplevels() %>%
        group_by(CMHSP) %>%
        summarize(num = sum(num, na.rm = T),
                  den = sum(den, na.rm = T)) %>%
        mutate(Score = round(num/den * 100, digits = 1),
               Score = ifelse(is.nan(Score) == T,0,Score)) %>%
        arrange(desc(Score)) %>%
        ungroup() %>%
        mutate(avg_score = round(sum(num, na.rm = T)/sum(den, na.rm = T) * 100, 
                                 digits = 1)) %>%
        plot_ly(x = CMHSP, y = Score, type = "bar",
                marker = list(color = "#DC863B"),
                name = paste0("% score"),
                text = paste("Numerator: ", num,
                             "<br>Denominator: ", den)) %>%
        add_trace(x = CMHSP, 
                  y = rep(avg_score, each = nlevels(as.factor(CMHSP))), 
                  type = "line",
                  line = list(dash = 5),
                  marker = list(color = "#555555"),
                  name = "Weighted Average", 
                  yaxis = "y") %>%
        layout(title = paste0(input$measure,", by CMHSP"),
               xaxis = list(title = input$group, showticklabels = F,
                            categoryarray = CMHSP, categoryorder = "array"),
               yaxis = list(title = "% score",
                            range = c(0, 100)),
               legend = list(xanchor = "right", yanchor = "top", x = 1, y = 1, 
                             font = list(size = 10)))
      
    })
    
    output$metric_nm <- renderText({
      
      paste0(input$measure)
      
    })
    
    output$define <- renderText({
      
      if ( input$measure == "Overall Access" ) {
        paste0("This is a global access measure of the percentage of all individuals
               seeking services who are determined to be eligible (and who are 
               not placed on a waiting list.")
      } else if ( input$measure == "% Assessed Eligible" ) {
        paste0("This measure shows the percentage of individuals who received an 
               assessment who were determined to be eligible for specialty 
               behavioral health services. If there is significant variation in 
               the percentage of persons determined to meet CMHSP eligibility 
               criteria between CMHSPs, the PIHP may want to dig deeper to determine 
               what is causing this variation. If one CMHSP had a significantly 
               lower rate for this measure, it is possible that CMSHP is utilizing 
               more stringent access standards for entry into care. How is the 
               PIHP ensuring that a standard front door process is followed 
               throughout the region? CMHSPs serving the mild to moderate population 
               are likely to have a higher percentage of persons determined eligible.")
      } else if ( input$measure == "% Meeting Acute Criteria" ) {
        paste0("This measure shows the percent of people assessed to be eligible 
               for specialty behavioral health services who met urgent or emergent 
               criteria. Although the mental health code defines an urgent situation 
               as 'a situation in which an individual is determined to be at risk 
               of experiencing an emergency situation in the near future if he or 
               she does not receive care, treatment, or support services.' It does 
               not define emergent, routine, or immediate admission criteria. The 
               access system standards state 'Access system staff shall first 
               determine whether the presenting mental health need is urgent, 
               emergent or routine and, if so, will address emergent and urgent 
               need first. To assure understanding of the problem from the point 
               of view of the person who is seeking help, methods for determining 
               urgent or emergent situations must incorporate caller or client-defined 
               crisis situations. Workers must be able to demonstrate empathy as 
               a key customer service method. Therefore, there is considerable 
               room for interpretation and subjective decision making. It may be 
               beneficial for the PIHP to facilitate a discussion among the CMHSPs 
               to determine how their access departments define these terms on a 
               day to day basis. It should also be clarified if these conditions 
               are tracked as a data element that is monitored and if not, how 
               the CMHSP determined their answer for these items.' (See: Medicaid 
               Managed Specialty Services and Supports Contract, Attachment P4.1.1; 
               page 4 Screening For Crisis)")
      } else if ( input$measure == "% of eligibles on waitlist" ) {
        paste0("This measure shows the percent of people assessed to be eligible 
               for specialty behavioral health services who were placed on a 
               waitlist for any or all services.  The Access Standards  make it 
               clear that no Medicaid beneficiary can be placed on a wait list 
               for a medically necessary Medicaid service and that the CMHSP 
               maintains a waiting list for persons not eligible for Medicaid or 
               MIChild. The Mental Health Code requirement is somewhat broader, 
               requiring each CMHSP to maintain a waiting list to record all 
               service needs that are unmet.  However, CMHSPs may interpret this 
               differently. To ensure consistent processes are followed throughout 
               the region, the PIHP may want to engage in dialogue with the CMHSPs 
               to identify how they do or do not utilize a wait list. For example, 
               if a child is eligible for and in need of homebased services, 
               however there is no capacity with a homebased worker therefore the 
               child is served via case management – the CMHSP may or may not 
               record this consumer on the wait list as the child is receiving a 
               service and they believe the services are meeting medical necessity. 
               One concern with not documenting this on the wait list is the lack 
               of documentation of increased need for this service and the possibility 
               of that child being underserved. Another example could be regarding 
               use of evidenced base practice interventions. If an adult consumer 
               is eligible for and in need of DBT services but there is not an 
               opening with a DBT therapist, the CMHSP may or may not place that 
               consumer on a waiting list for DBT. The waiting list has the potential 
               to inform Utilization Management and Provider Network regarding 
               service needs.  (See:   Medicaid Managed Specialty Services and 
               Supports Contract, Attachment P4.1.1; page 9 Waiting Lists; and 
               Mental Health Code, section 330.1124 Waiting List for Admissions")
      } else if ( input$measure == "% of waitlist with partial service" ) {
        paste0("This measure shows the percent of people who were placed on a  
               waitlist for specialty behavioral health services who received at 
               least some services.  The Access Standards make it 
               clear that no Medicaid beneficiary can be placed on a wait list 
               for a medically necessary Medicaid service and that the CMHSP 
               maintains a waiting list for persons not eligible for Medicaid or 
               MIChild. The Mental Health Code requirement is somewhat broader, 
               requiring each CMHSP to maintain a waiting list to record all 
               service needs that are unmet.  However, CMHSPs may interpret this 
               differently. To ensure consistent processes are followed throughout 
               the region, the PIHP may want to engage in dialogue with the CMHSPs 
               to identify how they do or do not utilize a wait list. For example, 
               if a child is eligible for and in need of homebased services, 
               however there is no capacity with a homebased worker therefore the 
               child is served via case management – the CMHSP may or may not 
               record this consumer on the wait list as the child is receiving a 
               service and they believe the services are meeting medical necessity. 
               One concern with not documenting this on the wait list is the lack 
               of documentation of increased need for this service and the possibility 
               of that child being underserved. Another example could be regarding 
               use of evidenced base practice interventions. If an adult consumer 
               is eligible for and in need of DBT services but there is not an 
               opening with a DBT therapist, the CMHSP may or may not place that 
               consumer on a waiting list for DBT. The waiting list has the potential 
               to inform Utilization Management and Provider Network regarding 
               service needs.  (See:   Medicaid Managed Specialty Services and 
               Supports Contract, Attachment P4.1.1; page 9 Waiting Lists; and 
               Mental Health Code, section 330.1124 Waiting List for Admissions")
      } else if ( input$measure == "% Referred for External MH Svs" ) {
        paste0("This measure shows the percentage of individuals who are determined 
               to be ineligible for specialty behavioral health services yet who 
               are referred to external mental health service providers.")
      } else if ( input$measure == "% Referred to FFS" ) {
        paste0("This measures shows the percentage of individuals who had an 
               assessment scheduled who were not served because they were MA FFS 
               enrolled and referred to other MA FFS providers (not health plan). 
               It excludes individuals who did not receive eligibility determination 
               (dropped out, no show, etc.) from the denominator, in order to 
               identify persons who were actually assessed.")
      } else if ( input$measure == "% Referred to MHP" ) {
        paste0("There are some CMHSPs that are serving those with a mild/moderate 
               impairment thru contracts with the Medicaid Health Plans to provide 
               the 20 session benefit. In some regions there continues to be a 
               lack of health plan providers and/or other barriers to individuals 
               accessing behavioral health benefits thru their Medicaid Health Plan. 
               The Mental Health and Substance Abuse Section of the Medicaid Provider 
               Manual  provides guidance to assist health plans and PIHPs in making 
               coverage determination decisions related to outpatient care for MHP 
               beneficiaries If a CMHSP has all zeros (or a very small number listed), 
               it is possible they are serving the mild/moderate population. The 
               PIHP may want to review the CMHSP’s access processes for this 
               population and assess local need. Advocacy or collaboration with 
               the local Medicaid Health Plans may be needed to improve access of 
               care for this population. The PIHP may wat to explore the following 
               issues: Does the CMHSP have clear business/clinical processes to 
               document in the record who is being served via a mild/moderate 
               benefit? How does that benefit compare to the benefit provided to 
               the priority population? Has serving the mild/moderate population 
               had any negative impact of the services available to the priority 
               population? (See: Medicaid Provider Manual,Mental Health and Substance 
               Abuse Chapter, Section 1.6 Beneficiary Eligibility, page 885")
      } else if ( input$measure == "% Screened Out" ) {
        paste0("This measure shows the percentage of people who requested CMHSP 
               services yet who did not meet eligibility as determined by a 
               screening (phone or other).")
      } else if ( input$measure == "% total requesting CMH services" ) {
        paste0("This measure displays the percentage of all people who telephoned 
               or walked in who requested services the CMHSP provides.  Lower scores 
               in this area could be indicative of a need for increased community 
               awareness of CMHSP services and/or development of capacity to access 
               multiple public and social services through a single access point.")
      } else if ( input$measure == "% total requesting non-CMH services" ) {
        paste0("This measure displays the percentage of all people who telephoned 
               or walked in who requested services the CMHSP does not provide.")
      } else if ( input$measure == "Assessment Drop-out Rate" ) {
        paste0("There has been a significant emphasis on improving timely access 
               to care over the past few years as evidenced by such initiatives 
               as just-in-time scheduling and open access. It may be helpful to 
               compare the percentage of scheduled assessments that were not 
               completed (no show, withdrew from services, etc) by CMSHP or 
               region to region. Agencies with a higher rate of completed 
               assessments may be utilizing certain engagement techniques that 
               could help set best practice standards for the region. Please 
               remember that this issue is often significantly impacted by the 
               availability (or lack thereof) of public transportation within a 
               community. Therefore, performance may vary significantly from 
               urban to rural areas.")
      } else
        paste0("Error: Unexpected Measure Name")
      
    })
    
    # output$heatmap 
      # d3heatmap allowing selection of fill measure
      # filter range of years
      # x = CMHSP, y = service
    
    # output$dygraph
      # line chart with measure selection
      # Compare PIHPs, CMHSPs
      # include selection of time periods
    
    output$downloadData <- downloadHandler(
      filename = "MDHHS_ServiceDisposition_Combined.csv",
      content = function(file) {
        write.csv(needs, file)
      })
    
    })

