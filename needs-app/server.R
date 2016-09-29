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
        filter(Item!="1"&Item!="12a"&Item !="12b"&Item!="14"&Item!="15") %>%
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
    
    # VIZ
    
    output$network <- renderVisNetwork({
      
      #need_network <- need_network()
      
      # make link ids for all levels of to/from options
      # links <- unique(c(unique(as.character(need_network()$from)),
      #                   unique(as.character(need_network()$to))))
      # 
      # name_df <- data.frame("name" = links, "id" = 0:(length(links)-1))
      name_df <-
        unique(c(unique(as.character(need_network()$from)),
                 unique(as.character(need_network()$to)))) %>%
        data.frame("name" = .) %>%
        arrange(name) %>%
        mutate(id = row_number(name)-1)
      
      # Join link IDs to need_network df
      n <- need_network() %>%
        left_join(name_df, by = c("from" = "name")) %>%
        rename(from_id  = id) %>%
        left_join(name_df, by = c("to" = "name")) %>%
        rename(to_id = id) %>%
        droplevels()
      
      name_df <- 
        name_df %>%
        mutate(name = as.factor(recode(name, 
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
                                       waiting = "Waitlist")))
      
      nodes <- 
        name_df %>% 
        rename(label = name) %>%
        mutate(title = recode(id,
                              `0` = "Scheduled for a psychosocial <br>intake assessment.",
                              `1` = "Total service requests or <br>inquiries a CMSHP received.",
                              `2` = "People who requested a <br>service the CMSHP provides",
                              `3` = "Determined eligible for <br>services based on assessment.",
                              `4` = "Placed on wait list for all <br>services.",
                              `5` = "Scheduled for assessment but <br>never showed or withdrew from services <br>prior to assessment.",
                              `6` = "...",
                              `7` = "Requested a non-behavioral <br>health service (food bank, housing shelter).",
                              `8` = "Referred to Medicaid fee for <br>service provider.",
                              `9` = "Referred to Medicaid health <br>plan for services.",
                              `10` = "Did not meet CMHSP criteria <br>and were screened out.",
                              `11` = "Screened out for other reasons <br>(e.g. eligibility could not be determined, <br>withdrew services, declined services)",
                              `12` = "Called seeking to access <br>SUD primary services.",
                              `13` = "Placed on wait list for some <br>services, but authorized for other services.",
                              `14` = "Either waitlist option."))
      
      edges <- 
        n %>% 
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
      
      need_network <- need_network()
      
      # make link ids for all levels of to/from options
      links <- unique(c(levels(need_network$from),
                        levels(need_network$to)))
      name_df <- data.frame("name" = links, "id" = 0:(length(links)-1))
      
      # Join link IDs to need_network df
      n <- need_network %>%
        left_join(name_df, by = c("from" = "name")) %>%
        rename(from_id  = id) %>%
        left_join(name_df, by = c("to" = "name")) %>%
        rename(to_id = id) %>%
        droplevels()
      
      name_df <- 
        name_df %>%
        mutate(name = as.factor(recode(name, 
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
                                       waiting = "Waitlist")))
      
      
      sankeyNetwork(Links = n, 
                    Nodes = name_df, 
                    Source = "from_id",
                    Target = "to_id", 
                    Value = "people", 
                    NodeID = "name",
                    units = "people",
                    fontSize = 20, 
                    nodeWidth = 20, 
                    nodePadding = 20,
                    height = 500, 
                    width = 500
                    )
      
    })
    
    output$flow_df <- renderDataTable({
      
      if ( input$pihp == "All" & input$cmh == "All" ) {
        need_network <- 
          needs %>% 
          filter(Item!="1"&Item!="12a"&Item !="12b"&Item!="14"&Item!="15") %>%
          filter(FY >= input$fy[1] 
                 & FY <= input$fy[2]) %>%
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
      } else if ( input$pihp != "All" & input$cmh != "All") {
        need_network <- 
          needs %>% 
          filter(Item!="1"&Item!="12a"&Item !="12b"&Item!="14"&Item!="15") %>%
          filter(FY >= input$fy[1] 
                 & FY <= input$fy[2]
                 & CMHSP == input$cmh
                 & PIHPname == input$pihp) %>%
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
      } else if ( input$pihp == "All" & input$cmh != "All" ) {
        need_network <- 
          needs %>% 
          filter(Item!="1"&Item!="12a"&Item !="12b"&Item!="14"&Item!="15") %>%
          filter(FY >= input$fy[1] 
                 & FY <= input$fy[2]
                 & CMHSP == input$cmh) %>%
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
      } else if ( input$pihp != "All" & input$cmh == "All" ) {
        need_network <- 
          needs %>% 
          filter(Item!="1"&Item!="12a"&Item !="12b"&Item!="14"&Item!="15") %>%
          filter(FY >= input$fy[1] 
                 & FY <= input$fy[2]
                 & PIHPname == input$pihp) %>%
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
      } else
        print(paste0("Error!  Error!"))
      
      # make link ids for all levels of to/from options
      links <- unique(c(levels(need_network$from),
                        levels(need_network$to)))
      name_df <- data.frame("name" = links, "id" = 0:(length(links)-1))
      
      #total <- sum(need_network)
      
      # Join link IDs to need_network df
      n <- need_network %>%
        left_join(name_df, by = c("from" = "name")) %>%
        rename(from_id  = id) %>%
        left_join(name_df, by = c("to" = "name")) %>%
        rename(to_id = id) %>%
        droplevels() %>%
        mutate(from = as.factor(recode(from, 
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
               to = as.factor(recode(from, 
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
                            / sum(people[from == "All seeking service"]) * 100, 
                            digits = 1) 
               ) %>%
        select(from, to, people, pct)
      
      datatable(n,
                caption = 'Summary of Flow through Access Process',
                rownames = FALSE,
                colnames = c('From step...','To step...',
                             '# of people','% of total'),
                extensions = c(Responsive),
                options = list(pageLength = 15, lengthMenu = c(5, 15))
                )
      
    })
    
    output$bar <- renderDimple({
      
      need_dimple <-
        needs %>%
        select(FY,PIHPname,CMHSP,Name,Population,People) %>%
        group_by(FY,PIHPname, CMHSP,Population) %>%
        spread(Name,People) %>%
        ungroup() %>%
        filter(FY >= input$fy[1] 
               & FY <= input$fy[2]) %>%
        group_by(CMHSP) %>%
        summarize(all_wait = sum(all_wait),
                  assmt_sched = sum(assmt_sched),
                  eligible = sum(eligible),
                  immed_crit = sum(immed_crit),
                  no_elig_deter = sum(no_elig_deter),
                  not_eligible = sum(not_eligible),
                  out_nonMH = sum(out_nonMH),
                  req_CMHsvc = sum(req_CMHsvc),
                  rfr_to_FFS = sum(rfr_to_FFS),
                  rfr_to_mh_N = sum(rfr_to_mh_N),
                  rfr_to_mh_Y = sum(rfr_to_mh_Y),
                  rfr_to_MHP = sum(rfr_to_MHP),
                  screened_out = sum(screened_out),
                  screened_out_other = sum(screened_out_other),
                  seeking_SUD = sum(seeking_SUD),
                  some_wait = sum(some_wait),
                  total_in = sum(total_in),
                  urgent_crit = sum(urgent_crit),
                  waiting = sum(waiting)
                  ) %>%
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
        select(CMHSP, throughput:elig_wait) %>%
        group_by(CMHSP) %>%
        gather(Measure,Score, throughput:elig_wait) %>%
        mutate(MeasureDesc = recode(Measure,
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
                                    elig_wait = "% of eligibles on waitlist")) %>%
        filter(is.na(Score) == F
               & Score >= 0
               & Score <= 100
               & MeasureDesc == input$measure
               ) %>%
        droplevels() %>%
        arrange(desc(Score)) %>%
        ungroup()
      
#       cmh_table <-
#       need_dimple %>%
#         group_by(CMHSP) %>%
#         summarise(n = n()) %>%
#         mutate(cmh_color = ifelse(CMHSP == input$cmh,
#                                   yes == "#bb00cc",
#                                   no == "#00ccff"))
      
      need_dimple %>%
        dimple(x ="CMHSP", y = "Score", type = "bar") %>%
        set_bounds(x = "7%", y = "5%",
                   width = "90%", 
                   height = "65%") %>%
        xAxis(orderRule = "Date", 
              title = "Item",
              fontSize = "80%" #, hidden=TRUE
              )  %>%
        yAxis(title = "% score",
              fontSize = "80%",
              overrideMin = 0, 
              overrideMax = 100) # %>%
        # default_colors(list(cmh_table$pihp_color))

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
      }
      )
    
    } 
  )