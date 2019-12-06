# server.R #

shinyServer(function(input, output) {
  
  #### Make Reactive Datasets ####
  
  df_bubble1 <- reactive({
    
    # Filter by PIHP
    pihp_filt <- if (input$filter_pihp == "All") {
      data404 <- data404
    } else df <- data404 %<>% filter(pihp_name %in% input$filter_pihp)
    
    # Relabel selected grouping variable
    if (input$org_type == "PIHP") {
      df <- data404 %>% dplyr::rename(org_grp = pihp_name)
    } else if (input$org_type == "CMH") {
      df <- data404 %>% dplyr::rename(org_grp = cmhsp)
    } else print(paste0("Error.  Unrecognized input."))
    
    # Filter by Service Type
    svc_type_filt <- if (input$select_svc_type == "All") {
      levels(as.factor(df$svc_type))
    } else input$select_svc_type
    
    # Filter by HCPCS Code
    code_filt <- if (input$select_code == "All") {
      levels(as.factor(df$short_desc))
    } else input$select_code
    
    # Filter by population
    pop_filt <- if (input$select_population == "All") {
      levels(as.factor(df$population))
    } else input$select_population
    
    # Aggregating by selected org_grp
    df <-
      df %>%
    # df %<>%
      filter(svc_type %in% svc_type_filt
             & population %in% pop_filt
             & short_desc %in% code_filt
      ) %>%
      group_by(fy,org_grp) %>%
      summarize(
        cases = sum(cases, na.rm = T),
        units = sum(units, na.rm = T),
        cost = sum(cost, na.rm = T)
      ) %>%
      ungroup() %>%
      mutate(
        cost_per_case = round(cost / cases, digits = 2),
        cost_per_unit = round(cost / units, digits = 2),
        unit_per_case = round(units / cases, digits = 1),
        cost_1k_served = round(cost / 1000, digits = 1)
      ) %>%
      group_by(fy) %>%
      mutate(
        cost_pct_tot = round(cost / sum(cost, na.rm = T) * 100, digits = 5),
        pct_cmh_served = round(cases / sum(cases, na.rm = T) * 100, digits = 5)
      ) %>%
      ungroup() 
    
    # Calculating average by selected org_grp    
    st_avg <- df %>%
      group_by(fy) %>%
      summarize(
        cases = mean(cases, na.rm = T),
        units = mean(units, na.rm = T),
        cost = mean(cost, na.rm = T),
        cost_per_case = mean(cost_per_case, na.rm = T),
        cost_per_unit = mean(cost_per_unit, na.rm = T),
        unit_per_case = mean(unit_per_case, na.rm = T),
        cost_1k_served = mean(cost_1k_served, na.rm = T),
        cost_pct_tot = mean(cost_pct_tot, na.rm = T),
        pct_cmh_served = mean(pct_cmh_served, na.rm = T)
      ) %>%
      mutate(
        state = "Average"
      )
    
    if (input$org_type == "PIHP") {
      st_avg %<>% dplyr::rename(org_grp = state)
    } else if (input$org_type == "CMH") {
      st_avg %<>% dplyr::rename(org_grp = state)
    } else print(paste0("Error.  Unrecognized input."))
    
    st_avg <- st_avg[c(1,11,2:10)]
    
    if (input$state_avg == TRUE) {
      df <- rbind(df,st_avg)
    } else df <- df
    
    
    # Relabel display variables
    if (input$x == "Total Cases") {
      df %<>% dplyr::rename(x = cases)
    } else if (input$x == "Total Units") {
      df %<>% dplyr::rename(x = units)
    } else if (input$x == "Total Cost") {
      df %<>% dplyr::rename(x = cost)
    } else if (input$x == "Cost Per Case") {
      df %<>% dplyr::rename(x = cost_per_case)
    } else if (input$x == "Cost Per Unit") {
      df %<>% dplyr::rename(x = cost_per_unit)
    } else if (input$x == "Total Unit Per Case") {
      df %<>% dplyr::rename(x = unit_per_case)
    } else if (input$x == "Cost per 1K Served") {
      df %<>% dplyr::rename(x = cost_1k_served)
    } else if (input$x == "Percent of Total $") {
      df %<>% dplyr::rename(x = cost_pct_tot)
    } else if (input$x == "Percent Served") {
      df %<>% dplyr::rename(x = pct_cmh_served)
    } else print(paste0("Error.  Unrecognized input."))
    
    if (input$y == "Total Cases") {
      df %<>% dplyr::rename(y = cases)
    } else if (input$y == "Total Units") {
      df %<>% dplyr::rename(y = units)
    } else if (input$y == "Total Cost") {
      df %<>% dplyr::rename(y = cost)
    } else if (input$y == "Cost Per Case") {
      df %<>% dplyr::rename(y = cost_per_case)
    } else if (input$y == "Cost Per Unit") {
      df %<>% dplyr::rename(y = cost_per_unit)
    } else if (input$y == "Total Unit Per Case") {
      df %<>% dplyr::rename(y = unit_per_case)
    } else if (input$y == "Cost per 1K Served") {
      df %<>% dplyr::rename(y = cost_1k_served)
    } else if (input$y == "Percent of Total $") {
      df %<>% dplyr::rename(y = cost_pct_tot)
    } else if (input$y == "Percent Served") {
      df %<>% dplyr::rename(y = pct_cmh_served)
    } else print(paste0("Error.  Unrecognized input."))
    
    if (input$z == "Total Cases") {
      df %<>% dplyr::rename(z = cases)
    } else if (input$z == "Total Units") {
      df %<>% dplyr::rename(z = units)
    } else if (input$z == "Total Cost") {
      df %<>% dplyr::rename(z = cost)
    } else if (input$z == "Cost Per Case") {
      df %<>% dplyr::rename(z = cost_per_case)
    } else if (input$z == "Cost Per Unit") {
      df %<>% dplyr::rename(z = cost_per_unit)
    } else if (input$z == "Total Unit Per Case") {
      df %<>% dplyr::rename(z = unit_per_case)
    } else if (input$z == "Cost per 1K Served") {
      df %<>% dplyr::rename(z = cost_1k_served)
    } else if (input$z == "Percent of Total $") {
      df %<>% dplyr::rename(z = cost_pct_tot)
    } else if (input$z == "Percent Served") {
      df %<>% dplyr::rename(z = pct_cmh_served)
    } else print(paste0("Error.  Unrecognized input."))
    
    df %<>% select(fy, org_grp, x, y, z)
    
  })
 
  df_bubble2 <- reactive({
    
    if (input$org_type2 %in% c("PIHP", "CMH")) {
    
    # Relabel selected grouping variable (PIHP/CMH)
      if (input$org_type2 == "PIHP") {
        df <- data404 %>% dplyr::rename(org_grp2 = pihp_name)
      } else if (input$org_type2 == "CMH") {
        df <- data404 %>% dplyr::rename(org_grp2 = cmhsp)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Relabel selected grouping variable (Service Type, Service, HCPCS, Modifier)
      if (input$select_service == "Service Type") {
        df <- df %>% dplyr::rename(svs_grp = svc_type)
      } else if (input$select_service == "Service") {
        df <- df %>% dplyr::rename(svs_grp = svc_grp)
      } else if (input$select_service == "HCPCS Code") {
        df <- df %>% dplyr::rename(svs_grp = Code_shortDesc)
      } else if (input$select_service == "Code Modifier") {
        df <- df %>% dplyr::rename(svs_grp = CodeM_shortDesc)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Filter by population
      pop_filt2 <- if (input$select_population2 == "All") {
        levels(df$population)
      } else input$select_population2
      
      # Aggregating by selected org_grp
      df %<>%
        filter(
          org_grp2 %in% input$org_filt
          & svs_grp %in% input$svslvl_filt
          & population %in% pop_filt2) %>%
        group_by(fy,org_grp2,svs_grp) %>%
        summarize(
          cases = sum(cases, na.rm = T),
          units = sum(units, na.rm = T),
          cost = sum(cost, na.rm = T),
          cost_per_case = round(cost / cases, digits = 2),
          cost_per_unit = round(cost / units, digits = 2),
          unit_per_case = round(units / cases, digits = 1),
          cost_1k_served = round(cost / 1000, digits = 1)) %>%
        ungroup() %>%
        group_by(fy) %>%
        mutate(
          cost_pct_tot = round(units / sum(units, na.rm = T) * 100, digits = 5),
          pct_cmh_served = round(cases / sum(cases, na.rm = T) * 100, digits = 5)
        ) %>%
        ungroup()
      
      
      # Relabel display variables
      if (input$a == "Total Cases") {
        df %<>% dplyr::rename(a = cases)
      } else if (input$a == "Total Units") {
        df %<>% dplyr::rename(a = units)
      } else if (input$a == "Total Cost") {
        df %<>% dplyr::rename(a = cost)
      } else if (input$a == "Cost Per Case") {
        df %<>% dplyr::rename(a = cost_per_case)
      } else if (input$a == "Cost Per Unit") {
        df %<>% dplyr::rename(a = cost_per_unit)
      } else if (input$a == "Total Unit Per Case") {
        df %<>% dplyr::rename(a = unit_per_case)
      } else if (input$a == "Cost per 1K Served") {
        df %<>% dplyr::rename(a = cost_1k_served)
      } else if (input$a == "Percent of Total $") {
        df %<>% dplyr::rename(a = cost_pct_tot)
      } else if (input$a == "Percent Served") {
        df %<>% dplyr::rename(a = pct_cmh_served)
      } else print(paste0("Error.  Unrecognized input."))
      
      if (input$b == "Total Cases") {
        df %<>% dplyr::rename(b = cases)
      } else if (input$b == "Total Units") {
        df %<>% dplyr::rename(b = units)
      } else if (input$b == "Total Cost") {
        df %<>% dplyr::rename(b = cost)
      } else if (input$b == "Cost Per Case") {
        df %<>% dplyr::rename(b = cost_per_case)
      } else if (input$b == "Cost Per Unit") {
        df %<>% dplyr::rename(b = cost_per_unit)
      } else if (input$b == "Total Unit Per Case") {
        df %<>% dplyr::rename(b = unit_per_case)
      } else if (input$b == "Cost per 1K Served") {
        df %<>% dplyr::rename(b = cost_1k_served)
      } else if (input$b == "Percent of Total $") {
        df %<>% dplyr::rename(b = cost_pct_tot)
      } else if (input$b == "Percent Served") {
        df %<>% dplyr::rename(b = pct_cmh_served)
      } else print(paste0("Error.  Unrecognized input."))
      
      if (input$c == "Total Cases") {
        df %<>% dplyr::rename(c = cases)
      } else if (input$c == "Total Units") {
        df %<>% dplyr::rename(c = units)
      } else if (input$c == "Total Cost") {
        df %<>% dplyr::rename(c = cost)
      } else if (input$c == "Cost Per Case") {
        df %<>% dplyr::rename(c = cost_per_case)
      } else if (input$c == "Cost Per Unit") {
        df %<>% dplyr::rename(c = cost_per_unit)
      } else if (input$c == "Total Unit Per Case") {
        df %<>% dplyr::rename(c = unit_per_case)
      } else if (input$c == "Cost per 1K Served") {
        df %<>% dplyr::rename(c = cost_1k_served)
      } else if (input$c == "Percent of Total $") {
        df %<>% dplyr::rename(c = cost_pct_tot)
      } else if (input$c == "Percent Served") {
        df %<>% dplyr::rename(c = pct_cmh_served)
      } else print(paste0("Error.  Unrecognized input."))
      
      df %<>% select(fy, org_grp2, svs_grp, a, b, c)
      
    }
    
    else if (input$org_type2 == "State of MI") {

      # Relabel selected grouping variable (Service Type, Service, HCPCS, Modifier)
      if (input$select_service == "Service Type") {
        df <- data404 %>% dplyr::rename(svs_grp = svc_type)
      } else if (input$select_service == "Service") {
        df <- data404 %>% dplyr::rename(svs_grp = Service)
      } else if (input$select_service == "HCPCS Code") {
        df <- data404 %>% dplyr::rename(svs_grp = Code_shortDesc)
      } else if (input$select_service == "Code Modifier") {
        df <- data404 %>% dplyr::rename(svs_grp = CodeM_shortDesc)
      } else print(paste0("Error.  Unrecognized input."))

      # Filter by population
      pop_filt2 <- if (input$select_population2 == "All") {
        levels(df$population)
      } else input$select_population2

      # Aggregating by selected svs_grp
      df %<>%
        filter(
          svs_grp %in% input$svslvl_filt
          & population %in% pop_filt2) %>%
        group_by(fy,svs_grp) %>%
        summarize(
          cases = sum(cases, na.rm = T),
          units = sum(units, na.rm = T),
          cost = sum(cost, na.rm = T),
          cost_per_case = round(cost / cases, digits = 2),
          cost_per_unit = round(cost / units, digits = 2),
          unit_per_case = round(units / cases, digits = 1),
          cost_1k_served = round(cost / 1000, digits = 1)) %>%
        ungroup() %>%
        group_by(fy) %>%
        mutate(
          cost_pct_tot = round(units / sum(units, na.rm = T) * 100, digits = 5),
          pct_cmh_served = round(cases / sum(cases, na.rm = T) * 100, digits = 5)
        ) %>%
        ungroup()


      # Relabel display variables
      if (input$a == "Total Cases") {
        df %<>% dplyr::rename(a = cases)
      } else if (input$a == "Total Units") {
        df %<>% dplyr::rename(a = units)
      } else if (input$a == "Total Cost") {
        df %<>% dplyr::rename(a = cost)
      } else if (input$a == "Cost Per Case") {
        df %<>% dplyr::rename(a = cost_per_case)
      } else if (input$a == "Cost Per Unit") {
        df %<>% dplyr::rename(a = cost_per_unit)
      } else if (input$a == "Total Unit Per Case") {
        df %<>% dplyr::rename(a = unit_per_case)
      } else if (input$a == "Cost per 1K Served") {
        df %<>% dplyr::rename(a = cost_1k_served)
      } else if (input$a == "Percent of Total $") {
        df %<>% dplyr::rename(a = cost_pct_tot)
      } else if (input$a == "Percent Served") {
        df %<>% dplyr::rename(a = pct_cmh_served)
      } else print(paste0("Error.  Unrecognized input."))

      if (input$b == "Total Cases") {
        df %<>% dplyr::rename(b = cases)
      } else if (input$b == "Total Units") {
        df %<>% dplyr::rename(b = units)
      } else if (input$b == "Total Cost") {
        df %<>% dplyr::rename(b = cost)
      } else if (input$b == "Cost Per Case") {
        df %<>% dplyr::rename(b = cost_per_case)
      } else if (input$b == "Cost Per Unit") {
        df %<>% dplyr::rename(b = cost_per_unit)
      } else if (input$b == "Total Unit Per Case") {
        df %<>% dplyr::rename(b = unit_per_case)
      } else if (input$b == "Cost per 1K Served") {
        df %<>% dplyr::rename(b = cost_1k_served)
      } else if (input$b == "Percent of Total $") {
        df %<>% dplyr::rename(b = cost_pct_tot)
      } else if (input$b == "Percent Served") {
        df %<>% dplyr::rename(b = pct_cmh_served)
      } else print(paste0("Error.  Unrecognized input."))

      if (input$c == "Total Cases") {
        df %<>% dplyr::rename(c = cases)
      } else if (input$c == "Total Units") {
        df %<>% dplyr::rename(c = units)
      } else if (input$c == "Total Cost") {
        df %<>% dplyr::rename(c = cost)
      } else if (input$c == "Cost Per Case") {
        df %<>% dplyr::rename(c = cost_per_case)
      } else if (input$c == "Cost Per Unit") {
        df %<>% dplyr::rename(c = cost_per_unit)
      } else if (input$c == "Total Unit Per Case") {
        df %<>% dplyr::rename(c = unit_per_case)
      } else if (input$c == "Cost per 1K Served") {
        df %<>% dplyr::rename(c = cost_1k_served)
      } else if (input$c == "Percent of Total $") {
        df %<>% dplyr::rename(c = cost_pct_tot)
      } else if (input$c == "Percent Served") {
        df %<>% dplyr::rename(c = pct_cmh_served)
      } else print(paste0("Error.  Unrecognized input."))

      df %<>% select(fy, svs_grp, a, b, c)

    }
    
  })
  
  #### Filters ####
  
  output$select_code <- renderUI({
    
    hcpcs <- if (input$select_svc_type == "All") {
      levels(as.factor(data404$short_desc))
    } else
      levels(as.factor(data404$short_desc[data404$svc_type == input$select_svc_type]))
    
    selectInput(
      "select_code",
      label = tags$p("Select a Code:", style = "font-size: 115%;"),
      choices = c("","All",hcpcs),
      selected = ""
    )
    
  })
  
  output$x <- renderUI({
    
    x <- if (input$select_code == "All") {
      names(inputs_sub)
    } else names(inputs)
    
    selectInput(
      "x",
      label = tags$p("I'm interested in comparing"
                     , style = "font-size: 115%"),
      choices = c("",x),
      selected = ""
    )
    
  })
  
  output$y <- renderUI({
    
    y <- if (input$select_code == "All") {
      names(inputs_sub)
    } else names(inputs[,!(names(inputs) %in% (input$x))])
    
    selectInput(
      "y",
      label = tags$p("and"
                     , style = "font-size: 115%"),
      choices = c("",y),
      selected = ""
    )
    
  })
  
  output$z <- renderUI({
    
    z <- if (input$select_code == "All") {
      names(inputs_sub)
    } else names(inputs[,!(names(inputs) %in% c(input$x, input$y))])
    
    selectInput(
      "z",
      label = tags$p("Select a variable to scale the size of each bubble:"
                     , style = "font-size: 115%;"),
      choices = c("",z),
      selected = ""
    )
    
  })
  
  output$a <- renderUI({
    
    a <- if (input$select_service == "Service Type" |
             input$select_service == "Service") {
      names(inputs_sub)
    } else names(inputs)
    
    selectInput(
      "a",
      label = tags$p("I'm interested in comparing"
                     , style = "font-size: 115%"),
      choices = c("",a),
      selected = ("")
    )
    
  })
  
  output$b <- renderUI({
    
    b <- if (input$select_service == "Service Type" |
             input$select_service == "Service") {
      names(inputs_sub)
    } else names(inputs[,!(names(inputs) %in% (input$a))])
    
    selectInput(
      "b",
      label = tags$p("and"
                     , style = "font-size: 115%"),
      choices = c("",b),
      selected = ("")
    )
    
  })
  
  output$c <- renderUI({
    
    c <- if (input$select_service == "Service Type" |
             input$select_service == "Service") {
      names(inputs_sub)
    } else names(inputs[,!(names(inputs) %in% c(input$a, input$b))])
    
    selectInput(
      "c",
      label = tags$p("Select a variable to scale the size of each bubble:"
                     , style = "font-size: 115%;"),
      choices = c("",c),
      selected = ("")
    )
    
  })
  
  output$svslvl_filt <- renderUI({
    
    svslvl_filt <- if (input$select_service=="Service Type"){
      levels(as.factor(data404$svc_type))
    } else if (input$select_service=="Service"){
      levels(as.factor(data404$svc_grp))
    } else if (input$select_service=="HCPCS Code"){
      levels(as.factor(data404$Code_shortDesc))
    } else if (input$select_service=="Code Modifier"){
      levels(as.factor(data404$CodeM_shortDesc))
    } else print(paste0("Error.  Unrecognized input."))
    
    selectInput(
      "svslvl_filt",
      label = tags$p("Which service(s) would you like to see data for?"
                     , style = "font-size: 115%;"),
      choices = c("",svslvl_filt),
      selected = "",
      multiple = TRUE
    )
    
  })
  
  output$org_filt <- renderUI({
    
    if (input$org_type2 %in% c("PIHP", "CMH")) {
    
    org_filt <- if (input$org_type2 == "PIHP") {
      levels(unique(data404$pihp_name))
    } else if (input$org_type2 == "CMH") {
      levels(unique(data404$cmhsp))
    } else print(paste0("Error.  Unrecognized input."))

    selectInput(
      "org_filt",
      label = tags$p("Select PIHP/CMH:"
                     , style = "font-size: 115%;"),
      choices = c("",org_filt),
      selected = "",
      multiple = FALSE
    )
    
    } else if (input$org_type2 == "State of MI") {
      
      br()
      
    }
    
  })
  
  #### Visualizations ####
  
  output$bubble1 <- renderPlotly({
    
    withProgress(
      message = 'Creating Visualization...',
      detail = 'Processing...',
      value = 0.1,
      {
        
        # Grab max values from x and y vars
        max_x <- max(df_bubble1()$x, na.rm = T)+max(df_bubble1()$x*.1)
        max_y <- max(df_bubble1()$y, na.rm = T)+max(df_bubble1()$y*.1)
        
        # Ignore sizing variable
        if (input$ignore_z == TRUE) {
          
          df_bubble1() %>%
            filter(fy == input$sliderfy) %>%
            plot_ly(
              x = ~x, y = ~y, type = 'scatter', mode = 'markers', 
              size = 100, color = ~org_grp, colors = cmh_palette, marker = list(opacity = 0.5),
              hoverinfo = 'text',
              text = ~paste(
                org_grp,
                '<br>',input$x,':',
                if(grepl("Cost",input$x)) {
                  dollar_format(big.mark = ",")(x)
                } else if (grepl("Percent", input$x)) {
                  sprintf("%.1f %%",x)
                } else format(x, big.mark = ','),
                '<br>',input$y,':',
                if(grepl("Cost",input$y)) {
                  dollar_format(big.mark = ",")(y)
                } else if (grepl("Percent", input$y)) {
                  sprintf("%.1f %%",y)
                } else format(y, big.mark = ',')
              )
            ) %>%
            layout(
              title = if(input$select_code == "All") {
                ~paste('How does',input$x,'compare to',input$y,'for<br>',
                       input$select_svc_type,'across',input$org_type,"'s",'?<br>',
                       'Fiscal Year:', input$sliderfy)
              } else ~paste ('How does',input$x,'compare to',input$y,'for<br>',
                             input$select_code,'across',input$org_type,"'s",'?<br>',
                             'Fiscal Year:', input$sliderfy),
              xaxis = list(
                title = input$x,
                range = c(0, max_x),
                showgrid = FALSE
              ),
              yaxis = list(
                title = input$y,
                range = c(0, max_y),
                showgrid = FALSE
              ),
              showlegend = FALSE
            )
          
        } else
          
          df_bubble1() %>%
          filter(fy == input$sliderfy) %>%
          plot_ly(
            x = ~x, y = ~y, type = 'scatter', mode = 'markers', 
            size = ~z, color = ~org_grp, colors = cmh_palette, marker = list(opacity = 0.5),
            hoverinfo = 'text',
            text = ~paste(
              org_grp,
              '<br>',input$x,':',
              if(grepl("Cost",input$x)) {
                dollar_format(big.mark = ",")(x)
              } else if (grepl("Percent", input$x)) {
                sprintf("%.1f %%",x)
              } else format(x, big.mark = ','),
              '<br>',input$y,':',
              if(grepl("Cost",input$y)) {
                dollar_format(big.mark = ",")(y)
              } else if (grepl("Percent", input$y)) {
                sprintf("%.1f %%",y)
              } else format(y, big.mark = ','),
              '<br>',input$z,':',
              if(grepl("Cost",input$z)) {
                dollar_format(big.mark = ",")(z)
              } else if (grepl("Percent", input$z)) {
                sprintf("%.1f %%",z)
              } else format(z, big.mark = ',')
            )
          ) %>%
          layout(
            title = if(input$select_code == "All") {
              ~paste('How does',input$x,'compare to',input$y,'for<br>',
                     input$select_svc_type,'across',input$org_type,"'s",'?<br>',
                     'Fiscal Year:', input$sliderfy)
            } else ~paste ('How does',input$x,'compare to',input$y,'for<br>',
                           input$select_code,'across',input$org_type,"'s",'?<br>',
                           'Fiscal Year:', input$sliderfy),
            xaxis = list(
              title = input$x,
              range = c(0, max_x),
              showgrid = FALSE
            ),
            yaxis = list(
              title = input$y,
              range = c(0, max_y),
              showgrid = FALSE
            ),
            showlegend = FALSE
          )
        
      })
    
  })
  
  output$bubble2 <- renderPlotly({
    
    withProgress(
      message = 'Creating Visualization...',
      detail = 'Processing...',
      value = 0.1,
      {
        
        # Grab max values from x and y vars
        max_a <- max(df_bubble2()$a, na.rm = T)+max(df_bubble2()$a*.1)
        max_b <- max(df_bubble2()$b, na.rm = T)+max(df_bubble2()$b*.1)

        # Ignore sizing variable
        if (input$ignore_c == TRUE) {
          
          df_bubble2() %>%
            filter(fy == input$sliderfy1) %>%
            plot_ly(
              x = ~a, y = ~b, type = 'scatter', mode = 'markers', size = 100,
              color = ~svs_grp, colors = cmh_palette, marker = list(opacity = 0.5),
              hoverinfo = 'text',
              text = ~paste(
                svs_grp,
                '<br>',input$a,':',
                if(grepl("Cost",input$a)) {
                  dollar_format(big.mark = ",")(a)
                } else if (grepl("Percent", input$a)) {
                  sprintf("%.1f %%",a)
                } else format(a, big.mark = ','),
                '<br>',input$b,':',
                if(grepl("Cost",input$b)) {
                  dollar_format(big.mark = ",")(b)
                } else if (grepl("Percent", input$b)) {
                  sprintf("%.1f %%",b)
                } else format(b, big.mark = ',')
              )
            ) %>%
            layout(
              title = if(input$org_type2 %in% c('PIHP','CMH')) {
                ~paste('How does',input$a,'compare to',input$b,'for<br>','the selected',
                       input$select_service,'at',input$org_filt, '?<br>',
                       'Fiscal Year:',input$sliderfy1)
                } else ~paste('How does',input$a,'compare to',input$b,'for<br>',
                              'the selected',input$select_service,
                                  'for the State of MI?<br>','Fiscal Year:',input$sliderfy1),
              xaxis = list(
                title = input$a,
                range = c(0, max_a),
                showgrid = FALSE
              ),
              yaxis = list(
                title = input$b,
                range = c(0, max_b),
                showgrid = FALSE
              ),
              showlegend = FALSE
            )
          
        } else
          
          df_bubble2() %>%
          filter(fy == input$sliderfy1) %>%
          plot_ly(
            x = ~a, y = ~b, type = 'scatter', mode = 'markers',
            size = ~c, color = ~svs_grp,
            colors = cmh_palette, marker = list(opacity = 0.5),
            hoverinfo = 'text',
            text = ~paste(
              svs_grp,
              '<br>',input$a,':',
              if(grepl("Cost",input$a)) {
                dollar_format(big.mark = ",")(a)
              } else if (grepl("Percent", input$a)) {
                sprintf("%.1f %%",a)
              } else format(a, big.mark = ','),
              '<br>',input$b,':',
              if(grepl("Cost",input$b)) {
                dollar_format(big.mark = ",")(b)
              } else if (grepl("Percent", input$b)) {
                sprintf("%.1f %%",b)
              } else format(b, big.mark = ','),
              '<br>',input$c,':',
              if(grepl("Cost",input$c)) {
                dollar_format(big.mark = ",")(c)
              } else if (grepl("Percent", input$c)) {
                sprintf("%.1f %%",c)
              } else format(c, big.mark = ',')
            )
          ) %>%
          layout(
            title = if(input$org_type2 %in% c('PIHP','CMH')) {
              ~paste('How does',input$a,'compare to',input$b,'for<br>','the selected',
                     input$select_service,'at',input$org_filt, '?<br>',
                     'Fiscal Year:',input$sliderfy1)
            } else ~paste('How does',input$a,'compare to',input$b,'for<br>',
                          'the selected',input$select_service,
                          'for the State of MI?<br>','Fiscal Year:',input$sliderfy1),
            xaxis = list(
              title = input$a,
              range = c(0, max_a),
              showgrid = FALSE
            ),
            yaxis = list(
              title = input$b,
              range = c(0, max_b),
              showgrid = FALSE
            ),
            showlegend = FALSE
          )

      })
    
  })
  
  output$svs_groups <- renderDataTable({
    
    df <- service_groups %>%
      dplyr::rename(
        "Service Type" = svc_type,
        "Short Description" = short_description,
        "Long Description" = Description,
        "Code Modifier" = Code_Mod
      )
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data404_", Sys.Date(),".csv",sep="")
    },
    content = function(file) {
      write.csv(data404, file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("ServiceGroups_", Sys.Date(),".csv",sep="")
    },
    content = function(file) {
      write.csv(service_groups, file)
    }
  )
  
})