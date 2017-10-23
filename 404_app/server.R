# server.R #

shinyServer(function(input, output) {
  
  #### Make Reactive Datasets ####
  
  df_bubble1 <- reactive({
    
    # Filter by PIHP
    pihp_filt <- if (input$filter_pihp == "All") {
      data404 <- data404
    } else df <- data404 %<>% filter(PIHPname %in% input$filter_pihp)
    
    # Relabel selected grouping variable
    if (input$org_type == "PIHP") {
      df <- data404 %>% rename(org_grp = PIHPname)
    } else if (input$org_type == "CMH") {
      df <- data404 %>% rename(org_grp = CMHSP)
    } else print(paste0("Error.  Unrecognized input."))

    # Filter by Service Type
    servicetype_filt <- if (input$select_ServiceType == "All") {
      levels(df$ServiceType)
    } else input$select_ServiceType
    
    # Filterby HCPCS Code
    code_filt <- if (input$select_code == "All") {
      levels(df$Code_Desc)
    } else input$select_code
    
    # Filter by Population
    pop_filt <- if (input$select_Population == "All") {
      levels(df$Population)
    } else input$select_Population
    
    # Aggregating by selected org_grp
    df %<>%
      filter(ServiceType %in% servicetype_filt
             & Population %in% pop_filt
             & Code_Desc %in% code_filt
             ) %>%
      group_by(FY,org_grp) %>%
      summarize(
        SumOfCases = sum(SumOfCases, na.rm = T),
        SumOfUnits = sum(SumOfUnits, na.rm = T),
        SumOfCost = sum(SumOfCost, na.rm = T),
        CostPerCase = round(SumOfCost / SumOfCases, digits = 2),
        CostPerUnit = round(SumOfCost / SumOfUnits, digits = 2),
        UnitPerCase = round(SumOfUnits / SumOfCases, digits = 1),
        Cost1kSvd = round(SumOfCost / 1000, digits = 1)) %>%
      ungroup() %>%
      group_by(FY) %>%
      mutate(
        Cost_Perc_Tot = round(SumOfUnits / sum(SumOfUnits, na.rm = T) * 100, digits = 1),
        Perc_Svd = round(SumOfCases / sum(SumOfCases, na.rm = T) * 100, digits = 1)
      ) %>%
      ungroup() 
    
    # Calculating state average by selected org_grp    
    st_avg <- df %>%
      group_by(FY) %>%
      summarize(
        SumOfCases = mean(SumOfCases),
        SumOfUnits = mean(SumOfUnits),
        SumOfCost = mean(SumOfCost),
        CostPerCase = mean(CostPerCase),
        CostPerUnit = mean(CostPerUnit),
        UnitPerCase = mean(UnitPerCase),
        Cost1kSvd = mean(Cost1kSvd),
        Cost_Perc_Tot = mean(Cost_Perc_Tot),
        Perc_Svd = mean(Perc_Svd)
      ) %>%
      mutate(
        state = "State Average"
      )
    
    if (input$org_type == "PIHP") {
      st_avg %<>% rename(org_grp = state)
    } else if (input$org_type == "CMH") {
      st_avg %<>% rename(org_grp = state)
    } else print(paste0("Error.  Unrecognized input."))

    st_avg <- st_avg[c(1,11,2:10)]

    if (input$state_avg == TRUE) {
      df <- rbind(df,st_avg)
    } else df <- df
    
    
    # Relabel display variables
    if (input$x == "Total Cases") {
      df %<>% rename(x = SumOfCases)
    } else if (input$x == "Total Units") {
      df %<>% rename(x = SumOfUnits)
    } else if (input$x == "Total Cost") {
      df %<>% rename(x = SumOfCost)
    } else if (input$x == "Cost Per Case") {
      df %<>% rename(x = CostPerCase)
    } else if (input$x == "Cost Per Unit") {
      df %<>% rename(x = CostPerUnit)
    } else if (input$x == "Total Unit Per Case") {
      df %<>% rename(x = UnitPerCase)
    } else if (input$x == "Cost per 1K Served") {
      df %<>% rename(x = Cost1kSvd)
    } else if (input$x == "Percent of Total $") {
      df %<>% rename(x = Cost_Perc_Tot)
    } else if (input$x == "Percent Served") {
      df %<>% rename(x = Perc_Svd)
    } else print(paste0("Error.  Unrecognized input."))
    
    if (input$y == "Total Cases") {
      df %<>% rename(y = SumOfCases)
    } else if (input$y == "Total Units") {
      df %<>% rename(y = SumOfUnits)
    } else if (input$y == "Total Cost") {
      df %<>% rename(y = SumOfCost)
    } else if (input$y == "Cost Per Case") {
      df %<>% rename(y = CostPerCase)
    } else if (input$y == "Cost Per Unit") {
      df %<>% rename(y = CostPerUnit)
    } else if (input$y == "Total Unit Per Case") {
      df %<>% rename(y = UnitPerCase)
    } else if (input$y == "Cost per 1K Served") {
      df %<>% rename(y = Cost1kSvd)
    } else if (input$y == "Percent of Total $") {
      df %<>% rename(y = Cost_Perc_Tot)
    } else if (input$y == "Percent Served") {
      df %<>% rename(y = Perc_Svd)
    } else print(paste0("Error.  Unrecognized input."))
    
    if (input$z == "Total Cases") {
      df %<>% rename(z = SumOfCases)
    } else if (input$z == "Total Units") {
      df %<>% rename(z = SumOfUnits)
    } else if (input$z == "Total Cost") {
      df %<>% rename(z = SumOfCost)
    } else if (input$z == "Cost Per Case") {
      df %<>% rename(z = CostPerCase)
    } else if (input$z == "Cost Per Unit") {
      df %<>% rename(z = CostPerUnit)
    } else if (input$z == "Total Unit Per Case") {
      df %<>% rename(z = UnitPerCase)
    } else if (input$z == "Cost per 1K Served") {
      df %<>% rename(z = Cost1kSvd)
    } else if (input$z == "Percent of Total $") {
      df %<>% rename(z = Cost_Perc_Tot)
    } else if (input$z == "Percent Served") {
      df %<>% rename(z = Perc_Svd)
    } else print(paste0("Error.  Unrecognized input."))
    
    df %<>% select(FY, org_grp, x, y, z)
    
  })
  
  
  df_bubble2 <- reactive({
    
    # Relabel selected grouping variable
    if (input$org_type2 == "PIHP") {
      df <- data404 %>% rename(org_grp2 = PIHPname)
    } else if (input$org_type2 == "CMH") {
      df <- data404 %>% rename(org_grp2 = CMHSP)
    } else print(paste0("Error.  Unrecognized input."))
    
    # Filter by Population
    pop_filt2 <- if (input$select_Population2 == "All") {
      levels(df$Population)
    } else input$select_Population2

    # Aggregating by selected org_grp
    df %<>%
      filter(
        org_grp2 %in% input$org_filt
        & ServiceType == input$select_ServiceType2
        & Population %in% pop_filt2
        & Code_Desc %in% input$select_code2) %>%
      group_by(FY,org_grp2,ServiceType,Code_Desc) %>%
      summarize(
        SumOfCases = sum(SumOfCases, na.rm = T),
        SumOfUnits = sum(SumOfUnits, na.rm = T),
        SumOfCost = sum(SumOfCost, na.rm = T),
        CostPerCase = round(SumOfCost / SumOfCases, digits = 2),
        CostPerUnit = round(SumOfCost / SumOfUnits, digits = 2),
        UnitPerCase = round(SumOfUnits / SumOfCases, digits = 1),
        Cost1kSvd = round(SumOfCost / 1000, digits = 1)) %>%
      ungroup() %>%
      group_by(FY) %>%
      mutate(
        Cost_Perc_Tot = round(SumOfUnits / sum(SumOfUnits, na.rm = T) * 100, digits = 5),
        Perc_Svd = round(SumOfCases / sum(SumOfCases, na.rm = T) * 100, digits = 5)
      ) %>%
      ungroup()


    # Relabel display variables
    if (input$a == "Total Cases") {
      df %<>% rename(a = SumOfCases)
    } else if (input$a == "Total Units") {
      df %<>% rename(a = SumOfUnits)
    } else if (input$a == "Total Cost") {
      df %<>% rename(a = SumOfCost)
    } else if (input$a == "Cost Per Case") {
      df %<>% rename(a = CostPerCase)
    } else if (input$a == "Cost Per Unit") {
      df %<>% rename(a = CostPerUnit)
    } else if (input$a == "Total Unit Per Case") {
      df %<>% rename(a = UnitPerCase)
    } else if (input$a == "Cost per 1K Served") {
      df %<>% rename(a = Cost1kSvd)
    } else if (input$a == "Percent of Total $") {
      df %<>% rename(a = Cost_Perc_Tot)
    } else if (input$a == "Percent Served") {
      df %<>% rename(a = Perc_Svd)
    } else print(paste0("Error.  Unrecognized input."))

    if (input$b == "Total Cases") {
      df %<>% rename(b = SumOfCases)
    } else if (input$b == "Total Units") {
      df %<>% rename(b = SumOfUnits)
    } else if (input$b == "Total Cost") {
      df %<>% rename(b = SumOfCost)
    } else if (input$b == "Cost Per Case") {
      df %<>% rename(b = CostPerCase)
    } else if (input$b == "Cost Per Unit") {
      df %<>% rename(b = CostPerUnit)
    } else if (input$b == "Total Unit Per Case") {
      df %<>% rename(b = UnitPerCase)
    } else if (input$b == "Cost per 1K Served") {
      df %<>% rename(b = Cost1kSvd)
    } else if (input$b == "Percent of Total $") {
      df %<>% rename(b = Cost_Perc_Tot)
    } else if (input$b == "Percent Served") {
      df %<>% rename(b = Perc_Svd)
    } else print(paste0("Error.  Unrecognized input."))

    if (input$c == "Total Cases") {
      df %<>% rename(c = SumOfCases)
    } else if (input$c == "Total Units") {
      df %<>% rename(c = SumOfUnits)
    } else if (input$c == "Total Cost") {
      df %<>% rename(c = SumOfCost)
    } else if (input$c == "Cost Per Case") {
      df %<>% rename(c = CostPerCase)
    } else if (input$c == "Cost Per Unit") {
      df %<>% rename(c = CostPerUnit)
    } else if (input$c == "Total Unit Per Case") {
      df %<>% rename(c = UnitPerCase)
    } else if (input$c == "Cost per 1K Served") {
      df %<>% rename(c = Cost1kSvd)
    } else if (input$c == "Percent of Total $") {
      df %<>% rename(c = Cost_Perc_Tot)
    } else if (input$c == "Percent Served") {
      df %<>% rename(c = Perc_Svd)
    } else print(paste0("Error.  Unrecognized input."))

    df %<>% select(FY, org_grp2, ServiceType, Code_Desc, a, b, c)

  })

  
  #### Filters ####
  
  output$select_code <- renderUI({
    
    hcpcs <- if (input$select_ServiceType == "All") {
      levels(data404$Code_Desc)
    } else
      levels(droplevels(data404$Code_Desc[data404$ServiceType == input$select_ServiceType]))
    
    selectInput(
      "select_code",
      label = tags$p("Select a Code:", style = "font-size: 115%;"),
      choices = c("All",hcpcs),
      selected = ("Community Living Supports (15 Minutes)  ( H2015 )")
    )
    
  })
  
  output$select_code2 <- renderUI({
    
    hcpcs2 <- levels(droplevels(data404$Code_Desc[data404$ServiceType == input$select_ServiceType2]))
    
    selectInput(
      "select_code2",
      label = tags$p("Select Code(s):", style = "font-size: 115%;"),
      choices = (hcpcs2),
      selected = hcpcs2,
      multiple = TRUE
    )
    
  })
  
  output$x <- renderUI({
    
    x <- if (input$select_code == "All") {
      names(inputs_sub)
    } else names(inputs[,!(names(inputs) %in% c(input$y, input$z))])
    
    selectInput(
      "x",
      label = tags$p("Select a variable for the x-axis (horizontal):"
                     , style = "font-size: 115%;"),
      choices = (x),
      selected = ("Total Cost")
    )
    
  })
  
  output$y <- renderUI({
    
    y <- if (input$select_code == "All") {
      names(inputs_sub)
    } else names(inputs[,!(names(inputs) %in% c(input$x, input$z))])
    
    selectInput(
      "y",
      label = tags$p("Select a variable for the y-axis (vertical):"
                     , style = "font-size: 115%;"),
      choices = (y),
      selected = ("Percent of Total $")
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
      choices = (z),
      selected = ("Cost per 1K Served")
    )
    
  })
  
  output$a <- renderUI({
    
    a <- if (input$select_code == "All") {
      names(inputs_sub)
    } else names(inputs[,!(names(inputs) %in% c(input$b, input$c))])
    
    selectInput(
      "a",
      label = tags$p("Select a variable for the x-axis (horizontal):"
                     , style = "font-size: 115%;"),
      choices = (a),
      selected = ("Total Cost")
    )
    
  })
  
  output$b <- renderUI({
    
    b <- if (input$select_code == "All") {
      names(inputs_sub)
    } else names(inputs[,!(names(inputs) %in% c(input$a, input$c))])
    
    selectInput(
      "b",
      label = tags$p("Select a variable for the y-axis (vertical):"
                     , style = "font-size: 115%;"),
      choices = (b),
      selected = ("Percent of Total $")
    )
    
  })
  
  output$c <- renderUI({
    
    c <- if (input$select_code == "All") {
      names(inputs_sub)
    } else names(inputs[,!(names(inputs) %in% c(input$a, input$b))])
    
    selectInput(
      "c",
      label = tags$p("Select a variable to scale the size of each bubble:"
                     , style = "font-size: 115%;"),
      choices = (c),
      selected = ("Cost per 1K Served")
    )
    
  })
  
  output$org_filt <- renderUI({
    
    org_filt <- if (input$org_type2 == "PIHP") {
      levels(unique(data404$PIHPname))
    } else if (input$org_type2 == "CMH") {
      levels(unique(data404$CMHSP))
    } else print(paste0("Error.  Unrecognized input."))
    
    selectInput(
      "org_filt",
      label = tags$p("Select PIHP(s)/CMH(s):"
                     , style = "font-size: 115%;"),
      choices = (org_filt),
      selected = if(input$org_type2 == "PIHP") {
        levels(unique(data404$PIHPname))
      } else if(input$org_type2 == "CMH") {
        levels(unique(data404$CMHSP))
      } else print(paste0("Error.  Unrecognized input.")),
      multiple = TRUE
    )
    
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
      filter(FY == input$sliderFY) %>%
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
                   input$select_ServiceType,'across',input$org_type,"'s",'?<br>',
                   'Fiscal Year:', input$sliderFY)
          } else ~paste ('How does',input$x,'compare to',input$y,'for<br>',
                         input$select_code,'across',input$org_type,"'s",'?<br>',
                         'Fiscal Year:', input$sliderFY),
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
        filter(FY == input$sliderFY) %>%
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
                     input$select_ServiceType,'across',input$org_type,"'s",'?<br>',
                     'Fiscal Year:', input$sliderFY)
            } else ~paste ('How does',input$x,'compare to',input$y,'for<br>',
                           input$select_code,'across',input$org_type,"'s",'?<br>',
                           'Fiscal Year:', input$sliderFY),
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
        filter(FY == input$sliderFY1) %>%
        plot_ly(
          x = ~a, y = ~b, type = 'scatter', mode = 'markers',
          color = ~org_grp2, colors = cmh_palette, marker = list(opacity = 0.5),
          hoverinfo = 'text',
          text = ~paste(
            org_grp2,
            '<br>',Code_Desc,
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
          title = ~paste('How does',input$a,'compare to',input$b,'for<br>',
                         input$select_ServiceType2,'service codes at the selected PIHP(s)/CMH(s)?<br>',
                         'Fiscal Year:', input$sliderFY1),
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
        filter(FY == input$sliderFY1) %>%
        plot_ly(
          x = ~a, y = ~b, type = 'scatter', mode = 'markers',
          size = ~c, color = ~org_grp2, colors = cmh_palette, marker = list(opacity = 0.5),
          hoverinfo = 'text',
          text = ~paste(
            org_grp2,
            '<br>',Code_Desc,
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
          title = ~paste('How does',input$a,'compare to',input$b,'for<br>',
                   input$select_ServiceType2,'service codes at the selected PIHP(s)/CMH(s)?<br>',
                   'Fiscal Year:', input$sliderFY1),
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
      rename(
        "Service Type" = ServiceType,
        "Short Description" = short_description,
        "Long Description" = Description,
        "Code Modifier" = Code_Mod
      )
    
  })
  
  })