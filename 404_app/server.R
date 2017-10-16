# server.R #

shinyServer(function(input, output) {
  
  #### Make Reactive Datasets ####
  
  df_bubble <- reactive({
    
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
    # st_avg <- df %>%
    #   group_by(FY) %>%
    #   summarize(
    #     SumOfCases = mean(SumOfCases),
    #     SumOfUnits = mean(SumOfUnits),
    #     SumOfCost = mean(SumOfCost),
    #     CostPerCase = mean(CostPerCase),
    #     CostPerUnit = mean(CostPerUnit),
    #     UnitPerCase = mean(UnitPerCase),
    #     Cost1kSvd = mean(Cost1kSvd),
    #     Cost_Perc_Tot = mean(Cost_Perc_Tot),
    #     Perc_Svd = mean(Perc_Svd)
    #   ) %>%
    #   mutate(org_grp = "State Average")
    
    # if (input$org_type == "PIHP") {st_avg %<>% rename(org_grp = PIHPname)}
    # else if (input$org_type == "CMH") {st_avg %<>% rename(org_grp = CMHSP)}
    # 
    # st_avg[c(1,11,2:10)]
    # 
    # if (input$state_avg == TRUE) {
    #   df <- rbind(df,st_avg)
    # } else df <- df
    
    
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
      selected = ("All")
    )
    
  })
  
  output$x <- renderUI({
    
    x <- if (input$select_code == "All") {
      c("Total Cost","Cost per 1K Served","Percent of Total $")
    } else c("Total Cases","Total Units","Total Cost","Cost Per Case"
             ,"Cost Per Unit","Total Unit Per Case","Cost per 1K Served"
             ,"Percent of Total $","Percent Served")
    
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
      c("Total Cost","Cost per 1K Served","Percent of Total $")
    } else c("Total Cases","Total Units","Total Cost","Cost Per Case"
             ,"Cost Per Unit","Total Unit Per Case","Cost per 1K Served"
             ,"Percent of Total $","Percent Served")
    
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
      c("Total Cost","Cost per 1K Served","Percent of Total $")
    } else c("Total Cases","Total Units","Total Cost","Cost Per Case"
             ,"Cost Per Unit","Total Unit Per Case","Cost per 1K Served"
             ,"Percent of Total $","Percent Served")
    
    selectInput(
      "z",
      label = tags$p("Select a variable to scale the size of each bubble:"
                     , style = "font-size: 115%;"),
      choices = (z),
      selected = ("Cost per 1K Served")
    )
    
  })

  
  #### Visualizations ####
  
  output$bubble <- renderPlotly({
    
    # Grab max values from x and y vars
    max_x <- max(df_bubble()$x, na.rm = T)+max(df_bubble()$x*.1)
    max_y <- max(df_bubble()$y, na.rm = T)+max(df_bubble()$y*.1)
    
    # Exclude potential data issues
    if (input$ignore_z == TRUE) {
      
    df_bubble() %>%
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
      
      df_bubble() %>%
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