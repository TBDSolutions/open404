# server.R #

shinyServer(function(input, output) {
  
  #### Make Reactive Datasets ####

  df_bubble <- reactive({
    
    # Relabel selected grouping variable
    if (input$org_type == "PIHPname") {
      df <- data404 %>% rename(org_grp = PIHPname)
    } else if (input$org_type == "CMHSP") {
      df <- data404 %>% rename(org_grp = CMHSP)
    } else print(paste0("Error.  Unrecognized input."))
    
    df %<>%
      filter(ServiceType == input$select_ServiceType) %>%
      group_by(FY,org_grp) %>%
      summarize(
        SumCases = sum(SumOfCases, na.rm = T),
        SumUnits = sum(SumOfUnits, na.rm = T),
        SumCost = sum(SumOfCost, na.rm = T),
        CostPerCase = round(SumCost / SumCases, digits = 2),
        CostPerUnit = round(SumCost / SumUnits, digits = 2),
        UnitPerCase = round(SumUnits / SumCases, digits = 1),
        Cost1kSvd = round(SumCost / 1000, digits = 1)) %>%
      ungroup() %>%
      group_by(FY) %>%
      mutate(
        Cost_Perc_Tot = round(SumUnits / sum(SumUnits, na.rm = T) * 100, digits = 1),
        Perc_Svd = round(SumCases / sum(SumCases, na.rm = T) * 100, digits = 1)
      ) %>%
      ungroup()
    
    # if (input$select_PIHP == "All") {
    #   df <- data404
    # } else if (input$select_PIHP %in% levels(unique(data404$PIHPname)) ) {
    #   df <- data404 %>% filter(PIHPname %in% input$select_PIHP)
    # } else print(paste0("Error.  Unrecognized input."))
    # 
    # if (input$select_CMHSP == "All") {
    #   df <- data404
    # } else if (input$select_CMHSP %in% levels(unique(data404$CMHSP)) ) {
    #   df <- data404 %>% filter(CMHSP %in% input$select_CMHSP)
    # } else print(paste0("Error.  Unrecognized input."))
    # 
    # if (input$select_Population == "All") {
    #   df <- data404
    # } else if (input$select_Population %in% levels(unique(data404$Population)) ) {
    #   df <- data404 %>% filter(Population %in% input$select_Population)
    # } else print(paste0("Error.  Unrecognized input."))
    # 
    # if (input$select_ServiceType == "All") {
    #   df <- data404
    # } else if (input$select_ServiceType %in% levels(unique(data404$ServiceType)) ) {
    #   df <- data404 %>% filter(ServiceType %in% input$select_ServiceType)
    # } else print(paste0("Error.  Unrecognized input."))

    df %>%
      select(
        FY,
        org_grp,
        # w = matches(input$w),
        x = matches(input$x),
        y = matches(input$y),
        z = matches(input$z)
      )
    
  })

  #### Visualizations ####
  
  output$bubble <- renderPlotly({
    df_bubble() %>%
      filter(FY == input$sliderFY) %>%
      plot_ly(
        x = ~x, y = ~y, type = 'scatter', mode = 'markers', 
        size = ~z, color = ~org_type, marker = list(opacity = 0.5),
        hoverinfo = 'text',
        text = ~paste(input$org_type,':', org_type,
                      '<br>',input$x,':', x,
                      '<br>',input$y,':', y)
      ) %>%
      layout(
        xaxis = list(
          title = input$x,
          showgrid = FALSE),
        yaxis = list(
          title = input$y,
          showgrid = FALSE),
        showlegend = FALSE
        )
    
  })

})
