# server.R #

shinyServer(function(input, output) {
  
  #### Make Reactive Datasets ####

  df_bubble <- reactive({
    
    # Relabel selected grouping variable
    if (input$org_type == "PIHP") {
      df <- data404 %>% rename(org_grp = PIHPname)
    } else if (input$org_type == "CMH") {
      df <- data404 %>% rename(org_grp = CMHSP)
    } else print(paste0("Error.  Unrecognized input."))
    
    if (input$select_ServiceType == "All") {
      df <- df
    } else if (input$select_ServiceType %in% levels(unique(df$ServiceType))) {
      df <- df %>% filter(ServiceType %in% input$select_ServiceType)
    } else print(paste0("Error.  Unrecognized input."))
    
    # if (input$select_PIHP == "All") {
    #   df <- data404
    # } else if (input$select_PIHP %in% levels(unique(data404$PIHPname)) ) {
    #   df <- data404 %>% filter(PIHPname %in% input$select_PIHP)
    # } else print(paste0("Error.  Unrecognized input."))
    
    # filt_org <-
    #   if (input$select_PIHP == "All") {unique(df$org_type)
    #   } else if (input$select_PIHP == "All") {unique(scrub_fas$assess_type)
    #   } else input$radio_type
    
    df %<>%
      # filter(ServiceType == input$select_ServiceType) %>%
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
      ungroup() %>%
      select(
        FY
        ,org_grp
        ,x = matches(input$x)
        ,y = matches(input$y)
        ,z = matches(input$z)
      )
    
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
  
    
  })
  
  #### Filters ####
  
  output$select_code <- renderUI({
    
    hcpcs <- if(input$select_ServiceType == "All"){
      levels(data404$Code_Mod)
    } else
      levels(droplevels(data404$Code_Mod[data404$ServiceType == input$select_ServiceType]))
    
    selectInput(
      "select_code",
      label = "Select HCPCS Code:",
      choices = c(hcpcs)
    )
    
  })
  
  output$select_org <- renderUI({

    org <- if(input$org_type == "PIHP"){
      c("All",levels(unique(data404$PIHPname)))
    } else if(input$org_type == "CMH"){
      c("All",levels(unique(data404$CMHSP)))
    } else print(paste0("Error.  Unrecognized input."))

    selectInput(
      "select_org",
      label = "Filter by Organization:",
      choices = c(org)
    )

  })

  #### Visualizations ####
  
  output$bubble <- renderPlotly({
    df_bubble() %>%
      filter(FY == input$sliderFY) %>%
      plot_ly(
        x = ~x, y = ~y, type = 'scatter', mode = 'markers', 
      size = ~z, color = ~org_grp, marker = list(opacity = 0.5),
      hoverinfo = 'text',
      text = ~paste(org_grp,
                    '<br>',input$x,':', x,
                    '<br>',input$y,':', y)
      ) %>%
      layout(
        title = ~paste('How does',input$x,'compare to',input$y,'for<br>',
                       input$select_ServiceType,'across',input$org_type,"'s",'?'),
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
