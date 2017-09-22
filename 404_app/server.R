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
    
    if (input$select_Population == "All") {
      df <- df
    } else if (input$select_Population %in% levels(unique(data404$Population)) ) {
      df <- df %>% filter(Population %in% input$select_Population)
    } else print(paste0("Error.  Unrecognized input."))
    
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
      ungroup() 
    
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
    } else if (input$x == "Unit Per Case") {
      df %<>% rename(x = UnitPerCase)
    } else if (input$x == "Cost per 1K Served") {
      df %<>% rename(x = Cost1kSvd)
    } else if (input$x == "% of Total Cost") {
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
    } else if (input$y == "Unit Per Case") {
      df %<>% rename(y = UnitPerCase)
    } else if (input$y == "Cost per 1K Served") {
      df %<>% rename(y = Cost1kSvd)
    } else if (input$y == "% of Total Cost") {
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
    } else if (input$z == "Unit Per Case") {
      df %<>% rename(z = UnitPerCase)
    } else if (input$z == "Cost per 1K Served") {
      df %<>% rename(z = Cost1kSvd)
    } else if (input$z == "% of Total Cost") {
      df %<>% rename(z = Cost_Perc_Tot)
    } else if (input$z == "Percent Served") {
      df %<>% rename(z = Perc_Svd)
    } else print(paste0("Error.  Unrecognized input."))
    
    df %<>% select(FY, org_grp, x, y, z)
    
  })
  
  #### Filters ####
  
  output$select_code <- renderUI({
    
    hcpcs <- if (input$select_ServiceType == "All") {
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

    org <- if (input$org_type == "PIHP") {
      c("All",levels(unique(data404$PIHPname)))
    } else if (input$org_type == "CMH") {
      c("All",levels(unique(data404$CMHSP)))
    } else print(paste0("Error.  Unrecognized input."))

    selectInput(
      "select_org",
      label = "Filter Organization Level:",
      choices = c(org)
    )

  })

  #### Visualizations ####
  
  output$bubble <- renderPlotly({
    
    # Grab max values from x and y vars
    max_x <- max(df_bubble()$x, na.rm = T)
    max_y <- max(df_bubble()$y, na.rm = T)
    
    df_bubble() %>%
      filter(FY == input$sliderFY) %>%
      plot_ly(
        x = ~x, y = ~y, type = 'scatter', mode = 'markers', 
      size = ~z, color = ~org_grp, marker = list(opacity = 0.5),
      hoverinfo = 'text',
      text = ~paste(
        org_grp,
        '<br>',input$x,':', 
        ifelse(
          grepl("Cost",input$x),
          yes = dollar_format(big.mark = ",")(x),
          no = format(x, big.mark = ",")
        ),
        '<br>',input$y,':', 
        ifelse(
          grepl("Cost",input$y),
          yes = dollar_format(big.mark = ",")(y),
          no = format(y, big.mark = ",")
        )
        )
      ) %>%
      layout(
        title = ~paste('How do',input$x,'compare to',input$y,'for<br>',
                       input$select_ServiceType,'across',input$org_type,"s",'?'),
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
