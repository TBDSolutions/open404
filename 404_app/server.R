# server.R #

shinyServer(function(input, output) {
  
  #### Make Reactive Datasets ####

  df <- reactive({
    
    PIHP <-
      if (input$select_PIHP == "All") {
        levels(unique(data404$PIHPname))
      } else input$select_PIHP
    
    # CMHSP <- 
    #   if (input$CMHSP == "All") {
    #     levels(unique(data404$CMHSP))
    #   } else input$CMHSP
    # 
    # Population <- 
    #   if (input$Population == "All") {
    #     levels(unique(data404$Population))
    #   } else input$Populations
    # 
    # ServiceType <- 
    #   if (input$ServiceType == "All") {
    #     levels(unique(data404$ServiceType))
    #   } else input$ServiceType
    # 
    # Service <- 
    #   if (input$Service == "All") {
    #     levels(unique(data404$Service))
    #   } else input$Service
    # 
    # short_description <- 
    #   if (input$short_description == "All") {
    #     levels(unique(data404$short_description))
    #   } else input$short_description
    # 
    # Description <- 
    #   if (input$Description == "All") {
    #     levels(unique(data404$Description))
    #   } else input$Description
    # 
    # Code <- 
    #   if (input$Code == "All") {
    #     levels(unique(data404$Code))
    #   } else input$Code

    data404 %>%
      select(
        FY,PIHPname,PIHP,CMHSP,Population,ServiceType,
        Service,short_description,Description,Code,Code_Mod,
        w = matches(input$w),
        x = matches(input$x),
        y = matches(input$y),
        z = matches(input$z)
      ) 
    # %>%
    #   filter(
    #     PIHPname %in% PIHP
    #   )
    
  })

  #### Visualizations ####
  
  output$bubble <- renderPlotly({
    df() %>%
      filter(FY == input$sliderFY) %>%
      plot_ly(
        x = ~x, y = ~y, type = 'scatter', mode = 'markers', 
        size = ~z, color = ~w, marker = list(opacity = 0.5)
      ) %>%
      layout(
        title = '',
        xaxis = list(showgrid = FALSE),
        yaxis = list(showgrid = FALSE)
      )
    
  })

})
