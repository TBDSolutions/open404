# server.R #

shinyServer(function(input, output) {
  
  #### Make Reactive Datasets ####

  df <- reactive({

    data404 %>%
      select(
        FY,PIHPname,PIHP,CMHSP,Population,ServiceType,
        Service,Description,Code,Code_Mod,
        x = matches(input$x),
        y = matches(input$y)
      ) 

  })
  
  #### Visualizations ####
  
  output$bubble <- renderPlotly({
    df() %>%
      filter(FY == input$sliderFY) %>%
      plot_ly(
        x = ~x, y = ~y, type = 'scatter', 
        mode = 'markers', marker = list(opacity = 0.5)
      ) %>%
      layout(
        title = '',
        xaxis = list(showgrid = FALSE),
        yaxis = list(showgrid = FALSE)
      )
    
  })

})
