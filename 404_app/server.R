# server.R #

server <- function(input, output) {
  
  #### Make Reactive Datasets ####

  df <- reactive({

df <-  data404 %>%
      select(FY,PIHPname,PIHP,CMHSP,Population,ServiceType
             ,Service,Description,Code,Code_Mod
             ,contains(input$x),contains(input$y)
      ) %>%
      rename(x = input$x,
             y = input$y)

  })
  
  #### Visualizations ####
  
  output$bubble <- renderPlotly({
    df %>%
      filter(
        FY == input$sliderFY) %>%
    plot_ly(x = ~x, y = ~input$y, type = 'scatter', 
            mode = 'markers', marker = list(opacity = 0.5)) %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE))
  })

}
