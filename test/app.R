shinyApp(
    fluidPage(
        fluidRow(
            tabsetPanel(
                tabPanel("Tab1",
                         verbatimTextOutput("choice1")),
                tabPanel("Tab2",
                         verbatimTextOutput("choice2"))
            )
        ),
        fluidRow(
            selectInput("id1", "Pick something",
                        choices = c("a","b","c"),
                        selected = "a")
        )
    ),
    function(input, output, session){
        output$choice1 <- renderPrint(input$id1)
        output$choice2 <- renderPrint({
            paste("The choice is:", input$id1)
        })
    }
)