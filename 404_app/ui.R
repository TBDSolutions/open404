# ui.R #

ui <- fluidPage(
  plotlyOutput("bubble"),
  sliderInput(
    "sliderFY",
    "Fiscal Year:",
    min = 2006,
    max = 2016,
    value = 2006,
    animate = TRUE
  ),
  selectInput(
    inputId = "x",
    label = "X Variable:",
    choices = c("SumOfCases","SumOfUnits","SumOfCost",
                "CostPerCase","CostPerUnit","UnitPerCase",
                "Unit_Perc_Tot","Cost_Perc_Tot","Cost1kSvd","Perc_Svd"),
    selected = "SumOfCases"
  ),
  selectInput(
    inputId = "y",
    label = "Y Variable:",
    choices = c("SumOfCases","SumOfUnits","SumOfCost",
                "CostPerCase","CostPerUnit","UnitPerCase",
                "Unit_Perc_Tot","Cost_Perc_Tot","Cost1kSvd","Perc_Svd"),
    selected = "SumOfUnits"
  )
)
