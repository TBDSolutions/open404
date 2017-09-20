# ui.R #

ui <- fluidPage(
  theme = shinytheme("yeti"),
  plotlyOutput("bubble"),
  sliderInput(
    "sliderFY",
    "Fiscal Year:",
    min = 2006,
    max = 2016,
    value = 2006,
    sep = "",
    animate = animationOptions(loop = TRUE, interval = 1000)
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
  ),
  selectInput(
    inputId = "z",
    label = "Z Variable:",
    choices = c("SumOfCases","SumOfUnits","SumOfCost",
                "CostPerCase","CostPerUnit","UnitPerCase",
                "Unit_Perc_Tot","Cost_Perc_Tot","Cost1kSvd","Perc_Svd"),
    selected = "Perc_Svd"
  ),
  selectInput(
    inputId = "w",
    label = "W Variable:",
    choices = c("PIHPname","CMHSP","Population","ServiceType",
                "Service","Description","Code"),
    selected = "PIHPname"
  ),
  selectInput(
    inputId = "select_PIHP",
    label = "PIHP Filter:",
    choices = c("All","CMHPSM","DWMHA","LRP","MCMHS","MSHN"
                ,"NMRE","Northcare","OCCMHA","Region10","SWMBH"),
    selected = "All"
  )
)
