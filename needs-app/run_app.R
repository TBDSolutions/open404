library(shiny)

needsapp <- "C:/Users/joshh/Documents/GitHub/open404/needs-app"

runApp(needsapp)

library(shinyapps)

deployApp(needsapp, account = "joshh")

deployApp("needs-app", account = "tbdsolutions")

# Get info about ShinyApps accounts

accounts()
accountInfo("joshh")
accountInfo("tbdsolutions")