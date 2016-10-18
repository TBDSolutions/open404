library(shiny)

#needsapp <- "C:/Users/joshh/Documents/GitHub/open404/needs-app"

runApp("needs-app")

library(shinyapps)

deployApp(needsapp, account = "joshh")

deployApp(needsapp, account = "tbdsolutions")

# Get info about ShinyApps accounts

rsconnect::showLogs(needsapp, account = "joshh")

applications()
accounts()
accountInfo("joshh")
accountInfo("tbdsolutions")
