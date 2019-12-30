# deploy.R

library(rsconnect)

deployApp("404_app", account = "tbdsolutions")




getwd()

# Get info about ShinyApps accounts

accounts()
accountInfo("joshh")
accountInfo("tbdsolutions")
applications("tbdsolutions")
applications("joshh")

# configure applications
configureApp(appName = "exploreCAFAS_node_mshn", 
             appDir = "../exploreCAFAS_node_mshn",
             account = "tbdsolutions",
             redeploy = F, 
             size = "xxlarge")