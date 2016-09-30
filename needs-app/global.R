## global.R ##

library(dplyr)
library(tidyr)
library(rcdimple)
library(DT)
library(networkD3)
library(visNetwork)
library(plotly)
library(shinydashboard)

# Load de-identified data

needs <- read.csv("data/needs.csv")

