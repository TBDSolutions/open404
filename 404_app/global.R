# global.R #

#### Load packages ####

library(shiny); library(shinythemes)
library(plotly)
library(tidyverse); library(magrittr)
library(shinydashboard)
library(shinythemes)
library(feather)

#### Read datasets ####

data404 <- read_feather("~/GitHub/open404/data/clean/Master.feather")

#data404 <- read_feather("C:/Users/jennad/Documents/GitHub/open404/404_app/data/Master.feather")
