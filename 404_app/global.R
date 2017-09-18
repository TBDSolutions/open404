# global.R #

#### Load packages ####

library(shiny)
library(plotly)
library(dplyr)

#### Read datasets ####

data404 <- read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/Master.csv")