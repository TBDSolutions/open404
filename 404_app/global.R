# global.R #

#### Load packages ####

library(shiny)
library(plotly)
library(tidyverse)
library(feather)

#### Read datasets ####

data404 <- read_feather("~/GitHub/open404/data/clean/Master.feather")