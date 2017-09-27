# global.R #

#### Load packages ####

library(shiny); library(shinythemes)
library(plotly)
library(tidyverse); library(magrittr)
library(shinydashboard)
library(shinythemes)
library(feather)
library(scales)



#### Read datasets ####

data404 <- read_feather("~/GitHub/open404/data/clean/Master.feather")

#data404 <- read_feather("C:/Users/jennad/Documents/GitHub/open404/404_app/data/Master.feather")




#### Colors ####

cmh_palette <- c("#a3ab73","#4e64ed","#b6d325","#971da8","#66ad00","#fb79fd",
                 "#009721","#f22a9b","#9ed75a","#7630a1","#6f9100","#ad7cff",
                 "#fda812","#0071da","#c69e00","#7a93ff","#8f8e00","#b0008a",
                 "#86d88f","#ab0066","#009266","#f62f51","#02c6ee","#c8081d",
                 "#62d8d4","#cf4900","#334da2","#cbcb67","#ff8de3","#50571e",
                 "#e6a5ff","#a26700","#64467f","#e0c476","#c60052","#ffaa79",
                 "#ff94cb","#9a4400","#bc7ea0","#982b1e","#ffa6b2","#774723",
                 "#ff7a6f","#893b26","#c1798a","#a3615b")
