

width_px = "150px"

scale_fun <- function(x) as.vector(scale(x))
#### Load packages ####

library(shiny); library(shinythemes);library(shinydashboard)
library(plotly);library(scales)
library(tidyverse);library(magrittr);library(arrow)
library(hrbrthemes)
library(gcookbook)
library(stringi)
library(extrafont)
#font_import()

#hrbrthemes::import_roboto_condensed()

#### Read datasets ####

data404 <- 
  arrow::read_feather('C:/Users/joet/Documents/GitHub/open404/data/clean/df_404.feather') %>%
  mutate(
    code_mod = paste0(code,replace_na(modifier,replace = ""))
  ) %>%
  mutate_at(
    vars(pihp_name,cmhsp,code,code_mod),
    list(~as.factor(.))
  )

service_groups <- arrow::read_feather("C:/Users/joet/Documents/GitHub/open404/data/clean//svc_grps.feather")

#### Formatting Variables ####

data404 %<>%
  mutate(
    code_shortDesc = as.factor(paste(data404$short_desc," (",(data404$code),")")),
    codeM_shortDesc = as.factor(paste(data404$short_desc," (",(data404$code_mod),")"))
  )%>%
  mutate(state = 'MI')


pihpCMH_LU<-data404%>%
  distinct(pihp,pihp_name,cmhsp)


state_data<-read.csv("../data/TotalServedAnnual.csv")%>%
                     rename(cmhsp = CMHSP, fy = FY)%>%
                     left_join(pihpCMH_LU, by = "cmhsp")%>%
                     mutate(fy = as.factor(fy))%>%
                     select(-pihp)
  



