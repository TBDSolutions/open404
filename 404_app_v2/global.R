

width_px = "150px"

scale_fun <- function(x) as.vector(scale(x))

#font_import()
library(shiny)
#library(rlang)
library(DT)
#library(ggiraph)
library(shinythemes)
library(shiny)

#### Load packages ####

#library(scales)
library(tidyverse)
#library(dplyr)
library(arrow)
library(hrbrthemes)
#library(gcookbook)
#library(stringi)
#library(extrafont)
library(rsconnect)


#hrbrthemes::import_roboto_condensed()

#### Read datasets ####

data404 <- 
  arrow::read_feather('datafiles/df_404.feather') %>%
  mutate(
    code_mod = paste0(code,replace_na(modifier,replace = ""))
  ) %>%
  mutate_at(
    vars(pihp_name,cmhsp,code,code_mod),
    list(~as.factor(.))
  )

service_groups <- arrow::read_feather("datafiles/svc_grps.feather")

#### Formatting Variables ####

data404 %<>%
  mutate(
    code_shortDesc = as.factor(paste(data404$short_desc," (",(data404$code),")")),
    codeM_shortDesc = as.factor(paste(data404$short_desc," (",(data404$code_mod),")"))
  )%>%
  mutate(state = 'MI')


pihpCMH_LU<-data404%>%
  distinct(pihp,pihp_name,cmhsp)


state_data<-read.csv("datafiles/TotalServedAnnual.csv")%>%
                     rename(cmhsp = CMHSP, fy = FY)%>%
                     left_join(pihpCMH_LU, by = "cmhsp")%>%
                     mutate(fy = as.factor(fy))%>%
                     select(-pihp)
  



