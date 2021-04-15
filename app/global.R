

width_px = "150px"

scale_fun <- function(x) as.vector(scale(x))

#font_import()
library(shiny)
library(DT)


#### Load packages ####
library(scales)
library(tidyverse)
#library(hrbrthemes)

library(stringi)
#library(extrafont)

library(plotly)
library(viridis)
library(grid)
library(gtable)
library(shinycssloaders)


#library(extrafont)
library(hrbrthemes)
library(shinythemes)
library(shinyWidgets)

#hrbrthemes::import_roboto_condensed()

#loadfonts(device="win")

#extrafont::fonttable() %>%
#  filter(FullName %in% c("Gill Sans MT", "Segoe UI"))


#### Read datasets ####

#data404 <- read_feather('datafiles/df_404.feather') %>%

data404 <-read_csv("datafiles/data404_newMod.csv")%>%

  mutate(
    fy = as.factor(fy),
    pihp = as.numeric(pihp),
    pihp_name = as.character(pihp_name),
    cmhsp = as.character(cmhsp),
    population = as.factor(population),
    svc_type = as.character(svc_type),
    svc_grp = as.character(svc_grp),
    code = as.character(code),
    short_desc = as.character(short_desc),
    modifier = as.character(modifier),
    unit_type = as.character(unit_type),
    cases = as.numeric(cases),
    units = as.numeric(units))%>%
  mutate_at(
    vars(pihp_name,cmhsp,code,code_mod),
    list(~as.factor(.))
  )




#service_groups <- read_feather("datafiles/svc_grps.feather")

service_groups<-read_csv("datafiles/svc_grps.csv")%>%
                 mutate_if(is.factor,as.character)

#### Formatting Variables ####

pihpCMH_LU<-data404%>%
  distinct(pihp,pihp_name,cmhsp)

state_data<-read_csv("datafiles/TotalServedAnnual.csv")%>%
                     rename(cmhsp = CMHSP, fy = FY)%>%
                     left_join(pihpCMH_LU, by = "cmhsp")%>%
                     mutate(fy = as.factor(fy))%>%
                     select(-pihp)
  


### code referecne table 

code_ref<-data404%>%
          select(code,code_shortDesc)%>%
          distinct()










