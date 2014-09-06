library(dplyr)
svs_groups <- summarize(group_by(Master, Service, ServiceType), 
                        n_distinct(ServiceType))

svs_groups <- summarize(group_by(Master, FirstofService.Description, Service), 
                        n_distinct(Service))

svs_groups <- summarize(group_by(Master, FirstOfHCPCS.Code, FirstofService.Description), 
                        n_distinct(FirstofService.Description))

svs_groups <- 
  Master %>%
  #filter(FirstofService.Description=='Assessment for Autism') %>%
  group_by(Service,FirstofService.Description,FirstOfHCPCS.Code) %>%
  summarize(n_distinct(FirstOfHCPCS.Code)) %>%
  arrange(Service)

svs_groups <- arrange(svs_groups, FirstOfHCPCS.Code, FirstofService.Description)


levels(unique_06to13$CMHSP)

lrp_mshn <-
unique_06to13 %>%
  filter(FY==2013, 
         CMHSP=="Allegan"|
         CMHSP=="Muskegon"|
         CMHSP=="Network180"|
         CMHSP=="Ottawa"|
         CMHSP=="West Michigan"|
         CMHSP=="Bay-Arenac"|
         CMHSP=="CMH for Central Michigan"|
         CMHSP=="Clinton Eaton Ingham"|
         CMHSP=="Gratiot"|
         CMHSP=="Huron"|
         CMHSP=="Ionia"|
         CMHSP=="Lifeways"|
         CMHSP=="Montcalm"|
         CMHSP=="Newaygo"|
         CMHSP=="Saginaw"|
         CMHSP=="Shiawassee"|
         CMHSP=="Tuscola") %>%
  summarize(served=sum(TotalServed))
  