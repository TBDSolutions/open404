library(dplyr)
svs_groups <- summarize(group_by(Master, Service, ServiceType), 
                        n_distinct(ServiceType))

svs_groups <- summarize(group_by(Master, FirstofService.Description, Service), 
                        n_distinct(Service))

svs_groups <- summarize(group_by(Master, FirstOfHCPCS.Code, FirstofService.Description), 
                        n_distinct(FirstofService.Description))

svs_groups <- summarize(group_by(Master, Code_Mod, FirstOfHCPCS.Code), 
                        n_distinct(FirstOfHCPCS.Code))

svs_groups <- arrange(svs_groups, FirstOfHCPCS.Code, FirstofService.Description)

