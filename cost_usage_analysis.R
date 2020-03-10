library(lubridate)
library(tidyverse)

# Lets control for population growth


ttl<-state_data%>%
  filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
  group_by(cmhsp)%>%
  mutate(prev_year = lag(TotalServed),
         pct_change = (TotalServed - prev_year)/prev_year)

wshtw_pop<-state_data%>%
#  filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
      filter(cmhsp %in% c('Washtenaw'))%>%
      mutate(prev_year = lag(TotalServed),
             pct_change_w = (TotalServed - prev_year)/prev_year)

groups <-ttl%>%
      left_join(wshtw_pop%>%select(fy,pct_change_w),by = "fy")%>%
      mutate(top = abs(pct_change_w - pct_change))%>%
      group_by(cmhsp)%>%
      summarise(top = mean(top,na.rm = T))%>%
  ungroup()%>%
    mutate(group =  case_when( cmhsp == 'Washtenaw' ~ "Washt",
                                       cmhsp == 'Kalamazoo' ~ "Kzoo",
                                       cmhsp == "Ottawa" ~ "Ottawa",
                                     #  top  <.05 ~ "sister CMH",
                                    #        TRUE ~ "unrelated"
                               )
           )


plot<-state_data%>%
    #filter(!groups == "unrelated")%>% 
   filter(cmhsp %in% c('Washtenaw'
                     #  ,"Kalamazoo" 
                       , "Ottawa"
                       
                       ))%>%
    group_by(cmhsp)%>%
  # filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
    mutate(prev_year = lag(TotalServed),
           pct_change = (TotalServed - prev_year)/prev_year)%>%
     ungroup()%>%
    left_join(groups, by = "cmhsp")%>%
    filter(!group %in% 'unrelated')
  
 



ggplot(plot,aes(x = fy,y = pct_change , group = cmhsp))+ geom_line(aes(color = group),size = 1)+
  ggtitle("year-over-year population growth percent")



ggplot(wshtw_pop,aes(x = fy,y = TotalServed , group = 1))+ geom_line()+
  ggtitle("Population Growth")




####
plot_agg<-state_data%>%
  # filter(cmhsp %in% c('Washtenaw',"Kalamazoo","CMH for Central Michigan"))%>%
  group_by(cmhsp)%>%
   filter(fy %in% c("2013","2014","2015","2016","2017","2018"))%>%
  mutate(prev_year = lag(TotalServed),
         pct_change = (TotalServed - prev_year)/prev_year)%>%
  ungroup()%>%
  left_join(test, by = "cmhsp")%>%
  filter(!is.na(pct_change))%>%
  group_by(fy,control_group)%>%
  summarise(avg = mean(pct_change,na.rm = T))

ggplot(plot_agg,aes(x = fy,y = avg , group = control_group))+ geom_line(aes(color = control_group))





############ Now lets look at some heavy hitting codes that eat up resources as a percent to total 



wash2018<-data404%>%
#  filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
  filter(cmhsp %in% c('Washtenaw'),
       #  fy == "2018"
        fy %in% c("2017","2018")
     #    code == "90862"
         )%>%
  group_by(code,fy)%>%
  summarise(cost_pct_tot = sum(cost_pct_tot,na.rm = T))%>%
  ungroup()%>%
  group_by(code)%>%
  summarise(post_avg_pct_tot = mean(cost_pct_tot,na.rm = T))%>%
  ungroup()


###
washPre<-data404%>%
    filter(fy %in% c("2012","2013","2014","2015"))%>%
  filter(cmhsp %in% c('Washtenaw'),
       #  !fy %in% "2018"
        #      code == "90801"
  )%>%
  group_by(code,fy)%>%
  summarise(cost_pct_tot = sum(cost_pct_tot,na.rm = T))%>%
  ungroup()%>%
  group_by(code)%>%
  summarise(pre_avg_pct_tot = mean(cost_pct_tot,na.rm = T))%>%
  ungroup()

 sum(wash2018$cost_pct_tot_2018)
###
test<-washPre%>%
      left_join(wash2018, by = "code")%>%
      replace_na(list(pre_avg_pct_tot = 0, post_avg_pct_tot = 0))%>%
      mutate(delta =  post_avg_pct_tot - pre_avg_pct_tot)%>%
     filter(delta >0 , 
            !code %in% c("H0043","H2015"))

 sum(abs(test$post_avg_pct_tot))
 sum(abs(test$pre_avg_pct_tot))
  
ggplot(wash,aes(x = fy,y = cost_pct_tot , group = code))+ geom_line()

  plot(test$delta)
  
###  Plot a heavy hitter 
consum<-data404%>%
#filter(fy %in% c("2012","2013","2014","2015"))%>%
filter(cmhsp %in% c('Washtenaw'),
       #  !fy %in% "2018"
         #    code == "H2011" # save as example service 
           #  code == "T1000"
        code == "H2016"
)%>%
group_by(code,fy)%>%
summarise(cost_pct_tot = sum(cost_pct_tot,na.rm = T))

ggplot(consum,aes(x = fy,y = cost_pct_tot , group = 1))+ geom_line()


######################### lets look at cost per case ##############################



washPost<-data404%>%
  #  filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
  filter(cmhsp %in% c('Washtenaw'),
         #  fy == "2018"
         fy %in% c("2017","2018")
    #     ,code == "H2011"
  )%>%
  group_by(code,fy)%>%
  summarise(cost_per_case = mean(cost_per_case,na.rm = T))%>%
  ungroup()%>%
  group_by(code)%>%
  summarise(post_cost_per_case = mean(cost_per_case,na.rm = T))%>%
  ungroup()


###
washPre<-data404%>%
  filter(fy %in% c("2012","2013","2014","2015"))%>%
  filter(cmhsp %in% c('Washtenaw'),
         #  !fy %in% "2018"
         #      code == "90801"
  )%>%
  group_by(code,fy)%>%
  summarise(cost_per_case = mean(cost_per_case,na.rm = T))%>%
  ungroup()%>%
  group_by(code)%>%
  summarise(pre_cost_per_case = mean(cost_per_case,na.rm = T))%>%
  ungroup()

sum(wash2018$costs_per_case_2018)
###
cost_per_case_chng<-washPre%>%
                    left_join(washPost, by = "code")%>%
                    replace_na(list(pre_cost_per_case = 0, post_cost_per_case = 0))%>%
                    mutate(delta =  round( (post_cost_per_case - pre_cost_per_case),2) ,
                           delta_pct =  (post_cost_per_case - pre_cost_per_case)/pre_cost_per_case
                      )%>%
                    filter(delta_pct >0 , 
                           !code %in% c("H0043","H2015"))

sum(abs(test$post_avg_pct_tot))
sum(abs(test$pre_avg_pct_tot))


#####

###  Plot a heavy hitter 
consum<-data404%>%
  #filter(fy %in% c("2012","2013","2014","2015"))%>%
  filter(cmhsp %in% c('Washtenaw'),
         #  !fy %in% "2018"
         #code == "T1000" # save as example service avg_usage
         code == "T1020"
  )%>%
  group_by(code,fy)%>%
  summarise(cost_per_case = mean(cost_per_case,na.rm = T))

ggplot(consum,aes(x = fy,y = cost_per_case , group = 1))+ geom_line()

#################################### Overall cost year over year 



ttl<-data404%>%
 # filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
  group_by(cmhsp,fy)%>%
  summarise(cost = sum(cost,na.rm = T))%>%
  ungroup()%>%
  mutate(prev_year = lag(cost),
         pct_change = (cost - prev_year)/prev_year)

wshtw_cost<-data404%>%
  #filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
  filter(cmhsp %in% c('Washtenaw'))%>%
  group_by(fy)%>%
  summarise(cost = sum(cost,na.rm = T))%>%
  ungroup()%>%
  mutate(prev_year = lag(cost),
         pct_change = (cost - prev_year)/prev_year)


groups <-ttl%>%
  left_join(wshtw%>%select(fy,pct_change_w),by = "fy")%>%
  mutate(top = abs(pct_change_w - pct_change))%>%
  group_by(cmhsp)%>%
  summarise(top = mean(top,na.rm = T))%>%
  ungroup()


## Looking at cost compred to adjusted population growth 


desired = wshtw_cost%>%
  filter(fy == "2011")%>%
  select(cost)%>%
  pull()


wshtw_cost<-wshtw_cost%>%
  left_join(wshtw_pop, by = "fy")%>%
  mutate( desired = desired,
          year = as.numeric(as.character(fy)),
          pop_adj = case_when(year <= 2011 ~ cost,
                              year > 2011 ~ desired + (desired * pct_change_w),
                              TRUE ~ cost)
  )


ggplot(wshtw_cost,aes(x = fy,y = cost , group = 1))+ geom_line()+
  geom_line(aes(y = pop_adj), color = "green")+
  geom_hline(yintercept = (79354944 - 30000000))+
  geom_vline(xintercept = 6)





################################## Pareto of resouce usage over time 


### Service groups to focus on 

pareto<-data404%>%
 # filter(fy %in% c("2018","2017"))%>%
  filter(fy %in% c("2018"))%>%
  filter(cmhsp %in% c('Washtenaw'))%>%
#  filter(code == "H0043")
  group_by(fy,code)%>%
  summarise(cost_pct_tot = sum(cost_pct_tot,na.rm = T))%>%
  ungroup()%>%
  group_by(code)%>%
  summarise(cost_pct_tot = mean(cost_pct_tot))%>%
  arrange(desc(cost_pct_tot))%>%
 # mutate( cum_s)
  mutate(pareto = cumsum(cost_pct_tot),
         rank = row_number())%>%
  filter(pareto < 90)%>%
  select(code,cost_pct_tot,rank)
  

###############


wshtw2<-data404%>%
  filter(fy %in% c("2015"))%>%
  filter(cmhsp %in% c('Washtenaw'))%>%
  group_by(fy,code)%>%
  summarise(cost_pct_tot = sum(cost_pct_tot,na.rm = T),
            cost_per_case = mean(cost_per_case))%>%
  ungroup()%>%
  arrange(desc(cost_pct_tot))%>%
  # mutate( cum_s)
  mutate(pareto = cumsum(cost_pct_tot),
         rank = row_number(),
         fy = "2015")%>%
  filter(pareto < 90)%>%
  select(fy,code,cost_pct_tot,rank,cost_per_case)




test<-wshtw1%>%
      union_all(wshtw2)%>%
      select(code,fy,cost_pct_tot)%>%
      pivot_wider(names_from = fy,values_from = cost_pct_tot, values_fill = list(cost_pct_tot = 0))%>%
      mutate(diff = `2018` - `2015`)


rm(ttl,pareto,plot,pihpCMH_LU,wshtw_pop,consum)
######### Build clustering set based on pop growth and service group cost to total 

### Get the population growth trend which will not change based 
### Service grouping type


ttl_pop_clust<-state_data%>%
  filter(fy %in% c("2016","2017","2018"))%>%
  group_by(cmhsp)%>%
  mutate(prev_year = lag(TotalServed),
         pct_change = (TotalServed - prev_year)/prev_year)

wshtw_pop_clust<-state_data%>%
  filter(fy %in% c("2016","2017","2018"))%>%
  filter(cmhsp %in% c('Washtenaw'))%>%
  mutate(prev_year = lag(TotalServed),
         pct_change_w = (TotalServed - prev_year)/prev_year)

pop_trend <-ttl_pop_clust%>%
            left_join(wshtw_pop_clust%>%select(fy,pct_change_w),by = "fy")%>%
            mutate(top = abs(pct_change_w - pct_change))%>%
            group_by(cmhsp)%>%
            summarise(pop_trend = mean(top,na.rm = T))%>%
            ungroup()
######

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###Begin Loop to create cluster by pct to toal service type

codes <- pareto%>%
  select(code)%>%
  mutate(code = as.character(code))%>%
  pull()



total_cost_clust<-data404%>%
          select(
                 cmhsp,svc_type,fy,population,cost_pct_tot
          )%>%
          filter(
                 fy %in% c("2018","2017","2016")
          )%>%
          group_by(
                 cmhsp,svc_type,fy,population
          )%>%
          summarise(
                 cost_pct_tot = sum(cost_pct_tot,na.rm = T)
          )%>%
          ungroup()%>%
          group_by(
                 cmhsp,svc_type,population
          )%>%
          summarise(
                cost_pct_avg = round( mean(cost_pct_tot,na.rm = T), digits = 1),
                cost_pct_sd = round(sd(cost_pct_tot,na.rm = T)*100, digit = 1) 
          )%>%
        ungroup()


total_cost_clust%>%
filter(svc_type == "Employment Services",
       population == "DD")%>%
mutate(group = case_when(cmhsp == "Washtenaw" ~ "Wshtnw", 
                         cmhsp %in% c("Clinton Eaton Ingham","CMH for Central Michigan",
                                      "Huron","Oakland") ~ "Sister CMH",
                         TRUE ~ "everyone else"),
       cmhsp = fct_reorder(cmhsp,cost_pct_avg))%>%
ggplot(aes(x = cmhsp,y = cost_pct_avg,fill = group))+ 
  geom_bar(stat = "identity",alpha = .90)+
  ggtitle("Employment Services - Percent of CMH Cost", subtitle = "DD population")+
  scale_fill_manual(values = c("dark grey", "dark red", "forest green"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Cost Pct to Total")
  

#######################################

  pop_trend%>%
  mutate(group = case_when(cmhsp == "Washtenaw" ~ "Wshtnw", 
                           cmhsp %in% c("Clinton Eaton Ingham","CMH for Central Michigan",
                                        "Huron","Oakland") ~ "Sister CMH",
                           TRUE ~ "everyone else"),
         cmhsp = fct_reorder(cmhsp,pop_trend), 
         pop_trend = case_when(cmhsp == "Washtenaw" ~ .1,
                               TRUE ~ pop_trend ) )%>%
  ggplot(aes(x = cmhsp,y = pop_trend,fill = group))+ 
  geom_bar(stat = "identity",alpha = .90)+
  ggtitle("Total CMH Population Growth Trend")+
  scale_fill_manual(values = c("dark grey", "dark red", "forest green"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Population Growth Trend")



#########
total_cost_clust%>%
  ungroup()%>%
  filter(#svc_type == "Employment Services",
    #  population == "DD",
    cmhsp == "Washtenaw"
  )%>%
  mutate(svc_type = fct_reorder(as.factor(svc_type),cost_pct_avg))%>%
  ggplot(aes(x = svc_type,y = cost_pct_avg))+ 
  geom_bar(stat = "identity",alpha = .80, position="stack")+
  ggtitle("Svc Type Cost Percent to Total")+
 # scale_fill_manual(values = c("dark grey", "dark red", "forest green"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Svc Type Cost Percent to Total")+
  coord_flip()









cmh_cluster_ass%>%
  filter(svc_type == "Employment Services",
         population == "DD")
### Joining CMH population data with by service/population data

clust_df<-pop_trend%>%
          inner_join(total_cost_clust, by = c("cmhsp"))



#%%%%%%%%%%%% Get your clust on

### storing each iteration into this list
results_list<-list()


### creating the level of granularity of the cluster groups (svc_type,population)
### We could also make a variable for metric changes as well. 
clust_list_of_param<-pop_trend%>%
                     inner_join(total_cost_clust, by = c("cmhsp"))%>%
                     select(svc_type,population)%>%
                     distinct()

  
for(i in 1:nrow(clust_list_of_param)){

x<- as.character(clust_list_of_param$svc_type[i])
y<- as.character(clust_list_of_param$population[i])



clust_df<-pop_trend%>%
  inner_join(total_cost_clust, by = c("cmhsp"))%>%
  filter(svc_type == x,
         population == y)%>%
  select(-svc_type,-population,-cost_pct_sd)



row.names(clust_df)<-clust_df$cmhsp
clust_df$cmhsp<-NULL

clust_df_scaled <- as.data.frame(scale(clust_df))
                   #       )%>%
                  #        rownames_to_column("cmhsp")%>%
                  #        mutate(
                  #      pop_trend = pop_trend * .45,
                  #        cost_pct_avg = cost_pct_avg * .45,
                  #       cost_pct_sd = cost_pct_sd * .1
                  #        )%>%
                  #        column_to_rownames("cmhsp")
                       

### Calulating distance matrix 
dist_mat <- dist(clust_df_scaled, method = 'euclidean')

score<-as.data.frame(t(as.matrix(dist_mat)))%>%
        select(score = Washtenaw)%>%
        mutate(cmhsp = rownames(as.data.frame(t(as.matrix(dist_mat)))))


clust_df<-pop_trend%>%
  inner_join(total_cost_clust, by = c("cmhsp"))%>%
  filter(svc_type == x,
         population == y)%>%
  select(-svc_type,-population,-cost_pct_sd)

sister_cmh<-clust_df%>%
            left_join(score,by = "cmhsp")%>%
            top_n(5,desc(score))%>%
            mutate(svc_type = x, 
                   population = y,
            group = case_when(cmhsp == "Washtenaw" ~ "wshtn", 
                           TRUE ~ "sister_cmh"))%>%
            select(cmhsp,svc_type,population,group)
  


results_list[[paste(i,sep = "")]]<-sister_cmh


}



cmh_cluster_ass<-do.call(rbind, results_list)%>%
                 arrange(svc_type,population,cmhsp)


### Tie the cluster assignments back to the original data for comparision 
### of cost per case/units per case/cost per unit ect. for FY 2018

### Creating Pareto information 

pareto<-data404%>%
  # filter(fy %in% c("2018","2017"))%>%
  filter(fy %in% c("2018"))%>%
  filter(cmhsp %in% c('Washtenaw'))%>%
  #  filter(code == "H0043")
  group_by(fy,code)%>%
  summarise(cost_pct_tot = sum(cost_pct_tot,na.rm = T))%>%
  ungroup()%>%
  group_by(code)%>%
  summarise(cost_pct_tot = mean(cost_pct_tot))%>%
  arrange(desc(cost_pct_tot))%>%
  # mutate( cum_s)
  mutate(pareto = cumsum(cost_pct_tot),
         rank = row_number())%>%
  # filter(pareto < 90)%>%
  select(code,cost_pct_tot,rank)





final_df<-data404%>%
  filter(
    fy %in% c("2018")
  )%>%
  group_by(
    cmhsp,svc_type,svc_grp,population,code
  )%>%
  summarise(
    cost_per_case = round( mean(cost_per_case,na.rm = T), digits = 1),
    unit_per_case = round( mean(unit_per_case,na.rm = T), digits = 1),
    cases = sum(cases,na.rm = T)

  )%>%
  ungroup()%>%
  left_join(cmh_cluster_ass, by = c(
                                    "cmhsp","population","svc_type"
  ))%>%
#  arrange(svc_type,population,cmhsp,group)%>%
  filter(!is.na(group))%>%
  group_by(svc_grp,population,code,group)%>%
  summarise(
    cost_per_case = round( mean(cost_per_case,na.rm = T) , digits = 0),
  #  unit_per_case = mean(unit_per_case,na.rm = T)
  #   cases = mean(cases,na.rm = T)
    
  )%>%
  pivot_wider(names_from = group,values_from = cost_per_case,
              names_prefix = "cost_per_case_")%>%
  mutate(code_diff = cost_per_case_wshtn - cost_per_case_sister_cmh)%>%
  group_by(svc_grp,population)%>%
  mutate(svc_grp_total_diff = sum(code_diff,na.rm = T))%>%
  ungroup()%>%
  mutate(code = fct_reorder(code,code_diff))%>%
  # adding pareto rank 
  left_join(pareto, by = "code")

  

test<-final_df%>%
      filter(
             svc_grp_total_diff >= 5000,
             !is.na(svc_grp_total_diff))%>%
      top_n(10,svc_grp_total_diff)



#ggplot(total_cost_clust,aes(x = code,y = code_diff)) + geom_bar(stat = "identity")

write_csv(test,"cluster_data.csv")


### Feeding to algorithm

hclust_ward <- hclust(dist_mat, method = 'ward.D')

cut_avg <- cutree(hclust_ward, k = 10)

clust_ward<-as.data.frame(as.matrix(cut_avg))%>%
  mutate(cmh = rownames(as.data.frame(as.matrix(cut_avg))))%>%
  rename(cluster = V1)


clust_assignement <-clust_ward%>%filter(cmh == "Washtenaw")%>%select(cluster)%>%pull()

sister_cmhs<-clust_ward%>%
              left_join(score, by = "cmh")%>%
              filter(cluster == clust_assignement)%>%
              arrange(score)%>%
              slice(1:4)%>%
              select(cmh)%>%
              pull()













