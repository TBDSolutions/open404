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
      left_join(wshtw%>%select(fy,pct_change_w),by = "fy")%>%
      mutate(top = abs(pct_change_w - pct_change))%>%
      group_by(cmhsp)%>%
      summarise(top = mean(top,na.rm = T))%>%
  ungroup()%>%
    mutate(group =  case_when( cmhsp == 'Washtenaw' ~ "Washt",
                                       cmhsp == 'Kalamazoo' ~ "Kzoo",
                                       cmhsp == "Ottawa" ~ "Ottawa",
                                     #  top  <.05 ~ "sister CMH",
                                            TRUE ~ "unrelated"))


plot<-state_data%>%
    #filter(!groups == "unrelated")%>% 
  # filter(cmhsp %in% c('Washtenaw',"Kalamazoo","CMH for Central Michigan"))%>%
    group_by(cmhsp)%>%
  # filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
    mutate(prev_year = lag(TotalServed),
           pct_change = (TotalServed - prev_year)/prev_year)%>%
     ungroup()%>%
    left_join(groups, by = "cmhsp")%>%
    filter(!group %in% 'unrelated')
  
 



ggplot(plot,aes(x = fy,y = pct_change , group = cmhsp))+ geom_line(aes(color = group),size = 4)+
  ggtitle("year-over-year population growth")



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
  select(code,cost_pct_tot,rank)%>%
  

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



######### Build clustering set based on pop growth and service group cost to total 

ttl_pop_clust<-state_data%>%
  filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
  group_by(cmhsp)%>%
  mutate(prev_year = lag(TotalServed),
         pct_change = (TotalServed - prev_year)/prev_year)

wshtw_pop_clust<-state_data%>%
  filter(fy %in% c("2014","2015","2016","2017","2018"))%>%
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
#Getting the cmhs cost pct total for that service 

total_cost_clust<-data404%>%
  # filter(fy %in% c("2018","2017"))%>%
  filter(fy %in% c("2018"),
         code == "H2014" )%>%
 # filter(cmhsp %in% c('Washtenaw'))%>%
  #  filter(code == "H0043")
  group_by(cmhsp,code)%>%
  summarise(cost_pct_tot = sum(cost_pct_tot,na.rm = T))%>%
  select(cmhsp, cost_pct_tot)




clust_df<-groups%>%
          inner_join(total_cost_clust, by = c("cmhsp"))


########
# By service type cost to total 

pareto_svc_type<-pareto%>%
  left_join(service_groups%>%select(code,svc_type)%>%distinct(),by = "code")%>%
  group_by(svc_type)%>%
  summarise(cost = sum(cost_pct_tot))


pareto_list<-pareto%>%
  left_join(service_groups%>%select(code,svc_type)%>%distinct(),by = "code")%>%
  filter( svc_type == "Peer Services",
          !code == "H0043")%>%
  select(code)%>%pull()



total_cost_clust<-data404%>%
  select(cmhsp,svc_type,fy,cost_pct_tot)%>%
  filter(fy %in% c("2018","2017","2016","2015","2014"),

     svc_type == "Peer Services",

         )%>%
#   filter(cmhsp %in% c('Washtenaw'))%>%
  #  filter(code == "H0043")
  group_by(cmhsp,fy)%>%
  summarise(cost_pct_tot = sum(cost_pct_tot,na.rm = T))%>%
  ungroup()%>%
  group_by(cmhsp)%>%
  summarise(cost_pct_avg = mean(cost_pct_tot,na.rm = T),
            cost_pct_sd = sd(cost_pct_tot,na.rm = T))
 # select(cmhsp, cost_pct_tot)



hist(total_cost_clust$cost_pct_sd)

clust_df<-pop_trend%>%
  inner_join(total_cost_clust, by = c("cmhsp"))



### Get your clust on!!!! Then measure cost per case on each service group


row.names(clust_df)<-clust_df$cmhsp
clust_df$cmhsp<-NULL

clust_df_scaled <- as.data.frame(scale(clust_df))


dist_mat <- dist(clust_df_scaled, method = 'euclidean')

score<-as.data.frame(t(as.matrix(dist_mat)))%>%
      select(score = Washtenaw)%>%
      mutate(cmh = rownames(as.data.frame(t(as.matrix(dist_mat)))))

###

hclust_ward <- hclust(dist_mat, method = 'ward.D')

#plot
library(dendextend)
avg_dend_obj <- as.dendrogram(hclust_ward)
avg_col_dend <- color_branches(avg_dend_obj, h = 5)
plot(avg_col_dend)


plot(hclust_ward)

cut_avg <- cutree(hclust_avg, k = 5)



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


#################################
#Compare





peer<-data404%>%
      filter(
           fy == "2018",
           cmhsp %in% sister_cmhs,
           code %in% pareto_list
      )%>%
     group_by(cmhsp,code)%>%
     summarise(cost_per_case = mean(cost_per_case))%>%
     arrange(code,desc(cost_per_case))%>%
     group_by(code)%>%
     mutate(rank = row_number())%>%
  #   filter(code == "99214")%>%
     mutate(group = case_when(cmhsp == "Washtenaw" ~ "subject", 
                                TRUE ~ "control"))
     filter(rank == "1")


     
     
(max(test$cost_per_case)*.25)   
(max(test$cost_per_case) * 1.10)
     
ggplot(phy,aes(x = cmhsp,y = cost_per_case)) + 
  geom_bar(stat = "identity",alpha = .7, aes(fill = group))+
  ggtitle("code 99214",subtitle = "service type: Psychiatric and Medication Services")
      



ggplot(peer,aes(x = cmhsp,y = cost_per_case)) + 
  geom_bar(stat = "identity",alpha = .7, aes(fill = group))+
  ggtitle("code H2011",subtitle = "service type: Peer Services")


















