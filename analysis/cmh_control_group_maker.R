
library(tidyverse)
library(tidycensus)
library(dendextend)
library(cluster)    
library(factoextra) 

devtools::install_github("j-hagedorn/TBDfun", ref = "master")

library(TBDfun)


########################### Building the dataset ###############################
#====================
# Population Density 
#====================


land_area_by_county<-read_csv("sister_cmh_finder/2010LandArea.csv")%>%
                     filter(str_starts(FIPS,"26"))%>%
                     mutate(FIPS = as.character(FIPS))

total_pop_by_county <- get_acs(geography = "county", 
              variables = "B00001_001", 
              geometry = FALSE,
              state = 'MI',
              year = 2018
              #  options(tigris_use_cache = TRUE)
              )

pop_density_by_county<-land_area_by_county%>%
                       left_join(total_pop_by_county, by = c("FIPS" = "GEOID"))%>%
                       select(FIPS,county = NAME,land_area = LND110210D, population_by_county = estimate)%>%
                       mutate(county = str_squish(str_remove_all(county,"County, Michigan")))

#=============================
# Converting counties to CMHs
#=============================

pop_density_by_county<-pop_density_by_county%>%
     mutate(cmh = case_when(county %in% c("Allegan") ~ 'Allegan',
                              county %in% c("Iosco","Ogemaw","Oscoda") ~ 'AuSable Valley',
                              county %in% c("Barry") ~ 'Barry',
                              county %in% c("Oakland") ~ 'Oakland',
                              county %in% c("St. Clair") ~'St. Clair',
                              county %in% c("St. Joseph") ~ 'St. Joseph',
                              county %in% c("Arenac","Bay") ~ 'Bay-Arenac',
                              county %in% c("Berrien") ~ 'Berrien',
                              county %in% c("Clinton","Eaton","Ingham") ~ 'CEI',
                              county %in% c("Clare","Gladwin", "Isabella","Mecosta","Midland","Osceola")~ 'Central Michigan',
                              county %in% c("Baraga","Houghton","Keweenaw","Ontonagon") ~ 'Copper Country',
                              county %in% c("Wayne") ~'Detroit-Wayne',
                              county %in% c("Genesee") ~ 'Genesee',
                              county %in% c("Gogebic") ~ 'Gogebic',
                              county %in% c("Gratiot") ~ 'Gratiot',
                              county %in% c("Chippewa","Mackinac","Schoolcraft") ~'Hiawatha',
                              county %in% c("Huron") ~ 'Huron',
                              county %in% c("Ionia") ~'Ionia',
                              county %in% c("Kalamazoo") ~'Kalamazoo',
                              county %in% c("Lapeer") ~ 'Lapeer',
                              county %in% c("Lenawee") ~ 'Lenawee',
                              county %in% c("Hillsdale","Jackson") ~ 'Lifeways',
                              county %in% c("Livingston") ~ 'Livingston',
                              county %in% c("Macomb") ~ 'Macomb',
                              county %in% c("Benzie","Manistee") ~ 'Manistee-Benzie',
                              county %in% c("Monroe") ~ 'Monroe',
                              county %in% c("Montcalm")~'Montcalm',
                              county %in% c("Muskegon") ~ 'Muskegon',
                              county %in% c("Kent")~ 'Network180',
                              county %in% c("Newaygo") ~ 'Newaygo',
                              county %in% c("Antrim","Charlevoix","Cheboygan","Emmet","Kalkaska","Otsego") ~ 'North Country',
                              county %in% c("Alcona","Alpena","Montmorency","Presque Isle")~ 'Northeast Michigan',
                              county %in% c("Crawford","Grand Traverse","Leelanau", "Missaukee","Roscommon","Wexford") ~ 'Northern Lakes',
                              county %in% c("Dickinson","Iron","Menominee") ~ 'Northpointe',
                              county %in% c("Ottawa")~ 'Ottawa',
                              county %in% c("Alger","Delta","Luce","Marquette")~'Pathways',
                              county %in% c("Branch") ~'Pines',
                              county %in% c("Saginaw")~ 'Saginaw',
                              county %in% c("Sanilac")~ 'Sanilac',
                              county %in% c("Shiawassee")~ 'Shiawassee',
                              county %in% c("Calhoun") ~ 'Summit Pointe',
                              county %in% c("Tuscola")~'Tuscola',
                              county %in% c("Van Buren") ~ 'Van Buren',
                              county %in% c("Cass") ~ 'Woodlands',
                              county %in% c("Lake","Mason","Oceana")~ 'West Michigan',
                              county %in% c("Washtenaw") ~  'Washtenaw',
                              TRUE ~ NA_character_))

# ref table for other county data
county_cmh_ref_table <- pop_density_by_county%>%
  select(FIPS,county,cmh)%>%
  filter(!is.na(county))


pop_density_by_cmh<-pop_density_by_county%>%
                      filter(!is.na(county))%>%
                      group_by(cmh)%>%
                      summarise(total_pop = sum(population_by_county,na.rm = T),
                                total_area = sum(land_area,na.rm = T),
                                cmh_pop_den = round(total_pop/total_area,2))%>%
                       ungroup()%>%
                      select(cmh,cmh_pop_den)
#=======================
# Community Indexes
#=======================

cdc_index<-read_csv("sister_cmh_finder/county_cdc_social_vulnerability.csv")%>%
           select(FIPS,socioeconomic_score = RPL_THEME1, 
                  household_compostion = RPL_THEME2, 
                  minority = RPL_THEME3, 
                  trans_housing = RPL_THEME4
                  
                  )%>%
           mutate(FIPS = as.character(FIPS))%>%
           left_join(county_cmh_ref_table, by = c("FIPS"))%>%
           group_by(cmh)%>%
            summarise_at(
              
              vars(socioeconomic_score,household_compostion,
                   minority,trans_housing),
              list(~mean(., na.rm = T))
            )


#=======================
# CMH population Growth
#=======================

cmh_population_growth<-read_csv("sister_cmh_finder/TotalServedAnnual.csv")%>%
                        rename(cmh = CMHSP, fy = FY)%>%
                        mutate(fy = as.factor(fy))%>%
                        filter(fy %in% c("2015","2016","2017","2018"))%>%
                        group_by(cmh)%>%
                        mutate(prev_year = lag(TotalServed),
                               pct_change = (TotalServed - prev_year)/prev_year)%>%
                        filter(!fy == "2015")%>%
                        ungroup()%>%
                        group_by(cmh)%>%
                        summarise(avg_cmh_pop_growth = mean(pct_change,na.rm = T))%>%
                        mutate(cmh = case_when(str_detect(cmh,"CMH for Central Michigan") ~ 'Central Michigan',
                                                str_detect(cmh,"Clinton Eaton Ingham") ~ 'CEI',
                                                TRUE ~ cmh))


#=========================
# Bringing tables together
#=========================


variables_df<-cmh_population_growth%>%
              left_join(pop_density_by_cmh, by = "cmh")%>%
              left_join(cdc_index, by = "cmh")



########################### Finding the Sister CMHS ###############################

#========================================
# Formatting and scaling variables 
#========================================
row.names(variables_df)<-variables_df$cmh
variables_df$cmh<-NULL

variables_df_scaled <- as.data.frame(scale(variables_df))

#==============================================================
# calulating the distane matrix. Euclidean was chosen 
# because it is the most recognized, used and intuitive method 
# for calulating the distance between an arbitrary amount of 
# points on a plane. Mquitty was chosen as the linkage method 
# because it best represented the scores on the distance matrix
# and good sense in general. 
#===============================================================
dist_mat <- dist(variables_df_scaled, method = 'euclidean')

score<-as.data.frame(t(as.matrix(dist_mat)))%>%
  select(score = Washtenaw)%>%
  mutate(cmh = fct_reorder(as.factor(rownames(as.data.frame(t(as.matrix(dist_mat))))),score))
  
ggplot(score,aes(y = score, x = cmh)) + geom_point()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

#=============================================
# optimal cluster diagram using the 
# average silhouettte method The silhouette
# value is a measure of how similar an object 
# is to its own cluster (cohesion) compared to 
# other clusters (separation)
#=============================================

fviz_nbclust(variables_df_scaled, FUN = hcut, method = "silhouette")


#===========================================
# visualizing the results with a dendogram
#===========================================

hclust_ward <- hclust(dist_mat, method = 'mcquitty')%>%
  as.dendrogram()

par(mar=c(1,1,1,7))
hclust_ward %>%
  set("labels_col", value = c("skyblue", "orange", "red","#7B3A04","#FD8B93","#700CBC","#559E54","#FCBA12","#236AB9"), k=9) %>%
  set("branches_k_color", c("skyblue", "orange", "red","#7B3A04","#FD8B93","#700CBC","#559E54","#FCBA12","#236AB9"), k = 9) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 350, lty = 2)
mtext(side=3, line=3, at=-0.07, adj=0, cex=1, mytitle)
mtext(side=3, line=2, at=-0.07, adj=0, cex=0.7, mysubtitle)
rect.dendrogram( hclust_ward, k=7,which = 3,horiz = TRUE,col=rgb(0.1, 0.2, 0.1, 0.1),
                 border = 1,lty = 5)
title(main = "CMH Cluster Groupings with Washtenaw's Group Highlighted",font.main= 4, col.main= "#292F33")


write_csv(variables_df,"C:/Users/joet/Documents/GitHub/washt/sister cmh data/variables_df.csv")


