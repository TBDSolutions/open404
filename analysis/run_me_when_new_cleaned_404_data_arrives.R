library(tidyverse)
library(DBI)

mdhhs_idd_db <- DBI::dbConnect(odbc::odbc(), "ref_tables_bh")

#=================================================================#
# Creating new app dataset based on most recent FY addtions =====
#================================================================#

df404_19<-read_csv('data/clean/df_404.csv') %>%
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
  mutate(
    code_mod = paste0(code,replace_na(modifier,replace = ""))
  ) %>%
  mutate_at(
    vars(pihp_name,cmhsp,code,code_mod),
    list(~as.factor(.))
  )


svc_grps_19<-read_csv('data/clean/svc_grps.csv') %>%
  mutate(code = str_replace_all(code,"X",'0'))

persons_served_19<-read_csv('data/TotalServedAnnual.csv') %>%
  rename(cmhsp = CMHSP, fy = FY) %>%
  mutate(
    fy = as.factor(fy)
  )


#=======================================================#
# Creating metrics based on unique persons served ====
#=======================================================#
df404_19<-
  df404_19 %>%
  left_join(persons_served_19, by = c('cmhsp',"fy")) %>%
  mutate(
    cost_1k_served = round((cost/TotalServed) * 1000,2)
    ,pct_cmh_served = round((cases/TotalServed) * 100, 1)
    
    ) %>%
  mutate(
    code_shortDesc = as.factor(paste(df404_19$short_desc," (",(df404_19$code),")")),
    codeM_shortDesc = as.factor(paste(df404_19$short_desc," (",(df404_19$code_mod),")"))
  )%>%
  mutate(state = 'MI')


#================================================#
## Adding code and code modifiers =====
#================================================#

### Getting mod ref table from DB
mod_ref<-dbGetQuery(mdhhs_idd_db, 
                    "SELECT * FROM [ref_tables_bh].[dbo].[modifier_ref]")


# Creating two seperate tables

# one table to join based on modifier codes only
# the other table will join on those specific 
# modifiers that have a different meaning depending 
# on the code their attached to. 

modifiers<-mod_ref%>%
  filter(str_length(hcpc_code)==0)%>%
  select(modifier,mod_short_desc = short_desc)%>%
  distinct()

code_and_mod<-mod_ref%>%
  filter(str_length(hcpc_code)>0)%>%
  mutate(code_mod = str_squish(paste(hcpc_code,str_squish(modifier),sep = "")))%>%
  select(code_mod,cod_mod_short_desc = short_desc)%>%
  distinct()


# Creating the manipulations 
# needed to create the column format
# in the app 


df404_19<-
  df404_19%>%
  left_join(modifiers, by = c("modifier"))%>%
  left_join(code_and_mod, by = c("code_mod"))%>%
  mutate(
    new_modDesc = coalesce(mod_short_desc,cod_mod_short_desc),
    
    test = case_when(is.na(new_modDesc) == FALSE ~ paste(new_modDesc,"(",code_mod,")",sep = " "), 
                     TRUE ~ NA_character_),
    codeM_shortDesc = coalesce(test,as.character(codeM_shortDesc))
  )%>%
  select(-new_modDesc,-test,-mod_short_desc,-cod_mod_short_desc,-TotalServed) 



write_csv(df404_19,"404_app/datafiles/data404_newMod.csv")

write_csv(svc_grps_19,"404_app/datafiles/svc_grps.csv")








