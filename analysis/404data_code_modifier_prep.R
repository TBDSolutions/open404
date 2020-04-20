
# Script to bring on the latest modifier descriptions from the database. The end file
# will be send to the datafiles folder in the shiny app structre for use. 



library(DBI)
mdhhs_idd_db <- DBI::dbConnect(odbc::odbc(), "ref_tables_bh")

data404 <-read_csv("404_app/datafiles/data404.csv")%>%
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

data404<-data404%>%
  mutate(
    code_shortDesc = as.factor(paste(data404$short_desc," (",(data404$code),")")),
    codeM_shortDesc = as.factor(paste(data404$short_desc," (",(data404$code_mod),")"))
  )%>%
  mutate(state = 'MI')


### Getting mod ref table from DB
mod_ref<-dbGetQuery(mdhhs_idd_db, 
                    "SELECT * FROM [ref_tables_bh].[dbo].[modifier_ref]")


### Creating two seperate tables
#================================================
# one table to join based on modifier codes only
# the other table will join on those specific 
# modifiers that have a different meaning depending 
# on the code their attached to. 
#================================================
modifiers<-mod_ref%>%
  filter(str_length(hcpc_code)==0)%>%
  select(modifier,mod_short_desc = short_desc)%>%
  distinct()

code_and_mod<-mod_ref%>%
  filter(str_length(hcpc_code)>0)%>%
  mutate(code_mod = str_squish(paste(hcpc_code,str_squish(modifier),sep = "")))%>%
  select(code_mod,cod_mod_short_desc = short_desc)%>%
  distinct()

#==================================
# Creating the manipulations 
# needed to create the column format
# in the app 
#===================================

data404<-data404%>%
  left_join(modifiers, by = c("modifier"))%>%
  left_join(code_and_mod, by = c("code_mod"))%>%
  mutate(
    new_modDesc = coalesce(mod_short_desc,cod_mod_short_desc),
    
    test = case_when(is.na(new_modDesc) == FALSE ~ paste(new_modDesc,"(",code_mod,")",sep = " "), 
                     TRUE ~ NA_character_),
    codeM_shortDesc = coalesce(test,as.character(codeM_shortDesc))
  )%>%
  select(-new_modDesc,-test,-mod_short_desc,-cod_mod_short_desc)

#==================================
# Writing to datafiles folder that
# will be used by the shiny app
#==================================

write_csv(data404,"404_app/datafiles/data404_newMod.csv")
















