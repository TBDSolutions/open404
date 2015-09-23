combineNeeds <- function(directory) {
  ## 'directory' is a char vector of len 1 indicating location of CSV files
  files <- list.files(directory,full.names = TRUE) # make list of full file names
  n <- length(files)
  df <- data.frame() #create empty data frame
  for (i in 1:n) {
    # loop through files, rbinding them together
    x <- read.csv(files[i], header = F, fill = T, na.strings = "", strip.white = T)
    CMHSP <- as.character(x[1,6])
    FY <- as.character(x[2,2])
    library(stringr)
    FY <- str_sub(FY, start = -4L, end = -1L) #extract last 4 chars for Fiscal Year
    x <- x[-1:-6,] #remove rows 1-6
    x <- x[-8:-10,] #remove rows 8-10
    library(dplyr)
    library(tidyr)
    x <-
      x %>%
      mutate(CMHSP = CMHSP, FY = FY) %>%
      select(FY, CMHSP, Item = V1, Desc = V2, DD = V3, MIA = V4, MIC = V5, Other = V6) %>%
      filter(Item != "2" & Item != "17") %>%
      gather(Population, People, DD:Other) 
      
    df <- rbind(df, x)
  }
  df <- subset(df, is.na(Desc) == F)
  df <-
    df %>%
    mutate(Item = factor(df$Item),
           Desc = factor(df$Desc))
  library(car)
  df$Name <- recode(needs$Desc,"'Total # of people who telephoned or walked in'='total_in';
                                '# referred out due to non MH needs (of row 1)'='out_nonMH';
                                '# seeking substance abuse services (of row 1)'='seeking_SUD';
                                'Total # who requested services the CMHSP provides (of row1)'='req_CMHsvc';
                                'Of the # in Row 5 - How many people did not meet eligibility through phone or other screen'='screened_out';
                                'Of the # in Row 5 - How many people were scheduled for assessment'='assmt_sched';
                                'Of the # in Row 7 - How many did not receive eligibility determination (dropped out, no show, etc.) '='no_elig_deter';
                                'Of the # in Row 7 - how many were not served because they were MA FFS enrolled and referred to other MA FFS providers (not health plan)'='rfr_to_FFS';
                                'Of the # in Row 7 - how many were not served because they were MA HP enrolled and referred out to MA health plan'='rfr_to_MHP';
                                'Of the # in Row 7 - how many otherwise did not meet cmhsp non-entitlement eligibility criteria'='not_eligible';
                                'Of the # in row 12 - How many were referred out to other mental health providers'='rfr_to_mh_Y';
                                'Of the # in row 12 - How many were not referred out to other mental health providers'='rfr_to_mh_N';
                                'Of the # in Row 7 - How many people met the cmhsp eligibility criteria'='eligible';
                                'Of the # in Row 13 - How many met emergency/urgent conditions criteria'='urgent_crit';
                                'Of the # in Row 13 - How many met immediate admission criteria'='immed_crit';
                                'Of the # in Row 13 - How many were put on a waiting list'='waiting';
                                '     Of the # in row 16 - How many received some cmhsp services, but wait listed for other services'='some_wait';
                                'Of the # in row 16 - How many were wait listed for all cmhsp services'='all_wait'")
  
  # recode to make groups
  df$Phase <- recode(df$Name,"'total_in'='Start';
                                'out_nonMH'='Entry';
                                'seeking_SUD'='Entry';
                                'req_CMHsvc'='Entry';
                                'screened_out'='Screening';
                                'assmt_sched'='Screening';
                                'no_elig_deter'='Eligibility';
                                'rfr_to_FFS'='Eligibility';
                                'rfr_to_MHP'='Eligibility';
                                'not_eligible'='Eligibility';
                                'rfr_to_mh_Y'='Eligibility';
                                'rfr_to_mh_N'='Eligibility';
                                'eligible'='Eligibility';
                                'urgent_crit'='Placement';
                                'immed_crit'='Placement';
                                'waiting'='Waiting';
                                'some_wait'='Waiting';
                                'all_wait'='Waiting'")
  
  library(gdata)
  df$Phase <- reorder(df$Phase, new.order=c("Start","Entry","Screening","Eligibility","Waiting"))
  
  df$FY <- as.factor(df$FY)
  df$CMHSP <- as.factor(df$CMHSP)
  df$People <- as.numeric(df$People)
  
  return(df)
}

needs <- combineNeeds(directory = "network_adequacy/data/needsAssess")

needphase <-
needs %>%
  tbl_df() %>%
  filter(Name %in% c('total_in','req_CMHsvc','assmt_sched','eligible','waiting')) %>%
  droplevels %>%
  group_by(FY,CMHSP,Population,Phase) %>%
  summarise(People = sum(People)) %>%
  ungroup() %>%
  spread(Phase,People) %>%
  mutate(Placement = Eligibility - Waiting) %>%
  gather(Phase,People, Start:Placement) %>%
  filter(Phase != "Waiting") %>%
  group_by(FY,CMHSP,Population,Phase) %>%
  summarise(People = sum(People)) %>%
  mutate(CMHpop = paste(CMHSP, Population, ""),
         Change = round((People - lag(People))
                        /(lag(People)/100), digits = 1),
         Running = round(People
                         /(first(People)/100), digits = 1))

needphaseCMH <-
  needs %>%
  tbl_df() %>%
  filter(Name %in% c('total_in','req_CMHsvc','assmt_sched','eligible','waiting')
         & Population != "Other") %>%
  droplevels %>%
  group_by(FY,CMHSP,Population,Phase) %>%
  summarise(People = sum(People)) %>%
  ungroup() %>%
  spread(Phase,People) %>%
  mutate(Placement = Eligibility - Waiting) %>%
  gather(Phase,People, Start:Placement) %>%
  filter(Phase != "Waiting") %>%
  group_by(FY,CMHSP,Phase) %>%
  summarise(People = sum(People)) %>%
  mutate(Change = round((People - lag(People))
                        /(lag(People)/100), digits = 1),
         Running = round(People
                         /(first(People)/100), digits = 1))

needspread <-
needs %>%
  select(FY,CMHSP,Name,Population,People) %>%
  spread(Name,People) %>%
  group_by(FY,CMHSP,Population) %>%
  summarize(throughput = round((sum(eligible)-sum(waiting))/sum(total_in)*100, digits = 1),
         in_nonMH = round(sum(out_nonMH)/sum(total_in)*100, digits = 1),
         in_req = round(sum(req_CMHsvc)/sum(total_in)*100, digits = 1),
         req_screenout = round(sum(screened_out)/sum(req_CMHsvc)*100, digits = 1),
         assmt_ffs_HP = round((sum(rfr_to_FFS)+sum(rfr_to_MHP))/sum(assmt_sched)*100, digits = 1),
         inelig_rfrMH = round(sum(rfr_to_mh_Y)/sum(not_eligible)*100, digits = 1),
         elig_urg_imm = round((sum(urgent_crit)+sum(immed_crit))/sum(eligible)*100, digits = 1),
         elig_wait = round(sum(waiting)/sum(eligible)*100, digits = 1)) 


no_elig_deter/assmt_sched # Potential service need indicated

needprocess <-
  needs