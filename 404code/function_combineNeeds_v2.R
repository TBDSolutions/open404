combineNeeds <- function(directory) {
  ## 'directory' is a char vector of len 1 indicating location of CSV files

  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(car)
  
  files <- list.files(directory,full.names = TRUE) # make list of full file names
  n <- length(files)
  df <- data.frame() #create empty data frame
  
  for (i in 1:n) {
    # loop through files, rbinding them together
    
    try(x <- read_excel(files[i],  col_names = F, sheet = 1))
    
    CMHSP <- as.character(x[1,6])
    FY <- as.character(x[2,2])
    FY <- str_sub(FY, start = -4L, end = -1L) #extract last 4 chars for Fiscal Year
    
    if (ncol(x) >= 6) {
      
      x <- x[-1:-6,] #remove rows 1-6
      x <- x[-8:-10,] #remove rows 8-10
      names(x)[1] <- "Item"   # Rename column 
      names(x)[2] <- "Desc"   # Rename column
      names(x)[3] <- "DD"   # Rename column
      names(x)[4] <- "MIA"   # Rename column 
      names(x)[5] <- "MIC"   # Rename column
      names(x)[6] <- "Other"   # Rename column
      
      Undup <- grepl("^y", x[2,3], ignore.case = T)  # does undup field start with "y"
      
      x <-
        x %>%
        mutate(CMHSP = CMHSP, FY = FY, Undup = Undup,
               Item = gsub("\\..*","",Item)) %>%
        mutate(Item = gsub("[[:punct:]]", "", Item)) %>%
        select(FY, CMHSP, Item, Desc, DD, MIA, MIC, Other, Undup) %>% 
        filter(!is.na(Item) & !is.na(Desc)) %>%
        filter(Item != "17" & Item != "2") %>%
        gather(Population, People, DD:Other) 
      
      df <- rbind(df, x)
      
    } else {
      # Print an error and put in dataframe to allow examination
      print(paste0("Too few columns in ", CMHSP, "_", FY, " file."))
      assign(paste0(CMHSP,"_", FY), x)
    }
    
  }
  
  df <-
    df %>%
    mutate(FY = factor(FY), CMHSP = factor(CMHSP),
           Desc = factor(Desc),
           People = as.numeric(People),
           # Berrien submitted on the old form in 2015
           Name = ifelse(as.integer(as.character(FY)) < 2015
                         | (as.integer(as.character(FY)) == 2015
                            & CMHSP == "Berrien Mental Health Authority"),
                         yes = car::recode(Item,"'1'='total_in';
                                      '3'='out_nonMH';
                                      '4'='seeking_SUD';
                                      '5'='req_CMHsvc';
                                      '6'='screened_out';
                                      '7'='assmt_sched';
                                      '8' = 'screened_out_other';
                                      '9'='no_elig_deter';
                                      '10'='rfr_to_FFS';
                                      '11'='rfr_to_MHP';
                                      '12'='not_eligible';
                                      '12a'='rfr_to_mh_Y';
                                      '12b'='rfr_to_mh_N';
                                      '13'='eligible';
                                      '14'='urgent_crit';
                                      '15'='immed_crit';
                                      '16'='waiting';
                                      '16a'='some_wait';
                                      '16b'='all_wait'"),
                         no = car::recode(Item,"'1'='total_in';
                                     '3'='out_nonMH';
                                     '4'='req_CMHsvc';
                                     '5'='screened_out';
                                     '6'='assmt_sched';
                                     '7'='screened_out_other';
                                     '8' = 'no_elig_deter';
                                     '9'='rfr_to_FFS';
                                     '10'='rfr_to_MHP';
                                     '11'='not_eligible';
                                     '11a'='rfr_to_mh_Y';
                                     '11b'='rfr_to_mh_N';
                                     '12'='eligible';
                                     '13'='urgent_crit';
                                     '14'='immed_crit';
                                     '15'='waiting';
                                     '15a'='some_wait';
                                     '15b'='all_wait'")))
  

  # if (as.integer(as.character(df$FY)) < 2015
  #     | (as.integer(as.character(df$FY)) == 2015 
  #        & df$CMHSP == "Berrien Mental Health Authority")) {
  # 
  #   df$Name <- recode(df$Item,"'1'='total_in';
  #                     '3'='out_nonMH';
  #                     '4'='seeking_SUD';
  #                     '5'='req_CMHsvc';
  #                     '6'='screened_out';
  #                     '7'='assmt_sched';
  #                     '8' = 'screened_out_other';
  #                     '9'='no_elig_deter';
  #                     '10'='rfr_to_FFS';
  #                     '11'='rfr_to_MHP';
  #                     '12'='not_eligible';
  #                     '12a'='rfr_to_mh_Y';
  #                     '12b'='rfr_to_mh_N';
  #                     '13'='eligible';
  #                     '14'='urgent_crit';
  #                     '15'='immed_crit';
  #                     '16'='waiting';
  #                     '16a'='some_wait';
  #                     '16b'='all_wait'")
  # } else # Map to new 2015 fields
  #   df$Name <- recode(df$Item,"'1'='total_in';
  #                     '3'='out_nonMH';
  #                     '4'='req_CMHsvc';
  #                     '5'='screened_out';
  #                     '6'='assmt_sched';
  #                     '7'='screened_out_other';
  #                     '8' = 'no_elig_deter';
  #                     '9'='rfr_to_FFS';
  #                     '10'='rfr_to_MHP';
  #                     '11'='not_eligible';
  #                     '11a'='rfr_to_mh_Y';
  #                     '11b'='rfr_to_mh_N';
  #                     '12'='eligible';
  #                     '13'='urgent_crit';
  #                     '14'='immed_crit';
  #                     '15'='waiting';
  #                     '15a'='some_wait';
  #                     '15b'='all_wait'")
  
  
  # recode to make groups
  df$Phase <- recode(df$Name,"'total_in'='Start';
                     'out_nonMH'='Entry';
                     'seeking_SUD'='Entry';
                     'req_CMHsvc'='Entry';
                     'screened_out'='Screening';
                     'assmt_sched'='Screening';
                     'screened_out_other' = 'Screening';
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
  
  df <-
  df %>% 
    mutate(Item = factor(Item), Name = factor(Name), Phase = factor(Phase)) %>%
    select(FY, CMHSP, Population, Phase, Name, Item, Desc, People, Undup)
  
  return(df)
  
}


