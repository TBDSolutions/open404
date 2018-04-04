combineNeeds <- function(directory) {
  ## 'directory' is a char vector of len 1 indicating location of CSV files

  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  files <- list.files(directory,full.names = TRUE) # make list of full file names
  n <- length(files)
  df <- data.frame() #create empty data frame
  
  for (i in 1:n) {
    # loop through files, rbinding them together
    
    try(x <- read_excel(files[i],  col_names = F, sheet = 1))
    
    CMHSP <- as.character(x[1,6])
    FY <- as.character(x[2,2])
    FY <- str_sub(FY, start = -4L, end = -1L) #extract last 4 chars for Fiscal Year
    
    # identify CMHSP NA errors... Should I filter it??
    if( is.na(CMHSP) ){
      print(paste0("CMHSP error: " , CMHSP, " in ", files[i]))
    }
    
    # identify FY errors... Should I filter it?? (detroit11 might be fixable)
    if(is.na(as.numeric(FY))){
      print(paste0("FY error: ", FY, " in ", files[i]))
    }
    
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
        mutate( CMHSP = CMHSP, FY = FY, Undup = Undup,
               Item = gsub("[[:punct:]]", "", gsub(".*w","",Item))) %>%
        select(FY, CMHSP, Item, Desc, DD, MIA, MIC, Other, Undup) %>% 
        filter(!is.na(Item) & !is.na(Desc)) %>%
        filter(Item != "17" & Item != "2") %>%
        gather(Population, People, DD:Other) 
      
      # Identify the place where the Item is an empty string. (allegan16 -> due to * in an item column)
      if("" %in% x$Item){
        print(paste0("Empty item in ", files[i]))
      }
      # # identify the non integer numbers for people variable
      # if(x$People %>% is.na() %>% sum() > 0){
      #   print(paste0("NA people in ", files[i], i))
      # }else if(x$People %>% as.numeric() %>% is.na()){
      #   print(paste0("Non integer number of people in ", files[i]))
      # }
      
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
           People = as.numeric(People))
    # %>% filter(!is.na(People))
  
  library(car)
  df$Name <- recode(df$Item,"'1'='total_in';
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
                       '16b'='all_wait'")
  
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
  
#   library(gdata)
#   df$Phase <- reorder(df$Phase, new.order=c("Start","Entry","Screening","Eligibility","Waiting"))
  
  df <-
  df %>% 
    mutate(Item = factor(Item), Name = factor(Name), Phase = factor(Phase)) %>%
    select(FY, CMHSP, Population, Phase, Name, Item, Desc, People, Undup)
  
  return(df)
  
}


