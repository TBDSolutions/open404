library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

files <- list.files("C:/Users/Josh/SkyDrive/Projects/NeedsAssessment/data",full.names = TRUE) # make list of full file names
n <- length(files)
df <- data.frame() #create empty data frame

for (i in 1:n) {
  # loop through files, rbinding them together
  
  try(x <- read_excel(files[i],  col_names = F, sheet = 1))
  CMHSP <- as.character(x[1,6])
  FY <- as.character(x[2,2])
  FY <- str_sub(FY, start = -4L, end = -1L) #extract last 4 chars for Fiscal Year
  x <- x[-1:-6,] #remove rows 1-6
  x <- x[-8:-10,] #remove rows 8-10
  names(x)[1] <- "Item"   # Rename column 
  names(x)[2] <- "Desc"   # Rename column
  names(x)[3] <- "DD"   # Rename column
  names(x)[4] <- "MIA"   # Rename column 
  names(x)[5] <- "MIC"   # Rename column
  names(x)[6] <- "Other"   # Rename column

  x <-
  x %>%
    mutate(CMHSP = CMHSP, FY = FY,
           Item = gsub("\\..*","",Item)) %>%
    select(FY, CMHSP, Item, Desc, DD, MIA, MIC, Other) %>%
    filter(!is.na(Item) & !is.na(Desc)) %>%
    filter(Item != "17") %>%
    gather(Population, People, DD:Other) 
  
  assign(paste0(CMHSP,"_", FY), x)
  
}

# d <- lapply(files, read_excel)
# v <- as.vector(lapply(d, length))
# Convert #2 response re: undupl to logical var
grepl("^y", , ignore.case = T)


try(
  x <-
    x %>%
    mutate(CMHSP = CMHSP, FY = FY) %>%
    select(FY, CMHSP, Item = X1, Desc = X2, DD = X3, MIA = X4, MIC = X5, Other = X6) %>%
    filter(Item != "2" & Item != "17") %>%
    gather(Population, People, DD:Other) 