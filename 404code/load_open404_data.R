# Load subMasterPlus datafile
library(RCurl) # requires install.packages("RCurl")
data <- getURL("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/subMasterPlus",
               ssl.verifypeer=0L, followlocation=1L)
writeLines(data,'temp.csv')
subMasterPlus <- read.csv('temp.csv')

# Load merged10to12 datafile (with census data for population rates from 2010-2012)
data <- getURL("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/merged10to12",
               ssl.verifypeer=0L, followlocation=1L)
writeLines(data,'temp.csv')
merged10to12 <- read.csv('temp.csv')

# Load alt10to12 datafile (with Lifeways corrections for 2012)
data <- getURL("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/alt10to12",
               ssl.verifypeer=0L, followlocation=1L)
writeLines(data,'temp.csv')
alt10to12 <- read.csv('temp.csv')

