
#############################################################
########                    Open404                 ######### 
########                                            #########
########      Formats multiple years of MDCH CMH    #########
########      Sub-Element Cost Report(404) Data     #########
########                                            #########
########         by Joshua Hagedorn                 #########
########                                            #########
######## 'Open404' is licensed under a              #########
########  Creative Commons BY-NC-SA                 #########
########  Attribution-NonCommercial-ShareAlike      #########
########  3.0 Unported License.                     #########
########                                            #########
########  'Open404' is a revision of 're404er'      #########
########   by Hope Network (C. Idema, J. Dietsch,   #########
########                    J. Hagedorn)            #########
########                                            #########
########                            August 2014     #########
#############################################################

## Currently, the cost reports are separated based on year and population
## To have a meaningful master dataset, first have to compile and format each individual dataset

# Note: 
## When adding future data, just repeat the upload and formatting code described
## below, changing the FY code to correspond to the current year being loaded in

createMaster <- function(){

library(plyr)

##########################################################################################
## Reading in each individual dataset, beginning with FY 2006, developmentally disabled ##
##########################################################################################

dd_2006<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-DD_FY2006.csv', sep=',', header=TRUE)
attach(dd_2006)

#structure of dataframe
#str(dd_2006)

#deleting Text4, will create new CMHSP variable
dd_2006$Text4 <- NULL

#adding NA's where there is missing data
dd_2006[dd_2006=='']<-NA

#deleting rows with all NA
dd_2006<-dd_2006[rowSums(is.na(dd_2006)) !=ncol(dd_2006),]

#renaming variables
#install.packages('reshape')

library(reshape)
NewNames<-c(SumofDD.Cases = 'SumOfCases', SumOfDD.Units = 'SumOfUnits', SumofDD.Cost = 'SumOfCost')
dd_2006<-rename(dd_2006, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=12604, length=12604, label = c('2006'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=274, length=12604, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))

#creating population variable
Population<-gl(n=1, k=12604, length=12604, label=c('DD'))

#binding new variables to dataframe
dd_2006<-cbind(dd_2006, CMHSP, FY, Population)

#reordering columns 
dd_2006<-dd_2006[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
dd_2006$SumOfCost<-gsub("$", fixed=TRUE, "", dd_2006$SumOfCost)
dd_2006$SumOfCost<-gsub(".00", fixed=TRUE, "", dd_2006$SumOfCost)
dd_2006$SumOfCost<-gsub(",", fixed=TRUE, "", dd_2006$SumOfCost)

dd_2006$SumOfCases<-gsub(".00", fixed=TRUE, "", dd_2006$SumOfCases)
dd_2006$SumOfCases<-gsub(",", fixed=TRUE, "", dd_2006$SumOfCases)

dd_2006$SumOfUnits<-gsub(".00", fixed=TRUE, "", dd_2006$SumOfUnits)
dd_2006$SumOfUnits<-gsub(",", fixed=TRUE, "", dd_2006$SumOfUnits)

#changing sumofcases, cost, and units to numeric
dd_2006<- transform(dd_2006, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                    SumOfCost = as.numeric(SumOfCost))

#number of NAs in each variable
#sum(is.na(dd_2006$SumOfCost))
#sum(is.na(dd_2006$SumOfCases))
#sum(is.na(dd_2006$SumOfUnits))

#Printing first 10 rows of dataframe to see what it looks like
head(dd_2006, n=10)

#######################################################################
## Repeating Process for DD 2007 ######################################
#######################################################################
dd_2007<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-DD_FY2007.csv', sep=',', header=TRUE)
attach(dd_2007)

#structure of dataframe
#str(dd_2007)

#deleting Text4, will create new CMHSP variable
dd_2007$Text4<-NULL

#adding NA's where there is missing data
dd_2007[dd_2007=='']<-NA

#deleting rows with all NA
dd_2007<-dd_2007[rowSums(is.na(dd_2007)) !=ncol(dd_2007),]

#renaming variables
NewNames<-c(SumofDD.Cases = 'SumOfCases', SumOfDD.Units = 'SumOfUnits', SumofDD.Cost = 'SumOfCost')
dd_2007<-rename(dd_2007, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14260, length=14260, label = c('2007'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=310, length=14260, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))

#creating population variable
Population<-gl(n=1, k=14260, length=14260, label=c('DD'))

#binding new variables to dataframe
dd_2007<-cbind(dd_2007, CMHSP, FY, Population)

#reordering columns 
dd_2007<-dd_2007[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
dd_2007$SumOfCost<-gsub("$", fixed=TRUE, "", dd_2007$SumOfCost)
dd_2007$SumOfCost<-gsub(".00", fixed=TRUE, "", dd_2007$SumOfCost)
dd_2007$SumOfCost<-gsub(",", fixed=TRUE, "", dd_2007$SumOfCost)

dd_2007$SumOfCases<-gsub(".00", fixed=TRUE, "", dd_2007$SumOfCases)
dd_2007$SumOfCases<-gsub(",", fixed=TRUE, "", dd_2007$SumOfCases)

dd_2007$SumOfUnits<-gsub(".00", fixed=TRUE, "", dd_2007$SumOfUnits)
dd_2007$SumOfUnits<-gsub(",", fixed=TRUE, "", dd_2007$SumOfUnits)

#changing sumofcases, cost, and units to numeric
dd_2007<- transform(dd_2007, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                    SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for DD 2008 ######################################
#######################################################################
dd_2008<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-DD_FY2008.csv', sep=',', header=TRUE)
attach(dd_2008)

#structure of dataframe
#str(dd_2008)

#deleting Text4, will create new CMHSP variable
dd_2008$Text4<-NULL

#adding NA's where there is missing data
dd_2008[dd_2008=='']<-NA

#deleting rows with all NA
dd_2008<-dd_2008[rowSums(is.na(dd_2008)) !=ncol(dd_2008),]

#renaming variables
NewNames<-c(SumofDD.Cases = 'SumOfCases', SumOfDD.Units = 'SumOfUnits', SumofDD.Cost = 'SumOfCost')
dd_2008<-rename(dd_2008, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14076, length=14076, label = c('2008'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=306, length=14076, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))

#creating population variable
Population<-gl(n=1, k=14076, length=14076, label=c('DD'))

#binding new variables to dataframe
dd_2008<-cbind(dd_2008, CMHSP, FY, Population)

#reordering columns 
dd_2008<-dd_2008[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
dd_2008$SumOfCost<-gsub("$", fixed=TRUE, "", dd_2008$SumOfCost)
dd_2008$SumOfCost<-gsub(".00", fixed=TRUE, "", dd_2008$SumOfCost)
dd_2008$SumOfCost<-gsub(",", fixed=TRUE, "", dd_2008$SumOfCost)

dd_2008$SumOfCases<-gsub(".00", fixed=TRUE, "", dd_2008$SumOfCases)
dd_2008$SumOfCases<-gsub(",", fixed=TRUE, "", dd_2008$SumOfCases)

dd_2008$SumOfUnits<-gsub(".00", fixed=TRUE, "", dd_2008$SumOfUnits)
dd_2008$SumOfUnits<-gsub(",", fixed=TRUE, "", dd_2008$SumOfUnits)

#changing sumofcases, cost, and units to numeric
dd_2008<- transform(dd_2008, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                    SumOfCost = as.numeric(SumOfCost))

#######################################################################
## Repeating Process for DD 2009 ######################################
#######################################################################
dd_2009<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-DD_FY2009.csv', sep=',', header=TRUE)
attach(dd_2009)

#structure of dataframe
#str(dd_2009)

#By deleting the Text 4 variable, there are rows in the dataframe 
#that are completely empty, first we'll add NA's where there is missing data
dd_2009$Text4<-NULL

#adding NA's where there is missing data
dd_2009[dd_2009=='']<-NA

#deleting rows with all NA
dd_2009<-dd_2009[rowSums(is.na(dd_2009)) !=ncol(dd_2009),]

#renaming variables
NewNames<-c(SumofDD.Cases = 'SumOfCases', SumOfDD.Units = 'SumOfUnits', SumofDD.Cost = 'SumOfCost')
dd_2009<-rename(dd_2009, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14352, length=14352, label = c('2009'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=312, length=14352, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))

#creating population variable
Population<-gl(n=1, k=14352, length=14352, label=c('DD'))

#binding new variables to dataframe
dd_2009<-cbind(dd_2009, CMHSP, FY, Population)

#reordering columns 
dd_2009<-dd_2009[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
dd_2009$SumOfCost<-gsub("$", fixed=TRUE, "", dd_2009$SumOfCost)
dd_2009$SumOfCost<-gsub(".00", fixed=TRUE, "", dd_2009$SumOfCost)
dd_2009$SumOfCost<-gsub(",", fixed=TRUE, "", dd_2009$SumOfCost)

dd_2009$SumOfCases<-gsub(".00", fixed=TRUE, "", dd_2009$SumOfCases)
dd_2009$SumOfCases<-gsub(",", fixed=TRUE, "", dd_2009$SumOfCases)

dd_2009$SumOfUnits<-gsub(".00", fixed=TRUE, "", dd_2009$SumOfUnits)
dd_2009$SumOfUnits<-gsub(",", fixed=TRUE, "", dd_2009$SumOfUnits)

#changing sumofcases, cost, and units to numeric
dd_2009<- transform(dd_2009, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                    SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for DD 2010 ######################################
#######################################################################
dd_2010<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-DD_FY2010.csv', sep=',', header=TRUE)
attach(dd_2010)

#structure of dataframe
#str(dd_2010)

#deleting Text4, will create new CMHSP variable
dd_2010$Text4<-NULL

#adding NA's where there is missing data
dd_2010[dd_2010=='']<-NA

#deleting rows with all NA
dd_2010<-dd_2010[rowSums(is.na(dd_2010)) !=ncol(dd_2010),]

#renaming variables
NewNames<-c(SumofDD.Cases = 'SumOfCases', SumOfDD.Units = 'SumOfUnits', SumofDD.Cost = 'SumOfCost')
dd_2010<-rename(dd_2010, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=15088, length=15088, label = c('2010'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=328, length=15088, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=15088, length=15088, label=c('DD'))

#binding new variables to dataframe
dd_2010<-cbind(dd_2010, CMHSP, FY, Population)

#reordering columns 
dd_2010<-dd_2010[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
dd_2010$SumOfCost<-gsub("$", fixed=TRUE, "", dd_2010$SumOfCost)
dd_2010$SumOfCost<-gsub(".00", fixed=TRUE, "", dd_2010$SumOfCost)
dd_2010$SumOfCost<-gsub(",", fixed=TRUE, "", dd_2010$SumOfCost)

dd_2010$SumOfCases<-gsub(".00", fixed=TRUE, "", dd_2010$SumOfCases)
dd_2010$SumOfCases<-gsub(",", fixed=TRUE, "", dd_2010$SumOfCases)

dd_2010$SumOfUnits<-gsub(".00", fixed=TRUE, "", dd_2010$SumOfUnits)
dd_2010$SumOfUnits<-gsub(",", fixed=TRUE, "", dd_2010$SumOfUnits)

#changing sumofcases, cost, and units to numeric
dd_2010<- transform(dd_2010, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                    SumOfCost = as.numeric(SumOfCost))


#dd_2010
#######################################################################
## Repeating Process for DD 2011 ######################################
#######################################################################
dd_2011<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-DD_FY2011.csv', sep=',', header=TRUE)
attach(dd_2011)

#structure of dataframe
#str(dd_2011)

#deleting Text4, will create new CMHSP variable
dd_2011$Text4<-NULL

nrow(dd_2011)
#adding NA's where there is missing data
dd_2011[dd_2011=='']<-NA

#deleting rows with all NA
dd_2011<-dd_2011[rowSums(is.na(dd_2011)) !=ncol(dd_2011),]

#renaming variables
NewNames<-c(SumofDD.Cases = 'SumOfCases', SumOfDD.Units = 'SumOfUnits', SumofDD.Cost = 'SumOfCost')
dd_2011<-rename(dd_2011, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=15364, length=15364, label = c('2011'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=334, length=15364, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=15364, length=15364, label=c('DD'))

#binding new variables to dataframe
dd_2011<-cbind(dd_2011, CMHSP, FY, Population)

str(dd_2011)
#reordering columns 
dd_2011<-dd_2011[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
dd_2011$SumOfCost<-gsub("$", fixed=TRUE, "", dd_2011$SumOfCost)
dd_2011$SumOfCost<-gsub(".00", fixed=TRUE, "", dd_2011$SumOfCost)
dd_2011$SumOfCost<-gsub(",", fixed=TRUE, "", dd_2011$SumOfCost)

dd_2011$SumOfCases<-gsub(".00", fixed=TRUE, "", dd_2011$SumOfCases)
dd_2011$SumOfCases<-gsub(",", fixed=TRUE, "", dd_2011$SumOfCases)

dd_2011$SumOfUnits<-gsub(".00", fixed=TRUE, "", dd_2011$SumOfUnits)
dd_2011$SumOfUnits<-gsub(",", fixed=TRUE, "", dd_2011$SumOfUnits)

#changing sumofcases, cost, and units to numeric
dd_2011<- transform(dd_2011, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                    SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for DD 2012 ######################################
#######################################################################
dd_2012<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-DD_FY2012.csv', sep=',', header=TRUE)
attach(dd_2012)

#structure of dataframe
#str(dd_2012)

#deleting Text4, will create new CMHSP variable
dd_2012$Text4<-NULL

#adding NA's where there is missing data
dd_2012[dd_2012=='']<-NA

#deleting rows with all NA
dd_2012<-dd_2012[rowSums(is.na(dd_2012)) !=ncol(dd_2012),]

#renaming variables
NewNames<-c(SumofDD.Cases = 'SumOfCases', SumOfDD.Units = 'SumOfUnits', SumofDD.Cost = 'SumOfCost')
dd_2012<-rename(dd_2012, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=15410, length=15410, label = c('2012'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=335, length=15410, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=15410, length=15410, label=c('DD'))

#binding new variables to dataframe
dd_2012<-cbind(dd_2012, CMHSP, FY, Population)

#reordering columns 
dd_2012<-dd_2012[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
dd_2012$SumOfCost<-gsub("$", fixed=TRUE, "", dd_2012$SumOfCost)
dd_2012$SumOfCost<-gsub(".00", fixed=TRUE, "", dd_2012$SumOfCost)
dd_2012$SumOfCost<-gsub(",", fixed=TRUE, "", dd_2012$SumOfCost)

dd_2012$SumOfCases<-gsub(".00", fixed=TRUE, "", dd_2012$SumOfCases)
dd_2012$SumOfCases<-gsub(",", fixed=TRUE, "", dd_2012$SumOfCases)

dd_2012$SumOfUnits<-gsub(".00", fixed=TRUE, "", dd_2012$SumOfUnits)
dd_2012$SumOfUnits<-gsub(",", fixed=TRUE, "", dd_2012$SumOfUnits)

#changing sumofcases, cost, and units to numeric
dd_2012<- transform(dd_2012, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                    SumOfCost = as.numeric(SumOfCost))

#######################################################################
## Repeating Process for DD 2013 ######################################
#######################################################################
dd_2013<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-DD_FY2013.csv', sep=',', header=TRUE)
attach(dd_2013)

#structure of dataframe
#str(dd_2013)

#deleting Text4, will create new CMHSP variable
dd_2013$Text4<-NULL

#adding NA's where there is missing data
dd_2013[dd_2013=='']<-NA

#deleting rows with all NA
dd_2013<-dd_2013[rowSums(is.na(dd_2013)) !=ncol(dd_2013),]

#renaming variables
NewNames<-c(SumofDD.Cases = 'SumOfCases', SumOfDD.Units = 'SumOfUnits', SumofDD.Cost = 'SumOfCost')
dd_2013<-rename(dd_2013, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=18768, length=18768, label = c('2013'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=408, length=18768, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=18768, length=18768, label=c('DD'))

#binding new variables to dataframe
dd_2013<-cbind(dd_2013, CMHSP, FY, Population)

#reordering columns 
dd_2013<-dd_2013[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
dd_2013$SumOfCost<-gsub("$", fixed=TRUE, "", dd_2013$SumOfCost)
dd_2013$SumOfCost<-gsub(".00", fixed=TRUE, "", dd_2013$SumOfCost)
dd_2013$SumOfCost<-gsub(",", fixed=TRUE, "", dd_2013$SumOfCost)

dd_2013$SumOfCases<-gsub(".00", fixed=TRUE, "", dd_2013$SumOfCases)
dd_2013$SumOfCases<-gsub(",", fixed=TRUE, "", dd_2013$SumOfCases)

dd_2013$SumOfUnits<-gsub(".00", fixed=TRUE, "", dd_2013$SumOfUnits)
dd_2013$SumOfUnits<-gsub(",", fixed=TRUE, "", dd_2013$SumOfUnits)

#changing sumofcases, cost, and units to numeric
dd_2013<- transform(dd_2013, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                    SumOfCost = as.numeric(SumOfCost))

#######################################################################
## Repeating Process for MIA 2006 #####################################
#######################################################################
MIA_2006<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIA_FY2006.csv', sep=',', header=TRUE)
attach(MIA_2006)

#structure of dataframe
#str(MIA_2006)

#deleting CMHName, will create new CMHSP variable
MIA_2006$CMHName<-NULL

#adding NA's where there is missing data
MIA_2006[MIA_2006=='']<-NA

#deleting rows with all NA
MIA_2006<-MIA_2006[rowSums(is.na(MIA_2006)) !=ncol(MIA_2006),]

#renaming variables
NewNames<-c(SumofMIA.Cases = 'SumOfCases', FirstofMIA.Units = 'SumOfUnits', SumofMIA.Cost = 'SumOfCost')
MIA_2006<-rename(MIA_2006, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=11776, length=11776, label = c('2006'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=256, length=11776, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=11776, length=11776, label=c('MIA'))

#binding the new variables to the dataframe
MIA_2006<-cbind(MIA_2006, CMHSP, FY, Population)

#reordering columns 
MIA_2006<-MIA_2006[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIA_2006$SumOfCost<-gsub("$", fixed=TRUE, "", MIA_2006$SumOfCost)
MIA_2006$SumOfCost<-gsub(".00", fixed=TRUE, "", MIA_2006$SumOfCost)
MIA_2006$SumOfCost<-gsub(",", fixed=TRUE, "", MIA_2006$SumOfCost)

MIA_2006$SumOfCases<-gsub(".00", fixed=TRUE, "", MIA_2006$SumOfCases)
MIA_2006$SumOfCases<-gsub(",", fixed=TRUE, "", MIA_2006$SumOfCases)

MIA_2006$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIA_2006$SumOfUnits)
MIA_2006$SumOfUnits<-gsub(",", fixed=TRUE, "", MIA_2006$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIA_2006<- transform(MIA_2006, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIA 2007 #####################################
#######################################################################
MIA_2007<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIA_FY2007.csv', sep=',', header=TRUE)
attach(MIA_2007)

#structure of dataframe
#str(MIA_2007)

#deleting CMHName, will create new CMHSP variable
MIA_2007$CMHName<-NULL

#adding NA's where there is missing data
MIA_2007[MIA_2007=='']<-NA

#deleting rows with all NA
MIA_2007<-MIA_2007[rowSums(is.na(MIA_2007)) !=ncol(MIA_2007),]

#renaming variables
NewNames<-c(SumofMIA.Cases = 'SumOfCases', FirstofMIA.Units = 'SumOfUnits', SumofMIA.Cost = 'SumOfCost')
MIA_2007<-rename(MIA_2007, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=13248, length=13248, label = c('2007'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=288, length=13248, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=13248, length=13248, label=c('MIA'))

#binding the new variables to the dataframe
MIA_2007<-cbind(MIA_2007, CMHSP, FY, Population)

#reordering columns 
MIA_2007<-MIA_2007[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIA_2007$SumOfCost<-gsub("$", fixed=TRUE, "", MIA_2007$SumOfCost)
MIA_2007$SumOfCost<-gsub(".00", fixed=TRUE, "", MIA_2007$SumOfCost)
MIA_2007$SumOfCost<-gsub(",", fixed=TRUE, "", MIA_2007$SumOfCost)

MIA_2007$SumOfCases<-gsub(".00", fixed=TRUE, "", MIA_2007$SumOfCases)
MIA_2007$SumOfCases<-gsub(",", fixed=TRUE, "", MIA_2007$SumOfCases)

MIA_2007$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIA_2007$SumOfUnits)
MIA_2007$SumOfUnits<-gsub(",", fixed=TRUE, "", MIA_2007$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIA_2007<- transform(MIA_2007, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))

#######################################################################
## Repeating Process for MIA 2008 #####################################
#######################################################################
MIA_2008<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIA_FY2008.csv', sep=',', header=TRUE)
attach(MIA_2008)

#structure of dataframe
#str(MIA_2008)

#deleting CMHName, will create new CMHSP variable
MIA_2008$CMHName<-NULL

#adding NA's where there is missing data
MIA_2008[MIA_2008=='']<-NA

#deleting rows with all NA
MIA_2008<-MIA_2008[rowSums(is.na(MIA_2008)) !=ncol(MIA_2008),]

#renaming variables
NewNames<-c(SumofMIA.Cases = 'SumOfCases', FirstofMIA.Units = 'SumOfUnits', SumofMIA.Cost = 'SumOfCost')
MIA_2008<-rename(MIA_2008, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=13340, length=13340, label = c('2008'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=290, length=13340, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=13340, length=13340, label=c('MIA'))

#binding the new variables to the dataframe
MIA_2008<-cbind(MIA_2008, CMHSP, FY, Population)

#reordering columns 
MIA_2008<-MIA_2008[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIA_2008$SumOfCost<-gsub("$", fixed=TRUE, "", MIA_2008$SumOfCost)
MIA_2008$SumOfCost<-gsub(".00", fixed=TRUE, "", MIA_2008$SumOfCost)
MIA_2008$SumOfCost<-gsub(",", fixed=TRUE, "", MIA_2008$SumOfCost)

MIA_2008$SumOfCases<-gsub(".00", fixed=TRUE, "", MIA_2008$SumOfCases)
MIA_2008$SumOfCases<-gsub(",", fixed=TRUE, "", MIA_2008$SumOfCases)

MIA_2008$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIA_2008$SumOfUnits)
MIA_2008$SumOfUnits<-gsub(",", fixed=TRUE, "", MIA_2008$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIA_2008<- transform(MIA_2008, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))

#######################################################################
## Repeating Process for MIA 2009 #####################################
#######################################################################
MIA_2009<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIA_FY2009.csv', sep=',', header=TRUE)
attach(MIA_2009)

#structure of dataframe
#str(MIA_2009)

#deleting CMHName, will create new CMHSP variable
MIA_2009$CMHName<-NULL

#adding NA's where there is missing data
MIA_2009[MIA_2009=='']<-NA

#deleting rows with all NA
MIA_2009<-MIA_2009[rowSums(is.na(MIA_2009)) !=ncol(MIA_2009),]

#renaming variables
NewNames<-c(SumofMIA.Cases = 'SumOfCases', FirstofMIA.Units = 'SumOfUnits', SumofMIA.Cost = 'SumOfCost')
MIA_2009<-rename(MIA_2009, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14352, length=14352, label = c('2009'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=312, length=14352, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=14352, length=14352, label=c('MIA'))

#binding the new variables to the dataframe
MIA_2009<-cbind(MIA_2009, CMHSP, FY, Population)

#reordering columns 
MIA_2009<-MIA_2009[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIA_2009$SumOfCost<-gsub("$", fixed=TRUE, "", MIA_2009$SumOfCost)
MIA_2009$SumOfCost<-gsub(".00", fixed=TRUE, "", MIA_2009$SumOfCost)
MIA_2009$SumOfCost<-gsub(",", fixed=TRUE, "", MIA_2009$SumOfCost)

MIA_2009$SumOfCases<-gsub(".00", fixed=TRUE, "", MIA_2009$SumOfCases)
MIA_2009$SumOfCases<-gsub(",", fixed=TRUE, "", MIA_2009$SumOfCases)

MIA_2009$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIA_2009$SumOfUnits)
MIA_2009$SumOfUnits<-gsub(",", fixed=TRUE, "", MIA_2009$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIA_2009<- transform(MIA_2009, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIA 2010 #####################################
#######################################################################
MIA_2010<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIA_FY2010.csv', sep=',', header=TRUE)
attach(MIA_2010)

#structure of dataframe
#str(MIA_2010)

#deleting CMHName, will create new CMHSP variable
MIA_2010$CMHName<-NULL

#adding NA's where there is missing data
MIA_2010[MIA_2010=='']<-NA

#deleting rows with all NA
MIA_2010<-MIA_2010[rowSums(is.na(MIA_2010)) !=ncol(MIA_2010),]

#renaming variables
NewNames<-c(SumofMIA.Cases = 'SumOfCases', FirstofMIA.Units = 'SumOfUnits', SumofMIA.Cost = 'SumOfCost')
MIA_2010<-rename(MIA_2010, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14076, length=14076, label = c('2010'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=306, length=14076, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=14076, length=14076, label=c('MIA'))

#binding the new variables to the dataframe
MIA_2010<-cbind(MIA_2010, CMHSP, FY, Population)

#reordering columns 
MIA_2010<-MIA_2010[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIA_2010$SumOfCost<-gsub("$", fixed=TRUE, "", MIA_2010$SumOfCost)
MIA_2010$SumOfCost<-gsub(".00", fixed=TRUE, "", MIA_2010$SumOfCost)
MIA_2010$SumOfCost<-gsub(",", fixed=TRUE, "", MIA_2010$SumOfCost)

MIA_2010$SumOfCases<-gsub(".00", fixed=TRUE, "", MIA_2010$SumOfCases)
MIA_2010$SumOfCases<-gsub(",", fixed=TRUE, "", MIA_2010$SumOfCases)

MIA_2010$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIA_2010$SumOfUnits)
MIA_2010$SumOfUnits<-gsub(",", fixed=TRUE, "", MIA_2010$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIA_2010<- transform(MIA_2010, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIA 2011 #####################################
#######################################################################
MIA_2011<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIA_FY2011.csv', sep=',', header=TRUE)
attach(MIA_2011)

#structure of dataframe
#str(MIA_2011)

#deleting CMHName, will create new CMHSP variable
MIA_2011$CMHName<-NULL

#adding NA's where there is missing data
MIA_2011[MIA_2011=='']<-NA

#deleting rows with all NA
MIA_2011<-MIA_2011[rowSums(is.na(MIA_2011)) !=ncol(MIA_2011),]

#renaming variables
NewNames<-c(SumofMIA.Cases = 'SumOfCases', FirstofMIA.Units = 'SumOfUnits', SumofMIA.Cost = 'SumOfCost')
MIA_2011<-rename(MIA_2011, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14582, length=14582, label = c('2011'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=317, length=14582, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=14582, length=14582, label=c('MIA'))

#binding the new variables to the dataframe
MIA_2011<-cbind(MIA_2011, CMHSP, FY, Population)

#reordering columns 
MIA_2011<-MIA_2011[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIA_2011$SumOfCost<-gsub("$", fixed=TRUE, "", MIA_2011$SumOfCost)
MIA_2011$SumOfCost<-gsub(".00", fixed=TRUE, "", MIA_2011$SumOfCost)
MIA_2011$SumOfCost<-gsub(",", fixed=TRUE, "", MIA_2011$SumOfCost)

MIA_2011$SumOfCases<-gsub(".00", fixed=TRUE, "", MIA_2011$SumOfCases)
MIA_2011$SumOfCases<-gsub(",", fixed=TRUE, "", MIA_2011$SumOfCases)

MIA_2011$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIA_2011$SumOfUnits)
MIA_2011$SumOfUnits<-gsub(",", fixed=TRUE, "", MIA_2011$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIA_2011<- transform(MIA_2011, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIA 2012 #####################################
#######################################################################
MIA_2012<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIA_FY2012.csv', sep=',', header=TRUE)
attach(MIA_2012)

#structure of dataframe
#str(MIA_2012)

#deleting CMHName, will create new CMHSP variable
MIA_2012$CMHName<-NULL

#adding NA's where there is missing data
MIA_2012[MIA_2012=='']<-NA

#deleting rows with all NA
MIA_2012<-MIA_2012[rowSums(is.na(MIA_2012)) !=ncol(MIA_2012),]

#renaming variables
NewNames<-c(SumofMIA.Cases = 'SumOfCases', FirstofMIA.Units = 'SumOfUnits', SumofMIA.Cost = 'SumOfCost')
MIA_2012<-rename(MIA_2012, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14490, length=14490, label = c('2012'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=315, length=14490, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=14490, length=14490, label=c('MIA'))

#binding new variables to dataframe
MIA_2012<-cbind(MIA_2012, CMHSP, FY, Population)

#reordering columns 
MIA_2012<-MIA_2012[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIA_2012$SumOfCost<-gsub("$", fixed=TRUE, "", MIA_2012$SumOfCost)
MIA_2012$SumOfCost<-gsub(".00", fixed=TRUE, "", MIA_2012$SumOfCost)
MIA_2012$SumOfCost<-gsub(",", fixed=TRUE, "", MIA_2012$SumOfCost)

MIA_2012$SumOfCases<-gsub(".00", fixed=TRUE, "", MIA_2012$SumOfCases)
MIA_2012$SumOfCases<-gsub(",", fixed=TRUE, "", MIA_2012$SumOfCases)

MIA_2012$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIA_2012$SumOfUnits)
MIA_2012$SumOfUnits<-gsub(",", fixed=TRUE, "", MIA_2012$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIA_2012<- transform(MIA_2012, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))

#######################################################################
## Repeating Process for MIA 2013 #####################################
#######################################################################
MIA_2013<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIA_FY2013.csv', sep=',', header=TRUE)
attach(MIA_2013)

#structure of dataframe
#str(MIA_2013)

#deleting CMHName, will create new CMHSP variable
MIA_2013$CMHName<-NULL

#adding NA's where there is missing data
MIA_2013[MIA_2013=='']<-NA

#deleting rows with all NA
MIA_2013<-MIA_2013[rowSums(is.na(MIA_2013)) !=ncol(MIA_2013),]

#renaming variables
NewNames<-c(SumofMIA.Cases = 'SumOfCases', FirstofMIA.Units = 'SumOfUnits', SumofMIA.Cost = 'SumOfCost')
MIA_2013<-rename(MIA_2013, NewNames)

#creating FY variable and binding it to dataframe
MIA_2013$FY<-gl(n=1, k=15962, length=15962, label = c('2013'))

#formatting the CMHSP variable so it appears on every row instead of just a few
MIA_2013$CMHSP<-gl(n=46, k=347, length=15962, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
MIA_2013$Population<-gl(n=1, k=15962, length=15962, label=c('MIA'))

#reordering columns 
MIA_2013<-MIA_2013[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIA_2013$SumOfCost<-gsub("$", fixed=TRUE, "", MIA_2013$SumOfCost)
MIA_2013$SumOfCost<-gsub(".00", fixed=TRUE, "", MIA_2013$SumOfCost)
MIA_2013$SumOfCost<-gsub(",", fixed=TRUE, "", MIA_2013$SumOfCost)

MIA_2013$SumOfCases<-gsub(".00", fixed=TRUE, "", MIA_2013$SumOfCases)
MIA_2013$SumOfCases<-gsub(",", fixed=TRUE, "", MIA_2013$SumOfCases)

MIA_2013$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIA_2013$SumOfUnits)
MIA_2013$SumOfUnits<-gsub(",", fixed=TRUE, "", MIA_2013$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIA_2013<- transform(MIA_2013, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))

#######################################################################
## Repeating Process for MIC 2006 #####################################
#######################################################################
MIC_2006<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIC_FY2006.csv', sep=',', header=TRUE)
attach(MIC_2006)

#structure of dataframe
#str(MIC_2006)

#deleting CMHName, will create new CMHSP variable
MIC_2006$CMHName<-NULL

#adding NA's where there is missing data
MIC_2006[MIC_2006=='']<-NA

#deleting rows with all NA
MIC_2006<-MIC_2006[rowSums(is.na(MIC_2006)) !=ncol(MIC_2006),]

#renaming variables
NewNames<-c(SumofMIC.Cases = 'SumOfCases', FirstofMIC.Units = 'SumOfUnits', SumofMIC.Cost = 'SumOfCost')
MIC_2006<-rename(MIC_2006, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=11776, length=11776, label = c('2006'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=256, length=11776, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=11776, length=11776, label=c('MIC'))

#binding the new variables to the dataframe
MIC_2006<-cbind(MIC_2006, CMHSP, FY, Population)

#reordering columns 
MIC_2006<-MIC_2006[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIC_2006$SumOfCost<-gsub("$", fixed=TRUE, "", MIC_2006$SumOfCost)
MIC_2006$SumOfCost<-gsub(".00", fixed=TRUE, "", MIC_2006$SumOfCost)
MIC_2006$SumOfCost<-gsub(",", fixed=TRUE, "", MIC_2006$SumOfCost)

MIC_2006$SumOfCases<-gsub(".00", fixed=TRUE, "", MIC_2006$SumOfCases)
MIC_2006$SumOfCases<-gsub(",", fixed=TRUE, "", MIC_2006$SumOfCases)

MIC_2006$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIC_2006$SumOfUnits)
MIC_2006$SumOfUnits<-gsub(",", fixed=TRUE, "", MIC_2006$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIC_2006<- transform(MIC_2006, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIC 2007 #####################################
#######################################################################
MIC_2007<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIC_FY2007.csv', sep=',', header=TRUE)
attach(MIC_2007)

#structure of dataframe
#str(MIC_2007)

#deleting CMHName, will create new CMHSP variable
MIC_2007$CMHName<-NULL

#adding NA's where there is missing data
MIC_2007[MIC_2007=='']<-NA

#deleting rows with all NA
MIC_2007<-MIC_2007[rowSums(is.na(MIC_2007)) !=ncol(MIC_2007),]

#renaming variables
NewNames<-c(SumofMIC.Cases = 'SumOfCases', FirstofMIC.Units = 'SumOfUnits', SumofMIC.Cost = 'SumOfCost')
MIC_2007<-rename(MIC_2007, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=13248, length=13248, label = c('2007'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=288, length=13248, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=13248, length=13248, label=c('MIC'))

#binding the new variables to the dataframe
MIC_2007<-cbind(MIC_2007, CMHSP, FY, Population)

#reordering columns 
MIC_2007<-MIC_2007[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIC_2007$SumOfCost<-gsub("$", fixed=TRUE, "", MIC_2007$SumOfCost)
MIC_2007$SumOfCost<-gsub(".00", fixed=TRUE, "", MIC_2007$SumOfCost)
MIC_2007$SumOfCost<-gsub(",", fixed=TRUE, "", MIC_2007$SumOfCost)

MIC_2007$SumOfCases<-gsub(".00", fixed=TRUE, "", MIC_2007$SumOfCases)
MIC_2007$SumOfCases<-gsub(",", fixed=TRUE, "", MIC_2007$SumOfCases)

MIC_2007$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIC_2007$SumOfUnits)
MIC_2007$SumOfUnits<-gsub(",", fixed=TRUE, "", MIC_2007$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIC_2007<- transform(MIC_2007, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIC 2008 #####################################
#######################################################################
MIC_2008<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIC_FY2008.csv', sep=',', header=TRUE)
attach(MIC_2008)

#structure of dataframe
#str(MIC_2008)

#deleting CMHName, will create new CMHSP variable
MIC_2008$CMHName<-NULL

#adding NA's where there is missing data
MIC_2008[MIC_2008=='']<-NA

#deleting rows with all NA
MIC_2008<-MIC_2008[rowSums(is.na(MIC_2008)) !=ncol(MIC_2008),]

#renaming variables
NewNames<-c(SumofMIC.Cases = 'SumOfCases', FirstofMIC.Units = 'SumOfUnits', SumofMIC.Cost = 'SumOfCost')
MIC_2008<-rename(MIC_2008, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=13156, length=13156, label = c('2008'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=286, length=13156, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=13156, length=13156, label=c('MIC'))

#binding the new variables to the dataframe
MIC_2008<-cbind(MIC_2008, CMHSP, FY, Population)

#reordering columns 
MIC_2008<-MIC_2008[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIC_2008$SumOfCost<-gsub("$", fixed=TRUE, "", MIC_2008$SumOfCost)
MIC_2008$SumOfCost<-gsub(".00", fixed=TRUE, "", MIC_2008$SumOfCost)
MIC_2008$SumOfCost<-gsub(",", fixed=TRUE, "", MIC_2008$SumOfCost)

MIC_2008$SumOfCases<-gsub(".00", fixed=TRUE, "", MIC_2008$SumOfCases)
MIC_2008$SumOfCases<-gsub(",", fixed=TRUE, "", MIC_2008$SumOfCases)

MIC_2008$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIC_2008$SumOfUnits)
MIC_2008$SumOfUnits<-gsub(",", fixed=TRUE, "", MIC_2008$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIC_2008<- transform(MIC_2008, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIC 2009 #####################################
#######################################################################
MIC_2009<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIC_FY2009.csv', sep=',', header=TRUE)
attach(MIC_2009)

#structure of dataframe
#str(MIC_2009)

#deleting CMHName, will create new CMHSP variable
MIC_2009$CMHName<-NULL

#adding NA's where there is missing data
MIC_2009[MIC_2009=='']<-NA

#deleting rows with all NA
MIC_2009<-MIC_2009[rowSums(is.na(MIC_2009)) !=ncol(MIC_2009),]

#renaming variables
NewNames<-c(SumofMIC.Cases = 'SumOfCases', FirstofMIC.Units = 'SumOfUnits', SumofMIC.Cost = 'SumOfCost')
MIC_2009<-rename(MIC_2009, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14352, length=14352, label = c('2009'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=312, length=14352, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=14352, length=14352, label=c('MIC'))

#binding the new variables to the dataframe
MIC_2009<-cbind(MIC_2009, CMHSP, FY, Population)

#reordering columns 
MIC_2009<-MIC_2009[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIC_2009$SumOfCost<-gsub("$", fixed=TRUE, "", MIC_2009$SumOfCost)
MIC_2009$SumOfCost<-gsub(".00", fixed=TRUE, "", MIC_2009$SumOfCost)
MIC_2009$SumOfCost<-gsub(",", fixed=TRUE, "", MIC_2009$SumOfCost)

MIC_2009$SumOfCases<-gsub(".00", fixed=TRUE, "", MIC_2009$SumOfCases)
MIC_2009$SumOfCases<-gsub(",", fixed=TRUE, "", MIC_2009$SumOfCases)

MIC_2009$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIC_2009$SumOfUnits)
MIC_2009$SumOfUnits<-gsub(",", fixed=TRUE, "", MIC_2009$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIC_2009<- transform(MIC_2009, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIC 2010 #####################################
#######################################################################
MIC_2010<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIC_FY2010.csv', sep=',', header=TRUE)
attach(MIC_2010)

#structure of dataframe
#str(MIC_2010)

#deleting CMHName, will create new CMHSP variable
MIC_2010$CMHName<-NULL

#adding NA's where there is missing data
MIC_2010[MIC_2010=='']<-NA

#deleting rows with all NA
MIC_2010<-MIC_2010[rowSums(is.na(MIC_2010)) !=ncol(MIC_2010),]

#renaming variables
NewNames<-c(SumofMIC.Cases = 'SumOfCases', FirstofMIC.Units = 'SumOfUnits', SumofMIC.Cost = 'SumOfCost')
MIC_2010<-rename(MIC_2010, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14076, length=14076, label = c('2010'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=306, length=14076, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=14076, length=14076, label=c('MIC'))

#binding the new variables to the dataframe
MIC_2010<-cbind(MIC_2010, CMHSP, FY, Population)

#reordering columns 
MIC_2010<-MIC_2010[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIC_2010$SumOfCost<-gsub("$", fixed=TRUE, "", MIC_2010$SumOfCost)
MIC_2010$SumOfCost<-gsub(".00", fixed=TRUE, "", MIC_2010$SumOfCost)
MIC_2010$SumOfCost<-gsub(",", fixed=TRUE, "", MIC_2010$SumOfCost)

MIC_2010$SumOfCases<-gsub(".00", fixed=TRUE, "", MIC_2010$SumOfCases)
MIC_2010$SumOfCases<-gsub(",", fixed=TRUE, "", MIC_2010$SumOfCases)

MIC_2010$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIC_2010$SumOfUnits)
MIC_2010$SumOfUnits<-gsub(",", fixed=TRUE, "", MIC_2010$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIC_2010<- transform(MIC_2010, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIC 2011 #####################################
#######################################################################
MIC_2011<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIC_FY2011.csv', sep=',', header=TRUE)
attach(MIC_2011)

#structure of dataframe
#str(MIC_2011)

#deleting CMHName, will create new CMHSP variable
MIC_2011$CMHName<-NULL

#adding NA's where there is missing data
MIC_2011[MIC_2011=='']<-NA

#deleting rows with all NA
MIC_2011<-MIC_2011[rowSums(is.na(MIC_2011)) !=ncol(MIC_2011),]

#renaming variables
NewNames<-c(SumofMIC.Cases = 'SumOfCases', FirstofMIC.Units = 'SumOfUnits', SumofMIC.Cost = 'SumOfCost')
MIC_2011<-rename(MIC_2011, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14582, length=14582, label = c('2011'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=317, length=14582, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))
#creating population variable
Population<-gl(n=1, k=14582, length=14582, label=c('MIC'))

#binding the new variables to the dataframe
MIC_2011<-cbind(MIC_2011, CMHSP, FY, Population)

#reordering columns 
MIC_2011<-MIC_2011[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIC_2011$SumOfCost<-gsub("$", fixed=TRUE, "", MIC_2011$SumOfCost)
MIC_2011$SumOfCost<-gsub(".00", fixed=TRUE, "", MIC_2011$SumOfCost)
MIC_2011$SumOfCost<-gsub(",", fixed=TRUE, "", MIC_2011$SumOfCost)

MIC_2011$SumOfCases<-gsub(".00", fixed=TRUE, "", MIC_2011$SumOfCases)
MIC_2011$SumOfCases<-gsub(",", fixed=TRUE, "", MIC_2011$SumOfCases)

MIC_2011$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIC_2011$SumOfUnits)
MIC_2011$SumOfUnits<-gsub(",", fixed=TRUE, "", MIC_2011$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIC_2011<- transform(MIC_2011, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))


#######################################################################
## Repeating Process for MIC 2012 #####################################
#######################################################################
MIC_2012<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIC_FY2012.csv', sep=',', header=TRUE)
attach(MIC_2012)

#structure of dataframe
#str(MIC_2012)

#deleting CMHName, will create new CMHSP variable
MIC_2012$CMHName<-NULL

#adding NA's where there is missing data
MIC_2012[MIC_2012=='']<-NA

#deleting rows with all NA
MIC_2012<-MIC_2012[rowSums(is.na(MIC_2012)) !=ncol(MIC_2012),]

#renaming variables
NewNames<-c(SumofMIC.Cases = 'SumOfCases', FirstofMIC.Units = 'SumOfUnits', SumofMIC.Cost = 'SumOfCost')
MIC_2012<-rename(MIC_2012, NewNames)

#creating FY variable and binding it to dataframe
FY<-gl(n=1, k=14490, length=14490, label = c('2012'))

#formatting the CMHSP variable so it appears on every row instead of just a few
CMHSP<-gl(n=46, k=315, length=14490, label=c('Allegan', 'AuSable Valley', 'Barry', 'Bay-Arenac',
                                             'Berrien', 'Clinton Eaton Ingham', 'CMH for Central Michigan',
                                             'Copper County', 'Detroit-Wayne', 'Genesee', 'Gogebic', 'Gratiot',
                                             'Hiawatha', 'Huron', 'Ionia', 'Kalamazoo', 'Lapeer', 'Lenawee', 
                                             'Lifeways', 'Livingston', 'Macomb','Manistee-Benzie', 'Monroe',
                                             'Montcalm','Muskegon','Networy180', 'Newaygo', 'North country',
                                             'Northeast Michigan', 'Northern Lakes', 'Northpointe', 'Oakland',
                                             'Ottawa', 'Pathways', 'Pines', 'Saginaw','Sanilac', 
                                             'Shiawassee', 'St. Clair', 'St. Joseph', 'Summit Pointe', 'Tuscola',
                                             'Van Buren', 'Washtenaw', 'West Michigan', 'Woodlands'))

#creating population variable
Population<-gl(n=1, k=14490, length=14490, label=c('MIC'))

#binding new variables to dataframe
MIC_2012<-cbind(MIC_2012, CMHSP, FY, Population)

#reordering columns 
MIC_2012<-MIC_2012[c(12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Removing characters from the vectors
MIC_2012$SumOfCost<-gsub("$", fixed=TRUE, "", MIC_2012$SumOfCost)
MIC_2012$SumOfCost<-gsub(".00", fixed=TRUE, "", MIC_2012$SumOfCost)
MIC_2012$SumOfCost<-gsub(",", fixed=TRUE, "", MIC_2012$SumOfCost)

MIC_2012$SumOfCases<-gsub(".00", fixed=TRUE, "", MIC_2012$SumOfCases)
MIC_2012$SumOfCases<-gsub(",", fixed=TRUE, "", MIC_2012$SumOfCases)

MIC_2012$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIC_2012$SumOfUnits)
MIC_2012$SumOfUnits<-gsub(",", fixed=TRUE, "", MIC_2012$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIC_2012<- transform(MIC_2012, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))

#######################################################################
## Repeating Process for MIC 2013 #####################################
#######################################################################
MIC_2013<-read.csv('https://raw.githubusercontent.com/j-hagedorn/open404/master/data/CMH-MIC_FY2013.csv', sep=',', header=TRUE)
attach(MIC_2013)

#structure of dataframe
#str(MIC_2013)

## THIS BLOCK DIFFERENT THAN OTHER DATASETS DUE TO IRREGULAR COUNTS OF SVS PER CMH
#adding NA's where there is missing data
MIC_2013[MIC_2013=='']<-NA

#deleting rows with all NA
MIC_2013<-MIC_2013[rowSums(is.na(MIC_2013)) !=ncol(MIC_2013),]

library(zoo)
MIC_2013$CMHName <- na.locf(MIC_2013$CMHName)

MIC_2013 <- subset(MIC_2013, is.na(MIC_2013$FirstofService.Description) == F)

#renaming variables
NewNames<-c(CMHName = 'CMHSP', SumofMIC.Cases = 'SumOfCases', FirstofMIC.Units = 'SumOfUnits', SumofMIC.Cost = 'SumOfCost')
MIC_2013<-rename(MIC_2013, NewNames)

#creating FY variable and binding it to dataframe
MIC_2013$FY<-gl(n=1, k=18122, length=18122, label = c('2013'))

#creating population variable
MIC_2013$Population<-gl(n=1, k=18122, length=18122, label=c('MIC'))

#reordering columns 
MIC_2013<-MIC_2013[c(13, 14, 1:12)]

## END DIFF CODE BLOCK

#Removing characters from the vectors
MIC_2013$SumOfCost<-gsub("$", fixed=TRUE, "", MIC_2013$SumOfCost)
MIC_2013$SumOfCost<-gsub(".00", fixed=TRUE, "", MIC_2013$SumOfCost)
MIC_2013$SumOfCost<-gsub(",", fixed=TRUE, "", MIC_2013$SumOfCost)

MIC_2013$SumOfCases<-gsub(".00", fixed=TRUE, "", MIC_2013$SumOfCases)
MIC_2013$SumOfCases<-gsub(",", fixed=TRUE, "", MIC_2013$SumOfCases)

MIC_2013$SumOfUnits<-gsub(".00", fixed=TRUE, "", MIC_2013$SumOfUnits)
MIC_2013$SumOfUnits<-gsub(",", fixed=TRUE, "", MIC_2013$SumOfUnits)

#changing sumofcases, cost, and units to numeric
MIC_2013<- transform(MIC_2013, SumOfCases = as.numeric(SumOfCases), SumOfUnits = as.numeric(SumOfUnits), 
                     SumOfCost = as.numeric(SumOfCost))

#######################################################################
## Stacking Dataframes to create master 404 ###########################
#######################################################################
Master<-rbind(MIA_2006, MIA_2007, MIA_2008, MIA_2009, MIA_2010, MIA_2011,MIA_2012,MIA_2013,
              MIC_2006, MIC_2007, MIC_2008, MIC_2009, MIC_2010, MIC_2011,MIC_2012,MIC_2013,
              dd_2006, dd_2007, dd_2008, dd_2009, dd_2010, dd_2011, dd_2012, dd_2013)

attach(Master)

######################################################################
## Recoding and creating new variables ###############################
######################################################################

#Changing HCPCS code from factor to character
  # Coerces NAs because some codes have letters in them
  # so need to change var to character instead of numeric

Master$FirstOfHCPCS.Code<-as.character(Master$FirstOfHCPCS.Code)

#removing characters from factor name in UnitType
Master$UnitType <- gsub("Encounter / Trip  Per session. One day/partial day = one session", fixed=TRUE,"Encounter Trip  Per session One day partial day" 
                        ,Master$UnitType)

Master$UnitType <-gsub("Encounter / Trip. Per session. One night = one session", fixed=TRUE, 
                       "Encounter Trip Per session One night one session",Master$UnitType)

Master$UnitType<-gsub("Per session. One day/partial day = one session", fixed=TRUE, 
                      "Per session One day partial day", Master$UnitType)

Master$UnitType<-gsub("Per session. One night = one session", fixed=TRUE, "Per session One night", Master$UnitType)

## Formatting UnitType to be quantiative and standardized##

#NA =  non-numeric unit types
#For any time ranges, used highest value
#All values are based on 1.00 = 1 hour

#install.packages('car')
library("car")
Unit_Hours <-recode(Master$UnitType,
                    "'' = 'NA';                                                                
                    '# of items'='NA';                                                      
                    '# of tests'='NA';                                                      
                    '# of treatments'='NA';                                                 
                    '# of units'='NA';                                                    
                    '# of visits'='NA';                                             
                    '15 minutes'='.25';                                                    
                    '15 Minutes'='.25';                                              
                    '30 Minutes'='.50';                                                    
                    '30 Minutes or less'='.50';                                             
                    'Days'='24.00';                                                            
                    'Encounter'='NA';                                                      
                    'Encounter / Trip'='NA';                                               
                    'Encounter 20-30 Min'='.50';                                           
                    'Encounter 45-50 Min'='.83';                                            
                    'Encounter 75-80 Min'='1.33';                                            
                    'Encounter Face-to-Face'='NA';                                         
                    'Evaluation'='NA';                                                     
                    'Face to Face Contact'='NA';                                           
                    'Hour'='1.00';                                                          
                    'Items'='NA';                                                         
                    'Minutes'='NA';                                                        
                    'Month'='720.00';                                                         
                    'Per diem'='24.00';                                              
                    'Per Diem'='24.00';                                              
                    'Per mile'='NA';                                                       
                    'Per Mile'='NA';                                                      
                    'Per one-way trip'='NA';                                              
                    'Refer to code descriptions.'='NA';                                   
                    'Service'='NA';                                                      
                    'Up to 15 min'='.25';                                                   
                    'Per Hour'='1.00';                                                      
                    'Per Screen'='NA';                                                      
                    'Per Test'='NA';                                                      
                    '25 minutes'='.42';                                                    
                    '35 Minutes'='.58';                                                    
                    '50 Minutes'='.83';                                                    
                    '70 Minutes'='1.17';                                                    
                    'Direct Observation Encounter'='NA';                                   
                    'Each Additional 15 Minutes'='.25'; #same as 15 minutes                                     
                    'Encounter Trip  Per session One day partial day'='8.00';
                    'Encounter Trip  Per session One day partial day'='8.00';
                    'Encounter Trip Per session One night one session'='8.00';         
                    'Encounter Face-to-Face, generally less than 10 minutes'='.17';         
                    'Encounter Session at least 45 min'='.75';                              
                    'First Hour'='1.00';                                                    
                    'Month   Service'='720.00';                                                
                    'Per Service'='NA';                                                     
                    'Per Item'='NA';                                                   
                    'Per session One day partial day'='8.00';
                    'Per session One night'='8.00'")

# table(Unit_Hours)
Master<-cbind(Master, Unit_Hours)

#Dropping Revenue code, unit type; not needed for analysis
Master$FirstOfRevenue.Code<-NULL
Master$UnitType<-NULL

#Reordering the columns
Master<-Master[c(1:6,13,7:12)]

## Creating a "Service" variable to group the service descriptions##
## This creates meaningful groups for analysis ##

#First need to add in fake HCPCS codes so I can create a "Service" variable based on HCPCS code

Master <- subset(Master, is.na(Master$FirstofService.Description) == F)

#To use ifelse, first need to change variables to character
Master$FirstOfHCPCS.Code<-as.character(Master$FirstOfHCPCS.Code)
Master$FirstofService.Description<-as.character(Master$FirstofService.Description)

#Using "ifelse" to replace NA HCPCS codes with fake codes
Master$FirstOfHCPCS.Code<-ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="State Mental Retardation Facility - Inpatient (ICF/MR) PT65",'X01',
                            ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="State Psychiatric Hospital - Inpatient PT22", "X02",
                              ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Local Psychiatric Hospital/IMD PT68", "X03",
                                ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Local Psychiatric Hospital - Acute Community PT73", "X04", 
                                  ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Extended Observation Beds ", "X05",
                                    ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description =="Inpatient Hospital Ancillary Services - Room and Board", "X06",
                                      ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Leave of Absence", "X07", 
                                        ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Pharmacy", "X08", 
                                          ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Medical/Surgical Supplies and Devices", "X09",
                                            ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Laboratory", "X10",
                                              ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services -EKG/ECG", "X11",
                                                ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - EEG", "X12", 
                                                  ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Room and Board", "X13",
                                                    ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Psychiatric/Psychological Treatments/Services", "X14", 
                                                      ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Other Diagnosis Services", "X15", 
                                                        ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Other Therapeutic Services", "X16",
                                                          ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Radiology", "X17",
                                                            ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Respiratory Services", "X18",
                                                              ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Inpatient Hospital Ancillary Services -Physical Therapy", "X19",
                                                                ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Occupational Therapy", "X20",
                                                                  ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Speech-Language Pathology", "X21",
                                                                    ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Emergency Room", "X22",
                                                                      ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Pulmonary Function", "X23",
                                                                        ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Audiology", "X24",
                                                                          ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Magnetic Resonance Technology (MRT)", "X25",
                                                                            ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Pharmacy", "X26",
                                                                              ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description== "Additional Codes-ECT Facility Charge" , "X27", 
                                                                                ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="ECT Anesthesia", "X28",
                                                                                  ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="ECT Recovery Room", "X29",
                                                                                    ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Peer Directed and Operated Support Services","X30",
                                                                                      ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Pharmacy (Drugs and Other Biologicals)","X31",
                                                                                        ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Crisis Observation Care","X32",
                                                                                          ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Outpatient Partial Hospitalization","X33",
                                                                                            ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Other","X34", Master$FirstOfHCPCS.Code))))))))))))))))))))))))))))))))))

# table(Master$FirstOfHCPCS.Code)

# Fix formatting with newline
Master$FirstOfHCPCS.Code <- gsub(pattern="90791\n", replacement="90791", x=Master$FirstOfHCPCS.Code)

# Checking to make sure there are no missing HCPCS codes
sum(is.na(Master$FirstOfHCPCS.Code)) #Result is 0

# Make a new variable by concatenating HCPCS.Code and Modifier.  This will be most granular level of service definition
Master$Code_Mod <- paste(Master$FirstOfHCPCS.Code,Master$FirstOfModifier, sep = "", collapse = NULL)

# Remove NA values from Code_Mod
Master$Code_Mod <- as.character(Master$Code_Mod)
Master$Code_Mod <-gsub(pattern="NA", replacement="", x=Master$Code_Mod)

#Need to change vars back to a factor
Master$FirstOfHCPCS.Code<-as.factor(Master$FirstOfHCPCS.Code)
Master$FirstofService.Description<-as.factor(Master$FirstofService.Description)
Master$Code_Mod<-as.factor(Master$Code_Mod)
#str(Master$FirstOfHCPCS.Code)
#str(Master$FirstofService.Description)


#Master$UnitPerCase.1<-NULL

#Reordering the columns
Master<-Master[c(2,1,3:6,14,7:13)]

#Grouping Service.Description into more general categories (variable named 'Service')
library("car")
Service <-recode(Master$FirstOfHCPCS.Code,
                 "'X01'='State Mental Retardation Facility';
                 'T2025'='Fiscal Intermediary Services';
                 'H2000'='Behavioral Treatment';
                 'H2019'='Behavioral Treatment';
                 '104'='Ancillary Services / ECT';
                 '00104'='Ancillary Services / ECT';
                 '90870'='Ancillary Services / ECT';
                 '80100'='Laboratory';
                 '80101'='Laboratory';
                 '82075'='Assessment';
                 '90801'='Assessment';
                 '90802'='Assessment';
                 '90887'='Assessment';
                 '96100'='Assessment';
                 '96101'='Assessment';
                 '96102'='Assessment';
                 '96103'='Assessment';
                 '96105'='Assessment';
                 '96110'='Assessment';
                 '96111'='Assessment';
                 '96115'='Assessment';
                 '96116'='Assessment';
                 '96117'='Assessment';
                 '96118'='Assessment';
                 '96119'='Assessment';
                 '96120'='Assessment';
                 '97802'='Assessment';
                 '97803'='Assessment';
                 'H0001'='Assessment';
                 'H0002'='Assessment';
                 'H0003'='Laboratory';
                 'H0031'='Assessment';
                 'H0048'='Laboratory';
                 'H0049'='Assessment';
                 'T1001'='Assessment';
                 'T1023'='Assessment';
                 'T2011'='Assessment';
                 '90772'='Medication Administration';
                 '90782'='Medication Administration';
                 '90788'='Medication Administration';
                 '90862'='Medication Management';
                 '96372'='Medication Administration';
                 '99211'='Medication Administration';
                 '99506'='Medication Administration';
                 '99605'='Medication Administration';
                 'G0351'='Medication Administration';
                 'H0020'='Pharmaceuticals';
                 'H0033'='Pharmaceuticals';
                 'M0064'='Medication Management';
                 'T1999'='Pharmaceuticals';
                 'H0045'='Respite';
                 'S5150'='Respite';
                 'S5151'='Respite';
                 'S9125'='Respite';
                 'T1005'='Health Services';
                 'T2036'='Respite';
                 'T2037'='Respite';
                 'H0019'='Residential Treatment';
                 'H2016'='Residential Treatment';
                 'S9976'='Residential Treatment';
                 'T1020'='Residential Treatment';
                 'H0022'='Substance Abuse Outpatient';
                 'H0025'='Prevention';
                 'S9482'='Prevention';
                 'T1027'='Prevention';
                 'T2024'='Prevention';
                 '92506'='OT/PT/SLT';
                 '92507'='OT/PT/SLT';
                 '92508'='OT/PT/SLT';
                 '92526'='OT/PT/SLT';
                 '92610'='OT/PT/SLT';
                 '97001'='OT/PT/SLT';
                 '97002'='OT/PT/SLT';
                 '97110'='OT/PT/SLT';
                 '97112'='OT/PT/SLT';
                 '97113'='OT/PT/SLT';
                 '97116'='OT/PT/SLT';
                 '97124'='OT/PT/SLT';
                 '97140'='OT/PT/SLT';
                 '97150'='OT/PT/SLT';
                 '97530'='OT/PT/SLT';
                 '97532'='OT/PT/SLT';
                 '97533'='OT/PT/SLT';
                 '97535'='OT/PT/SLT';
                 '97537'='OT/PT/SLT';
                 '97542'='OT/PT/SLT';
                 '97750'='OT/PT/SLT';
                 '97755'='OT/PT/SLT';
                 '97760'='OT/PT/SLT';
                 '97003'='OT/PT/SLT';
                 '97003/ 97004'='OT/PT/SLT';
                 '97004'='OT/PT/SLT';
                 '97504'='OT/PT/SLT';
                 '97703'='OT/PT/SLT';
                 'G0176'='OT/PT/SLT';
                 'S8990'='OT/PT/SLT';
                 '92626'='OT/PT/SLT';
                 '92627'='OT/PT/SLT';
                 '92630'='OT/PT/SLT';
                 '92633'='OT/PT/SLT';
                 'G0177'='Family Services';
                 'S5110'='Family Services';
                 'S5111'='Family Services';
                 'T1009'='Family Services';
                 'T1015'='Family Services';
                 'H0006'='Case Management';
                 'H0032'='Case Management';
                 'H0039'='Interdisciplinary Team';
                 'H2021'='Interdisciplinary Team';
                 'H2022'='Interdisciplinary Team';
                 'T1016'='Supports Coordination';
                 'T1017'='Case Management';
                 'T2023'='Case Management';
                 'H0023'='Peer Services';
                 'H0038'='Peer Services';
                 'H0046'='Peer Services';
                 'H2030'='Clubhouse';
                 'T1012'='Peer Services';
                 'A0080'='Transportation';
                 'A0090'='Transportation';
                 'A0100'='Transportation';
                 'A0110'='Transportation';
                 'A0120'='Transportation';
                 'A0130'='Transportation';
                 'A0160'='Transportation';
                 'A0170'='Transportation';
                 'A0425'='Transportation';
                 'A0427'='Transportation';
                 'A0428'='Transportation';
                 'S0209'='Transportation';
                 'S0215'='Transportation';
                 'T2001'='Transportation';
                 'T2002'='Transportation';
                 'T2003'='Transportation';
                 'T2004'='Transportation';
                 'T2005'='Transportation';
                 'A0140'='Transportation';
                 'S5160'='Equipment';
                 'S5161'='Equipment';
                 '97761'='Equipment';
                 '97762'='Equipment';
                 'E1399'='Equipment';
                 'S5165'='Equipment';
                 'S5199'='Equipment';
                 'T2028'='Equipment';
                 'T2029'='Equipment';
                 'T2039'='Equipment';
                 'H2014'='Skill-Building/Non-Vocational';
                 'H2023'='Vocational Services';
                 'T2015'='Vocational Services';
                 '90804'='Substance Abuse Outpatient';
                 '90805'='Substance Abuse Outpatient';
                 '90806'='Substance Abuse Outpatient';
                 '90807'='Substance Abuse Outpatient';
                 '90808'='Substance Abuse Outpatient';
                 '90809'='Substance Abuse Outpatient';
                 '90810'='Outpatient Therapy';
                 '90812'='Outpatient Therapy';
                 '90814'='Outpatient Therapy';
                 '90815'='Outpatient Therapy';
                 '90816'='Outpatient Therapy';
                 '90817'='Outpatient Therapy';
                 '90818'='Outpatient Therapy';
                 '90819'='Outpatient Therapy';
                 '90821'='Outpatient Therapy';
                 '90823'='Outpatient Therapy';
                 '90824'='Outpatient Therapy';
                 '90826'='Outpatient Therapy';
                 '90827'='Outpatient Therapy';
                 '90846'='Outpatient Therapy';
                 '90847'='Outpatient Therapy';
                 '90849'='Outpatient Therapy';
                 '90853'='Outpatient Therapy';
                 '90811'='Outpatient Therapy';
                 '90813'='Outpatient Therapy';
                 '90822'='Outpatient Therapy';
                 '90828'='Outpatient Therapy';
                 '90829'='Outpatient Therapy';
                 '90857'='Outpatient Therapy';
                 'H0004'='Substance Abuse Outpatient';
                 'H0005'='Substance Abuse Outpatient';
                 'H0015'='Substance Abuse Outpatient';
                 'H2027'='Outpatient Therapy';
                 'H2035'='Substance Abuse Outpatient';
                 'H2036'='Substance Abuse Outpatient';
                 'H0010'='Sub-Acute Detoxification';
                 'H0012'='Sub-Acute Detoxification';
                 'H0014'='Sub-Acute Detoxification';
                 'H0018'='Crisis Services';
                 'H2011'='Crisis Services';
                 'H2020'='Crisis Services';
                 'S9484'='Crisis Services';
                 'T2034'='Crisis Services';
                 'H0030'='Crisis Services';
                 'H0036'='Community Living Supports';
                 'H0043'='Community Living Supports';
                 'H2015'='Community Living Supports';
                 'H2033'='Community Living Supports';
                 'S5116'='Community Living Supports';
                 'S5120'='Community Living Supports';
                 'S5140'='Community Living Supports';
                 'S5145'='Community Living Supports';
                 'T2038'='Community Living Supports';
                 'D0150'='Dental';
                 'D0180'='Dental';
                 'D1110'='Dental';
                 'D2330'='Dental';
                 'D2332'='Dental';
                 'D2391'='Dental';
                 'D0220'='Dental'; 
                 'D0230'='Dental'; 
                 'D0274'='Dental';
                 'D2331'='Dental';
                 'D2392'='Dental';
                 'D2393'='Dental';
                 'D2750'='Dental';
                 'D4910'='Dental';
                 'D7210'='Dental';
                 'D7310'='Dental';
                 'D9920'='Dental';
                 'E1340'='Dental';
                 '99224'='Health Services';
                 '99225'='Health Services';
                 '99226'='Health Services';
                 '99271'='Health Services';
                 '99272'='Health Services';
                 '99273'='Health Services';
                 '99275'='Health Services';
                 '97804'='Health Services';
                 '97810'='Health Services';
                 '99203'='Health Services';
                 '99205'='Health Services';
                 '99212'='Health Services';
                 '99213'='Health Services';
                 '99214'='Health Services';
                 '99215'='Health Services';
                 '99221'='Health Services';
                 '99222'='Health Services';
                 '99223'='Health Services';
                 '99231'='Health Services';
                 '99232'='Health Services';
                 '99233'='Health Services';
                 '99241'='Health Services';
                 '99242'='Health Services';
                 '99243'='Health Services';
                 '99244'='Health Services';
                 '99245'='Health Services';
                 '99251'='Health Services';
                 '99252'='Health Services';
                 '99253'='Health Services';
                 '99254'='Health Services';
                 '99255'='Health Services';
                 '99201'='Health Services';
                 '99202'='Health Services';
                 '99204'='Health Services';
                 '99238'='Health Services';
                 '99261'='Health Services';
                 '99262'='Health Services';
                 '99263'='Health Services';
                 '99274'='Health Services';
                 'H0034'='Health Services';
                 'S9123'='Health Services';
                 'S9124'='Health Services';
                 'S9445'='Health Services';
                 'S9446'='Health Services';
                 'S9470'='Health Services';
                 'T1000'='Health Services';
                 'T1002'='Health Services';
                 'T1003'='Health Services';
                 '97811'='Health Services';
                 'K0739'='Ancillary Services / ECT';
                 'ALL'='Other';
                 'T5999'='Equipment';
                 'X02'='State Hospitalization';
                 'X03'='Local Hospitalization';
                 'X04'='Local Hospitalization';
                 'X05'='Ancillary Services / ECT';
                 'X06'='Ancillary Services / ECT';
                 'X07'='Ancillary Services / ECT';
                 'X08'='Ancillary Services / ECT';
                  'X09'='Ancillary Services / ECT';
                  'X10'='Ancillary Services / ECT';
                  'X11'='Ancillary Services / ECT';
                  'X12'='Ancillary Services / ECT';
                  'X13'='Ancillary Services / ECT';
                  'X14'='Ancillary Services / ECT';
                  'X15'='Ancillary Services / ECT';
                  'X16'='Ancillary Services / ECT';
                  'X17'='Ancillary Services / ECT';
                  'X18'='Ancillary Services / ECT';
                  'X19'='Ancillary Services / ECT';
                  'X20'='Ancillary Services / ECT';
                  'X21'='Ancillary Services / ECT';
                  'X22'='Ancillary Services / ECT';
                  'X23'='Ancillary Services / ECT';
                  'X24'='Ancillary Services / ECT';
                  'X25'='Ancillary Services / ECT';
                  'X26'='Ancillary Services / ECT';
                  'X27'='Ancillary Services / ECT';
                  'X28'='Ancillary Services / ECT';
                  'X29'='Ancillary Services / ECT';
                 'X30'='Peer Services';
                 'X31'='Pharmaceuticals';
                 'X32'='Crisis Services';
                 'X33'='Partial Hospitalization';
                 'X34'='Other';
                 'G0409'='Peer Services';
                 'H0037'='Medication Management';
                 'H2010'='Medication Management';
                 'H0050'='Outpatient Therapy';
                 '90785'='Outpatient Therapy';
                '90791' = 'Assessment';
                '90792'='Assessment';
                '90832'='Outpatient Therapy';
                '90833'='Outpatient Therapy';
                '90834'='Outpatient Therapy';
                '90836'='Outpatient Therapy';
                '90839'='Outpatient Therapy';
                '90840'='Outpatient Therapy'; 
                '99334'='Assessment';
                '99335'='Assessment'; 
                '99336'='Assessment'; 
                '99337'='Assessment';
                '99347'='Assessment';
                '99348'='Assessment'; 
                '99349'='Assessment';
                '99350'='Assessment'; 
                'S5108'='Behavioral Treatment';
                '90837'='Outpatient Therapy';
                '90838'='Assessment';
                '99324'='Assessment';
                '99325'='Assessment';
                '99326'='Assessment';
                '99327'='Assessment';
                '99328'='Assessment';
                '99341'='Assessment';
                '99342'='Assessment';
                '99343'='Assessment';
                '99344'='Assessment';
                '99345'='Assessment'")

table(Service)

#attaching "Service" to "Master" dataframe
Master<-cbind(Master, Service) 

#Grouping Service into even more general categories (variable named 'ServiceType')
library("car")
ServiceType <-recode(Master$Service,
                     "'Ancillary Services / ECT'='Hospital-based Services';
                      'Assessment'='Screening & Assessment';
                      'Behavioral Treatment'='Outpatient Treatment';
                       'Case Management'='Care Coordination';
                      'Supports Coordination'='Care Coordination';
                      'Interdisciplinary Team'='Care Coordination';
                       'Clubhouse'='Employment Services';
                       'Community Living Supports'='Home & Community Based Services';
                       'Crisis Services'='Crisis and Respite';
                       'Dental'='Physical Health Services';
                       'Equipment'='Equipment';
                       'Family Services'='Outpatient Treatment';
                       'Fiscal Intermediary Services'='Care Coordination';
                       'Health Services'='Physical Health Services';
                       'Laboratory'='Screening & Assessment';
                       'Local Hospitalization'='Hospital-based Services';
                       'Medication Administration'='Medication';
                       'Medication Management'='Medication';
                       'OT/PT/SLT'='Physical Health Services';
                       'Other'='Other';
                       'Outpatient Therapy'='Outpatient Treatment';
                       'Partial Hospitalization'='Crisis and Respite';
                       'Peer Services'='Care Coordination';
                       'Pharmaceuticals'='Medication';
                       'Prevention'='Outpatient Treatment';
                      'Residential Treatment'='Home & Community Based Services';
                      'Respite'='Crisis and Respite';
                      'Skill-Building/Non-Vocational'='Employment Services';
                      'State Hospitalization'='Hospital-based Services';
                      'State Mental Retardation Facility'='Hospital-based Services';
                      'Sub-Acute Detoxification'='Hospital-based Services';
                      'Substance Abuse Outpatient'='Outpatient Treatment';
                      'Transportation'='Transportation';
                      'Vocational Services'='Employment Services'")
# table(ServiceType)

# attaching "ServiceType" to "Master" dataframe
Master<-cbind(Master, ServiceType)

## Adding PIHP regions to the dataset ##

library(car)

#First, need to recode Network180, Copper Country, and North Country
#Currently entered as "Networy180", "Copper County," and "North country "

Master$CMHSP<-recode(Master$CMHSP, "'Copper County'='Copper Country';
                     'Networy180'='Network180';
                     'North country '='North Country'; 'North country'='North Country'")

#levels(Master$CMHSP)

PIHP<-recode(Master$CMHSP, "'Copper Country'='1';
             'Network180'='3';
             'Gogebic'='1';
             'Hiawatha'='1';
             'Northpointe'='1'; 
             'Pathways'='1';
             'AuSable Valley'='2';
             'Manistee-Benzie'='2';
             'North Country'='2';
             'Northeast Michigan'='2';
             'Northern Lakes'='2';
             'Allegan'='3';
             'Muskegon'='3';
             'Network180'='3';
             'Ottawa'='3';
             'West Michigan'='3';
             'Barry'='4';
             'Berrien'='4';
             'Kalamazoo'='4';
             'Pines'='4';
             'St. Joseph'='4';
             'Summit Pointe'='4';
             'Van Buren'='4';
             'Woodlands'='4';
             'Bay-Arenac'='5';
             'Clinton Eaton Ingham'='5';
             'CMH for Central Michigan'='5';
             'Gratiot'='5';
             'Huron'='5';
             'Ionia'='5';
             'Lifeways'='5';
             'Montcalm'='5';
             'Newaygo'='5';
             'Saginaw'='5';
             'Shiawassee'='5';
             'Tuscola'='5';
             'Lenawee'='6';
             'Livingston'='6';
             'Monroe'='6';
             'Washtenaw'='6';
             'Detroit-Wayne'='7';
             'Oakland'='8';
             'Macomb'='9';
             'Genesee'='10';
             'Lapeer'='10';
             'Sanilac'='10';
             'St. Clair'='10'")

PIHPname<-recode(PIHP, "'1'='Northcare';
                         '2'='NMRE';
                         '3'='LRP';
                         '4'='SWMBH';
                         '5'='MSHN'; 
                         '6'='CMHPSM';
                         '7'='DWMHA';
                         '8'='OCCMHA';
                         '9'='MCMHS';
                         '10'='Region10'")

Master<-cbind(Master,PIHP,PIHPname)
#table(Master$PIHP)

#reordering the columns
Master<-Master[c(1,17,18,2,3,16,15,4:14)]

return(Master)
}

Master <- createMaster()

# Calculating Units per 1000 to standardize utilization across CMH/PIHPs

# This makes comparisons between two CMH/PIHPs that have very different
# population sizes meaningful. Need unique counts per CMHSP per year.

# Master$Unitsper1000<-(Master$SumOfUnits/(x*1000))

#Printing first 10 rows to see what dataframe looks like
# head(Master, n=10)

#Output Master .csv file
write.csv(Master, 
          file="C:\\Users\\Josh\\Documents\\GitHub\\open404\\data\\clean\\Master",
          row.names = FALSE)

