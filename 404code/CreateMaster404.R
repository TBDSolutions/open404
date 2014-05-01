
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
########                            May 2014        #########
#############################################################

## Currently, the cost reports are separated based on year and population
## To have a meaningful master dataset, first have to compile and format each individual dataset

# Note: 
## When adding future data, just repeat the upload and formatting code described
## below, changing the FY code to correspond to the current year being loaded in

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
install.packages('reshape')

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
## Stacking Dataframes to create master 404 ###########################
#######################################################################
Master<-rbind(MIA_2006, MIA_2007, MIA_2008, MIA_2009, MIA_2010, MIA_2011,
              MIC_2006, MIC_2007, MIC_2008, MIC_2009, MIC_2010, MIC_2011,
              dd_2006, dd_2007, dd_2008, dd_2009, dd_2010, dd_2011, MIC_2012, 
              MIA_2012, dd_2012)

attach(Master)

######################################################################
## Recoding and creating new variables ###############################
######################################################################

#Changing HCPCS code from factor to character
##COERCES NAs because some codes have letters in them
#so need to change var to character instead of numeric

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

install.packages('car')
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

#table(Unit_Hours)
Master<-cbind(Master, Unit_Hours)

#Dropping Revenue code, unit type, and first of modifier; not needed for analysis
Master$FirstOfRevenue.Code<-NULL
Master$FirstOfModifier<-NULL
Master$UnitType<-NULL

#Reordering the columns
Master<-Master[c(1,2,3,4,5,12,6,7,8,9,10,11)]

## Creating a "Service" variable to group the servie descriptions##
## This creates meaningful groups for analysis ##

#First need to add in fake HCPCS codes so I can create a "Service" variable based on HCPCS code

#To use ifelse, first need to change variables to character
Master$FirstOfHCPCS.Code<-as.character(Master$FirstOfHCPCS.Code)
Master$FirstofService.Description<-as.character(Master$FirstofService.Description)

#Using "ifelse" to replace NA HCPCS codes with fake codes
Master$FirstOfHCPCS.Code<-ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="State Mental Retardation Facility - Inpatient (ICF/MR) PT65",0,
                                 ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="State Psychiatric Hospital - Inpatient PT22", 1,
                                        ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Local Psychiatric Hospital/IMD PT68"|
                                                 Master$FirstofService.Description== "Local Psychiatric Hospital - Acute Community PT73" | Master$FirstofService.Description=="Extended Observation Beds ", 2,
                                               ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description =="Inpatient Hospital Ancillary Services - Room and Board" |
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Leave of Absence" |
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Pharmacy" | 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Medical/Surgical Supplies and Devices"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Laboratory"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services -EKG/ECG"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - EEG"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Room and Board"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Psychiatric/Psychological Treatments/Services"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Other Diagnosis Services"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Other Therapeutic Services"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Radiology"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Respiratory Services"| 
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services -Physical Therapy"|
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Occupational Therapy"|
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Speech-Language Pathology"|
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Emergency Room"|
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Pulmonary Function"|
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Audiology"|
                                                        Master$FirstofService.Description=="Inpatient Hospital Ancillary Services - Magnetic Resonance Technology (MRT)"|
                                                        Master$FirstofService.Description== "Inpatient Hospital Ancillary Services - Pharmacy"|
                                                        Master$FirstofService.Description== "Additional Codes-ECT Facility Charge" | 
                                                        Master$FirstofService.Description=="ECT Anesthesia" |
                                                        Master$FirstofService.Description=="ECT Recovery Room", 3,
                                                      ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Peer Directed and Operated Support Services",4,
                                                             ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Pharmacy (Drugs and Other Biologicals)",5,
                                                                    ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Crisis Observation Care",6,
                                                                           ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Outpatient Partial Hospitalization",7,
                                                                                  ifelse(is.na(Master$FirstOfHCPCS.Code)==TRUE & Master$FirstofService.Description=="Other", 8, Master$FirstOfHCPCS.Code)))))))))

#table(Master$FirstOfHCPCS.Code)

#Checking to make sure there are no missing HCPCS codes
sum(is.na(Master$FirstOfHCPCS.Code)) #Result is 0

#Need to change vars back to a factor
Master$FirstOfHCPCS.Code<-as.factor(Master$FirstOfHCPCS.Code)
Master$FirstofService.Description<-as.factor(Master$FirstofService.Description)
#str(Master$FirstOfHCPCS.Code)
#str(Master$FirstofService.Description)

#Grouping Service.Description into more general categories (variable named 'Service')
library("car")
Service <-recode(Master$FirstOfHCPCS.Code,
                 "'0'='State Mental Retardation Facility';
                 'T2025'='Fiscal Intermediary Services';
                 'H2000'='Behavioral Treatment';
                 'H2019'='Behavioral Treatment';
                 '104'='Ancillary Services / ECT';
                 '00104'='Ancillary Services / ECT';
                 '90870'='Ancillary Services / ECT';
                 '80100'='Screening & Assessment';
                 '80101'='Screening & Assessment';
                 '82075'='Screening & Assessment';
                 '90801'='Screening & Assessment';
                 '90802'='Screening & Assessment';
                 '90887'='Screening & Assessment';
                 '96100'='Screening & Assessment';
                 '96101'='Screening & Assessment';
                 '96102'='Screening & Assessment';
                 '96103'='Screening & Assessment';
                 '96105'='Screening & Assessment';
                 '96110'='Screening & Assessment';
                 '96111'='Screening & Assessment';
                 '96115'='Screening & Assessment';
                 '96116'='Screening & Assessment';
                 '96117'='Screening & Assessment';
                 '96118'='Screening & Assessment';
                 '96119'='Screening & Assessment';
                 '96120'='Screening & Assessment';
                 '97802'='Screening & Assessment';
                 '97803'='Screening & Assessment';
                 'H0001'='Screening & Assessment';
                 'H0002'='Screening & Assessment';
                 'H0003'='Screening & Assessment';
                 'H0031'='Screening & Assessment';
                 'H0048'='Screening & Assessment';
                 'H0049'='Screening & Assessment';
                 'T1001'='Screening & Assessment';
                 'T1023'='Screening & Assessment';
                 'T2011'='Screening & Assessment';
                 '90772'='Medication';
                 '90782'='Medication';
                 '90788'='Medication';
                 '90862'='Medication';
                 '96372'='Medication';
                 '99211'='Medication';
                 '99506'='Medication';
                 '99605'='Medication';
                 'G0351'='Medication';
                 'H0020'='Medication';
                 'H0033'='Medication';
                 'M0064'='Medication';
                 'T1999'='Medication';
                 'H0045'='Respite';
                 'S5150'='Respite';
                 'S5151'='Respite';
                 'S9125'='Respite';
                 'T1005'='Respite';
                 'T2036'='Respite';
                 'T2037'='Respite';
                 'H0019'='Residential Treatment';
                 'H2016'='Community Living Supports';
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
                 'H0006'='Care Coordination';
                 'H0032'='Care Coordination';
                 'H0039'='Care Coordination';
                 'H2021'='Care Coordination';
                 'H2022'='Care Coordination';
                 'T1016'='Care Coordination';
                 'T1017'='Care Coordination';
                 'T2023'='Care Coordination';
                 'H0023'='Peer Services';
                 'H0038'='Peer Services';
                 'H0046'='Peer Services';
                 'H2030'='Peer Services';
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
                 'S5160'='Monitoring';
                 'S5161'='Monitoring';
                 '97761'='Equipment';
                 '97762'='Equipment';
                 'E1399'='Equipment';
                 'S5165'='Equipment';
                 'S5199'='Equipment';
                 'T2028'='Equipment';
                 'T2029'='Equipment';
                 'T2039'='Equipment';
                 'H2014'='Employment Services';
                 'H2023'='Employment Services';
                 'T2015'='Employment Services';
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
                 'H0010'='Crisis Services';
                 'H0012'='Crisis Services';
                 'H0014'='Crisis Services';
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
                 'T5999'='Other';
                 '1'='State Hospitalization';
                 '2'='Local Hospitalization';
                 '3'='Ancillary Services / ECT';
                 '4'='Peer Services';
                 '5'='Medication';
                 '6'='Crisis Services';
                 '7'='Crisis Services';
                 '8'='Other';
                 'G0409'='Peer Services';
                 'H0037'='Medication';
                 'H2010'='Medication';
                 'H0050'='Outpatient Therapy'")
table(Service)

#attaching "Service" to "Master" dataframe
Master<-cbind(Master, Service) 

#reordering the columns
Master<-Master[c(1,2,3,13,4:12)]

## Adding PIHP regions to the dataset ##

library(car)

#First, need to recode Network180, Copper Country, and North Country
#Currently entered as "Networy180", "Copper County," and "North country "

Master$CMHSP<-recode(Master$CMHSP, "'Copper County'='Copper Country';
                     'Networy180'='Network180';
                     'North country '='North Country'")

#table(Master$CMHSP)

PIHP<-recode(Master$CMHSP, "'Copper Country'='1';
             'Network180'='3';
             'Gogebic'='1';
             'Hiawatha'='1';
             'Northpointe'='1'; 
             'Pathways'='1';
             'AuSable Valley'='2';
             'Manistee-Benzie'='2';
             'North country'='2';
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

Master<-cbind(Master,PIHP)
#table(Master$PIHP)

#Reordering the Columns
Master<-Master[c(2,14,1,3:13)]

#Calculating Units per 1000 to standardize utilization across CMH/PIHPs

#This makes comparisons between two CMH/PIHPs that have very different
#population sizes meaningful

Master$Unitsper1000<-(Master$SumOfUnits/1000)

#Printing first 10 rows to see what dataframe looks like
head(Master, n=10)

############################################
#Creating Aggregate Dataframes for Analysis#
############################################

## By creating aggregate dataframes, we can look at the totals for each CMH, PIHP, 
## population, or any combination of the three.

## Aggregating this way will allow for easier analysis

install.packages('plyr')
library(plyr)


##There are multiple HCPCS codes per Service, and so the calculated rates do not end up acurately reflecting the 
##totals.  Therefore, they will be re-calculated and added to the dataframes later.

#Creating a data frame that sums the variables at each level of service, region, population, unit hour and year
Agg_PIHP<-ddply(Master, .(FY, PIHP, Service, FirstofService.Description, Population, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))


#Creating a data frame that sums the variables at each level of service, CMH, population, unit hour and year
Agg_CMH<-ddply(Master, .(FY, CMHSP, Service, FirstofService.Description, Population, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))


#Printing first 10 rows of dataframes just to see what they look like

head(Agg_PIHP, n=10)
head(Agg_CMH, n=10)


####################################################################################################

##Creating subset dataframes from Master so analysis can be performed for each population##

MIC <-data.frame(subset(Master, Master$Population == "MIC", select = c(1:15)))
attach(MIC)

#Removing 'Population' since whole dataframe is MIC
MIC$Population<-NULL

MIA <-data.frame(subset(Master, Master$Population == "MIA", select = c(1:15)))
attach(MIA)

#Removing 'Population' since whole dataframe is MIA
MIA$Population<-NULL

DD <-data.frame(subset(Master, Master$Population == "DD", select = c(1:15)))
attach(DD)

#Removing 'Population' since whole dataframe is DD
DD$Population<-NULL

################################

#Creating aggregate dataframes for each population at the level of CMH and PIHP

DD_CMH<-ddply(DD, .(FY, CMHSP, Service, FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
              SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
              Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
              CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

DD_PIHP<-ddply(DD, .(FY, PIHP, Service, FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

MIC_CMH<-ddply(MIC, .(FY, CMHSP, FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

MIC_PIHP<-ddply(MIC, .(FY, PIHP, Service,FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

MIA_CMH<-ddply(MIA, .(FY, CMHSP, Service,FirstofService.Description, Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

MIA_PIHP<-ddply(MIA, .(FY, PIHP, Service, FirstofService.Description,Unit_Hours), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Printing first 10 rows to see an example of what the dataframes look like
head(MIA_PIHP, n=10)
head(MIA_CMH, n=10)

######################################
## Formatting Datasets for Analysis ##
######################################

## To be able to understand differences across services, we will 
## look only at the service groups (Service).  These services
## need to become columns instead of rows.  The below process
## creates such a dataframe

#Getting rid of Unit_Hours to get equal #s of rows
PIHP<-ddply(Master, .(FY, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
            SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
            Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
            CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Dental is not included because values only go until 2009
#DentalTest<-data.frame(subset(PIHP, Service=="Dental", select=c(1:10)))

#Creating data frames for every service and then merging into 1 df

LocalHosp<-data.frame(subset(PIHP, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(PIHP, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(PIHP, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(PIHP, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(PIHP, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(PIHP, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(PIHP, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(PIHP, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(PIHP, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(PIHP, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(PIHP, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(PIHP, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(PIHP, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(PIHP, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(PIHP, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(PIHP, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(PIHP, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(PIHP, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(PIHP, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(PIHP, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(PIHP, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_PIHP<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                          OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                          SUD, Behavioral, Medication, Outpatient, Respite,
                          Peer, Screening, Ancillary, Family, Transportation,
                          Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_PIHP)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_PIHP <- rename(Services_PIHP, my_changes)

#Removing FY, Service, and CMH duplicates
Services_PIHP$Service<-NULL
Services_PIHP$FY.1<-NULL
Services_PIHP$FY.2<-NULL
Services_PIHP$FY.3<-NULL
Services_PIHP$FY.4<-NULL
Services_PIHP$FY.5<-NULL
Services_PIHP$FY.6<-NULL
Services_PIHP$FY.7<-NULL
Services_PIHP$FY.8<-NULL
Services_PIHP$FY.9<-NULL
Services_PIHP$FY.10<-NULL
Services_PIHP$FY.11<-NULL
Services_PIHP$FY.12<-NULL
Services_PIHP$FY.13<-NULL
Services_PIHP$FY.14<-NULL
Services_PIHP$FY.15<-NULL
Services_PIHP$FY.16<-NULL
Services_PIHP$FY.17<-NULL
Services_PIHP$FY.18<-NULL
Services_PIHP$FY.19<-NULL
Services_PIHP$FY.20<-NULL
Services_PIHP$FY.21<-NULL
Services_PIHP$PIHP.1<-NULL
Services_PIHP$PIHP.2<-NULL
Services_PIHP$PIHP.3<-NULL
Services_PIHP$PIHP.4<-NULL
Services_PIHP$PIHP.5<-NULL
Services_PIHP$PIHP.6<-NULL
Services_PIHP$PIHP.7<-NULL
Services_PIHP$PIHP.8<-NULL
Services_PIHP$PIHP.9<-NULL
Services_PIHP$PIHP.10<-NULL
Services_PIHP$PIHP.11<-NULL
Services_PIHP$PIHP.12<-NULL
Services_PIHP$PIHP.13<-NULL
Services_PIHP$PIHP.14<-NULL
Services_PIHP$PIHP.15<-NULL
Services_PIHP$PIHP.16<-NULL
Services_PIHP$PIHP.17<-NULL
Services_PIHP$PIHP.18<-NULL
Services_PIHP$PIHP.19<-NULL
Services_PIHP$PIHP.20<-NULL
Services_PIHP$PIHP.21<-NULL
Services_PIHP$Service.1<-NULL
Services_PIHP$Service.2<-NULL
Services_PIHP$Service.3<-NULL
Services_PIHP$Service.4<-NULL
Services_PIHP$Service.5<-NULL
Services_PIHP$Service.6<-NULL
Services_PIHP$Service.7<-NULL
Services_PIHP$Service.8<-NULL
Services_PIHP$Service.9<-NULL
Services_PIHP$Service.10<-NULL
Services_PIHP$Service.11<-NULL
Services_PIHP$Service.12<-NULL
Services_PIHP$Service.13<-NULL
Services_PIHP$Service.14<-NULL
Services_PIHP$Service.15<-NULL
Services_PIHP$Service.16<-NULL
Services_PIHP$Service.17<-NULL
Services_PIHP$Service.18<-NULL
Services_PIHP$Service.19<-NULL
Services_PIHP$Service.20<-NULL
Services_PIHP$Service.21<-NULL
Services_PIHP$Population.1<-NULL
Services_PIHP$Population.2<-NULL
Services_PIHP$Population.3<-NULL
Services_PIHP$Population.4<-NULL
Services_PIHP$Population.5<-NULL
Services_PIHP$Population.6<-NULL
Services_PIHP$Population.7<-NULL
Services_PIHP$Population.8<-NULL
Services_PIHP$Population.9<-NULL
Services_PIHP$Population.10<-NULL
Services_PIHP$Population.11<-NULL
Services_PIHP$Population.12<-NULL
Services_PIHP$Population.13<-NULL
Services_PIHP$Population.14<-NULL
Services_PIHP$Population.15<-NULL
Services_PIHP$Population.16<-NULL
Services_PIHP$Population.17<-NULL
Services_PIHP$Population.18<-NULL
Services_PIHP$Population.19<-NULL
Services_PIHP$Population.20<-NULL
Services_PIHP$Population.21<-NULL

#Printing first 10 rows to see an example of the dataframe
head(Services_PIHP, n=10)


#############################################

#Creating a data frame that sums the variables at each level of service, CMH, PIHP and year
CMH<-ddply(Master, .(FY, CMHSP, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
           SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
           Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
           CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

#Dental is not included because values only go until 2009 and are only 
#reported by a few CMHs
#DentalTest<-data.frame(subset(CMH, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(CMH, Service == "Local Hospitalization", select = c(1:11)))
CareCoord<-data.frame(subset(CMH, Service == "Care Coordination", select = c(1:11)))
CLS<-data.frame(subset(CMH, Service == "Community Living Supports", select = c(1:11)))
Employment<-data.frame(subset(CMH, Service == "Employment Services", select = c(1:11)))
Health<-data.frame(subset(CMH, Service == "Health Services", select = c(1:11)))
OTPTSLT<-data.frame(subset(CMH, Service == "OT/PT/SLT", select = c(1:11)))
Prevention<-data.frame(subset(CMH, Service == "Prevention", select = c(1:11)))
StateHosp<-data.frame(subset(CMH, Service == "State Hospitalization", select = c(1:11)))
Crisis<-data.frame(subset(CMH, Service == "Crisis Services", select = c(1:11)))
Residential<-data.frame(subset(CMH, Service == "Residential Treatment", select = c(1:11)))
SUD<-data.frame(subset(CMH, Service == "Substance Abuse Outpatient", select = c(1:11)))
Behavioral<-data.frame(subset(CMH, Service == "Behavioral Treatment", select = c(1:11)))
Medication<-data.frame(subset(CMH, Service == "Medication", select = c(1:11)))
Outpatient<-data.frame(subset(CMH, Service == "Outpatient Therapy", select = c(1:11)))
Respite<-data.frame(subset(CMH, Service == "Respite", select = c(1:11)))
Peer<-data.frame(subset(CMH, Service == "Peer Services", select = c(1:11)))
Screening<-data.frame(subset(CMH, Service == "Screening & Assessment", select = c(1:11)))
Ancillary<-data.frame(subset(CMH, Service == "Ancillary Services / ECT", select = c(1:11)))
Family<-data.frame(subset(CMH, Service == "Family Services", select = c(1:11)))
Transportation<-data.frame(subset(CMH, Service == "Transportation", select = c(1:11)))
Monitoring<-data.frame(subset(CMH, Service == "Monitoring", select = c(1:11)))

#Merging all of the service dataframes
Services_CMH<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                         OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                         SUD, Behavioral, Medication, Outpatient, Respite,
                         Peer, Screening, Ancillary, Family, Transportation,
                         Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_CMH)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_CMH <- rename(Services_CMH, my_changes)

#Removing FY, Service, and CMH duplicates
Services_CMH$Service<-NULL
Services_CMH$FY.1<-NULL
Services_CMH$FY.2<-NULL
Services_CMH$FY.3<-NULL
Services_CMH$FY.4<-NULL
Services_CMH$FY.5<-NULL
Services_CMH$FY.6<-NULL
Services_CMH$FY.7<-NULL
Services_CMH$FY.8<-NULL
Services_CMH$FY.9<-NULL
Services_CMH$FY.10<-NULL
Services_CMH$FY.11<-NULL
Services_CMH$FY.12<-NULL
Services_CMH$FY.13<-NULL
Services_CMH$FY.14<-NULL
Services_CMH$FY.15<-NULL
Services_CMH$FY.16<-NULL
Services_CMH$FY.17<-NULL
Services_CMH$FY.18<-NULL
Services_CMH$FY.19<-NULL
Services_CMH$FY.20<-NULL
Services_CMH$FY.21<-NULL
Services_CMH$CMHSP.1<-NULL
Services_CMH$CMHSP.2<-NULL
Services_CMH$CMHSP.3<-NULL
Services_CMH$CMHSP.4<-NULL
Services_CMH$CMHSP.5<-NULL
Services_CMH$CMHSP.6<-NULL
Services_CMH$CMHSP.7<-NULL
Services_CMH$CMHSP.8<-NULL
Services_CMH$CMHSP.9<-NULL
Services_CMH$CMHSP.10<-NULL
Services_CMH$CMHSP.11<-NULL
Services_CMH$CMHSP.12<-NULL
Services_CMH$CMHSP.13<-NULL
Services_CMH$CMHSP.14<-NULL
Services_CMH$CMHSP.15<-NULL
Services_CMH$CMHSP.16<-NULL
Services_CMH$CMHSP.17<-NULL
Services_CMH$CMHSP.18<-NULL
Services_CMH$CMHSP.19<-NULL
Services_CMH$CMHSP.20<-NULL
Services_CMH$CMHSP.21<-NULL
Services_CMH$Service.1<-NULL
Services_CMH$Service.2<-NULL
Services_CMH$Service.3<-NULL
Services_CMH$Service.4<-NULL
Services_CMH$Service.5<-NULL
Services_CMH$Service.6<-NULL
Services_CMH$Service.7<-NULL
Services_CMH$Service.8<-NULL
Services_CMH$Service.9<-NULL
Services_CMH$Service.10<-NULL
Services_CMH$Service.11<-NULL
Services_CMH$Service.12<-NULL
Services_CMH$Service.13<-NULL
Services_CMH$Service.14<-NULL
Services_CMH$Service.15<-NULL
Services_CMH$Service.16<-NULL
Services_CMH$Service.17<-NULL
Services_CMH$Service.18<-NULL
Services_CMH$Service.19<-NULL
Services_CMH$Service.20<-NULL
Services_CMH$Service.21<-NULL
Services_CMH$PIHP.1<-NULL
Services_CMH$PIHP.2<-NULL
Services_CMH$PIHP.3<-NULL
Services_CMH$PIHP.4<-NULL
Services_CMH$PIHP.5<-NULL
Services_CMH$PIHP.6<-NULL
Services_CMH$PIHP.7<-NULL
Services_CMH$PIHP.8<-NULL
Services_CMH$PIHP.9<-NULL
Services_CMH$PIHP.10<-NULL
Services_CMH$PIHP.11<-NULL
Services_CMH$PIHP.12<-NULL
Services_CMH$PIHP.13<-NULL
Services_CMH$PIHP.14<-NULL
Services_CMH$PIHP.15<-NULL
Services_CMH$PIHP.16<-NULL
Services_CMH$PIHP.17<-NULL
Services_CMH$PIHP.18<-NULL
Services_CMH$PIHP.19<-NULL
Services_CMH$PIHP.20<-NULL
Services_CMH$PIHP.21<-NULL


#########################################################

#Repeating above processes for each population ##########

#########
## MIC ##
#########

MIC_Test<-ddply(MIC, .(FY, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))
#Creating data frames for every service and then merging into 1 df

#Dental is not included because all values are empty
#DentalTest<-data.frame(subset(MIC_Test, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(MIC_Test, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(MIC_Test, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(MIC_Test, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(MIC_Test, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(MIC_Test, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(MIC_Test, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(MIC_Test, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(MIC_Test, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(MIC_Test, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(MIC_Test, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(MIC_Test, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(MIC_Test, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(MIC_Test, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(MIC_Test, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(MIC_Test, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(MIC_Test, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(MIC_Test, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(MIC_Test, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(MIC_Test, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(MIC_Test, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(MIC_Test, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_MIC_PIHP<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                              OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                              SUD, Behavioral, Medication, Outpatient, Respite,
                              Peer, Screening, Ancillary, Family, Transportation,
                              Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_MIC_PIHP)

#Will keep first FY and PIHP, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_MIC_PIHP <- rename(Services_MIC_PIHP, my_changes)

#Removing FY, Service, and PIHP duplicates
Services_MIC_PIHP$Service<-NULL
Services_MIC_PIHP$FY.1<-NULL
Services_MIC_PIHP$FY.2<-NULL
Services_MIC_PIHP$FY.3<-NULL
Services_MIC_PIHP$FY.4<-NULL
Services_MIC_PIHP$FY.5<-NULL
Services_MIC_PIHP$FY.6<-NULL
Services_MIC_PIHP$FY.7<-NULL
Services_MIC_PIHP$FY.8<-NULL
Services_MIC_PIHP$FY.9<-NULL
Services_MIC_PIHP$FY.10<-NULL
Services_MIC_PIHP$FY.11<-NULL
Services_MIC_PIHP$FY.12<-NULL
Services_MIC_PIHP$FY.13<-NULL
Services_MIC_PIHP$FY.14<-NULL
Services_MIC_PIHP$FY.15<-NULL
Services_MIC_PIHP$FY.16<-NULL
Services_MIC_PIHP$FY.17<-NULL
Services_MIC_PIHP$FY.18<-NULL
Services_MIC_PIHP$FY.19<-NULL
Services_MIC_PIHP$FY.20<-NULL
Services_MIC_PIHP$FY.21<-NULL
Services_MIC_PIHP$PIHP.1<-NULL
Services_MIC_PIHP$PIHP.2<-NULL
Services_MIC_PIHP$PIHP.3<-NULL
Services_MIC_PIHP$PIHP.4<-NULL
Services_MIC_PIHP$PIHP.5<-NULL
Services_MIC_PIHP$PIHP.6<-NULL
Services_MIC_PIHP$PIHP.7<-NULL
Services_MIC_PIHP$PIHP.8<-NULL
Services_MIC_PIHP$PIHP.9<-NULL
Services_MIC_PIHP$PIHP.10<-NULL
Services_MIC_PIHP$PIHP.11<-NULL
Services_MIC_PIHP$PIHP.12<-NULL
Services_MIC_PIHP$PIHP.13<-NULL
Services_MIC_PIHP$PIHP.14<-NULL
Services_MIC_PIHP$PIHP.15<-NULL
Services_MIC_PIHP$PIHP.16<-NULL
Services_MIC_PIHP$PIHP.17<-NULL
Services_MIC_PIHP$PIHP.18<-NULL
Services_MIC_PIHP$PIHP.19<-NULL
Services_MIC_PIHP$PIHP.20<-NULL
Services_MIC_PIHP$PIHP.21<-NULL
Services_MIC_PIHP$Service.1<-NULL
Services_MIC_PIHP$Service.2<-NULL
Services_MIC_PIHP$Service.3<-NULL
Services_MIC_PIHP$Service.4<-NULL
Services_MIC_PIHP$Service.5<-NULL
Services_MIC_PIHP$Service.6<-NULL
Services_MIC_PIHP$Service.7<-NULL
Services_MIC_PIHP$Service.8<-NULL
Services_MIC_PIHP$Service.9<-NULL
Services_MIC_PIHP$Service.10<-NULL
Services_MIC_PIHP$Service.11<-NULL
Services_MIC_PIHP$Service.12<-NULL
Services_MIC_PIHP$Service.13<-NULL
Services_MIC_PIHP$Service.14<-NULL
Services_MIC_PIHP$Service.15<-NULL
Services_MIC_PIHP$Service.16<-NULL
Services_MIC_PIHP$Service.17<-NULL
Services_MIC_PIHP$Service.18<-NULL
Services_MIC_PIHP$Service.19<-NULL
Services_MIC_PIHP$Service.20<-NULL
Services_MIC_PIHP$Service.21<-NULL

## MIC CMH ##

MIC_CMH2<-ddply(MIC, .(FY, CMHSP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

#Dental is not included because all values are empty
#DentalTest<-data.frame(subset(MIC_CMH2, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(MIC_CMH2, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(MIC_CMH2, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(MIC_CMH2, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(MIC_CMH2, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(MIC_CMH2, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(MIC_CMH2, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(MIC_CMH2, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(MIC_CMH2, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(MIC_CMH2, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(MIC_CMH2, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(MIC_CMH2, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(MIC_CMH2, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(MIC_CMH2, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(MIC_CMH2, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(MIC_CMH2, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(MIC_CMH2, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(MIC_CMH2, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(MIC_CMH2, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(MIC_CMH2, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(MIC_CMH2, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(MIC_CMH2, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_MIC_CMH<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                             OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                             SUD, Behavioral, Medication, Outpatient, Respite,
                             Peer, Screening, Ancillary, Family, Transportation,
                             Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_MIC_CMH)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")
Services_MIC_CMH <- rename(Services_MIC_CMH, my_changes)

#Removing FY, Service, and CMH duplicates
Services_MIC_CMH$Service<-NULL
Services_MIC_CMH$FY.1<-NULL
Services_MIC_CMH$FY.2<-NULL
Services_MIC_CMH$FY.3<-NULL
Services_MIC_CMH$FY.4<-NULL
Services_MIC_CMH$FY.5<-NULL
Services_MIC_CMH$FY.6<-NULL
Services_MIC_CMH$FY.7<-NULL
Services_MIC_CMH$FY.8<-NULL
Services_MIC_CMH$FY.9<-NULL
Services_MIC_CMH$FY.10<-NULL
Services_MIC_CMH$FY.11<-NULL
Services_MIC_CMH$FY.12<-NULL
Services_MIC_CMH$FY.13<-NULL
Services_MIC_CMH$FY.14<-NULL
Services_MIC_CMH$FY.15<-NULL
Services_MIC_CMH$FY.16<-NULL
Services_MIC_CMH$FY.17<-NULL
Services_MIC_CMH$FY.18<-NULL
Services_MIC_CMH$FY.19<-NULL
Services_MIC_CMH$FY.20<-NULL
Services_MIC_CMH$FY.21<-NULL
Services_MIC_CMH$CMHSP.1<-NULL
Services_MIC_CMH$CMHSP.2<-NULL
Services_MIC_CMH$CMHSP.3<-NULL
Services_MIC_CMH$CMHSP.4<-NULL
Services_MIC_CMH$CMHSP.5<-NULL
Services_MIC_CMH$CMHSP.6<-NULL
Services_MIC_CMH$CMHSP.7<-NULL
Services_MIC_CMH$CMHSP.8<-NULL
Services_MIC_CMH$CMHSP.9<-NULL
Services_MIC_CMH$CMHSP.10<-NULL
Services_MIC_CMH$CMHSP.11<-NULL
Services_MIC_CMH$CMHSP.12<-NULL
Services_MIC_CMH$CMHSP.13<-NULL
Services_MIC_CMH$CMHSP.14<-NULL
Services_MIC_CMH$CMHSP.15<-NULL
Services_MIC_CMH$CMHSP.16<-NULL
Services_MIC_CMH$CMHSP.17<-NULL
Services_MIC_CMH$CMHSP.18<-NULL
Services_MIC_CMH$CMHSP.19<-NULL
Services_MIC_CMH$CMHSP.20<-NULL
Services_MIC_CMH$CMHSP.21<-NULL
Services_MIC_CMH$Service.1<-NULL
Services_MIC_CMH$Service.2<-NULL
Services_MIC_CMH$Service.3<-NULL
Services_MIC_CMH$Service.4<-NULL
Services_MIC_CMH$Service.5<-NULL
Services_MIC_CMH$Service.6<-NULL
Services_MIC_CMH$Service.7<-NULL
Services_MIC_CMH$Service.8<-NULL
Services_MIC_CMH$Service.9<-NULL
Services_MIC_CMH$Service.10<-NULL
Services_MIC_CMH$Service.11<-NULL
Services_MIC_CMH$Service.12<-NULL
Services_MIC_CMH$Service.13<-NULL
Services_MIC_CMH$Service.14<-NULL
Services_MIC_CMH$Service.15<-NULL
Services_MIC_CMH$Service.16<-NULL
Services_MIC_CMH$Service.17<-NULL
Services_MIC_CMH$Service.18<-NULL
Services_MIC_CMH$Service.19<-NULL
Services_MIC_CMH$Service.20<-NULL
Services_MIC_CMH$Service.21<-NULL

#########
## MIA ##
#########

MIA_PIHP_Test<-ddply(MIA, .(FY, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                     SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                     Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                     CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Only PIHP 8 uses Dental codes, only though 2009
#MIADental<-data.frame(subset(MIA_PIHP_Test, Service == "Dental", select=c(1:9)))

#Creating data frames for every service and then merging into 1 df
LocalHosp<-data.frame(subset(MIA_PIHP_Test, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(MIA_PIHP_Test, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(MIA_PIHP_Test, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(MIA_PIHP_Test, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(MIA_PIHP_Test, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(MIA_PIHP_Test, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(MIA_PIHP_Test, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(MIA_PIHP_Test, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(MIA_PIHP_Test, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(MIA_PIHP_Test, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(MIA_PIHP_Test, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(MIA_PIHP_Test, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(MIA_PIHP_Test, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(MIA_PIHP_Test, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(MIA_PIHP_Test, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(MIA_PIHP_Test, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(MIA_PIHP_Test, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(MIA_PIHP_Test, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(MIA_PIHP_Test, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(MIA_PIHP_Test, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(MIA_PIHP_Test, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_MIA_PIHP<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                              OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                              SUD, Behavioral, Medication, Outpatient, Respite,
                              Peer, Screening, Ancillary, Family, Transportation,
                              Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_MIA_PIHP)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_MIA_PIHP <- rename(Services_MIA_PIHP, my_changes)

#Removing FY, Service, and CMH duplicates
Services_MIA_PIHP$Service<-NULL
Services_MIA_PIHP$FY.1<-NULL
Services_MIA_PIHP$FY.2<-NULL
Services_MIA_PIHP$FY.3<-NULL
Services_MIA_PIHP$FY.4<-NULL
Services_MIA_PIHP$FY.5<-NULL
Services_MIA_PIHP$FY.6<-NULL
Services_MIA_PIHP$FY.7<-NULL
Services_MIA_PIHP$FY.8<-NULL
Services_MIA_PIHP$FY.9<-NULL
Services_MIA_PIHP$FY.10<-NULL
Services_MIA_PIHP$FY.11<-NULL
Services_MIA_PIHP$FY.12<-NULL
Services_MIA_PIHP$FY.13<-NULL
Services_MIA_PIHP$FY.14<-NULL
Services_MIA_PIHP$FY.15<-NULL
Services_MIA_PIHP$FY.16<-NULL
Services_MIA_PIHP$FY.17<-NULL
Services_MIA_PIHP$FY.18<-NULL
Services_MIA_PIHP$FY.19<-NULL
Services_MIA_PIHP$FY.20<-NULL
Services_MIA_PIHP$FY.21<-NULL
Services_MIA_PIHP$PIHP.1<-NULL
Services_MIA_PIHP$PIHP.2<-NULL
Services_MIA_PIHP$PIHP.3<-NULL
Services_MIA_PIHP$PIHP.4<-NULL
Services_MIA_PIHP$PIHP.5<-NULL
Services_MIA_PIHP$PIHP.6<-NULL
Services_MIA_PIHP$PIHP.7<-NULL
Services_MIA_PIHP$PIHP.8<-NULL
Services_MIA_PIHP$PIHP.9<-NULL
Services_MIA_PIHP$PIHP.10<-NULL
Services_MIA_PIHP$PIHP.11<-NULL
Services_MIA_PIHP$PIHP.12<-NULL
Services_MIA_PIHP$PIHP.13<-NULL
Services_MIA_PIHP$PIHP.14<-NULL
Services_MIA_PIHP$PIHP.15<-NULL
Services_MIA_PIHP$PIHP.16<-NULL
Services_MIA_PIHP$PIHP.17<-NULL
Services_MIA_PIHP$PIHP.18<-NULL
Services_MIA_PIHP$PIHP.19<-NULL
Services_MIA_PIHP$PIHP.20<-NULL
Services_MIA_PIHP$PIHP.21<-NULL
Services_MIA_PIHP$Service.1<-NULL
Services_MIA_PIHP$Service.2<-NULL
Services_MIA_PIHP$Service.3<-NULL
Services_MIA_PIHP$Service.4<-NULL
Services_MIA_PIHP$Service.5<-NULL
Services_MIA_PIHP$Service.6<-NULL
Services_MIA_PIHP$Service.7<-NULL
Services_MIA_PIHP$Service.8<-NULL
Services_MIA_PIHP$Service.9<-NULL
Services_MIA_PIHP$Service.10<-NULL
Services_MIA_PIHP$Service.11<-NULL
Services_MIA_PIHP$Service.12<-NULL
Services_MIA_PIHP$Service.13<-NULL
Services_MIA_PIHP$Service.14<-NULL
Services_MIA_PIHP$Service.15<-NULL
Services_MIA_PIHP$Service.16<-NULL
Services_MIA_PIHP$Service.17<-NULL
Services_MIA_PIHP$Service.18<-NULL
Services_MIA_PIHP$Service.19<-NULL
Services_MIA_PIHP$Service.20<-NULL
Services_MIA_PIHP$Service.21<-NULL

## MIA CMH ##

MIA_CMH2<-ddply(MIA, .(FY, CMHSP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

#Dental is not included because all values are empty
#DentalTest<-data.frame(subset(MIC_CMH2, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(MIA_CMH2, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(MIA_CMH2, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(MIA_CMH2, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(MIA_CMH2, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(MIA_CMH2, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(MIA_CMH2, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(MIA_CMH2, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(MIA_CMH2, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(MIA_CMH2, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(MIA_CMH2, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(MIA_CMH2, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(MIA_CMH2, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(MIA_CMH2, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(MIA_CMH2, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(MIA_CMH2, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(MIA_CMH2, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(MIA_CMH2, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(MIA_CMH2, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(MIA_CMH2, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(MIA_CMH2, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(MIA_CMH2, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_MIA_CMH<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                             OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                             SUD, Behavioral, Medication, Outpatient, Respite,
                             Peer, Screening, Ancillary, Family, Transportation,
                             Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_MIA_CMH)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_MIA_CMH <- rename(Services_MIA_CMH, my_changes)

#Removing FY, Service, and CMH duplicates
Services_MIA_CMH$Service<-NULL
Services_MIA_CMH$FY.1<-NULL
Services_MIA_CMH$FY.2<-NULL
Services_MIA_CMH$FY.3<-NULL
Services_MIA_CMH$FY.4<-NULL
Services_MIA_CMH$FY.5<-NULL
Services_MIA_CMH$FY.6<-NULL
Services_MIA_CMH$FY.7<-NULL
Services_MIA_CMH$FY.8<-NULL
Services_MIA_CMH$FY.9<-NULL
Services_MIA_CMH$FY.10<-NULL
Services_MIA_CMH$FY.11<-NULL
Services_MIA_CMH$FY.12<-NULL
Services_MIA_CMH$FY.13<-NULL
Services_MIA_CMH$FY.14<-NULL
Services_MIA_CMH$FY.15<-NULL
Services_MIA_CMH$FY.16<-NULL
Services_MIA_CMH$FY.17<-NULL
Services_MIA_CMH$FY.18<-NULL
Services_MIA_CMH$FY.19<-NULL
Services_MIA_CMH$FY.20<-NULL
Services_MIA_CMH$FY.21<-NULL
Services_MIA_CMH$CMHSP.1<-NULL
Services_MIA_CMH$CMHSP.2<-NULL
Services_MIA_CMH$CMHSP.3<-NULL
Services_MIA_CMH$CMHSP.4<-NULL
Services_MIA_CMH$CMHSP.5<-NULL
Services_MIA_CMH$CMHSP.6<-NULL
Services_MIA_CMH$CMHSP.7<-NULL
Services_MIA_CMH$CMHSP.8<-NULL
Services_MIA_CMH$CMHSP.9<-NULL
Services_MIA_CMH$CMHSP.10<-NULL
Services_MIA_CMH$CMHSP.11<-NULL
Services_MIA_CMH$CMHSP.12<-NULL
Services_MIA_CMH$CMHSP.13<-NULL
Services_MIA_CMH$CMHSP.14<-NULL
Services_MIA_CMH$CMHSP.15<-NULL
Services_MIA_CMH$CMHSP.16<-NULL
Services_MIA_CMH$CMHSP.17<-NULL
Services_MIA_CMH$CMHSP.18<-NULL
Services_MIA_CMH$CMHSP.19<-NULL
Services_MIA_CMH$CMHSP.20<-NULL
Services_MIA_CMH$CMHSP.21<-NULL
Services_MIA_CMH$Service.1<-NULL
Services_MIA_CMH$Service.2<-NULL
Services_MIA_CMH$Service.3<-NULL
Services_MIA_CMH$Service.4<-NULL
Services_MIA_CMH$Service.5<-NULL
Services_MIA_CMH$Service.6<-NULL
Services_MIA_CMH$Service.7<-NULL
Services_MIA_CMH$Service.8<-NULL
Services_MIA_CMH$Service.9<-NULL
Services_MIA_CMH$Service.10<-NULL
Services_MIA_CMH$Service.11<-NULL
Services_MIA_CMH$Service.12<-NULL
Services_MIA_CMH$Service.13<-NULL
Services_MIA_CMH$Service.14<-NULL
Services_MIA_CMH$Service.15<-NULL
Services_MIA_CMH$Service.16<-NULL
Services_MIA_CMH$Service.17<-NULL
Services_MIA_CMH$Service.18<-NULL
Services_MIA_CMH$Service.19<-NULL
Services_MIA_CMH$Service.20<-NULL
Services_MIA_CMH$Service.21<-NULL


########
## DD ##
########

DD_CMH2<-ddply(DD, .(FY, CMHSP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
               SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
               Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
               CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

#Dental is not included because values only go through 2009
#DentalTest<-data.frame(subset(DD_CMH2, Service=="Dental", select=c(1:9)))

LocalHosp<-data.frame(subset(DD_CMH2, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(DD_CMH2, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(DD_CMH2, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(DD_CMH2, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(DD_CMH2, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(DD_CMH2, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(DD_CMH2, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(DD_CMH2, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(DD_CMH2, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(DD_CMH2, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(DD_CMH2, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(DD_CMH2, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(DD_CMH2, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(DD_CMH2, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(DD_CMH2, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(DD_CMH2, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(DD_CMH2, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(DD_CMH2, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(DD_CMH2, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(DD_CMH2, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(DD_CMH2, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_DD_CMH<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                            OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                            SUD, Behavioral, Medication, Outpatient, Respite,
                            Peer, Screening, Ancillary, Family, Transportation,
                            Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_DD_CMH)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_DD_CMH <- rename(Services_DD_CMH, my_changes)

#Removing FY, Service, and CMH duplicates
Services_DD_CMH$Service<-NULL
Services_DD_CMH$FY.1<-NULL
Services_DD_CMH$FY.2<-NULL
Services_DD_CMH$FY.3<-NULL
Services_DD_CMH$FY.4<-NULL
Services_DD_CMH$FY.5<-NULL
Services_DD_CMH$FY.6<-NULL
Services_DD_CMH$FY.7<-NULL
Services_DD_CMH$FY.8<-NULL
Services_DD_CMH$FY.9<-NULL
Services_DD_CMH$FY.10<-NULL
Services_DD_CMH$FY.11<-NULL
Services_DD_CMH$FY.12<-NULL
Services_DD_CMH$FY.13<-NULL
Services_DD_CMH$FY.14<-NULL
Services_DD_CMH$FY.15<-NULL
Services_DD_CMH$FY.16<-NULL
Services_DD_CMH$FY.17<-NULL
Services_DD_CMH$FY.18<-NULL
Services_DD_CMH$FY.19<-NULL
Services_DD_CMH$FY.20<-NULL
Services_DD_CMH$FY.21<-NULL
Services_DD_CMH$CMHSP.1<-NULL
Services_DD_CMH$CMHSP.2<-NULL
Services_DD_CMH$CMHSP.3<-NULL
Services_DD_CMH$CMHSP.4<-NULL
Services_DD_CMH$CMHSP.5<-NULL
Services_DD_CMH$CMHSP.6<-NULL
Services_DD_CMH$CMHSP.7<-NULL
Services_DD_CMH$CMHSP.8<-NULL
Services_DD_CMH$CMHSP.9<-NULL
Services_DD_CMH$CMHSP.10<-NULL
Services_DD_CMH$CMHSP.11<-NULL
Services_DD_CMH$CMHSP.12<-NULL
Services_DD_CMH$CMHSP.13<-NULL
Services_DD_CMH$CMHSP.14<-NULL
Services_DD_CMH$CMHSP.15<-NULL
Services_DD_CMH$CMHSP.16<-NULL
Services_DD_CMH$CMHSP.17<-NULL
Services_DD_CMH$CMHSP.18<-NULL
Services_DD_CMH$CMHSP.19<-NULL
Services_DD_CMH$CMHSP.20<-NULL
Services_DD_CMH$CMHSP.21<-NULL
Services_DD_CMH$Service.1<-NULL
Services_DD_CMH$Service.2<-NULL
Services_DD_CMH$Service.3<-NULL
Services_DD_CMH$Service.4<-NULL
Services_DD_CMH$Service.5<-NULL
Services_DD_CMH$Service.6<-NULL
Services_DD_CMH$Service.7<-NULL
Services_DD_CMH$Service.8<-NULL
Services_DD_CMH$Service.9<-NULL
Services_DD_CMH$Service.10<-NULL
Services_DD_CMH$Service.11<-NULL
Services_DD_CMH$Service.12<-NULL
Services_DD_CMH$Service.13<-NULL
Services_DD_CMH$Service.14<-NULL
Services_DD_CMH$Service.15<-NULL
Services_DD_CMH$Service.16<-NULL
Services_DD_CMH$Service.17<-NULL
Services_DD_CMH$Service.18<-NULL
Services_DD_CMH$Service.19<-NULL
Services_DD_CMH$Service.20<-NULL
Services_DD_CMH$Service.21<-NULL

## DD PIHP ##

DD_PIHP2<-ddply(DD, .(FY, PIHP, Service), summarize, sumOfUnits=round(sum(SumOfUnits),2), 
                SumOfCases=round(sum(SumOfCases),2), SumOfCost=round(sum(SumOfCost),2),
                Unitsper1000=round(sum(Unitsper1000),2),CostUnits=round(sum(SumOfCost) / sum(SumOfUnits),2),
                CostCase=round(sum(SumOfCost) / sum(SumOfCases),2),UnitsCase=round(sum(SumOfUnits) / sum(SumOfCases),2))

#Creating data frames for every service and then merging into 1 df

LocalHosp<-data.frame(subset(DD_PIHP2, Service == "Local Hospitalization", select = c(1:10)))
CareCoord<-data.frame(subset(DD_PIHP2, Service == "Care Coordination", select = c(1:10)))
CLS<-data.frame(subset(DD_PIHP2, Service == "Community Living Supports", select = c(1:10)))
Employment<-data.frame(subset(DD_PIHP2, Service == "Employment Services", select = c(1:10)))
Health<-data.frame(subset(DD_PIHP2, Service == "Health Services", select = c(1:10)))
OTPTSLT<-data.frame(subset(DD_PIHP2, Service == "OT/PT/SLT", select = c(1:10)))
Prevention<-data.frame(subset(DD_PIHP2, Service == "Prevention", select = c(1:10)))
StateHosp<-data.frame(subset(DD_PIHP2, Service == "State Hospitalization", select = c(1:10)))
Crisis<-data.frame(subset(DD_PIHP2, Service == "Crisis Services", select = c(1:10)))
Residential<-data.frame(subset(DD_PIHP2, Service == "Residential Treatment", select = c(1:10)))
SUD<-data.frame(subset(DD_PIHP2, Service == "Substance Abuse Outpatient", select = c(1:10)))
Behavioral<-data.frame(subset(DD_PIHP2, Service == "Behavioral Treatment", select = c(1:10)))
Medication<-data.frame(subset(DD_PIHP2, Service == "Medication", select = c(1:10)))
Outpatient<-data.frame(subset(DD_PIHP2, Service == "Outpatient Therapy", select = c(1:10)))
Respite<-data.frame(subset(DD_PIHP2, Service == "Respite", select = c(1:10)))
Peer<-data.frame(subset(DD_PIHP2, Service == "Peer Services", select = c(1:10)))
Screening<-data.frame(subset(DD_PIHP2, Service == "Screening & Assessment", select = c(1:10)))
Ancillary<-data.frame(subset(DD_PIHP2, Service == "Ancillary Services / ECT", select = c(1:10)))
Family<-data.frame(subset(DD_PIHP2, Service == "Family Services", select = c(1:10)))
Transportation<-data.frame(subset(DD_PIHP2, Service == "Transportation", select = c(1:10)))
Monitoring<-data.frame(subset(DD_PIHP2, Service == "Monitoring", select = c(1:10)))

#Merging all of the service dataframes
Services_DD_PIHP<-data.frame(LocalHosp, CareCoord, CLS, Employment, Health,
                             OTPTSLT, Prevention, StateHosp, Crisis, Residential,
                             SUD, Behavioral, Medication, Outpatient, Respite,
                             Peer, Screening, Ancillary, Family, Transportation,
                             Monitoring)

#Getting names of all the variables so they can be formatted
names(Services_DD_PIHP)

#Will keep first FY and CMH, all the rest will be removed

library("plyr") 
my_changes <- 
  c(sumOfUnits = "LocalUnits", 
    SumOfCases = "LocalCases",
    SumOfCost = "LocalCost",
    Unitsper1000 = "LocalUnits1000",
    CostCase = "LocalCostCase",
    CostUnits = "LocalCostUnits",   
    UnitsCase = "LocalUnitsCase",
    sumOfUnits.1 = "CareUnits", 
    SumOfCases.1 = "CareCases",
    SumOfCost.1 = "CareCost",
    Unitsper1000.1 = "CareUnits1000",
    CostCase.1 = "CareCostCase",
    CostUnits.1 = "CareCostUnits",   
    UnitsCase.1 = "CareUnitsCase",
    sumOfUnits.2 = "CLSUnits", 
    SumOfCases.2 = "CLSCases",
    SumOfCost.2 = "CLSCost",
    Unitsper1000.2 = "CLSUnits1000",
    CostCase.2 = "CLSCostCase",
    CostUnits.2 = "CLSCostUnits",   
    UnitsCase.2 = "CLSUnitsCase",
    sumOfUnits.3 = "EmpUnits", 
    SumOfCases.3 = "EmpCases",
    SumOfCost.3 = "EmpCost",
    Unitsper1000.3 = "EmpUnits1000",
    CostCase.3 = "EmpCostCase",
    CostUnits.3 = "EmpCostUnits",   
    UnitsCase.3 = "EmpUnitsCase",
    sumOfUnits.4 = "HealthUnits", 
    SumOfCases.4 = "HealthCases",
    SumOfCost.4 = "HealthCost",
    Unitsper1000.4 = "HealthUnits1000",
    CostCase.4 = "HealthCostCase",
    CostUnits.4 = "HealthCostUnits",   
    UnitsCase.4 = "HealthUnitsCase",
    sumOfUnits.5 = "OTUnits", 
    SumOfCases.5 = "OTCases",
    SumOfCost.5 = "OTCost",
    Unitsper1000.5 = "OTUnits1000",
    CostCase.5 = "OTCostCase",
    CostUnits.5 = "OTCostUnits",   
    UnitsCase.5 = "OTUnitsCase",
    sumOfUnits.6 = "PrevUnits", 
    SumOfCases.6 = "PrevCases",
    SumOfCost.6 = "PrevCost",
    Unitsper1000.6 = "PrevUnits1000",
    CostCase.6 = "PrevCostCase",
    CostUnits.6 = "PrevCostUnits",   
    UnitsCase.6 = "PrevUnitsCase",
    sumOfUnits.7 = "StateUnits", 
    SumOfCases.7 = "StateCases",
    SumOfCost.7 = "StateCost",
    Unitsper1000.7 = "StateUnits1000",
    CostCase.7 = "StateCostCase",
    CostUnits.7 = "StateCostUnits",   
    UnitsCase.7 = "StateUnitsCase",
    sumOfUnits.8 = "CrisisUnits", 
    SumOfCases.8 = "CrisisCases",
    SumOfCost.8 = "CrisisCost",
    Unitsper1000.8 = "CrisisUnits1000",
    CostCase.8 = "CrisisCostCase",
    CostUnits.8 = "CrisisCostUnits",   
    UnitsCase.8 = "CrisisUnitsCase",
    sumOfUnits.9 = "ResUnits", 
    SumOfCases.9 = "ResCases",
    SumOfCost.9 = "ResCost",
    Unitsper1000.9 = "ResUnits1000",
    CostCase.9 = "ResCostCase",
    CostUnits.9 = "ResCostUnits",   
    UnitsCase.9 = "ResUnitsCase",
    sumOfUnits.10 = "SUDUnits", 
    SumOfCases.10 = "SUDCases",
    SumOfCost.10 = "SUDCost",
    Unitsper1000.10 = "SUDUnits1000",
    CostCase.10 = "SUDCostCase",
    CostUnits.10 = "SUDCostUnits",   
    UnitsCase.10 = "SUDUnitsCase",
    sumOfUnits.11 = "BehavUnits", 
    SumOfCases.11 = "BehavCases",
    SumOfCost.11 = "BehavCost",
    Unitsper1000.11 = "BehavUnits1000",
    CostCase.11 = "BehavCostCase",
    CostUnits.11 = "BehavCostUnits",   
    UnitsCase.11 = "BehavUnitsCase",
    sumOfUnits.12 = "MedUnits", 
    SumOfCases.12 = "MedCases",
    SumOfCost.12 = "MedCost",
    Unitsper1000.12 = "MedUnits1000",
    CostCase.12 = "MedCostCase",
    CostUnits.12 = "MedCostUnits",   
    UnitsCase.12 = "MedUnitsCase",
    sumOfUnits.13 = "OutUnits", 
    SumOfCases.13 = "OutCases",
    SumOfCost.13 = "OutCost",
    Unitsper1000.13 = "OutUnits1000",
    CostCase.13 = "OutCostCase",
    CostUnits.13 = "OutCostUnits",   
    UnitsCase.13 = "OutUnitsCase",
    sumOfUnits.14 = "RespiteUnits", 
    SumOfCases.14 = "RespiteCases",
    SumOfCost.14 = "RespiteCost",
    Unitsper1000.14 = "RespiteUnits1000",
    CostCase.14 = "RespiteCostCase",
    CostUnits.14 = "RespiteCostUnits",   
    UnitsCase.14 = "RespiteUnitsCase",
    sumOfUnits.15 = "PeerUnits", 
    SumOfCases.15 = "PeerCases",
    SumOfCost.15 = "PeerCost",
    Unitsper1000.15 = "PeerUnits1000",
    CostCase.15 = "PeerCostCase",
    CostUnits.15 = "PeerCostUnits",   
    UnitsCase.15 = "PeerUnitsCase",
    sumOfUnits.16 = "ScreenUnits", 
    SumOfCases.16 = "ScreenCases",
    SumOfCost.16 = "ScreenCost",
    Unitsper1000.16 = "ScreenUnits1000",
    CostCase.16 = "ScreenCostCase",
    CostUnits.16 = "ScreenCostUnits",   
    UnitsCase.16 = "ScreenUnitsCase",
    sumOfUnits.17 = "AncillUnits", 
    SumOfCases.17 = "AncillCases",
    SumOfCost.17 = "AncillCost",
    Unitsper1000.17 = "AncillUnits1000",
    CostCase.17 = "AncillCostCase",
    CostUnits.17 = "AncillCostUnits",   
    UnitsCase.17 = "AncillUnitsCase",
    sumOfUnits.18 = "FamUnits", 
    SumOfCases.18 = "FamCases",
    SumOfCost.18 = "FamCost",
    Unitsper1000.18 = "FamUnits1000",
    CostCase.18 = "FamCostCase",
    CostUnits.18 = "FamCostUnits",   
    UnitsCase.18 = "FamUnitsCase",
    sumOfUnits.19 = "TransUnits", 
    SumOfCases.19 = "TransCases",
    SumOfCost.19 = "TransCost",
    Unitsper1000.19 = "TransUnits1000",
    CostCase.19 = "TransCostCase",
    CostUnits.19 = "TransCostUnits",   
    UnitsCase.19 = "TransUnitsCase",
    sumOfUnits.20 = "MonitUnits", 
    SumOfCases.20 = "MonitCases",
    SumOfCost.20 = "MonitCost",
    Unitsper1000.20 = "MonitUnits1000",
    CostCase.20 = "MonitCostCase",
    CostUnits.20 = "MonitCostUnits",   
    UnitsCase.20 = "MonitUnitsCase")

Services_DD_PIHP <- rename(Services_DD_PIHP, my_changes)

#Removing FY, Service, and CMH duplicates
Services_DD_PIHP$Service<-NULL
Services_DD_PIHP$FY.1<-NULL
Services_DD_PIHP$FY.2<-NULL
Services_DD_PIHP$FY.3<-NULL
Services_DD_PIHP$FY.4<-NULL
Services_DD_PIHP$FY.5<-NULL
Services_DD_PIHP$FY.6<-NULL
Services_DD_PIHP$FY.7<-NULL
Services_DD_PIHP$FY.8<-NULL
Services_DD_PIHP$FY.9<-NULL
Services_DD_PIHP$FY.10<-NULL
Services_DD_PIHP$FY.11<-NULL
Services_DD_PIHP$FY.12<-NULL
Services_DD_PIHP$FY.13<-NULL
Services_DD_PIHP$FY.14<-NULL
Services_DD_PIHP$FY.15<-NULL
Services_DD_PIHP$FY.16<-NULL
Services_DD_PIHP$FY.17<-NULL
Services_DD_PIHP$FY.18<-NULL
Services_DD_PIHP$FY.19<-NULL
Services_DD_PIHP$FY.20<-NULL
Services_DD_PIHP$FY.21<-NULL
Services_DD_PIHP$PIHP.1<-NULL
Services_DD_PIHP$PIHP.2<-NULL
Services_DD_PIHP$PIHP.3<-NULL
Services_DD_PIHP$PIHP.4<-NULL
Services_DD_PIHP$PIHP.5<-NULL
Services_DD_PIHP$PIHP.6<-NULL
Services_DD_PIHP$PIHP.7<-NULL
Services_DD_PIHP$PIHP.8<-NULL
Services_DD_PIHP$PIHP.9<-NULL
Services_DD_PIHP$PIHP.10<-NULL
Services_DD_PIHP$PIHP.11<-NULL
Services_DD_PIHP$PIHP.12<-NULL
Services_DD_PIHP$PIHP.13<-NULL
Services_DD_PIHP$PIHP.14<-NULL
Services_DD_PIHP$PIHP.15<-NULL
Services_DD_PIHP$PIHP.16<-NULL
Services_DD_PIHP$PIHP.17<-NULL
Services_DD_PIHP$PIHP.18<-NULL
Services_DD_PIHP$PIHP.19<-NULL
Services_DD_PIHP$PIHP.20<-NULL
Services_DD_PIHP$PIHP.21<-NULL
Services_DD_PIHP$Service.1<-NULL
Services_DD_PIHP$Service.2<-NULL
Services_DD_PIHP$Service.3<-NULL
Services_DD_PIHP$Service.4<-NULL
Services_DD_PIHP$Service.5<-NULL
Services_DD_PIHP$Service.6<-NULL
Services_DD_PIHP$Service.7<-NULL
Services_DD_PIHP$Service.8<-NULL
Services_DD_PIHP$Service.9<-NULL
Services_DD_PIHP$Service.10<-NULL
Services_DD_PIHP$Service.11<-NULL
Services_DD_PIHP$Service.12<-NULL
Services_DD_PIHP$Service.13<-NULL
Services_DD_PIHP$Service.14<-NULL
Services_DD_PIHP$Service.15<-NULL
Services_DD_PIHP$Service.16<-NULL
Services_DD_PIHP$Service.17<-NULL
Services_DD_PIHP$Service.18<-NULL
Services_DD_PIHP$Service.19<-NULL
Services_DD_PIHP$Service.20<-NULL
Services_DD_PIHP$Service.21<-NULL
