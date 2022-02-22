#-------------------------------------------------------------------------------
# You can skip this if you already have data.table installed...
# I only included it because my Mac didn't have it

# WINDOWS:
install.packages(c("data.table", "COVID19"))

# MACOS/LINUX:
install.packages("COVID19")
# follow macOS instructions in txt file.

#-------------------------------------------------------------------------------
# Remove existing environment variables

rm(list=ls())

#-------------------------------------------------------------------------------
# Make sure these libraries below are working correctly.

library(COVID19) # This package provides access to an international COVID database
library(data.table) # An enhancement of data.frame, with great capabilities
# QIUSHI: if you're using a macOS system and get an error when running...
# library(data.table)...
# let me know... spent the better part of a day trying to get it to work...
# so I might be able to help, but hopefully it isn't a problem.

#-------------------------------------------------------------------------------
# JESSE'S PATH:

jRoot <- "C:"
jPathOut <- file.path(jRoot, "Users", "HP", "OneDrive", "Desktop", "COVID")
print(jPathOut)

#-------------------------------------------------------------------------------
# CHARLIE'S PATH:

cRoot <- "~"
cPathOut <- file.path(cRoot, "Users", "charlieclark", "Documents", "GitHub", "ECO322Spring2022Project1", "data")
print(cPathOut)

#-------------------------------------------------------------------------------
# QIUSHI'S PATH:


#-------------------------------------------------------------------------------
# PATH TO BE USED (REDEFINE WITH YOUR OWN PATHOUT VARIABLE BEFORE RUNNING):

PathOut <- cPathOut # ENTER YOUR PATH VARABLE HERE!!!

#-------------------------------------------------------------------------------
# Put ENTIRE USA sorted by region into a data.table...
# We will then get 'new cases' as bulk in next section...
# (instead of parsing through it 20 times)

# store USA data (level 3) in variable...
USA <- covid19( country = c("United States") , level = 3 , verbose = FALSE)

# view first 100 entries...
View( USA[1:100,] )

# convert USA data.frame into a data.table...
setDT(USA)

# check class of "date" feature... 
USA[ , class(date)]

# create "WeekDay" feature to contain day of week for each date record...
USA[, WeekDay := weekdays(date)]

# view USA data, sepcifically "date", "WeekDay", and "confirmed"...
View( USA[, list(date,WeekDay,confirmed)])

# count number of observations for each county...
USA[, table(administrative_area_level_3 , useNA= "ifany")]

#-------------------------------------------------------------------------------
# Rename the area columns to City (really County) and State

# renames administrative_area_level_2 to State...
setnames(USA, 
        old = c("administrative_area_level_2"), new = c("State"))

# renames administrative_area_level_3 to City (really represents County)...
setnames( USA, 
          old = c("administrative_area_level_3") , c("City"))

# set keys in USA...        
setkeyv(USA, cols = c("State","City", "date"))
key(USA)

#-------------------------------------------------------------------------------
#Get cumulative cases

USA[, Previous := shift(confirmed , n=1, type = "lag" , fill = NA_integer_), by = list(City)]

USA[,DailyCases := confirmed - Previous]

View( USA[630:649 , list(State, City,  date, confirmed, Previous, DailyCases) ] ) 

USA[, Previous := NULL] #Gets rid of Previous column (it was used as part of our calculations)

#-------------------------------------------------------------------------------
#Parse through the data table to get each cities data. NOTE *Some counties were 
#used for major cities. I made note of this where applicable.*

NYC <- USA[USA$City == "New York City"]

LA  <- USA[USA$City == "Los Angeles"]

SF <- USA[USA$City == "San Francisco"]

HOUS <- USA[USA$City == "Houston" & USA$State == "Texas"]

DALLAS <- USA[USA$City == "Dallas" & USA$State == "Texas"]

MIAMI <- USA[USA$City == "Miami-Dade" & USA$State == "Florida"]

SEAT <- USA[USA$City == "King" & USA$State == "Washington"] #contains Seattle

CHARLOTTE <- USA[USA$City == "Mecklenburg" & USA$State == "North Carolina"]

FT_LAUD <- USA[USA$City == "Broward"] #watch for city overlap --contains Fort Lauderdale

ST_LOU <- USA[USA$City == "St. Louis" & USA$State == "Missouri"]

BATON <- USA[USA$City == "East Baton Rouge"] 

ORLEANS <- USA[USA$City == "Orleans" & USA$State == "Louisiana"] 

DAVIDSON <- USA[USA$City == "Davidson" & USA$State == "Tennessee"] #contains nashville

ORANGE <- USA[USA$City == "Orange" & USA$State == "Florida"] #has Orlando

PHIL <- USA[USA$City == "Philadelphia" & USA$State == "Pennsylvania"]

COOK <- USA[USA$City == "Cook"  & USA$State == "Illinois"] #Contains Chicago

DETROIT <- USA[USA$City == "Wayne" & USA$State == "Michigan"] #contains Detroit

MILWAUKEE  <- USA[USA$City == "Milwaukee" & USA$State == "Wisconsin"]

AUSTIN <- USA[USA$City == "Austin" & USA$State == "Texas"]

SAN_DIEGO<- USA[USA$City== "San Diego" & USA$State == "California"]

#-------------------------------------------------------------------------------
#You can skip this line...just to see what cities are contained in a given state

View (USA[USA$State == "North Carolina", list(City,date,confirmed)])

#-------------------------------------------------------------------------------
# going to create some visualizations using the data 

View(NYC)
