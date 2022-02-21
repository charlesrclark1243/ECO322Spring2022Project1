rm(list=ls())
#-------------------------------------------------------------------------------
#Make sure these libraries below are working correctly.
library(COVID19) # This package provides access to an international COVID database
library(data.table) # An enhancement of data.frame, with great capabilities
#-------------------------------------------------------------------------------
# Edit for your computers:
Root <- "C:"
PathOut <- file.path(Root, "Users", "HP", "OneDrive", "Desktop", "COVID")
print(PathOut)
#-------------------------------------------------------------------------------
#Put ENTIRE USA sorted by region into a DataTable. We will then get 'new cases' as bulk in next section
#(instead of parsing through it 20 times)
USA <- covid19( country = c("United States") , level = 3 , verbose = FALSE)
#Removes the following columns below from data.table (not needed since they are really just general geographical identifiers for the USA)
USA <- USA[,c("iso_numeric","iso_currency", "administrative_area_level_1","administrative_area_level", "iso_alpha_3","iso_alpha_2","key_local"):=NULL]

View( USA[1:100,] )

setDT(USA)

USA[ , class(date)]

USA[, WeekDay := weekdays(date)]

View( USA[, list(date,WeekDay,confirmed)])

USA[, table(administrative_area_level_3 , useNA= "ifany")]
#-------------------------------------------------------------------------------
#Rename the area columns to City (really County) and State
setnames(USA, 
        old = c("administrative_area_level_2"), new = c("State"))

setnames( USA, 
          old = c("administrative_area_level_3") , new = c("City"))

        
setkeyv(USA, cols = c("State","City", "date"))
key(USA)
#-------------------------------------------------------------------------------
#Get cumulative cases

USA[, Previous := shift(confirmed , n=1, type = "lag" , fill = NA_integer_), by = list(City)]

USA[,DailyCases := confirmed - Previous]

View( USA[630:650 , list(State, City,  date, confirmed, Previous, DailyCases) ] ) 

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

#Begin graphing? 
