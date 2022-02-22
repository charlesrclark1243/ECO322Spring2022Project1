#-------------------------------------------------------------------------------
# You can skip this if you already have data.table installed...
# I only included it because my Mac didn't have it

# WINDOWS:
install.packages(c("data.table", "COVID19"))
install.packages("stringr")

# MACOS/LINUX:
install.packages("COVID19")
# follow macOS instructions in txt file.
install.packages("stringr")

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
library(stringr)

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
# create new data.table that only contains date, DailyCases, State, City, ...
# latitude, longitude, and stringency_index (will be used for rest of project)

USA_trimmed <- USA[, list(date,
                          State,
                          City,
                          latitude,
                          longitude,
                          DailyCases,
                          stringency_index)]

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
# first, create trimmed versions of every city data.table...

NYC_trimmed <- USA_trimmed[City == "New York City"]

LA_trimmed  <- USA_trimmed[City == "Los Angeles"]

SF_trimmed <- USA_trimmed[City == "San Francisco"]

HOUS_trimmed <- USA_trimmed[City == "Houston" & State == "Texas"]

DALLAS_trimmed <- USA_trimmed[City == "Dallas" & State == "Texas"]

MIAMI_trimmed <- USA_trimmed[City == "Miami-Dade" & State == "Florida"]

SEAT_trimmed <- USA_trimmed[City == "King" & State == "Washington"] #contains Seattle

CHARLOTTE_trimmed <- USA_trimmed[City == "Mecklenburg" & State == "North Carolina"]

FT_LAUD_trimmed <- USA_trimmed[City == "Broward"] #watch for city overlap --contains Fort Lauderdale

ST_LOU_trimmed <- USA_trimmed[City == "St. Louis" & State == "Missouri"]

BATON_trimmed <- USA_trimmed[City == "East Baton Rouge"] 

ORLEANS_trimmed <- USA_trimmed[City == "Orleans" & State == "Louisiana"] 

DAVIDSON_trimmed <- USA_trimmed[City == "Davidson" & State == "Tennessee"] #contains nashville

ORANGE_trimmed <- USA_trimmed[City == "Orange" & State == "Florida"] #has Orlando

PHIL_trimmed <- USA_trimmed[City == "Philadelphia" & State == "Pennsylvania"]

COOK_trimmed <- USA_trimmed[City == "Cook"  & State == "Illinois"] #Contains Chicago

DETROIT_trimmed <- USA_trimmed[City == "Wayne" & State == "Michigan"] #contains Detroit

MILWAUKEE_trimmed  <- USA_trimmed[City == "Milwaukee" & State == "Wisconsin"]

AUSTIN_trimmed <- USA_trimmed[City == "Austin" & State == "Texas"]

SAN_DIEGO_trimmed <- USA_trimmed[City== "San Diego" & State == "California"]

#-------------------------------------------------------------------------------
# check type of NYC_trimmed to make sure it's a data.table...

typeof(NYC_trimmed)

#-------------------------------------------------------------------------------
# we should rename latitude, longitude, DailyCases, and stringency_index for
# for each city so we don't get errors when merging...

rename_specific_cols <- function(city_dt){
  cols_to_rename <- colnames(city_dt)[c(4:7)]
  
  for (col_name in cols_to_rename) {
    city_name_list <- as.vector(str_split_fixed(city_dt$City[1],
                                                pattern = " ",
                                                n = nchar(city_dt$City[1])))
    city_name <- paste(city_name_list, collapse = "")
    
    new_col_name <- paste(col_name, city_name, sep = "")
    
    setnames(city_dt,
             old = col_name,
             new = new_col_name)
  }
}

rename_specific_cols(NYC_trimmed)

rename_specific_cols(LA_trimmed) #  <- USA_trimmed[City == "Los Angeles"]

rename_specific_cols(SF_trimmed) # <- USA_trimmed[City == "San Francisco"]

rename_specific_cols(HOUS_trimmed) # <- USA_trimmed[City == "Houston" & State == "Texas"]

rename_specific_cols(DALLAS_trimmed) # <- USA_trimmed[City == "Dallas" & State == "Texas"]

rename_specific_cols(MIAMI_trimmed) # <- USA_trimmed[City == "Miami-Dade" & State == "Florida"]

rename_specific_cols(SEAT_trimmed) # <- USA_trimmed[City == "King" & State == "Washington"] #contains Seattle

rename_specific_cols(CHARLOTTE_trimmed) # <- USA_trimmed[City == "Mecklenburg" & State == "North Carolina"]

rename_specific_cols(FT_LAUD_trimmed) # <- USA_trimmed[City == "Broward"] #watch for city overlap --contains Fort Lauderdale

rename_specific_cols(ST_LOU_trimmed) # <- USA_trimmed[City == "St. Louis" & State == "Missouri"]

rename_specific_cols(BATON_trimmed) # <- USA_trimmed[City == "East Baton Rouge"] 

rename_specific_cols(ORLEANS_trimmed) # <- USA_trimmed[City == "Orleans" & State == "Louisiana"] 

rename_specific_cols(DAVIDSON_trimmed) # <- USA_trimmed[City == "Davidson" & State == "Tennessee"] #contains nashville

rename_specific_cols(ORANGE_trimmed) # <- USA_trimmed[City == "Orange" & State == "Florida"] #has Orlando

rename_specific_cols(PHIL_trimmed) # <- USA_trimmed[City == "Philadelphia" & State == "Pennsylvania"]

rename_specific_cols(COOK_trimmed) # <- USA_trimmed[City == "Cook"  & State == "Illinois"] #Contains Chicago

rename_specific_cols(DETROIT_trimmed) # <- USA_trimmed[City == "Wayne" & State == "Michigan"] #contains Detroit

rename_specific_cols(MILWAUKEE_trimmed) #  <- USA_trimmed[City == "Milwaukee" & State == "Wisconsin"]

rename_specific_cols(AUSTIN_trimmed) # <- USA_trimmed[City == "Austin" & State == "Texas"]

rename_specific_cols(SAN_DIEGO_trimmed) # <- USA_trimmed[City== "San Diego" & State == "California"]

