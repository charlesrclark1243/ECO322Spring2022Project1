rm(list=ls())

library(sf)
library(tigris)      
library(tidycensus) # Official US state, county boundaries
library(data.table)
library(mapview)

Root <- "E:"
PathOut <- file.path(Root, "Courses", "Datasets", "US_Spatial", "R_Data")

#---------------------------------------------------------------------------------------------------
# PART ONE: Work with a FIPS code built-in dataset. We need it so that we can add state names to the
# spatial data.
#---------------------------------------------------------------------------------------------------

# The tidycensus package has a built-in dataset of state and county boundaries, with Alaska and
# Hawaii shifted to ease mapping for all 50 states. The coordinate reference system is Lambert
# Azimuthal Equal Area, which is a popular choice for the US as a whole.
data(package="tidycensus")

# Gets the data ready to be used, but doesn't load them into memory yet:
data(fips_codes, package="tidycensus") 
FIPS <- fips_codes # Has both state and county FIPS codes.
setDT(FIPS) # transform to a data.table

# Keep only the unique records for states and state-like entities. Note how the number of records
# has dropped down to N=57:
StateFIPS <- unique(FIPS, by=c("state","state_code","state_name"))
class(StateFIPS) # a data.table and data.frame
StateFIPS[, `:=`(county_code=NULL, county=NULL)] # NULL-ify two unneeded variables

rm(fips_codes, FIPS) # no longer needed, so drop

#---------------------------------------------------------------------------------------------------
# PART TWO: Work with the spatial data.
#---------------------------------------------------------------------------------------------------

# Get the spatial boundaries data ready to be used, but don't yet put them in memory:
data(state_laea, package="tidycensus") 
SB <- state_laea  # Activates the sf data.frame state_laea, copies it to SB
class(SB) # sf and data.frame

# Get the names and View() the non-spatial data. The only variable is GEOID, which is identical to
# the state FIPS code.
names(SB)
View( st_drop_geometry(SB) ) 

plot( st_geometry(SB) ) # Plot the boundaries (ignore the warning)

#---------------------------------------------------------------------------------------------------
# PART THREE: Merge the state names in StateFIPS with the spatial data in SB. Do a "left" merge so
# as to keep *all* the spatial records.
#---------------------------------------------------------------------------------------------------
States <- merge(SB, StateFIPS,
                by.x="GEOID", by.y="state_code",
                all.x=TRUE)

# class(States) is sf and data.frame, unaffected by the merge. Note: N=51 because the left merge
# kept only observations with a spatial record. (There were additional records in StateFIPS.)
class(States) 
# Note that the variable GEOID is the same as the state FIPS code. Nevertheless, to avoid confusion
# later on we may as well make a copy of GEOID named FIPS. 

States$FIPS <- States$GEOID
names(States) 

save(States, file=file.path(PathOut, "StateBoundaries.RData"))
