rm(list=ls())
#-------------------------------------------------------------------------------
#Make sure these libraries below are working correctly.
library(COVID19) # This package provides access to an international COVID database
library(data.table) # An enhancement of data.frame, with great capabilities
library(sf)
library(tigris)      
library(tidycensus) # Official US state, county boundaries
library(data.table)
library(mapview)
#-------------------------------------------------------------------------------
# Edit for your computers:
Root <- "C:"
PathOut <- file.path(Root, "Users", "HP", "OneDrive", "Desktop", "COVID")
print(PathOut)
#-------------------------------------------------------------------------------
#Put ENTIRE USA sorted by region into a DataTable. We will then get 'new cases' as bulk in next section
#(instead of parsing through it 20 times)
USA <- covid19( country = c("United States") , level = 2 , verbose = FALSE)

View( USA[1:100,] )

setDT(USA)

USA[ , class(date)]

USA[, WeekDay := weekdays(date)]

#View( USA[, list(date,WeekDay,confirmed)])

USA[, table(administrative_area_level_2 , useNA= "ifany")]

setnames( USA, 
          old = c("administrative_area_level_2") , new = c("State"))


setkeyv(USA, cols = c("State", "date"))
key(USA)
USA[, Previous := shift(confirmed , n=1, type = "lag" , fill = NA_integer_), by = list(State)]

USA[,DailyCases := confirmed - Previous]

#View( USA[630:650 , list(State,  date, confirmed, Previous, DailyCases) ] ) 

USA[, Previous := NULL] #Gets rid of Previous column (it was used as part of our calculations)
#----------------------------------------------------------------------------------------------------------------
#Slice the original USA data frame to obtain the dates desired of study. 01-01-2021 through 12-31-2021
USA <- USA[USA$date >="2021-01-01" & USA$date <="2021-12-31"]

#Use built in R-state-package to grab data from each state from overall DataFrame
x <- c(1:50)
#This for loop slices the USA datatable to obtain the case data for each state in 2021.
#As a result ,there will be 50 datatable (1 for each state )
for (i in x){
  assign(state.abb[i], USA[USA$State == state.name[i]])
}
#Get all case concentrations as a proportion of population 
#from all 50 states (not the most efficient way but gets job done) this may include some people who got covid twice
AL_conc <-sum(AL$DailyCases) / (sum(AL$population)/365)
AK_conc <-sum(AK$DailyCases) / (sum(AK$population)/365)
AR_conc <-sum(AR$DailyCases) / (sum(AR$population)/365)
AZ_conc <-sum(AZ$DailyCases) / (sum(AZ$population)/365)
CA_conc <-sum(CA$DailyCases) / (sum(CA$population)/365)
CO_conc <-sum(CO$DailyCases) / (sum(CO$population)/365)
CT_conc <-sum(CT$DailyCases) / (sum(CT$population)/365)
DE_conc <-sum(DE$DailyCases) / (sum(DE$population)/365)
FL_conc <-sum(FL$DailyCases) / (sum(FL$population)/365)
GA_conc <-sum(GA$DailyCases) / (sum(GA$population)/365)
HI_conc <-sum(HI$DailyCases) / (sum(HI$population)/365)
IA_conc <-sum(IA$DailyCases) / (sum(IA$population)/365)
ID_conc <-sum(ID$DailyCases) / (sum(ID$population)/365)
IL_conc <-sum(IL$DailyCases) / (sum(IL$population)/365)
IN_conc <-sum(IN$DailyCases) / (sum(IN$population)/365)
KS_conc <-sum(KS$DailyCases) / (sum(KS$population)/365)
KY_conc <-sum(KY$DailyCases) / (sum(KY$population)/365)
LA_conc <-sum(LA$DailyCases) / (sum(LA$population)/365)
MA_conc <-sum(MA$DailyCases) / (sum(MA$population)/365)
MD_conc <-sum(MD$DailyCases) / (sum(MD$population)/365)
ME_conc <-sum(ME$DailyCases) / (sum(ME$population)/365)
MI_conc <-sum(MI$DailyCases) / (sum(MI$population)/365)
MN_conc <-sum(MN$DailyCases) / (sum(MN$population)/365)
MO_conc <-sum(MO$DailyCases) / (sum(MO$population)/365)
MS_conc <-sum(MS$DailyCases) / (sum(MS$population)/365)
MT_conc <-sum(MT$DailyCases) / (sum(MT$population)/365)
NC_conc <-sum(NC$DailyCases) / (sum(NC$population)/365)
ND_conc <-sum(ND$DailyCases) / (sum(ND$population)/365)
NE_conc <-sum(NE$DailyCases) / (sum(NE$population)/365)
NH_conc <-sum(NH$DailyCases) / (sum(NH$population)/365)
NJ_conc <-sum(NJ$DailyCases) / (sum(NJ$population)/365)
NM_conc <-sum(NM$DailyCases) / (sum(NM$population)/365)
NV_conc <-sum(NV$DailyCases) / (sum(NV$population)/365)
NY_conc <-sum(NY$DailyCases) / (sum(NY$population)/365)
OH_conc <-sum(OH$DailyCases) / (sum(OH$population)/365)
OK_conc <-sum(OK$DailyCases) / (sum(OK$population)/365)
OR_conc <-sum(OR$DailyCases) / (sum(OR$population)/365)
PA_conc <-sum(PA$DailyCases) / (sum(PA$population)/365)
RI_conc <-sum(RI$DailyCases) / (sum(RI$population)/365)
SC_conc <-sum(SC$DailyCases) / (sum(SC$population)/365)
SD_conc <-sum(SD$DailyCases) / (sum(SD$population)/365)
TN_conc <-sum(TN$DailyCases) / (sum(TN$population)/365)
TX_conc <-sum(TX$DailyCases) / (sum(TX$population)/365)
UT_conc <-sum(UT$DailyCases) / (sum(UT$population)/365)
VA_conc <-sum(VA$DailyCases) / (sum(VA$population)/365)
VT_conc <-sum(VT$DailyCases) / (sum(VT$population)/365)
WA_conc <-sum(WA$DailyCases) / (sum(WA$population)/365)
WI_conc <-sum(WI$DailyCases) / (sum(WI$population)/365)
WV_conc <-sum(WV$DailyCases) / (sum(WV$population)/365)
WY_conc <-sum(WY$DailyCases) / (sum(WY$population)/365)



#------------------------------------------------------------------------------------------------------------------
#Time for GIS Mapping. We are going to plot the number of cases of coronavirus as a proportion of population
# in each state for 2021 by using a heatmap that is featured in ggplot's plot_usmap() function.

library(usmap)
#Lets get the fips code for each state.
z <- c(01:56)
#Drop some numbers that dont have a state associated with them
z <- z[! z %in% c('3','7','11','14','43','52')]

#List of state concentrations in alphabetical order (this is so when we put it in the data table
# that the state and the corresponding fips code matches up)
conc <- c(AL_conc , AK_conc , AR_conc ,AZ_conc,CA_conc,CO_conc,CT_conc,DE_conc,FL_conc,GA_conc,
          HI_conc,IA_conc,ID_conc,IL_conc,IN_conc,KS_conc,KY_conc,LA_conc,MA_conc,MD_conc,ME_conc
          ,MI_conc,MN_conc,MO_conc,MS_conc,MT_conc,NC_conc,ND_conc,NE_conc,NH_conc,NJ_conc,NM_conc,
          NV_conc,NY_conc,OH_conc,OK_conc,OR_conc,PA_conc,RI_conc,SC_conc,SD_conc,TN_conc,TX_conc,UT_conc,
          VA_conc,VT_conc,WA_conc,WI_conc,WV_conc,WY_conc)

#Let's find the top 5 states with the most coronavirus cases as a proportion of population.
conc_copy <- conc

tail(sort(conc_copy),5)

#Alaska : 0.1495328
#West Virginia : 0.1354956
#Rhode Island :  0.1351258
#Kentucky : 0.1333515 
#Florida :  0.1323736 

# Let's find the top 5 states with the lowest COVID cases as a proportion of population.
conc_copy2<- conc

head(sort(conc_copy2),5)

#Hawaii : 0.06223515
#Maryland : 0.7012528
#Oregon : 0.07286704
#Washington : 0.07902409
#Nebraska : 0.08118381


#Let's find the median, average, and standard deviation of cases for each state as a proportion of population.

mean_conc <- mean(conc) #0.1061161

median_conc <- median(conc) #0.108609069

std_conc <- sd(conc)  #0.0180455


#Create a data frame with states in 1st column ,fips code in 2nd column and the state concentrations in 3rd
df <- data.frame(
  states = state.name,
  fips = z,
  concentration = conc)
View(df)

#use the usmap package's built in function 'plot_usmap' to obtain a heatmap of the concentration of cases as
#a proportion of population.
plot_usmap( data=df, values = "concentration"
            ,labels=TRUE, color = "dimgrey",size = 0.5)

#----------------------------------------------------------------------------
#Let's see the progression of percentage of population vaccinated for Suffolk County, Nassau County, and NYC
#First lets retrieve our data
USA_2 <- covid19( country = c("United States") , level = 3 , verbose = FALSE)

setnames( USA_2, 
          old = c("administrative_area_level_2","administrative_area_level_3") , new = c("State", "County"))
setDT(USA_2)
NewYork <- USA_2[USA_2$State == "New York"]
setDT(NewYork)

NewYork[, table(County , useNA= "ifany")]

NewYorkCounties<- NewYork[NewYork$County == "Suffolk" | NewYork$County == "Nassau"  |  NewYork$County == "New York City"]
NewYorkCounties <- NewYorkCounties[NewYorkCounties$date <= "2022-02-20"]


Suffolk <- NewYorkCounties[NewYorkCounties$County == "Suffolk"]
Nassau <-  NewYorkCounties[NewYorkCounties$County == "Nassau"]
NYC <- NewYorkCounties[NewYorkCounties$County == "New York City"]



#------------------------------------------------------------------------------------------------


#Grabbed these population from Census Data
suffolk_pop = 1525920
nassau_pop = 1395774
nyc_pop = 8804190

#set the previous dataframes as data.table for efficiency
setDT(Suffolk)
setDT(Nassau)
setDT(NYC)


#Create a new variable in data.tables that will track the proportion of population fully vaccinated
NYC <- NYC[, Perc_Vacc := people_fully_vaccinated/nyc_pop]
Suffolk <- Suffolk[, Perc_Vacc := people_fully_vaccinated/suffolk_pop]
Nassau <- Nassau[,Perc_Vacc := people_fully_vaccinated/nassau_pop]

#Slices the data.tables so we only have 2 variables : Date, and Perc_vacc
Suffolk_perc_Vacc <- Suffolk[, list(date,Perc_Vacc)]
setDT(Suffolk_perc_Vacc)
Suffolk_perc_Vacc <- Suffolk_perc_Vacc[date >= "2020-12-13"]

Nassau_perc_Vacc <-Nassau[, list(date,Perc_Vacc)]
setDT(Nassau_perc_Vacc)
Nassau_perc_Vacc <- Nassau_perc_Vacc[date >= "2020-12-13"]

NYC_perc_Vacc <- NYC[, list(date,Perc_Vacc)]
setDT(NYC_perc_Vacc)
NYC_perc_Vacc <- NYC_perc_Vacc[date >= "2020-12-13"]



#Plot an overlaying Time Series that tracks the proportion of population fully vaccinated.
#There as a rush to get vaccinated in Spring of 2021, but eventually leveled off as the year went on. 
#The graphs are very similar to each other, and Nassau has currently the highest proportion fully vaccinated.
#Suffolk is second, and NYC is in 3rd.
plot(Suffolk_perc_Vacc$date,
     Suffolk_perc_Vacc$Perc_Vacc,
     type = "l",
     col = 2,
     xlim=as.Date(c("2020-12-13", "2022-02-20")),
     xlab = "Date",
     ylab = "Percentage of Population Vaccinated",
     main = "Percentage of Population Vaccinated")
lines(Nassau_perc_Vacc$date,
      Nassau_perc_Vacc$Perc_Vacc,
      type = "l",
      col=3)
lines(NYC_perc_Vacc$date,
      NYC_perc_Vacc$Perc_Vacc,
      type = "l",
      col=4)
legend("topleft",
       c("Suffolk", "Nassau", "NYC"),
       lty = 1,
       col = 2:4)



