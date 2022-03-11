# ==========================================================================================================================

# Setup Stage

# ==========================================================================================================================

# delete all existing environment variables...
rm(list = ls())

# --------------------------------------------------------------------------------------------------------------------------

# install all necessary libraries (if not already installed)...
install.packages(c("COVID19", "data.table", "sf", "tigris", "tidycensus", "mapview", "usmap", "ggplot2"))

# --------------------------------------------------------------------------------------------------------------------------

# load all installed libraries...
library(COVID19)
library(data.table)
library(sf)
library(tigris)
library(tidycensus)
library(mapview)
library(usmap)
library(ggplot2)

# ==========================================================================================================================

# Exploratory Data Analysis (Jesse)

# ==========================================================================================================================

# store USA COVID-19 data into a data.frame object...
USA <- covid19(country = "United States", level = 2, verbose = FALSE)

# --------------------------------------------------------------------------------------------------------------------------

# view the first 100 entries in the USA data.frame...
View(USA[1:100, ])

# --------------------------------------------------------------------------------------------------------------------------

# convert the USA data.frame to a data.table...
setDT(USA)

# --------------------------------------------------------------------------------------------------------------------------

# determine the class of the date stored in the "date" feature of the USA data.table...
USA[, class(date)]

# --------------------------------------------------------------------------------------------------------------------------

# create a feature called "weekday" to store the day of the week for each record...
USA[, weekday := weekdays(date)]

# --------------------------------------------------------------------------------------------------------------------------

# view the first 100 records in the "date", "weekday", and "confirmed" features of the USA data.table as a data.table...
View(USA[1:100, .(date, weekday, confirmed)])

# --------------------------------------------------------------------------------------------------------------------------

# view a table based on state...
USA[, table(administrative_area_level_2, useNA = "ifany")]

# --------------------------------------------------------------------------------------------------------------------------

# rename the "administrative_area_level_2" feature to "state"...
setnames(USA, old = "administrative_area_level_2", "state_name")

# --------------------------------------------------------------------------------------------------------------------------

# set the "state" and "date" features as keys in the USA data.table...
setkeyv(USA, cols = c("state_name", "date"))

# --------------------------------------------------------------------------------------------------------------------------

# make sure the "state" and "date" features were added as keys...
key(USA)

# --------------------------------------------------------------------------------------------------------------------------

# create a feature called "previous" to be used in calculating daily confirmed cases...
USA[, previous := shift(confirmed, n = 1, type = "lag", fill = NA_integer_), by = state_name]

# --------------------------------------------------------------------------------------------------------------------------

# create a feature called "daily_cases" by subtracting "previous" from confirmed...
USA[, daily_cases := confirmed - previous]

# --------------------------------------------------------------------------------------------------------------------------

# view New York COVID data to make sure the daily_cases feature was calculated correctly...
View(USA[state_name == "New York", ])

# --------------------------------------------------------------------------------------------------------------------------

# erase the previous feature we used for calculating daily_cases...
USA[, previous := NULL]

# --------------------------------------------------------------------------------------------------------------------------

# slice the USA data.table to only include data from the year 2021...
USA <- USA[date >= "2021-01-01" & date <= "2021-12-31", ]

# --------------------------------------------------------------------------------------------------------------------------

# create a vector containing the integers in the interval [1, 50]...
state_indices <- c(1:50)

# --------------------------------------------------------------------------------------------------------------------------

# create variables for each state using a built-in R package and a for loop...
for (index in state_indices) {
  assign(state.abb[index], USA[state_name == state.name[index]])
}

# --------------------------------------------------------------------------------------------------------------------------

# use another loop to create concentration variables for each state...
for (index in state_indices) {
  assign(paste(state.abb[index], "conc", sep = "_"),
         sum(get(state.abb[index])$daily_cases) / (sum(get(state.abb[index])$population) / 365)) 
}

# --------------------------------------------------------------------------------------------------------------------------

# create a vector to represent the US state FIPS codes...
fips <- c(1:56)

# --------------------------------------------------------------------------------------------------------------------------

# drop some of the numbers without any associated states...
fips <- fips[! fips %in% c('3','7','11','14','43','52')]

# --------------------------------------------------------------------------------------------------------------------------

# create a an empty vector...
state_concs <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# populate the empty vector with each state's concentrations using a for loop...
for (index in state_indices) {
  state_concs[length(state_concs) + 1] <- get(paste(state.abb[index], "conc", sep = "_"))
}

# --------------------------------------------------------------------------------------------------------------------------

# make a copy of the state_concs vector...
state_concs_copy <- state_concs

# --------------------------------------------------------------------------------------------------------------------------

# find the five states with the most COVID-19 cases as a proportion of their populations (sort in decreasing order)...
head(sort(state_concs_copy, decreasing = TRUE), 5)

# --------------------------------------------------------------------------------------------------------------------------

# find the five states with the least COVID-19 cases as a proportion of their populations (resort in increasing order)...
head(sort(state_concs_copy, decreasing = FALSE), 5)

# --------------------------------------------------------------------------------------------------------------------------

# calculate the average/mean concentration of COVID-19 cases for the entire US (considering all state concentrations)...
mean_conc <- mean(state_concs)

# --------------------------------------------------------------------------------------------------------------------------

# calculate the median concentration of COVID-19 cases for the entire US (considering all state concentrations)...
median_conc <- median(state_concs)

# --------------------------------------------------------------------------------------------------------------------------

# calculate the standard deviation of the US COVID-19 concentration distribution (considering all state concentrations)...
std_dev_conc <- sd(state_concs)

# --------------------------------------------------------------------------------------------------------------------------

# create a data.frame object for the data we worked with...
state_concs_df <- data.frame(
  state = state.name,
  fips = fips,
  concentration = state_concs
)

# --------------------------------------------------------------------------------------------------------------------------

# view the previously defined data.frame object...
View(state_concs_df)

# --------------------------------------------------------------------------------------------------------------------------

# create a heat map using the previously defined data.frame object...
plot_usmap(data = state_concs_df,
           values = "concentration",
           labels = TRUE,
           size = 0.5) + 
  scale_fill_continuous(low = "white",
                        high = "dodgerblue",
                        name = "COVID-19 Concentration") +
  theme(legend.position = "right") +
  labs(title = "State COVID-19 Concentrations (2021)")

# --------------------------------------------------------------------------------------------------------------------------

# create ggplot object with state_concs_df...
k <- ggplot(data = state_concs_df,
            aes(x = concentration))

# --------------------------------------------------------------------------------------------------------------------------

# create histogram of concentrations...
k + geom_histogram(bins = 10, fill = "dodgerblue", color = "black") +
  labs(title = "State COVID-19 Concentrations (2021)",
       x= "Concentration", y = "Frequency") +
  scale_x_continuous(breaks = seq(0.06, 0.14, 0.02)) +
  scale_y_continuous(breaks = seq(0, 14, 2)) +
  theme_linedraw()

# --------------------------------------------------------------------------------------------------------------------------

# redefine the USA variable to include county-level data...
USA <- covid19(country = "United States", level = 3, verbose = FALSE)

# --------------------------------------------------------------------------------------------------------------------------

# convert the USA data.frame object to a data.table object...
setDT(USA)

# --------------------------------------------------------------------------------------------------------------------------

# rename the "administrative_area_level_2" and "administrative_area_level_3" features to "state" and "county"...
setnames(USA, old = c("administrative_area_level_2", "administrative_area_level_3"),
         new = c("state", "county"))

# --------------------------------------------------------------------------------------------------------------------------

# create a separate variable for New York State...
new_york <- USA[state == "New York", ]

# --------------------------------------------------------------------------------------------------------------------------

# make the new_york variable a data.frame...
setDT(new_york)

# --------------------------------------------------------------------------------------------------------------------------

# make a table using the "county" feature of the new_york data.table...
new_york[, table(county, useNA = "ifany")]

# --------------------------------------------------------------------------------------------------------------------------

# restrict the new_york data.table to only include Nassau, Suffolk, and New York City...
new_york <- new_york[county == "Nassau" | county == "Suffolk" | county == "New York City", ]

# --------------------------------------------------------------------------------------------------------------------------

# restrict the new_york data.table to include all records on and before 2022-02-02 as well as on and before 2020-12-13...
new_york <- new_york[date <= "2022-02-02" & date >= "2020-12-13", ]

# --------------------------------------------------------------------------------------------------------------------------

# create population variables for each of the three counties (found from Census data)...
nassau_pop = 1395774
suffolk_pop = 1525920
new_york_city_pop = 8804190

View(new_york)
# --------------------------------------------------------------------------------------------------------------------------

# check the number of records there are for each county...
length(new_york[county == "Nassau", ]$county)
length(new_york[county == "Suffolk", ]$county)
length(new_york[county == "New York City", ]$county)

# --------------------------------------------------------------------------------------------------------------------------

# create an three empty vectors, one for each county...
nassau_perc_vacc_vector <- vector()
suffolk_perc_vacc_vector <- vector()
new_york_city_perc_vacc_vector <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# populate the three vectors with each county's perc_vacc data...
for (index in c(1:length(new_york[date >= "2020-12-13" & date <= "2022-02-02"]$people_fully_vaccinated))) {
  if (new_york$county[index] == "Nassau") {
    nassau_perc_vacc_vector[length(nassau_perc_vacc_vector) + 1] <-
      new_york$people_fully_vaccinated[index] / nassau_pop
  }
  else if (new_york$county[index] == "Suffolk") {
    suffolk_perc_vacc_vector[length(suffolk_perc_vacc_vector) + 1] <-
      new_york$people_fully_vaccinated[index] / suffolk_pop
  }
  else {
    new_york_city_perc_vacc_vector[length(new_york_city_perc_vacc_vector) + 1] <-
      new_york$people_fully_vaccinated[index] / new_york_city_pop
  }
}

# --------------------------------------------------------------------------------------------------------------------------

# create a vector of Date objects from 2020-12-13 to 2022-02-02...
dates <- seq(as.Date("2020/12/13"), as.Date("2022/02/02"), by = "day")

# --------------------------------------------------------------------------------------------------------------------------

# check the number of dates...
length(dates)

# --------------------------------------------------------------------------------------------------------------------------

# create a new data.frame containing the perc_vacc data for each county and the dates...
perc_vacc_county_df <- data.frame(
  date = dates,
  nassau = nassau_perc_vacc_vector,
  suffolk = suffolk_perc_vacc_vector,
  new_york_city = new_york_city_perc_vacc_vector
)

# --------------------------------------------------------------------------------------------------------------------------

# make the perc_vacc_county_df a data.table...
setDT(perc_vacc_county_df)

# --------------------------------------------------------------------------------------------------------------------------

# create a ggplot object for the perc_vacc_county_df data.table...
v <- ggplot(data = perc_vacc_county_df,
            mapping = aes(x = date))

# --------------------------------------------------------------------------------------------------------------------------

# create a time series plot of each of the three counties' percentage of their populations that have been vaccinated...
v + geom_line(aes(y = nassau,
                 col = "Nassau County")) +
  geom_line(aes(y = suffolk,
               col = "Suffolk County")) +
  geom_line(aes(y = new_york_city,
               col = "New York City")) +
  scale_color_manual(name = "County",
                     values = c("Nassau County" = 3,
                                "Suffolk County" = 2,
                                "New York City" = 4)) +
  labs(title = "Percentage of Population Vaccinated in\nNassau, Suffolk, and New York City",
       x = "Date", y = "Percentage of Population Vaccinated") +
  guides(fill = guide_legend(title = "County")) +
  theme_linedraw()


# ==========================================================================================================================

# Main Experimentation (Charlie)

# ==========================================================================================================================

# remove all previously existing environment variables...
rm(list=ls())

# --------------------------------------------------------------------------------------------------------------------------

# import USA data...
USA <- covid19( country = c("United States") , level = 2, verbose = FALSE)

# --------------------------------------------------------------------------------------------------------------------------

# view first 100 entries...
View(USA[1:100, ])

# --------------------------------------------------------------------------------------------------------------------------

# convert USA data to data.table...
setDT(USA)

# --------------------------------------------------------------------------------------------------------------------------

# make sure the conversion worked...
is.data.table(USA)

# --------------------------------------------------------------------------------------------------------------------------

# rename administrative_area_level_2 to state...
setnames(USA, "administrative_area_level_2", "state_name")

# --------------------------------------------------------------------------------------------------------------------------

# make sure the renaming worked...
colnames(USA)

# --------------------------------------------------------------------------------------------------------------------------

# set keys for USA data...
setkeyv(USA, c("state_name", "date"))

# --------------------------------------------------------------------------------------------------------------------------

# make sure the key-setting worked...
key(USA)

# --------------------------------------------------------------------------------------------------------------------------

# make a new lagged feature called previous...
USA[, previous := shift(confirmed, 1, fill = NA_integer_), by = state_name]

# --------------------------------------------------------------------------------------------------------------------------

# view the first 100 entries...
View(USA[1:100, ])

# --------------------------------------------------------------------------------------------------------------------------

# create daily_cases feature by subtracting previous from current...
USA[, daily_cases := confirmed - previous]

# --------------------------------------------------------------------------------------------------------------------------

# recheck the first 100 entries...
View(USA[1:100, ])

# --------------------------------------------------------------------------------------------------------------------------

# delete lagged feature...
USA[, previous := NULL]

# --------------------------------------------------------------------------------------------------------------------------

# make sure previous was successfully deleted...
colnames(USA)

# --------------------------------------------------------------------------------------------------------------------------

# recreate the previous feature with daily deaths being lagged now...
USA[, previous := shift(deaths, 1, fill = NA_integer_), by = state_name]

# --------------------------------------------------------------------------------------------------------------------------

# view the first 100 entries...
View(USA[1:100, ])

# --------------------------------------------------------------------------------------------------------------------------

# create daily deaths feature by subtracting previous from deaths...
USA[, daily_deaths := deaths - previous]

# --------------------------------------------------------------------------------------------------------------------------

# recheck the first 100 entries...
View(USA[1:100, ])

# --------------------------------------------------------------------------------------------------------------------------

# delete lagged feature...
USA[, previous := NULL]

# --------------------------------------------------------------------------------------------------------------------------

# make sure previous was successfully deleted...
colnames(USA)

# --------------------------------------------------------------------------------------------------------------------------

# reassign USA data.table to a trimmed version of itself...
# only include date, state, daily_cases, daily_deaths, and stringency_index...
USA <- USA[, c("date", "state_name", "daily_cases", "daily_deaths", "stringency_index")]

# --------------------------------------------------------------------------------------------------------------------------

# check the first 100 entries...
View(USA[1:100, ])

# --------------------------------------------------------------------------------------------------------------------------

# trim USA data.table to only include dates in interval [1/1/2021, 12/31/2021]...
USA <- USA[date>="2021-01-01" & date<="2021-12-31", ]

# --------------------------------------------------------------------------------------------------------------------------

# make sure the trimming worked by looking at all the data for one state (Alabama, for example)...
View(USA[state_name == "Alabama", ])

# --------------------------------------------------------------------------------------------------------------------------

# check how many records there are with less than 0 daily cases...
length(USA[daily_cases < 0, ]$daily_cases)

# --------------------------------------------------------------------------------------------------------------------------

# replace each such record with a 0 value in daily_cases...
USA[daily_cases < 0, daily_cases := 0]

# --------------------------------------------------------------------------------------------------------------------------

# make sure all such records have been corrected...
length(USA[daily_cases < 0, ]$daily_cases) # should print 0...

# --------------------------------------------------------------------------------------------------------------------------

# check how many records there are with less than 0 daily deaths...
length(USA[daily_deaths < 0, ]$daily_deaths)

# --------------------------------------------------------------------------------------------------------------------------

# replace each such record with a 0 value in daily_deaths...
USA[daily_deaths < 0, daily_deaths := 0]

# --------------------------------------------------------------------------------------------------------------------------

# make sure all such records have been corrected...
length(USA[daily_deaths < 0, ]$daily_deaths) # should print 0...

# --------------------------------------------------------------------------------------------------------------------------

# create CA data.table...
CA <- USA[state_name == "California", ]

# --------------------------------------------------------------------------------------------------------------------------

# calculate average stringency_index for CA...
CA_avg_stringency_index <- mean(CA$stringency_index)

# --------------------------------------------------------------------------------------------------------------------------

# create separate data.tables based on a comparison to average_stringency_index...
CA_lenient <- CA[stringency_index <= CA_avg_stringency_index]
CA_strict <- CA[stringency_index > CA_avg_stringency_index]

# --------------------------------------------------------------------------------------------------------------------------

# calculate average of daily_cases for the two data.tables...
CA_lenient_avg_daily_cases <- mean(CA_lenient$daily_cases)
CA_strict_avg_daily_cases <- mean(CA_strict$daily_cases)

# --------------------------------------------------------------------------------------------------------------------------

# hypothesis test...
t.test(CA_lenient$daily_cases, CA_strict$daily_cases)

# --------------------------------------------------------------------------------------------------------------------------

# create NY data.table...
NY <- USA[state_name == "New York"]

# --------------------------------------------------------------------------------------------------------------------------

# calculate average stringency_index for NY...
NY_avg_stringency_index <- mean(NY$stringency_index)

# --------------------------------------------------------------------------------------------------------------------------

# create separate data.tables based on a comparison to average_stringency_index...
NY_lenient <- NY[stringency_index <= NY_avg_stringency_index]
NY_strict <- NY[stringency_index > NY_avg_stringency_index]

# --------------------------------------------------------------------------------------------------------------------------

# hypothesis test...
t.test(NY_lenient$daily_cases, NY_strict$daily_cases)

# --------------------------------------------------------------------------------------------------------------------------

# the two-sided test indicates there is a statistically significant difference...
# between the lenient and strict data.tables...
# however, the average daily cases in the strict set is much higher than...
# that of the lenient set...
# let's see if this is true statistically using a one-sided hypothesis test...

t.test(NY_lenient$daily_cases, NY_strict$daily_cases, alternative = "less")

# thus, it appears that there is a statistically significant difference between...
# the daily cases when NY's stringency index was <= the average value of the index...
# and the daily cases when NY's stringency index was > the average value of the index...
# in fact, the strict dataset had a higher average number of daily cases than the...
# lenient dataset did, significant at alpha = 0.10, 0.05, and 0.01 .

# --------------------------------------------------------------------------------------------------------------------------

# create vector with numbers 1:50...
state_indices <- c(1:50)

# --------------------------------------------------------------------------------------------------------------------------

# use built-in R packages to create data.tables for each state, using the...
# vector of numbers 1:50 to do so in a for loop...
for (index in state_indices) {
  assign(state.abb[index], USA[state_name == state.name[index]])
}

# --------------------------------------------------------------------------------------------------------------------------

# make sure each state has no NULL/NA stringency index value...
for (index in state_indices) {
  state_ptr <- get(state.abb[index])
  
  print(paste(state.name[index], any(is.na(state_ptr$stringency_index)), sep = " ----> " ))
}

# --------------------------------------------------------------------------------------------------------------------------

# it looks like Louisiana, Maryland, and Rhode Island have NA average stringency indices...
# we should look at these sets individually...

# start with Louisiana...

# get indices of NA records for Louisiana...
LA_na_indices <- which(is.na(LA$stringency_index), arr.ind = TRUE)

# --------------------------------------------------------------------------------------------------------------------------

# there seems to be only one NA record, so we only need to impute once...
# replace NA record using mean of stringency_index values for 15 days before and after...
# day with NA record for stringency_index...
LA$stringency_index[LA_na_indices] = mean(LA$stringency_index[LA_na_indices - 15:LA_na_indices + 15], na.rm = TRUE)

# --------------------------------------------------------------------------------------------------------------------------

# check if we were successful...
any(is.na(LA$stringency_index)) # should print FALSE...

# --------------------------------------------------------------------------------------------------------------------------

# next up is Maryland...

# get indices of NA records for Maryland...
MD_na_indices <- which(is.na(MD$stringency_index), arr.ind = TRUE)

# --------------------------------------------------------------------------------------------------------------------------

# Maryland has multiple NA records, however they all seem to be grouped consecutively...
# we can impute each NA record using the mean stringency_index for the 15 days...
# before the first NA record and the 15 days after the last NA record...

# store indices of first and last NA records...
MD_na_first <- MD_na_indices[1]
MD_na_last <- MD_na_indices[length(MD_na_indices)]

# --------------------------------------------------------------------------------------------------------------------------

# impute...
for (index in MD_na_indices) {
  MD$stringency_index[index] = mean(MD$stringency_index[MD_na_first - 15:MD_na_last + 15], na.rm = TRUE)
}

# --------------------------------------------------------------------------------------------------------------------------

# check if we were successful...
any(is.na(MD$stringency_index)) # should print FALSE...

# --------------------------------------------------------------------------------------------------------------------------

# finally moving onto Rhode Island...

# get NA indices for Rhode Island stringency index...
RI_na_indices <- which(is.na(RI$stringency_index), arr.ind = TRUE)

# --------------------------------------------------------------------------------------------------------------------------

# it looks like Rhode Island has multiple NA stringency indices...
# but they're all clumped together as well...
# let's repeat the process we followed for Maryland...

# store indices of first and last NA records...
RI_na_first <- RI_na_indices[1]
RI_na_last <- RI_na_indices[length(RI_na_indices)]

# --------------------------------------------------------------------------------------------------------------------------

# impute...
for (index in RI_na_indices) {
  RI$stringency_index[index] = mean(RI$stringency_index[RI_na_first - 15:RI_na_last + 15], na.rm = TRUE)
}

# --------------------------------------------------------------------------------------------------------------------------

# check if we were successful...
any(is.na(RI$stringency_index)) # should print FALSE...

# --------------------------------------------------------------------------------------------------------------------------

# make sure every state has no missing or NA/NULL numbers of daily cases...
for (index in state_indices) {
  state_ptr <- get(state.abb[index])
  
  print(paste(state.name[index], any(is.na(state_ptr$daily_cases)), sep = " ----> " ))
}

# it looks like each state has all its daily cases records filled (no NULL or NA values)...

# --------------------------------------------------------------------------------------------------------------------------

# make sure every state has no missing or NA/NULL numbers of daily deaths...
for (index in state_indices) {
  state_ptr <- get(state.abb[index])
  
  print(paste(state.name[index], any(is.na(state_ptr$daily_deaths)), sep = " ----> " ))
}

# it looks like each state has all its daily deaths records filled (no NULL or NA values)...

# --------------------------------------------------------------------------------------------------------------------------

# define function to create a categorical variable in a given data.table...
# (assumes the passed data.table was already initialized and defined OUTSIDE the function)...
# stringency_category values are assigned to each record after comparing the record's...
# stringency_index to the average stringency_index for the passed data.table...

create_stringency_category <- function(state_dt) {
  stringency_category_vector <- vector()
  
  for (index in c(1:length(state_dt$stringency_index))) {
    if (state_dt$stringency_index[index] <= mean(state_dt$stringency_index)) {
      stringency_category_vector[length(stringency_category_vector) + 1] <- "Less than or Equal to Average"
    }
    else {
      stringency_category_vector[length(stringency_category_vector) + 1] <- "Greater than Average"
    }
  }
  
  setDT(state_dt)
  
  state_dt[, stringency_category := stringency_category_vector]
  
  return (state_dt)
}

# --------------------------------------------------------------------------------------------------------------------------

# create lenient and strict data.tables for each state, based on the values...
# is each record's stringency_category value...

for (index in state_indices) {
  assign(state.abb[index], create_stringency_category(get(state.abb[index])))
  
  assign(paste(state.abb[index], "lenient", sep = "_"),
         setDT(get(state.abb[index])[stringency_category == "Less than or Equal to Average"]))
  
  assign(paste(state.abb[index], "strict", sep = "_"),
         setDT(get(state.abb[index])[stringency_category == "Greater than Average"]))
}

# --------------------------------------------------------------------------------------------------------------------------

# create empty vectors that will contain the name abbreviations for the states...
# that show statistically significant differences between the lenient and strict...
# average number of daily cases at the alpha = 0.01 (small), 0.05 (middle), and...
# 0.10 (large) levels of significance. Also create a vector for states with no significant difference...
# The elements they will contain are determined through 2-sided t tests on a state's average daily cases...
# in lenient and strict data.tables...

difference_states_small_alpha <- vector()
difference_states_middle_alpha <- vector()
difference_states_large_alpha <- vector()
no_difference_states <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# loop through each state and perform the 2-sided t tests mentioned above...
for (index in state_indices) {
  assign(paste(state.abb[index], "diff_ttest_obj", sep = "_"),
         t.test(
           get(paste(state.abb[index], "lenient", sep = "_"))$daily_cases,
           get(paste(state.abb[index], "strict", sep = "_"))$daily_cases
         ))
}

# --------------------------------------------------------------------------------------------------------------------------

# loop through each state's t_test objects and populate the three alpha vectors...
for (index in state_indices) {
  if (get(paste(state.abb[index], "diff_ttest_obj", sep = "_"))$p.value < 0.01) {
    difference_states_large_alpha[length(difference_states_large_alpha) + 1] <- state.abb[index]
    difference_states_middle_alpha[length(difference_states_middle_alpha) + 1] <- state.abb[index]
    difference_states_small_alpha[length(difference_states_small_alpha) + 1] <- state.abb[index]
  }
  else if (get(paste(state.abb[index], "diff_ttest_obj", sep = "_"))$p.value < 0.05) {
    difference_states_large_alpha[length(difference_states_large_alpha) + 1] <- state.abb[index]
    difference_states_middle_alpha[length(difference_states_middle_alpha) + 1] <- state.abb[index]
  }
  else if (get(paste(state.abb[index], "diff_ttest_obj", sep = "_"))$p.value < 0.10) {
    difference_states_large_alpha[length(difference_states_large_alpha) + 1] <- state.abb[index]
  }
  else {
    no_difference_states[length(no_difference_states) + 1] <- state.abb[index]
  }
}

# --------------------------------------------------------------------------------------------------------------------------

# calculate alpha = 0.01 (low) percentage of states with difference...
diff_small_alpha_percentage <- length(difference_states_small_alpha) / 50.0

# --------------------------------------------------------------------------------------------------------------------------

# calculate alpha = 0.05 (mid) percentage of states with difference...
diff_middle_alpha_percentage <- length(difference_states_middle_alpha) / 50.0

# --------------------------------------------------------------------------------------------------------------------------

# calculate alpha = 0.10 (high) percentage of states with difference...
diff_large_alpha_percentage <- length(difference_states_large_alpha) / 50.0

# --------------------------------------------------------------------------------------------------------------------------

# print results...
print(paste("Percentage of US states with significant difference at alpha = 0.01:",
            diff_small_alpha_percentage,
            sep = " "))

print(paste("Percentage of US states with significant difference at alpha = 0.05:",
            diff_middle_alpha_percentage,
            sep = " "))

print(paste("Percentage of US states with significant difference at alpha = 0.10:",
            diff_large_alpha_percentage,
            sep = " "))

# --------------------------------------------------------------------------------------------------------------------------

# create lenient_smaller_mean vectors for the alpha = 0.01, 0.05, 0.10 levels of significance...
lenient_smaller_mean_small_alpha_states <- vector()
lenient_smaller_mean_middle_alpha_states <- vector()
lenient_smaller_mean_large_alpha_states <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# also create lenient_not_smaller vector...
lenient_not_smaller_states <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# loop through each state and create strict_larger_mean ttest objects for each...
for (index in state_indices) {
  assign(paste(state.abb[index], "strict_larger_mean_ttest_obj", sep = "_"),
         t.test(get(paste(state.abb[index], "strict", sep = "_"))$daily_cases,
                get(paste(state.abb[index], "lenient", sep = "_"))$daily_cases,
                alternative = "greater")
  )
}

# --------------------------------------------------------------------------------------------------------------------------


# populate lenient_smaller and lenient_not_smaller vectors for each level of significance...
for (index in state_indices) {
  if (get(paste(state.abb[index], "strict_larger_mean_ttest_obj", sep = "_"))$p.value < 0.01) {
    lenient_smaller_mean_large_alpha_states[length(lenient_smaller_mean_large_alpha_states) + 1] <- state.abb[index]
    lenient_smaller_mean_middle_alpha_states[length(lenient_smaller_mean_middle_alpha_states) + 1] <- state.abb[index]
    lenient_smaller_mean_small_alpha_states[length(lenient_smaller_mean_small_alpha_states) + 1] <- state.abb[index]
  }
  else if (get(paste(state.abb[index], "strict_larger_mean_ttest_obj", sep = "_"))$p.value < 0.05) {
    lenient_smaller_mean_large_alpha_states[length(lenient_smaller_mean_large_alpha_states) + 1] <- state.abb[index]
    lenient_smaller_mean_middle_alpha_states[length(lenient_smaller_mean_middle_alpha_states) + 1] <- state.abb[index]
  }
  else if (get(paste(state.abb[index], "strict_larger_mean_ttest_obj", sep = "_"))$p.value < 0.10) {
    lenient_smaller_mean_large_alpha_states[length(lenient_smaller_mean_large_alpha_states) + 1] <- state.abb[index]
  }
  else {
    lenient_not_smaller_states[length(lenient_not_smaller_states) + 1] <- state.abb[index]
  }
}

# --------------------------------------------------------------------------------------------------------------------------

# calculate alpha = 0.01 (low) percentage of states with difference...
lenient_smaller_small_alpha_percentage <- length(lenient_smaller_mean_small_alpha_states) / 50.0

# --------------------------------------------------------------------------------------------------------------------------

# calculate alpha = 0.05 (mid) percentage of states with difference...
lenient_smaller_middle_alpha_percentage <- length(lenient_smaller_mean_middle_alpha_states) / 50.0

# --------------------------------------------------------------------------------------------------------------------------

# calculate alpha = 0.10 (high) percentage of states with difference...
lenient_smaller_large_alpha_percentage <- length(lenient_smaller_mean_large_alpha_states) / 50.0

# --------------------------------------------------------------------------------------------------------------------------

# print results...
print(paste("Percentage of US states with smaller lenient data average daily cases at alpha = 0.01:",
            lenient_smaller_small_alpha_percentage,
            sep = " "))

print(paste("Percentage of US states with smaller lenient data average daily cases at alpha = 0.05:",
            lenient_smaller_middle_alpha_percentage,
            sep = " "))

print(paste("Percentage of US states with smaller lenient data average daily cases at alpha = 0.10:",
            lenient_smaller_large_alpha_percentage,
            sep = " "))

# --------------------------------------------------------------------------------------------------------------------------

# create lenient_larger_mean vectors for the alpha = 0.01, 0.05, 0.10 levels of significance...
lenient_larger_mean_small_alpha_states <- vector()
lenient_larger_mean_middle_alpha_states <- vector()
lenient_larger_mean_large_alpha_states <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# also create lenient_not_smaller vector...
lenient_not_larger_states <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# loop through each state and create strict_larger_mean ttest objects for each...
for (index in state_indices) {
  assign(paste(state.abb[index], "strict_smaller_mean_ttest_obj", sep = "_"),
         t.test(get(paste(state.abb[index], "strict", sep = "_"))$daily_cases,
                get(paste(state.abb[index], "lenient", sep = "_"))$daily_cases,
                alternative = "less")
  )
}

# --------------------------------------------------------------------------------------------------------------------------

# populate lenient_smaller and lenient_not_smaller vectors for each level of significance...
for (index in state_indices) {
  if (get(paste(state.abb[index], "strict_smaller_mean_ttest_obj", sep = "_"))$p.value < 0.01) {
    lenient_larger_mean_large_alpha_states[length(lenient_larger_mean_large_alpha_states) + 1] <- state.abb[index]
    lenient_larger_mean_middle_alpha_states[length(lenient_larger_mean_middle_alpha_states) + 1] <- state.abb[index]
    lenient_larger_mean_small_alpha_states[length(lenient_larger_mean_small_alpha_states) + 1] <- state.abb[index]
  }
  else if (get(paste(state.abb[index], "strict_smaller_mean_ttest_obj", sep = "_"))$p.value < 0.05) {
    lenient_larger_mean_large_alpha_states[length(lenient_larger_mean_large_alpha_states) + 1] <- state.abb[index]
    lenient_larger_mean_middle_alpha_states[length(lenient_larger_mean_middle_alpha_states) + 1] <- state.abb[index]
  }
  else if (get(paste(state.abb[index], "strict_smaller_mean_ttest_obj", sep = "_"))$p.value < 0.10) {
    lenient_larger_mean_large_alpha_states[length(lenient_larger_mean_large_alpha_states) + 1] <- state.abb[index]
  }
  else {
    lenient_not_larger_states[length(lenient_not_larger_states) + 1] <- state.abb[index]
  }
}

# --------------------------------------------------------------------------------------------------------------------------

# calculate alpha = 0.01 (low) percentage of states with difference...
lenient_larger_small_alpha_percentage <- length(lenient_larger_mean_small_alpha_states) / 50.0

# --------------------------------------------------------------------------------------------------------------------------

# calculate alpha = 0.05 (mid) percentage of states with difference...
lenient_larger_middle_alpha_percentage <- length(lenient_larger_mean_middle_alpha_states) / 50.0

# --------------------------------------------------------------------------------------------------------------------------

# calculate alpha = 0.10 (high) percentage of states with difference...
lenient_larger_large_alpha_percentage <- length(lenient_larger_mean_large_alpha_states) / 50.0

# --------------------------------------------------------------------------------------------------------------------------

# print results...
print(paste("Percentage of US states with larger lenient data average daily cases at alpha = 0.01:",
            lenient_larger_small_alpha_percentage,
            sep = " "))

print(paste("Percentage of US states with larger lenient data average daily cases at alpha = 0.05:",
            lenient_larger_middle_alpha_percentage,
            sep = " "))

print(paste("Percentage of US states with larger lenient data average daily cases at alpha = 0.10:",
            lenient_larger_large_alpha_percentage,
            sep = " "))

# --------------------------------------------------------------------------------------------------------------------------

# create New York ggplot object...
f <- ggplot(data = NY, aes(x = NY$stringency_category,
                           y = NY$daily_cases))

# --------------------------------------------------------------------------------------------------------------------------

# create box plot for New York...
f + geom_boxplot(fill = "#0099ffbb") +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "New York Strigency Comparison") +
  theme_linedraw()

# --------------------------------------------------------------------------------------------------------------------------

# create violin plot for New York...
f + geom_violin(fill = "#0099ffbb",
                draw_quantiles = c(0.5)) +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "New York Stringency Comparison") +
  theme_linedraw()

# --------------------------------------------------------------------------------------------------------------------------

# create California ggplot object...
f <- ggplot(data = CA, aes(x = CA$stringency_category,
                           y = CA$daily_cases))

# --------------------------------------------------------------------------------------------------------------------------

# create box plot for California...
f + geom_boxplot(fill="#ff0055bb") +
  labs(x = "Stringency Catgegory",
       y = "Daily Cases",
       title = "California Stringency Comparison") +
  theme_linedraw()

# --------------------------------------------------------------------------------------------------------------------------

# create violin plot for California...
f + geom_violin(fill = "#ff0055bb",
                draw_quantiles = c(0.5)) +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "California Stringency Comparison") +
  theme_linedraw()

# --------------------------------------------------------------------------------------------------------------------------

# create Florida ggplot object...
f <- ggplot(data = FL, aes(x = FL$stringency_category,
                           y = FL$daily_cases))

# --------------------------------------------------------------------------------------------------------------------------

# create box plot for Florida...
f + geom_boxplot(fill="#bb3377bb") +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "Florida Stringency Comparison") +
  theme_linedraw()

# --------------------------------------------------------------------------------------------------------------------------

# create violin plot for Florida...
f + geom_violin(fill = "#bb3377bb",
                draw_quantiles = c(0.5)) +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "Florida Stringency Comparison") +
  theme_linedraw()

# --------------------------------------------------------------------------------------------------------------------------

# next, we should get the fips codes for each state...
# shoutout to Jesse for the code...

fips <- c(01:56)
fips <- fips[! fips %in% c('3','7','11','14','43','52')]

# ---------------------------------------------------------------------------------------------

# loop through each state to calculate and store each state's average stringency index...
for (index in state_indices) {
  assign(paste(state.abb[index], "avg_stringency_index", sep = "_"),
         mean(get(state.abb[index])$stringency_index))
}

# --------------------------------------------------------------------------------------------------------------------------

# create a vector to contain each state's average stringency index...
avg_stringency_indices_vector <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# iterate through each state average stringency index variable to populate the vector...
for (index in state_indices) {
  avg_stringency_indices_vector[length(avg_stringency_indices_vector) + 1] <- get(paste(
    state.abb[index], "avg_stringency_index", sep = "_"
  ))
}

# --------------------------------------------------------------------------------------------------------------------------

# create data.frame with state names in first column, fips in second, and average stringency index in third...
states_avg_stringency_index_df <- data.frame(
  states = state.name,
  fips = fips,
  avg_stringency_index = avg_stringency_indices_vector
)

# --------------------------------------------------------------------------------------------------------------------------

# create heat map using average stringency indices DataFrame...
plot_usmap(data = states_avg_stringency_index_df,
           values = "avg_stringency_index",
           labels = FALSE) +
  scale_fill_continuous(low = "white",
                        high = "green",
                        name = "Average Stringency Index (2021)",
                        label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Average Stringency Indices in the US")

# --------------------------------------------------------------------------------------------------------------------------

# loop through each state to calculate and store average daily cases...
for (index in state_indices) {
  assign(paste(state.abb[index], "avg_daily_cases", sep = "_"), 
         mean(get(state.abb[index])$daily_cases))
}

# --------------------------------------------------------------------------------------------------------------------------

# create a vector to contain each state's average daily cases...
states_avg_daily_cases_vector <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# populate the vector by iterating through each state...
for (index in state_indices) {
  states_avg_daily_cases_vector[length(states_avg_daily_cases_vector) + 1] <- get(
    paste(state.abb[index], "avg_daily_cases", sep = "_"
    ))
}

# --------------------------------------------------------------------------------------------------------------------------

# create another data.frame with the state names, fips codes, and average daily cases...
states_avg_daily_cases_df <- data.frame(
  states = state.name,
  fips = fips,
  avg_daily_cases = states_avg_daily_cases_vector
)

# --------------------------------------------------------------------------------------------------------------------------

# plot another heat map using the average daily cases DataFrame...
plot_usmap(data = states_avg_daily_cases_df,
           values = "avg_daily_cases",
           labels = FALSE) +
  scale_fill_continuous(low = "white",
                        high = "purple",
                        name = "Average Daily Cases (2021)",
                        label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Average Daily Cases in the US")

# --------------------------------------------------------------------------------------------------------------------------

# loop through each state to calculate and store each state's average daily deaths...
for (index in state_indices) {
  assign(paste(state.abb[index], "avg_daily_deaths", sep = "_"), 
         mean(get(state.abb[index])$daily_deaths))
}

# --------------------------------------------------------------------------------------------------------------------------

# create a vector to contain each state's average daily deaths...
states_avg_daily_deaths_vector <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# populate the vector by iterating through each state...
for (index in state_indices) {
  states_avg_daily_deaths_vector[length(states_avg_daily_deaths_vector) + 1] <- get(
    paste(state.abb[index], "avg_daily_deaths", sep = "_"
    ))
}

# --------------------------------------------------------------------------------------------------------------------------

# create another DataFrame with the state names, fips codes, and average daily deaths...
states_avg_daily_deaths_df <- data.frame(
  states = state.name,
  fips = fips,
  avg_daily_deaths = states_avg_daily_deaths_vector
)

# --------------------------------------------------------------------------------------------------------------------------

# plot another heat map using the average daily deaths data.frame...
plot_usmap(data = states_avg_daily_deaths_df,
           values = "avg_daily_deaths",
           labels = FALSE) +
  scale_fill_continuous(low = "white",
                        high = "red",
                        name = "Average Daily Deaths (2021)",
                        label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Average Daily Deaths in the US")

# --------------------------------------------------------------------------------------------------------------------------

# define an empty vector to contain 2-sided minimum levels of significance for each state...
states_diff_alpha <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# populate empty vector...
for (index in state_indices) {
  if (get(paste(state.abb[index], "diff_ttest_obj", sep = "_"))$p.value < 0.01) {
    states_diff_alpha[length(states_diff_alpha) + 1] <- "< 0.01"
  }
  else if (get(paste(state.abb[index], "diff_ttest_obj", sep = "_"))$p.value < 0.05) {
    states_diff_alpha[length(states_diff_alpha) + 1] <- "< 0.05"
  }
  else if (get(paste(state.abb[index], "diff_ttest_obj", sep = "_"))$p.value < 0.10) {
    states_diff_alpha[length(states_diff_alpha) + 1] <- "< 0.10"
  }
  else {
    states_diff_alpha[length(states_diff_alpha) + 1] <- ">= 0.10"
  }
}

# --------------------------------------------------------------------------------------------------------------------------

# create a data.frame to be used in a heat map plot...
states_diff_alpha_df <- data.frame(
  states = state.name,
  fips = fips,
  level_of_significance = states_diff_alpha
)

# --------------------------------------------------------------------------------------------------------------------------

# plot a heat map using the states_diff_alpha_df data.frame...
plot_usmap(data = states_diff_alpha_df,
           values = "level_of_significance",
           labels = FALSE) +
  scale_fill_manual(name = "Minimum Alpha",
                    values = c(">= 0.10" = "white",
                               "< 0.10" = "#77f59d33",
                               "< 0.05" = "#77f59d88",
                               "< 0.01" = "#77f59ddd")) +
  theme(legend.position = "right") +
  labs(title = "Minimum Levels of Significance (2-sided)")

# --------------------------------------------------------------------------------------------------------------------------

# create an empty vector to contain state minimum alphas...
states_lenient_larger_alpha <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# populate empty vector...
for (index in state_indices) {
  if (get(paste(state.abb[index], "strict_smaller_mean_ttest_obj", sep = "_"))$p.value < 0.01) {
    states_lenient_larger_alpha[length(states_lenient_larger_alpha) + 1] <- "< 0.01"
  }
  else if (get(paste(state.abb[index], "strict_smaller_mean_ttest_obj", sep = "_"))$p.value < 0.05) {
    states_lenient_larger_alpha[length(states_lenient_larger_alpha) + 1] <- "< 0.05"
  }
  else if (get(paste(state.abb[index], "strict_smaller_mean_ttest_obj", sep = "_"))$p.value < 0.10) {
    states_lenient_larger_alpha[length(states_lenient_larger_alpha) + 1] <- "< 0.10"
  }
  else {
    states_lenient_larger_alpha[length(states_lenient_larger_alpha) + 1] <- ">= 0.10"
  }
}

# --------------------------------------------------------------------------------------------------------------------------

# create a data.frame to be used in a heat map plot...
states_lenient_larger_alpha_df <- data.frame(
  states = state.name,
  fips = fips,
  level_of_significance = states_lenient_larger_alpha
)

# --------------------------------------------------------------------------------------------------------------------------

# plot a heat map using the states_lenient_larger_alpha data.frame...
plot_usmap(data = states_lenient_larger_alpha_df,
           values = "level_of_significance",
           labels = FALSE) +
  scale_fill_manual(name = "Minimum Alpha",
                    values = c(">= 0.10" = "white",
                               "< 0.10" = "#4596f333",
                               "< 0.05" = "#4596f388",
                               "< 0.01" = "#4596f3dd")) +
  theme(legend.position = "right") +
  labs(title = "Minimum Levels of Significance\n(1-sided, Lenient Larger Mean)")

# --------------------------------------------------------------------------------------------------------------------------

# create an empty vector to contain state minimum alphas...
states_lenient_smaller_alpha <- vector()

# --------------------------------------------------------------------------------------------------------------------------

# populate empty vector...
for (index in state_indices) {
  if (get(paste(state.abb[index], "strict_larger_mean_ttest_obj", sep = "_"))$p.value < 0.01) {
    states_lenient_smaller_alpha[length(states_lenient_smaller_alpha) + 1] <- "< 0.01"
  }
  else if (get(paste(state.abb[index], "strict_larger_mean_ttest_obj", sep = "_"))$p.value < 0.05) {
    states_lenient_smaller_alpha[length(states_lenient_smaller_alpha) + 1] <- "< 0.05"
  }
  else if (get(paste(state.abb[index], "strict_larger_mean_ttest_obj", sep = "_"))$p.value < 0.10) {
    states_lenient_smaller_alpha[length(states_lenient_smaller_alpha) + 1] <- "< 0.10"
  }
  else {
    states_lenient_smaller_alpha[length(states_lenient_smaller_alpha) + 1] <- ">= 0.10"
  }
}

# --------------------------------------------------------------------------------------------------------------------------

# create a data.frame to be used in a heat map plot...
states_lenient_smaller_alpha_df <- data.frame(
  states = state.name,
  fips = fips,
  level_of_significance = states_lenient_smaller_alpha
)

# --------------------------------------------------------------------------------------------------------------------------

# plot a heat map using the states_lenient_smaller_alpha data.frame...
plot_usmap(data = states_lenient_smaller_alpha_df,
           values = "level_of_significance",
           labels = FALSE) +
  scale_fill_manual(name = "Minimum Alpha",
                    values = c(">= 0.10" = "white",
                               "< 0.10" = "#8f556733",
                               "< 0.05" = "#8f556788",
                               "< 0.01" = "#8f5567dd")) +
  theme(legend.position = "right") +
  labs(title = "Minimum Levels of Significance\n(1-sided, Lenient Smaller Mean)")

# --------------------------------------------------------------------------------------------------------------------------

# create ggplot object for NY...
i <- ggplot(data = NY,
            mapping = aes(x = date))

# --------------------------------------------------------------------------------------------------------------------------

# create time series plot for NY...
i + geom_line(aes(y = daily_cases / 100,
                  col = "Daily Cases / 100",
                  linetype = "Daily Cases / 100",
                  size = "Daily Cases / 100")) +
  geom_line(aes(y = daily_deaths,
                col = "Daily Deaths",
                linetype = "Daily Deaths",
                size = "Daily Deaths")) +
  geom_line(aes(y = stringency_index,
                col = "Stringency Index",
                linetype = "Stringency Index",
                size = "Stringency Index")) +
  scale_color_manual(name = "Series",
                     values = c("Daily Cases / 100" = "purple",
                                "Daily Deaths" = "red",
                                "Stringency Index" = "green")) +
  scale_linetype_manual(name = "Series",
                        values = c("Daily Cases / 100" = 1,
                                   "Daily Deaths" = 2,
                                   "Stringency Index" = 1)) +
  scale_size_manual(name = "Series",
                    values = c("Daily Cases / 100" = 0.5,
                               "Daily Deaths" = 0.5,
                               "Stringency Index" = 1)) +
  labs(title = "New York Time Series Analysis",
       x = "Date", y = "Series Value") +
  guides(fill = guide_legend(title = "Series")) +
  theme_linedraw()

# --------------------------------------------------------------------------------------------------------------------------

# create ggplot object for LA...
i <- ggplot(data = LA,
            mapping = aes(x = date))

# --------------------------------------------------------------------------------------------------------------------------

# create time series plot for LA...
i + geom_line(aes(y = daily_cases / 100,
                  col = "Daily Cases / 100",
                  linetype = "Daily Cases / 100",
                  size = "Daily Cases / 100")) +
  geom_line(aes(y = daily_deaths,
                col = "Daily Deaths",
                linetype = "Daily Deaths",
                size = "Daily Deaths")) +
  geom_line(aes(y = stringency_index,
                col = "Stringency Index",
                linetype = "Stringency Index",
                size = "Stringency Index")) +
  scale_color_manual(name = "Series",
                     values = c("Daily Cases / 100" = "purple",
                                "Daily Deaths" = "red",
                                "Stringency Index" = "green")) +
  scale_linetype_manual(name = "Series",
                        values = c("Daily Cases / 100" = 1,
                                   "Daily Deaths" = 2,
                                   "Stringency Index" = 1)) +
  scale_size_manual(name = "Series",
                    values = c("Daily Cases / 100" = 0.5,
                               "Daily Deaths" = 0.5,
                               "Stringency Index" = 1)) +
  labs(title = "Louisiana Time Series Analysis",
       x = "Date", y = "Series Value") +
  guides(fill = guide_legend(title = "Series")) +
  theme_linedraw()

# --------------------------------------------------------------------------------------------------------------------------

# create ggplot object for HI...
i <- ggplot(data = HI,
            mapping = aes(x = date))

# --------------------------------------------------------------------------------------------------------------------------

# create time series plot for HI...
i + geom_line(aes(y = daily_cases / 100,
                  col = "Daily Cases / 100",
                  linetype = "Daily Cases / 100",
                  size = "Daily Cases / 100")) +
  geom_line(aes(y = daily_deaths,
                col = "Daily Deaths",
                linetype = "Daily Deaths",
                size = "Daily Deaths")) +
  geom_line(aes(y = stringency_index,
                col = "Stringency Index",
                linetype = "Stringency Index",
                size = "Stringency Index")) +
  scale_color_manual(name = "Series",
                     values = c("Daily Cases / 100" = "purple",
                                "Daily Deaths" = "red",
                                "Stringency Index" = "green")) +
  scale_linetype_manual(name = "Series",
                        values = c("Daily Cases / 100" = 1,
                                   "Daily Deaths" = 2,
                                   "Stringency Index" = 1)) +
  scale_size_manual(name = "Series",
                    values = c("Daily Cases / 100" = 0.5,
                               "Daily Deaths" = 0.5,
                               "Stringency Index" = 1)) +
  labs(title = "Hawaii Time Series Analysis",
       x = "Date", y = "Series Value") +
  guides(fill = guide_legend(title = "Series")) +
  theme_linedraw()

# ==========================================================================================================================

# End of Code

# ==========================================================================================================================