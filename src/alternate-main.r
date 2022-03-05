# remove all previously existing environment variables...

rm(list=ls())

# ---------------------------------------------------------------------------------------------
# set path...

# Charlie's path...
cRoot <- "~"
cPathOut <- file.path(cRoot, "Users", "charlieclark", "Documents", "GitHub", "ECO322Spring2022Project1")

pathOut <- cPathOut # REPLACE WITH YOUR OWN PATH OUT VARIABLE!!!

# ---------------------------------------------------------------------------------------------
# include data.table, COVID19, and ggplot2 packages...

library(COVID19)
library(data.table)
library(ggplot2)

# ---------------------------------------------------------------------------------------------
# import USA data...

USA <- covid19( country = c("United States") , level = 2, verbose = FALSE)
View(USA[1:100, ])

# ---------------------------------------------------------------------------------------------
# convert USA data to data.table...

setDT(USA)
is.data.table(USA)

# ---------------------------------------------------------------------------------------------
# rename administrative_area_level_2 to state and set keys for USA data...

# rename...
setnames(USA, c("administrative_area_level_2"), c("state"))
colnames(USA)

# keys...
setkeyv(USA, c("state", "date"))
key(USA)

# ---------------------------------------------------------------------------------------------
# make new feature called daily_cases...

# create n=1 lagged feature using confirmed cases for each state...
USA[, previous := shift(confirmed, 1, fill = NA_integer_), by = state]
View(USA[1:100, ])

# create daily_cases feature by subtracting previous from current...
USA[, daily_cases := confirmed - previous]
View(USA[1:100, ])

# delete lagged feature...
USA[, previous := NULL]
colnames(USA)

# ---------------------------------------------------------------------------------------------
# make new feature called daily_deaths...

# create n=1 lagged feature using deaths for each state...
USA[, previous := shift(deaths, 1, fill = NA_integer_), by = state]
View(USA[1:100, ])

# create daily deaths feature by subtracting previous from deaths...
USA[, daily_deaths := deaths - previous]
View(USA[1:100, ])

# delete lagged feature...
USA[, previous := NULL]
colnames(USA)

# ---------------------------------------------------------------------------------------------
# reassign USA data.table to a trimmed version of itself...
# only include date, state, daily_cases, daily_deaths, and stringency_index...

USA <- USA[, c("date", "state", "daily_cases", "daily_deaths", "stringency_index")]
View(USA[1:100, ])

# ---------------------------------------------------------------------------------------------
# trim USA data.table to only include dates in interval [1/1/2021, 12/31/2021]...

USA <- USA[date>="2021-01-01" & date<="2021-12-31", ]
View(USA[state == "Alabama", ])

# ---------------------------------------------------------------------------------------------
# correct any records with less than 0 daily cases by setting them to 0...

# check how many such records there are...
length(USA[daily_cases < 0, ]$daily_cases)

# replace each such record with a 0 value in daily_cases...
USA[daily_cases < 0, daily_cases := 0]

# make sure all such records have been corrected...
length(USA[daily_cases < 0, ]$daily_cases) # should print 0...

# ---------------------------------------------------------------------------------------------
# correct any records with less than 0 daily deaths by setting them to 0...

# check how many such records there are...
length(USA[daily_deaths < 0, ]$daily_deaths)

# replace each such record with a 0 value in daily_deaths...
USA[daily_deaths < 0, daily_deaths := 0]

# make sure all such records have been corrected...
length(USA[daily_deaths < 0, ]$daily_deaths) # should print 0...

# ---------------------------------------------------------------------------------------------
# create California data.table and use it as a test sample...

# create CA data.table...
CA <- USA[state == "California", ]

# calculate average stringency_index for CA...
CA_avg_stringency_index <- mean(CA$stringency_index)

# create separate data.tables based on a comparison to average_stringency_index...
CA_lenient <- CA[stringency_index <= CA_avg_stringency_index]
CA_strict <- CA[stringency_index > CA_avg_stringency_index]

# calculate average of daily_cases for the two data.tables...
CA_lenient_avg_daily_cases <- mean(CA_lenient$daily_cases)
CA_strict_avg_daily_cases <- mean(CA_strict$daily_cases)

# hypothesis test...
t.test(CA_lenient$daily_cases, CA_strict$daily_cases)

# ---------------------------------------------------------------------------------------------
# create New York data.table and use it as another test sample...

# create NY data.table...
NY <- USA[state == "New York"]

# calculate average stringency_index for NY...
NY_avg_stringency_index <- mean(NY$stringency_index)

# create separate data.tables based on a comparison to average_stringency_index...
NY_lenient <- NY[stringency_index <= NY_avg_stringency_index]
NY_strict <- NY[stringency_index > NY_avg_stringency_index]

# hypothesis test...
t.test(NY_lenient$daily_cases, NY_strict$daily_cases)

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

# ---------------------------------------------------------------------------------------------
# create data.tables for each state (shout out to Jesse for making this easy)...

# create vector with numbers 1:50...
state_indices <- c(1:50)

# use built-in R packages to create data.tables for each state, using the...
# vector of numbers 1:50 to do so in a for loop...
for (index in state_indices) {
  assign(state.abb[index], USA[state == state.name[index]])
}

# ---------------------------------------------------------------------------------------------
# make sure each state has no NULL/NA stringency index value...

for (index in state_indices) {
  state_ptr <- get(state.abb[index])
  
  print(paste(state.name[index], any(is.na(state_ptr$stringency_index)), sep = " ----> " ))
}

# it looks like Louisiana, Maryland, and Rhode Island have NA average stringency indices...
# we should look at these sets individually...

# start with Louisiana...

# get indices of NA records for Louisiana...
LA_na_indices <- which(is.na(LA$stringency_index), arr.ind = TRUE)

# there seems to be only one NA record, so we only need to interpolate once...
# replace NA record using mean of stringency_index values for 15 days before and after...
# day with NA record for stringency_index...
LA$stringency_index[LA_na_indices] = mean(LA$stringency_index[LA_na_indices - 15:LA_na_indices + 15], na.rm = TRUE)

# check if we were successful...
any(is.na(LA$stringency_index)) # should print FALSE...

# next up is Maryland...

# get indices of NA records for Maryland...
MD_na_indices <- which(is.na(MD$stringency_index), arr.ind = TRUE)

# Maryland has multiple NA records, however they all seem to be grouped consecutively...
# we can interpolate each NA record using the mean stringency_index for the 15 days...
# before the first NA record and the 15 days after the last NA record...

# store indices of first and last NA records...
MD_na_first <- MD_na_indices[1]
MD_na_last <- MD_na_indices[length(MD_na_indices)]

# interpolate...
for (index in MD_na_indices) {
  MD$stringency_index[index] = mean(MD$stringency_index[MD_na_first - 15:MD_na_last + 15], na.rm = TRUE)
}

# check if we were successful...
any(is.na(MD$stringency_index)) # should print FALSE...

# finally moving onto Rhode Island...

# get NA indices for Rhode Island stringency index...
RI_na_indices <- which(is.na(RI$stringency_index), arr.ind = TRUE)

# it looks like Rhode Island has multiple NA stringency indices...
# but they're all clumped together as well...
# let's repeat the process we followed for Maryland...

# store indices of first and last NA records...
RI_na_first <- RI_na_indices[1]
RI_na_last <- RI_na_indices[length(RI_na_indices)]

# interpolate...
for (index in RI_na_indices) {
  RI$stringency_index[index] = mean(RI$stringency_index[RI_na_first - 15:RI_na_last + 15], na.rm = TRUE)
}

# check if we were successful...
any(is.na(RI$stringency_index)) # should print FALSE...

# ---------------------------------------------------------------------------------------------
# make sure every state has an average number of daily cases...

for (index in state_indices) {
  state_ptr <- get(state.abb[index])
  
  print(paste(state.name[index], any(is.na(state_ptr$daily_cases)), sep = " ----> " ))
}

# it looks like each state has all its daily cases records filled (no NULL or NA values)...

# ---------------------------------------------------------------------------------------------
# make sure every state has an average number of daily deaths...

for (index in state_indices) {
  state_ptr <- get(state.abb[index])
  
  print(paste(state.name[index], any(is.na(state_ptr$daily_deaths)), sep = " ----> " ))
}

# it looks like each state has all its daily deaths records filled (no NULL or NA values)...

# ---------------------------------------------------------------------------------------------
# iterate through every state to determine what percentage of the US states...
# saw statistically significant effects from stricter-than-average regulations...

# define function to create a catgeorical varaible in a given data.table...
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

# create lenient and strict data.tables for each state, based on the values...
# is each record's stringency_category value...
for (index in state_indices) {
  assign(state.abb[index], create_stringency_category(get(state.abb[index])))
  
  assign(paste(state.abb[index], "lenient", sep = "_"),
         setDT(get(state.abb[index])[stringency_category == "Less than or Equal to Average"]))
  
  assign(paste(state.abb[index], "strict", sep = "_"),
         setDT(get(state.abb[index])[stringency_category == "Greater than Average"]))
}

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

# loop through each state and perform the 2-sided t tests mentioned above...
for (index in state_indices) {
  assign(paste(state.abb[index], "diff_ttest_obj", sep = "_"),
         t.test(
           get(paste(state.abb[index], "lenient", sep = "_"))$daily_cases,
           get(paste(state.abb[index], "strict", sep = "_"))$daily_cases
         ))
}

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

# ---------------------------------------------------------------------------------------------
# calculate percentages for each alpha (0.01, 0.05, 0.10)...

# calculate alpha = 0.01 (low) percentage of states with difference...
diff_small_alpha_percentage <- length(difference_states_small_alpha) / 50.0

# calculate alpha = 0.05 (mid) percentage of states with difference...
diff_middle_alpha_percentage <- length(difference_states_middle_alpha) / 50.0

# calculate alpha = 0.10 (high) percentage of states with difference...
diff_large_alpha_percentage <- length(difference_states_large_alpha) / 50.0

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

# create lenient_smaller_mean vectors for the alpha = 0.01, 0.05, 0.10 levels of significance...
lenient_smaller_mean_small_alpha_states <- vector()
lenient_smaller_mean_middle_alpha_states <- vector()
lenient_smaller_mean_large_alpha_states <- vector()

# also create lenient_not_smaller vector...
lenient_not_smaller_states <- vector()

# loop through each state and create strict_larger_mean ttest objects for each...
for (index in state_indices) {
  assign(paste(state.abb[index], "strict_larger_mean_ttest_obj", sep = "_"),
         t.test(get(paste(state.abb[index], "strict", sep = "_"))$daily_cases,
                get(paste(state.abb[index], "lenient", sep = "_"))$daily_cases,
                alternative = "greater")
         )
}

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

# calculate alpha = 0.01 (low) percentage of states with difference...
lenient_smaller_small_alpha_percentage <- length(lenient_smaller_mean_small_alpha_states) / 50.0

# calculate alpha = 0.05 (mid) percentage of states with difference...
lenient_smaller_middle_alpha_percentage <- length(lenient_smaller_mean_middle_alpha_states) / 50.0

# calculate alpha = 0.10 (high) percentage of states with difference...
lenient_smaller_large_alpha_percentage <- length(lenient_smaller_mean_large_alpha_states) / 50.0

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

# create lenient_larger_mean vectors for the alpha = 0.01, 0.05, 0.10 levels of significance...
lenient_larger_mean_small_alpha_states <- vector()
lenient_larger_mean_middle_alpha_states <- vector()
lenient_larger_mean_large_alpha_states <- vector()

# also create lenient_not_smaller vector...
lenient_not_larger_states <- vector()

# loop through each state and create strict_larger_mean ttest objects for each...
for (index in state_indices) {
  assign(paste(state.abb[index], "strict_smaller_mean_ttest_obj", sep = "_"),
         t.test(get(paste(state.abb[index], "strict", sep = "_"))$daily_cases,
                get(paste(state.abb[index], "lenient", sep = "_"))$daily_cases,
                alternative = "less")
  )
}

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

# calculate alpha = 0.01 (low) percentage of states with difference...
lenient_larger_small_alpha_percentage <- length(lenient_larger_mean_small_alpha_states) / 50.0

# calculate alpha = 0.05 (mid) percentage of states with difference...
lenient_larger_middle_alpha_percentage <- length(lenient_larger_mean_middle_alpha_states) / 50.0

# calculate alpha = 0.10 (high) percentage of states with difference...
lenient_larger_large_alpha_percentage <- length(lenient_larger_mean_large_alpha_states) / 50.0

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

# ---------------------------------------------------------------------------------------------
# use ggplot2 to create box plot visualizations...

# create New York ggplot object...
f <- ggplot(data = NY, aes(x = NY$stringency_category,
                           y = NY$daily_cases))

# create box plot for New York...
f + geom_boxplot(fill = "#0099ffbb") +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "New York Strigency Comparison") +
  theme_linedraw()

# create violin plot for New York...
f + geom_violin(fill = "#0099ffbb",
                draw_quantiles = c(0.5)) +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "New York Stringency Comparison") +
  theme_linedraw()

# create California ggplot object...
f <- ggplot(data = CA, aes(x = CA$stringency_category,
                           y = CA$daily_cases))

# create box plot for California...
f + geom_boxplot(fill="#ff0055bb") +
  labs(x = "Stringency Catgegory",
       y = "Daily Cases",
       title = "California Stringency Comparison") +
  theme_linedraw()

# create violin plot for California...
f + geom_violin(fill = "#ff0055bb",
                draw_quantiles = c(0.5)) +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "California Stringency Comparison") +
  theme_linedraw()

# create Florida ggplot object...
f <- ggplot(data = FL, aes(x = FL$stringency_category,
                           y = FL$daily_cases))

# create box plot for Florida...
f + geom_boxplot(fill="#bb3377bb") +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "Florida Stringency Comparison") +
  theme_linedraw()

# create violin plot for Florida...
f + geom_violin(fill = "#bb3377bb",
                draw_quantiles = c(0.5)) +
  labs(x = "Stringency Category",
       y = "Daily Cases",
       title = "Florida Stringency Comparison") +
  theme_linedraw()

# ---------------------------------------------------------------------------------------------
# now, we want to create some heat maps, so we need to import some more packages...

library(sf)
library(tigris)      
library(tidycensus) # official US state, county boundaries...
library(mapview)
library(usmap)

# ---------------------------------------------------------------------------------------------
# next, we should get the fips codes for each state...
# shoutout to Jesse for the code...

z <- c(01:56)
z <- z[! z %in% c('3','7','11','14','43','52')]

# ---------------------------------------------------------------------------------------------
# at this point, we can create a heat map for each state's average stringency index...

# loop through each state to calculate and store each state's average stringency index...
for (index in state_indices) {
  assign(paste(state.abb[index], "avg_stringency_index", sep = "_"),
         mean(get(state.abb[index])$stringency_index))
}

# create a vector to contain each state's average stringency index...
avg_stringency_indices_vector <- vector()

# iterate through each state average stringency index variable to populate the vector...
for (index in state_indices) {
  avg_stringency_indices_vector[length(avg_stringency_indices_vector) + 1] <- get(paste(
    state.abb[index], "avg_stringency_index", sep = "_"
  ))
}

# create DataFrame with state names in first column, fips in second, and average stringency index in third...
states_avg_stringency_index_df <- data.frame(
  states = state.name,
  fips = z,
  avg_stringency_index = avg_stringency_indices_vector
)

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

# ---------------------------------------------------------------------------------------------
# Next, we'll create a heat map for each state's average number of daily cases...

# loop through each state to calculate and store average daily cases...
for (index in state_indices) {
  assign(paste(state.abb[index], "avg_daily_cases", sep = "_"), 
         mean(get(state.abb[index])$daily_cases))
}

# create a vector to contain each state's average daily cases...
states_avg_daily_cases_vector <- vector()

# populate the vector by iterating through each state...
for (index in state_indices) {
  states_avg_daily_cases_vector[length(states_avg_daily_cases_vector) + 1] <- get(
    paste(state.abb[index], "avg_daily_cases", sep = "_"
  ))
}

# create another DataFrame with the state names, fips codes, and average daily cases...
states_avg_daily_cases_df <- data.frame(
  states = state.name,
  fips = z,
  avg_daily_cases = states_avg_daily_cases_vector
)

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

# ---------------------------------------------------------------------------------------------
# our last heat map will be for average daily cases in each state...

for (index in state_indices) {
  assign(paste(state.abb[index], "avg_daily_deaths", sep = "_"), 
         mean(get(state.abb[index])$daily_deaths))
}

# create a vector to contain each state's average daily deaths...
states_avg_daily_deaths_vector <- vector()

# populate the vector by iterating through each state...
for (index in state_indices) {
  states_avg_daily_deaths_vector[length(states_avg_daily_deaths_vector) + 1] <- get(
    paste(state.abb[index], "avg_daily_deaths", sep = "_"
    ))
}

# create another DataFrame with the state names, fips codes, and average daily deaths...
states_avg_daily_deaths_df <- data.frame(
  states = state.name,
  fips = z,
  avg_daily_deaths = states_avg_daily_deaths_vector
)

# plot another heat map using the average daily deaths DataFrame...
plot_usmap(data = states_avg_daily_deaths_df,
           values = "avg_daily_deaths",
           labels = FALSE) +
  scale_fill_continuous(low = "white",
                        high = "red",
                        name = "Average Daily Deaths (2021)",
                        label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Average Daily Deaths in the US")

# ---------------------------------------------------------------------------------------------
# now, let's create a some time series plots of daily cases and stringency index for some of the states....

# create ggplot object for NY...
i <- ggplot(data = NY,
            mapping = aes(x = date))

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

# create ggplot object for LA...
i <- ggplot(data = LA,
            mapping = aes(x = date))

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

# create ggplot object for HI...
i <- ggplot(data = HI,
            mapping = aes(x = date))

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
