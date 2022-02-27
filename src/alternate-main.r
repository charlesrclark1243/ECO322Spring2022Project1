# remove all previously existing environment variables...

rm(list=ls())

# ---------------------------------------------------------------------------------------------
# set path...

# Charlie's path...
cRoot <- "~"
cPathOut <- file.path(cRoot, "Users", "charlieclark", "Documents", "GitHub", "ECO322Spring2022Project1")

pathOut <- cPathOut # REPLACE WITH YOUR OWN PATH OUT VARIABLE!!!

# ---------------------------------------------------------------------------------------------
# include data.table and COVID19 packages...

library(COVID19)
library(data.table)

# ---------------------------------------------------------------------------------------------
# import USA data...

USA <- covid19( country = c("United States") , level = 2, verbose = FALSE)
View(USA[1:100, ])

# ---------------------------------------------------------------------------------------------
# convert USA data to data.table...

setDT(USA)
is.data.table(USA)

# ---------------------------------------------------------------------------------------------
# remove administrative_area_level_1 and administrative_area_level_3 columns...

USA[, c("administrative_area_level_1", "administrative_area_level_3") := c(NULL, NULL)]

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
# reassign USA data.table to a trimmed version of itself...
# only include date, state, daily_cases, and stringency_index...

USA <- USA[, c("date", "state", "daily_cases", "stringency_index")]
View(USA)

# ---------------------------------------------------------------------------------------------
# trim USA data.table to only include dates in interval [1/1/2021, 12/31/2021]...

USA <- USA[date>="2021-01-01" & date<="2021-12-31", ]
View(USA[state == "Alabama", ])

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
# make sure each state has an average stringency index...

for (index in state_indices) {
  curr_state <- get(state.abb[index])
  
  print(paste(state.name[index], mean(curr_state$stringency_index), sep = " ----> " ))
}

# it looks like Louisiana, Maryland, and Rhode Island have NA average stringency indices...
# we should look at these sets individually...

# start with Louisiana...

# get indices of NA records for Louisiana...
LA_na_indices <- which(is.na(LA$stringency_index), arr.ind = TRUE)

# there seems to be only one NA record, so we only need to interpolate once...
# replace NA record using mean of stringency_index values for 15 days before and after...
# day with NA record for stringency_index...
LA$stringency_index[LA_na_index] = mean(LA$stringency_index[LA_na_index - 15:LA_na_index + 15], na.rm = TRUE)

# check if we were successful...
any(is.na(LA$stringency_index))

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
any(is.na(MD$stringency_index))

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
any(is.na(RI$stringency_index))

# ---------------------------------------------------------------------------------------------
# iterate through every state to determine what percentage of the US states...
# saw statistically significant effects from stricter-than-average regulations...

diff_states_low <- vector()
diff_states_mid <- vector()
diff_states_high <- vector()

for (index in state_indices) {
  # get the current state's data...
  curr_state <- get(state.abb[index])
  
  # calculate the current state's average stringency index...
  curr_state_avg_stringency_index <- mean(curr_state$stringency_index)

  # separate state data into lenient and strict datasets based on comparisons to average...
  curr_state_lenient <- curr_state[stringency_index <= curr_state_avg_stringency_index]
  curr_state_strict <- curr_state[stringency_index > curr_state_avg_stringency_index]
  
  # perform hypothesis test (2-sided)...
  t_test_obj <- t.test(curr_state_lenient$daily_cases, curr_state_strict$daily_cases)

  # sort based on alpha = 0.01, 0.05, 0.10...
  if (t_test_obj$p.value < 0.01) {
    diff_states_low[length(diff_states_low) + 1] = state.abb[index]
  }
  
  if (t_test_obj$p.value < 0.05) {
    diff_states_mid[length(diff_states_mid) + 1] = state.abb[index]
  }
  
  if (t_test_obj$p.value < 0.10) {
    diff_states_high[length(diff_states_high) + 1] = state.abb[index]
  }
  
  print(paste(state.name[index], t_test_obj$p.value, sep = " ----> "))
}

# ---------------------------------------------------------------------------------------------
# calculate percentages for each alpha (0.01, 0.05, 0.10)...

# calculate alpha = 0.01 (low) percentage of states with difference...
low_percentage <- length(diff_states_low) / 50.0

# calculate alpha = 0.05 (mid) percentage of states with difference...
mid_percentage <- length(diff_states_mid) / 50.0

# calculate alpha = 0.10 (high) percentage of states with difference...
high_percentage <- length(diff_states_high) / 50.0

# print results...
print(paste("Percentage of US states with significant difference at alpha = 0.01:",
            low_percentage,
            sep = " "))

print(paste("Percentage of US states with significant difference at alpha = 0.05:",
            mid_percentage,
            sep = " "))

print(paste("Percentage of US states with significant difference at alpha = 0.10:",
            high_percentage,
            sep = " "))
