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

# calculate average stringency_index
CA_avg_stringency_index <- mean(CA$stringency_index)

# create separate data.tables based on a comparison to average_stringency_index...
CA_lenient <- CA[stringency_index <= CA_avg_stringency_index]
CA_strict <- CA[stringency_index > CA_avg_stringency_index]

# calculate average of daily_cases for the two data.tables...
CA_lenient_avg_daily_cases <- mean(CA_lenient$daily_cases)
CA_strict_avg_daily_cases <- mean(CA_strict$daily_cases)

# hypothesis test...
t.test(CA_lenient$daily_cases, CA_strict$daily_cases)
