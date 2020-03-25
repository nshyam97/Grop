# Test file for loading data from csv and ensuring row orders 
# are time stamped

library(lubridate)
library(tidyverse)
library(tsibble)

# Load csv using "readr" package (part of tidyverse)
A_190715 <- read.csv('./190715_BATTERY_A_K2.csv')
A_190716 <- read.csv('./190716_BATTERY_A_K2.csv')

# view in window
view(A_190715)
view(A_190715)

# Check class
class(A_190715$Time)
class(A_190716$Time)

# Time column loaded as a factor
# Change the class of 'Time' data in columns 2 to 'datetime'
# format is day/month/year_hour_min_sec hence: dmy_hms
A_190715$Time <- dmy_hms(A_190715$Time)
A_190716$Time <- dmy_hms(A_190716$Time)

# Recheck class
class(A_190715$Time)
class(A_190716$Time)
#[1] "POSIXct" "POSIXt"

# Order the timestamps
A_190715=arrange(A_190715,Time)
A_190716=arrange(A_190716,Time)

# Remove 'seconds' value from timestamps. The 'seconds' value makes 
# the time series an irregular interval time series which is significantly
# more difficult to work with

A_190715$Time <- format(A_190715$Time, format="%d-%m-%Y %H:%M")
A_190716$Time <- format(A_190716$Time, format="%d-%m-%Y %H:%M")

# Reconvert the "Time" column to 'datetime' object as
# formatting converts it to a character

A_190715$Time <- dmy_hm(A_190715$Time)
A_190716$Time <- dmy_hm(A_190716$Time)

# Remove index columns
A_190715<-A_190715[,-1]
A_190716<-A_190716[,-1]


# Use union for removing duplicates since each file has an 
# overlap in the time sequences
Joined <- union(A_190715,A_190716) 

##########################################################
# Only do this step when you've joined all files. 
# Convert "Joined" object to tsibble object
Joined_tsbls <- Joined %>%
  as_tsibble(
    index=Time
  )
#########################################################

# simple ggplot line graph for time series 
Joined_tsbls %>%
  ggplot(aes(x = Time, y = SOC_Batt_1a)) +
  geom_line() +
  xlab("Time") + ylab("Voltage") 