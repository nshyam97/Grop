# Load important libraries
library(readr)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)     # work with plots
library(lubridate)   # work with dates


# Set working directory
setwd("~/Desktop/KAUTHAR/MSc Data Science/10. Group Project/")
mydir = "Stack_4"

# Import csv files for battery power
batt_power_file = list.files(path = mydir, pattern = "*POWER.csv", full.names = TRUE)
batt_power_file
length(batt_power_file)
dat_pow = ldply(batt_power_file, read_csv)

# Import csv files for Battery A
batt_A_file = list.files(path = mydir, pattern = "*_BATTERY_A_K2.csv", full.names = TRUE)
batt_A_file
length(batt_A_file)
dat_A = ldply(batt_A_file, read_csv)

# Import csv files for Battery B
batt_B_file = list.files(path = mydir, pattern = "*_BATTERY_B_K2.csv", full.names = TRUE)
batt_B_file
length(batt_B_file)
dat_B = ldply(batt_B_file, read_csv)


# Change date time format
dat_A$Time = dmy_hms(dat_A$Time)
class(dat_A$Time)

dat_B$Time = dmy_hms(dat_B$Time)
class(dat_B$Time)

dat_pow$Time = dmy_hms(dat_pow$Time)
class(dat_pow$Time)


# Save combine data as new csv
write.csv(dat_A, "stack4_Battery_A.csv", row.names = FALSE)
write.csv(dat_B, "stack4_Battery_B.csv", row.names = FALSE)
write.csv(dat_pow, "stack4_Power.csv", row.names = FALSE)


# System data cleaning

mydir2 = "system_data"

system_dat_file = list.files(path = mydir2, pattern = "*.CSV", full.names = TRUE)
length(system_dat_file)
sys_dat = ldply(system_dat_file, read_csv)

head(sys_dat)
nrow(sys_dat)
class(sys_dat$Date)
class(sys_dat$Time)

write.csv(sys_dat, "system_data_merged.csv", row.names = FALSE)
