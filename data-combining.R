## Merging files 

# Import Libraries
#library(readr)
library(plyr)
library(tidyverse)
library(lubridate)   # work with dates

mydir = "Stack_2" # Change this to direct to your data directory

# List files 
A_K2 = list.files(path = mydir, pattern = "*A_K2.csv", full.names = TRUE)
B_K2 = list.files(path = mydir, pattern = "*B_K2.csv", full.names = TRUE)
Power = list.files(path = mydir, pattern = "*POWER.csv", full.names = TRUE)

# Read csv
A_df = A_K2 %>% ldply(read.csv) %>% unique() %>% mutate(Time = as.POSIXct(Time, format="%d/%m/%Y %H:%M:%S"))
B_df = B_K2 %>% ldply(read.csv) %>% unique() %>% mutate(Time = as.POSIXct(Time, format="%d/%m/%Y %H:%M:%S"))
Power_df = Power %>% ldply(read.csv) %>% unique() %>% mutate(Time = as.POSIXct(Time, format="%d/%m/%Y %H:%M:%S"))

mydir="system_data" # Directory for system data

# List system data files
P_demand = list.files(path = mydir, pattern = "*.CSV", full.names = TRUE)

P_demand = P_demand %>% 
  ldply(read.csv) %>% 
  mutate(timestamp= paste(Date,Time))%>%
  mutate(timestamp= as.POSIXct(timestamp, format="%Y/%m/%d %H:%M:%S"))

# Remove columns with legacy data
P_demand = P_demand[,c(3:12,21)]

# Reorder Columns so "timestamp" is now first
P_demand = P_demand[ , c(ncol(P_demand), 1:(ncol(P_demand)-1))]

# rename "timestamp" to "Time"
names(P_demand)[1] <- "Time"

# Save combine data as new csv
write.csv(A_df, "A.csv", row.names = FALSE)
write.csv(B_df, "B.csv", row.names = FALSE)
write.csv(Power_df, "Pow.csv", row.names = FALSE)