## Merging files 

# Import Libraries
library(readr)
library(plyr)
library(magrittr)
library(lubridate)   # work with dates

mydir = "Stack_1" # Change this to direct to your data directory

# List files 
A_K2 = list.files(path = mydir, pattern = "*A_K2.csv", full.names = TRUE)
B_K2 = list.files(path = mydir, pattern = "*B_K2.csv", full.names = TRUE)
Power = list.files(path = mydir, pattern = "*POWER.csv", full.names = TRUE)

# Read csv
A_df = A_K2 %>% ldply(read.csv) %>% unique() %>% mutate(Time = as.POSIXct(Time, format="%d/%m/%Y %H:%M:%S"))
B_df = B_K2 %>% ldply(read.csv) %>% unique() %>% mutate(Time = as.POSIXct(Time, format="%d/%m/%Y %H:%M:%S"))
Power_df = Power %>% ldply(read.csv) %>% unique() %>% mutate(Time = as.POSIXct(Time, format="%d/%m/%Y %H:%M:%S"))

# Save combine data as new csv
write.csv(A_df, "A.csv", row.names = FALSE)
write.csv(B_df, "B.csv", row.names = FALSE)
write.csv(Power_df, "Pow.csv", row.names = FALSE)
