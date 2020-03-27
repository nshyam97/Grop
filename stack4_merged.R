## Stack 4 merging files 

# Import Libraries
library(tidyverse)
library(lubridate)   # work with dates

# Set working directory
setwd("~/Desktop/KAUTHAR/MSc Data Science/10. Group Project/")
mydir = "Dataset/Stack_4"

# List files 
A_K2 = list.files(path = mydir, pattern = "*A_K2.csv", full.names = TRUE)
B_K2 = list.files(path = mydir, pattern = "*B_K2.csv", full.names = TRUE)
Power = list.files(path = mydir, pattern = "*POWER.csv", full.names = TRUE)

# Read csv
A_df = A_K2 %>% ldply(read.csv) %>% unique()
B_df = B_K2 %>% ldply(read.csv) %>% unique()
Power_df = Power %>% ldply(read.csv) %>% unique()

# Change time format
A_df$Time = dmy_hms(A_df$Time)
B_df$Time = dmy_hms(B_df$Time)
Power_df$Time = dmy_hms(Power_df$Time)

# Save combine data as new csv
write.csv(A_df, "stack4_A.csv", row.names = FALSE)
write.csv(B_df, "stack4_B.csv", row.names = FALSE)
write.csv(Power_df, "stack4_Pow.csv", row.names = FALSE)

