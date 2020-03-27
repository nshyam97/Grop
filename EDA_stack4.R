## Exploratory Data Analysis (Stack 4)

# Import libraries
library(tidyverse)
library(ggplot2)

# Set working directory
setwd("~/Desktop/KAUTHAR/MSc Data Science/10. Group Project/")

# Read csv files
A = read.csv("stack4_A.csv")
B = read.csv("stack4_B.csv")
P = read.csv("stack4_Pow.csv")

############################################################

# Looking through B dataset

# Check if there is a null value
# There are null values in power file (1288 null)
sum(is.na(A))
sum(is.na(B))
sum(is.na(P))

# Omit null values in power file
P = na.omit(P)

# Summaries of each dataset
summary(A)
summary(B)
summary(P)

# Combine TempMaxBatt for all batteries
B_df = B %>% select(Time, TempMaxBatt1a, TempMaxBatt1b, TempMaxBatt2a,
                    TempMaxBatt2b, TempMaxBatt3a, TempMaxBatt3b) %>%
       gather(key = "Batteries", value = "value", -Time)

# Plot boxplot
ggplot(data = B_df, aes(x = Batteries, y = value, color = Batteries)) + 
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2") + 
  labs(title = 'Maximum Temperature (C)',
       y = 'Temperature (C)', x = 'Battery Type') +
  scale_x_discrete(labels = c("1a", "1b", "2a", "2b", "3a", "3b"))

