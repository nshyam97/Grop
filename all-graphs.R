## Exploratory Data Analysis (Stack 4)

# Import libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
options(digits = 6)

# Set working directory
#setwd("~/Desktop/KAUTHAR/MSc Data Science/10. Group Project/")

# Read csv files
A = read.csv("A.csv")
B = read.csv("B.csv")
P = read.csv("Pow.csv")
P_demand = read.csv("P_demand.csv")

sum(is.na(A))
sum(is.na(B))
sum(is.na(P))
sum(is.na(P_demand))

# Omit null values in power file
P = na.omit(P)

############################################################

# Summarise Battery A by day
A_day_df <- A %>%
  group_by(day = date(A$Time)) %>%
  summarise_all(list(mean))

# Summarise Battery A by hour
A_hour_df <- A %>%
  group_by(day = date(A$Time), hour = hour(A$Time)) %>%
  summarise_all(list(mean))

# Summarise Battery B by day
B_day_df <- B %>%
  group_by(day = date(B$Time)) %>%
  summarise_all(list(mean))

# Summarise Battery B by hour
B_hour_df <- B %>%
  group_by(day = date(B$Time), hour = hour(B$Time)) %>%
  summarise_all(list(mean))

# Summarise Power by day
P_day_df <- P %>%
  group_by(day = date(P$Time)) %>%
  summarise_all(list(mean))

# Summarise Power by hour
P_hour_df <- P %>%
  group_by(day = date(P$Time), hour = hour(P$Time)) %>%
  summarise_all(list(mean))

############################################################

# Dates to monitor

# 24-25 July 2019
A2507 = A_hour_df[A_hour_df$day >= "2019-07-24" & A_hour_df$day <= "2019-07-25",]
B2507 = B_hour_df[B_hour_df$day >= "2019-07-24" & B_hour_df$day <= "2019-07-25",]
P2507 = P_hour_df[P_hour_df$day >= "2019-07-24" & P_hour_df$day <= "2019-07-25",]

# 9 Dec 2019
A0912 = A_hour_df[A_hour_df$day == "2019-12-09",]
B0912 = B_hour_df[B_hour_df$day == "2019-12-09",]
P0912 = P_hour_df[P_hour_df$day == "2019-12-09",]

# 12 Dec 2019
A1212 = A_hour_df[A_hour_df$day == "2019-12-12",]
B1212 = B_hour_df[B_hour_df$day == "2019-12-12",]
P1212 = P_hour_df[P_hour_df$day == "2019-12-12",]

# 9 Jan 2020
A0901 = A_hour_df[A_hour_df$day == "2020-01-09",]
B0901 = B_hour_df[B_hour_df$day == "2020-01-09",]
P0901 = P_hour_df[P_hour_df$day == "2020-01-09",]

############################################################

# Add index to plot
'A2507 = cbind(i = c(1:48), A2507)
B2507 = cbind(i = c(1:48), B2507)
P2507 = cbind(i = c(1:48), P2507)

A0912 = cbind(i = c(1:24), A0912)
B0912 = cbind(i = c(1:24), B0912)
P0912 = cbind(i = c(1:24), P0912)

A1212 = cbind(i = c(1:24), A1212)
B1212 = cbind(i = c(1:24), B1212)
P1212 = cbind(i = c(1:24), P1212)

A0901 = cbind(i = c(1:24), A0901)
B0901 = cbind(i = c(1:24), B0901)
P0901 = cbind(i = c(1:24), P0901)'

############################################################


# PLOTTING SOC AND TEMPS BY HOUR

# 1. 24-25 JUL 2019
# Select and combine data 
A2507_SOC = A2507 %>% 
            dplyr::select(i, day, hour, 
                          SOC_Batt_1a, SOC_Batt_1b,
                          SOC_Batt_2a, SOC_Batt_2b, 
                          SOC_Batt_3a, SOC_Batt_3b) %>%
            gather(key = "SOC", value = "value1", -c(i, day, hour))

B2507_tempMin = B2507 %>% 
                dplyr::select(TempMinBatt1a, TempMinBatt1b,
                              TempMinBatt2a, TempMinBatt2b, 
                              TempMinBatt3a, TempMinBatt3b) %>%
                gather(key = "MinTemp", value = "value2")

B2507_tempMax = B2507 %>% 
                dplyr::select(TempMaxBatt1a, TempMaxBatt1b, 
                              TempMaxBatt2a, TempMaxBatt2b,
                              TempMaxBatt3a, TempMaxBatt3b) %>%
                gather(key = "MaxTemp", value = "value3")

SOC_temp_2507 = as.data.frame(c(A2507_SOC, B2507_tempMin, B2507_tempMax))
head(SOC_temp_2507)

ggplot(data = SOC_temp_2507) +
  geom_line(aes(i, value1/1.76)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge (%)")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "SOC and Temp on 24-25 JUL 2019") + 
  facet_wrap(.~SOC, nrow = 2) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 


# 2. 9 DEC 2019

# Select and combine data 
A0912_SOC = A0912 %>% 
  dplyr::select(i, day, hour, 
                SOC_Batt_1a, SOC_Batt_1b,
                SOC_Batt_2a, SOC_Batt_2b, 
                SOC_Batt_3a, SOC_Batt_3b) %>%
  gather(key = "SOC", value = "value1", -c(i, day, hour))

B0912_tempMin = B0912 %>% 
  dplyr::select(TempMinBatt1a, TempMinBatt1b,
                TempMinBatt2a, TempMinBatt2b, 
                TempMinBatt3a, TempMinBatt3b) %>%
  gather(key = "MinTemp", value = "value2")

B0912_tempMax = B0912 %>% 
  dplyr::select(TempMaxBatt1a, TempMaxBatt1b, 
                TempMaxBatt2a, TempMaxBatt2b,
                TempMaxBatt3a, TempMaxBatt3b) %>%
  gather(key = "MaxTemp", value = "value3")

SOC_temp_0912 = as.data.frame(c(A0912_SOC, B0912_tempMin, B0912_tempMax))
head(SOC_temp_0912)

ggplot(data = SOC_temp_0912) +
  geom_line(aes(i, value1/1.76)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge (%)")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "SOC and Temp on 9 DEC 2019") + 
  facet_wrap(.~SOC, nrow = 2) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 


# 3. 12 DEC 2019

# Select and combine data 
A1212_SOC = A1212 %>% 
  dplyr::select(i, day, hour, 
                SOC_Batt_1a, SOC_Batt_1b,
                SOC_Batt_2a, SOC_Batt_2b, 
                SOC_Batt_3a, SOC_Batt_3b) %>%
  gather(key = "SOC", value = "value1", -c(i, day, hour))

B1212_tempMin = B1212 %>% 
  dplyr::select(TempMinBatt1a, TempMinBatt1b,
                TempMinBatt2a, TempMinBatt2b, 
                TempMinBatt3a, TempMinBatt3b) %>%
  gather(key = "MinTemp", value = "value2")

B1212_tempMax = B1212 %>% 
  dplyr::select(TempMaxBatt1a, TempMaxBatt1b, 
                TempMaxBatt2a, TempMaxBatt2b,
                TempMaxBatt3a, TempMaxBatt3b) %>%
  gather(key = "MaxTemp", value = "value3")

SOC_temp_1212 = as.data.frame(c(A1212_SOC, B1212_tempMin, B1212_tempMax))
head(SOC_temp_1212)

ggplot(data = SOC_temp_1212) +
  geom_line(aes(i, value1/1.76)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge (%)")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "SOC and Temp on 12 DEC 2019") + 
  facet_wrap(.~SOC, nrow = 2) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 


# 4. 9 JAN 2019

# Select and combine data 
A0901_SOC = A0901 %>% 
  dplyr::select(i, day, hour, 
                SOC_Batt_1a, SOC_Batt_1b,
                SOC_Batt_2a, SOC_Batt_2b, 
                SOC_Batt_3a, SOC_Batt_3b) %>%
  gather(key = "SOC", value = "value1", -c(i, day, hour))

B0901_tempMin = B0901 %>% 
  dplyr::select(TempMinBatt1a, TempMinBatt1b,
                TempMinBatt2a, TempMinBatt2b, 
                TempMinBatt3a, TempMinBatt3b) %>%
  gather(key = "MinTemp", value = "value2")

B0901_tempMax = B0901 %>% 
  dplyr::select(TempMaxBatt1a, TempMaxBatt1b, 
                TempMaxBatt2a, TempMaxBatt2b,
                TempMaxBatt3a, TempMaxBatt3b) %>%
  gather(key = "MaxTemp", value = "value3")

SOC_temp_0901 = as.data.frame(c(A0901_SOC, B0901_tempMin, B0901_tempMax))
head(SOC_temp_0901)

ggplot(data = SOC_temp_0901) +
  geom_line(aes(i, value1/1.76)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge (%)")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "SOC and Temp on 9 JAN 2020") + 
  facet_wrap(.~SOC, nrow = 2) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 

############################################################

# PLOTTING BUS VOLTS AND TEMPS BY HOUR

# 1. 24-25 JUL 2019
# Select and combine data 
A2507_V = A2507 %>% 
  dplyr::select(i, day, hour, 
                vtBusVoltsBatt1a, vtBusVoltsBatt1b,
                vtBusVoltsBatt2a, vtBusVoltsBatt2b,
                vtBusVoltsBatt3a, vtBusVoltsBatt3b) %>%
  gather(key = "Volts", value = "value1", -c(i, day, hour))

Volts_temp_2507 = as.data.frame(c(A2507_V, B2507_tempMin, B2507_tempMax))
head(Volts_temp_2507)

ggplot(data = Volts_temp_2507) +
  geom_line(aes(i, value1/15)) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts (V)")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Bus Volts and Temp on 24-25 JUL 2019") + 
  facet_wrap(.~Volts, nrow = 2) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 


# 2. 9 DEC 2019
# Select and combine data 
A0912_V = A0912 %>% 
  dplyr::select(i, day, hour, 
                vtBusVoltsBatt1a, vtBusVoltsBatt1b,
                vtBusVoltsBatt2a, vtBusVoltsBatt2b,
                vtBusVoltsBatt3a, vtBusVoltsBatt3b) %>%
  gather(key = "Volts", value = "value1", -c(i, day, hour))

Volts_temp_0912 = as.data.frame(c(A0912_V, B0912_tempMin, B0912_tempMax))
head(Volts_temp_0912)

ggplot(data = Volts_temp_0912) +
  geom_line(aes(i, value1/15)) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts (V)")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Bus Volts and Temp on 9 DEC 2019") + 
  facet_wrap(.~Volts, nrow = 2) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 


# 3. 12 DEC 2019
# Select and combine data 
A1212_V = A1212 %>% 
  dplyr::select(i, day, hour, 
                vtBusVoltsBatt1a, vtBusVoltsBatt1b,
                vtBusVoltsBatt2a, vtBusVoltsBatt2b,
                vtBusVoltsBatt3a, vtBusVoltsBatt3b) %>%
  gather(key = "Volts", value = "value1", -c(i, day, hour))

Volts_temp_1212 = as.data.frame(c(A1212_V, B1212_tempMin, B1212_tempMax))
head(Volts_temp_1212)

ggplot(data = Volts_temp_1212) +
  geom_line(aes(i, value1/15)) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts (V)")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Bus Volts and Temp on 12 DEC 2019") + 
  facet_wrap(.~Volts, nrow = 2) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 

# 4. 9 JAN 2019
# Select and combine data 
A0901_V = A0901 %>% 
  dplyr::select(i, day, hour, 
                vtBusVoltsBatt1a, vtBusVoltsBatt1b,
                vtBusVoltsBatt2a, vtBusVoltsBatt2b,
                vtBusVoltsBatt3a, vtBusVoltsBatt3b) %>%
  gather(key = "Volts", value = "value1", -c(i, day, hour))

Volts_temp_0901 = as.data.frame(c(A0901_V, B0901_tempMin, B0901_tempMax))
head(Volts_temp_0901)

ggplot(data = Volts_temp_0901) +
  geom_line(aes(i, value1/15)) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts (V)")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Bus Volts and Temp on 9 JAN 2020") + 
  facet_wrap(.~Volts, nrow = 2) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 

############################################################

# PLOTTING AVERAGE SOC AND TEMPS BY HOUR


Batt_names <- list(
  'TempMaxBatt1a'="1a",
  'TempMaxBatt1b'="1b",
  'TempMaxBatt2a'="2a",
  'TempMaxBatt2b'="2b",
  'TempMaxBatt3a'="3a",
  'TempMaxBatt3b'="3b"
)

batt_labeller <- function(variable,value){
  return(Batt_names[value])
}


# 1. 24-25 JUL 2019
Avg_SOC1 = P2507 %>% dplyr::select(i, day, hour, Average_Battery_SOC)

ASOC_2507 = as.data.frame(c(Avg_SOC1, B2507_tempMin, B2507_tempMax))
head(ASOC_2507)

ggplot(data = ASOC_2507) +
  geom_line(aes(i, Average_Battery_SOC/1.5)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Avg SOC and Temp on 24-25 JUL 2019") + 
  facet_wrap(.~MaxTemp, nrow = 2, labeller = batt_labeller) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 


# 2. 9 DEC 2019
Avg_SOC2 = P0912 %>% dplyr::select(i, day, hour, Average_Battery_SOC)

ASOC_0912 = as.data.frame(c(Avg_SOC2, B0912_tempMin, B0912_tempMax))
head(ASOC_0912)

ggplot(data = ASOC_0912) +
  geom_line(aes(i, Average_Battery_SOC/1.5)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Avg SOC and Temp on 9 DEC 2019") + 
  facet_wrap(.~MaxTemp, nrow = 2, labeller = batt_labeller) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 


# 3. 12 DEC 2019
Avg_SOC3 = P1212 %>% dplyr::select(i, day, hour, Average_Battery_SOC)

ASOC_1212 = as.data.frame(c(Avg_SOC3, B1212_tempMin, B1212_tempMax))
head(ASOC_1212)

ggplot(data = ASOC_1212) +
  geom_line(aes(i, Average_Battery_SOC/1.5)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Avg SOC and Temp on 12 DEC 2019") + 
  facet_wrap(.~MaxTemp, nrow = 2, labeller = batt_labeller) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 

# 4. 9 JAN 2019
Avg_SOC4 = P0901 %>% dplyr::select(i, day, hour, Average_Battery_SOC)

ASOC_0901 = as.data.frame(c(Avg_SOC4, B0901_tempMin, B0901_tempMax))
head(ASOC_0901)

ggplot(data = ASOC_0901) +
  geom_line(aes(i, Average_Battery_SOC/1.5)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Avg SOC and Temp on 9 JAN 2020") + 
  facet_wrap(.~MaxTemp, nrow = 2, labeller = batt_labeller) +
  geom_line(aes(i, value2, color="Min Temp")) +
  geom_line(aes(i, value3, color="Max Temp")) +
  theme(legend.position = "right") 


############################################################

# PLOTTING POWER FLOW AGAINST TIME

# 1. 24-25 JUL 2019
power_flow = P2507 %>% dplyr::select(i, day, hour, Channel_1_Power,
                                     Channel_2_Power, Channel_3_Power) %>%
    gather(key = "channel", value = "value", -c(i, day, hour))


ggplot(data = power_flow) +
  geom_line(aes(i, value)) +
  labs(x = "Time (hour)", y = "Power Flow", 
       title = "Power flow on 24-25 JUL 2019") + 
  facet_wrap(.~channel, nrow = 3) +
  theme(legend.position = "right") 

# 2. 9 DEC 2019
power_flow1 = P0912 %>% dplyr::select(i, day, hour, Channel_1_Power,
                                     Channel_2_Power, Channel_3_Power) %>%
  gather(key = "channel", value = "value", -c(i, day, hour))


ggplot(data = power_flow1) +
  geom_line(aes(i, value)) +
  labs(x = "Time (hour)", y = "Power Flow", 
       title = "Power flow on 9 DEC 2019") + 
  facet_wrap(.~channel, nrow = 3) +
  theme(legend.position = "right") 

# 3. 12 DEC 2019
power_flow2 = P1212 %>% dplyr::select(i, day, hour, Channel_1_Power,
                                      Channel_2_Power, Channel_3_Power) %>%
  gather(key = "channel", value = "value", -c(i, day, hour))


ggplot(data = power_flow2) +
  geom_line(aes(i, value)) +
  labs(x = "Time (hour)", y = "Power Flow", 
       title = "Power flow on 12 DEC 2019") + 
  facet_wrap(.~channel, nrow = 3) +
  theme(legend.position = "right") 

# 4. 9 JAN 2020
power_flow3 = P0901 %>% dplyr::select(i, day, hour, Channel_1_Power,
                                      Channel_2_Power, Channel_3_Power) %>%
  gather(key = "channel", value = "value", -c(i, day, hour))


ggplot(data = power_flow3) +
  geom_line(aes(i, value)) +
  labs(x = "Time (hour)", y = "Power Flow", 
       title = "Power flow on 9 JAN 2019") + 
  facet_wrap(.~channel, nrow = 3) +
  theme(legend.position = "right") 


