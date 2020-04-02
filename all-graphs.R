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

A$Time= ymd_hms(A$Time)
B$Time= ymd_hms(B$Time)
P$Time= ymd_hms(P$Time)
P_demand$Time= ymd_hms(P_demand$Time)

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
# Dates with significant spikes
dates_to_monitor = list(as.Date("2019-07-24"), 
                        as.Date("2019-07-25"),
                        as.Date("2019-12-09"), 
                        as.Date("2019-12-12"), 
                        as.Date("2020-01-09"),
                        as.Date("2020-02-18"), 
                        as.Date("2020-02-19"))

# 24/25th July
A2507 = filter(A_hour_df, day == dates_to_monitor[1] | day == dates_to_monitor[2])
B2507 = filter(B_hour_df, day == dates_to_monitor[1] | day == dates_to_monitor[2])
P2507 = filter(P_hour_df, day == dates_to_monitor[1] | day == dates_to_monitor[2])

# 9th December
A0912 = filter(A_hour_df, day == dates_to_monitor[3])
B0912 = filter(B_hour_df, day == dates_to_monitor[3])
P0912 = filter(P_hour_df, day == dates_to_monitor[3])

# 12th December
A1212 = filter(A_hour_df, day == dates_to_monitor[4])
B1212 = filter(B_hour_df, day == dates_to_monitor[4])
P1212 = filter(P_hour_df, day == dates_to_monitor[4])

# 9th January
A0901 = filter(A_hour_df, day == dates_to_monitor[5])
B0901 = filter(B_hour_df, day == dates_to_monitor[5])
P0901 = filter(P_hour_df, day == dates_to_monitor[5])

# 18th/19th February
A1802 = filter(A_hour_df, day == dates_to_monitor[6] | day == dates_to_monitor[7])
B1802 = filter(B_hour_df, day == dates_to_monitor[6] | day == dates_to_monitor[7])
P1802 = filter(P_hour_df, day == dates_to_monitor[6] | day == dates_to_monitor[7])
###################################################################################

# PLOTTING SOC AND TEMPS BY HOUR

# 1. 24-25 JUL 2019
# Select and combine data 
A2507_SOC = A2507 %>% 
            dplyr::select(Time,day, hour, 
                          SOC_Batt_1a, SOC_Batt_1b,
                          SOC_Batt_2a, SOC_Batt_2b, 
                          SOC_Batt_3a, SOC_Batt_3b) %>%
            gather(key = "SOC", value = "value1", -c(Time,day, hour))

B2507_tempMin = B2507 %>% 
                dplyr::select(day,hour,
                              TempMinBatt1a, TempMinBatt1b,
                              TempMinBatt2a, TempMinBatt2b, 
                              TempMinBatt3a, TempMinBatt3b) %>%
                gather(key = "MinTemp", value = "value2",-c(day, hour))

B2507_tempMax = B2507 %>% 
                dplyr::select(day, hour,
                              TempMaxBatt1a, TempMaxBatt1b, 
                              TempMaxBatt2a, TempMaxBatt2b,
                              TempMaxBatt3a, TempMaxBatt3b) %>%
                gather(key = "MaxTemp", value = "value3",-c(day, hour))

SOC_temp_2507 = as.data.frame(c(A2507_SOC, B2507_tempMin, B2507_tempMax))
head(SOC_temp_2507)

# Xlimits
xlim_0=ymd_hms("2019-07-24 00:00:00")
xlim_end=ymd_hms("2019-07-26 00:00:00")

ggplot(data = SOC_temp_2507) +
  geom_line(aes(Time, value1/1.76)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge (%)")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "SOC and Temperature on 24-25 JUL 2019") + 
  facet_wrap(.~SOC, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0,xlim_end),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 


# 2. 9 DEC 2019

# Select and combine data 
A0912_SOC = A0912 %>% 
  dplyr::select(Time, day, hour, 
                SOC_Batt_1a, SOC_Batt_1b,
                SOC_Batt_2a, SOC_Batt_2b, 
                SOC_Batt_3a, SOC_Batt_3b) %>%
  gather(key = "SOC", value = "value1", -c(Time, day, hour))

B0912_tempMin = B0912 %>% 
  dplyr::select(day,hour,
                TempMinBatt1a, TempMinBatt1b,
                TempMinBatt2a, TempMinBatt2b, 
                TempMinBatt3a, TempMinBatt3b) %>%
  gather(key = "MinTemp", value = "value2", -c(day, hour))

B0912_tempMax = B0912 %>% 
  dplyr::select(day,hour,
                TempMaxBatt1a, TempMaxBatt1b, 
                TempMaxBatt2a, TempMaxBatt2b,
                TempMaxBatt3a, TempMaxBatt3b) %>%
  gather(key = "MaxTemp", value = "value3", -c(day, hour))

SOC_temp_0912 = as.data.frame(c(A0912_SOC, B0912_tempMin, B0912_tempMax))
head(SOC_temp_0912)

# Xlimits
xlim_0_2=ymd_hms("2019-12-09 00:00:00")
xlim_end_2=ymd_hms("2019-12-10 00:00:00")

ggplot(data = SOC_temp_0912) +
  geom_line(aes(Time, value1/1.76)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge (%)")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "SOC and Temp on 9 DEC 2019") + 
  facet_wrap(.~SOC, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_2,xlim_end_2),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 

# 3. 12 DEC 2019

# Select and combine data 
A1212_SOC = A1212 %>% 
  dplyr::select(Time, day, hour, 
                SOC_Batt_1a, SOC_Batt_1b,
                SOC_Batt_2a, SOC_Batt_2b, 
                SOC_Batt_3a, SOC_Batt_3b) %>%
  gather(key = "SOC", value = "value1", -c(Time, day, hour))

B1212_tempMin = B1212 %>% 
  dplyr::select(day, hour,
                TempMinBatt1a, TempMinBatt1b,
                TempMinBatt2a, TempMinBatt2b, 
                TempMinBatt3a, TempMinBatt3b) %>%
  gather(key = "MinTemp", value = "value2", -c(day, hour))

B1212_tempMax = B1212 %>% 
  dplyr::select(day, hour,
                TempMaxBatt1a, TempMaxBatt1b, 
                TempMaxBatt2a, TempMaxBatt2b,
                TempMaxBatt3a, TempMaxBatt3b) %>%
  gather(key = "MaxTemp", value = "value3", -c(day, hour))

SOC_temp_1212 = as.data.frame(c(A1212_SOC, B1212_tempMin, B1212_tempMax))
head(SOC_temp_1212)

# Xlimits
xlim_0_3=ymd_hms("2019-12-12 00:00:00")
xlim_end_3=ymd_hms("2019-12-13 00:00:00")

ggplot(data = SOC_temp_1212) +
  geom_line(aes(Time, value1/1.76)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge (%)")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "SOC and Temp on 12 DEC 2019") + 
  facet_wrap(.~SOC, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_3,xlim_end_3),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 


# 4. 9 JAN 2019

# Select and combine data 
A0901_SOC = A0901 %>% 
  dplyr::select(Time, day, hour, 
                SOC_Batt_1a, SOC_Batt_1b,
                SOC_Batt_2a, SOC_Batt_2b, 
                SOC_Batt_3a, SOC_Batt_3b) %>%
  gather(key = "SOC", value = "value1", -c(Time, day, hour))

B0901_tempMin = B0901 %>% 
  dplyr::select(day, hour,
                TempMinBatt1a, TempMinBatt1b,
                TempMinBatt2a, TempMinBatt2b, 
                TempMinBatt3a, TempMinBatt3b) %>%
  gather(key = "MinTemp", value = "value2",-c(day, hour))

B0901_tempMax = B0901 %>% 
  dplyr::select(day, hour,
                TempMaxBatt1a, TempMaxBatt1b, 
                TempMaxBatt2a, TempMaxBatt2b,
                TempMaxBatt3a, TempMaxBatt3b) %>%
  gather(key = "MaxTemp", value = "value3",-c(day, hour))

SOC_temp_0901 = as.data.frame(c(A0901_SOC, B0901_tempMin, B0901_tempMax))
head(SOC_temp_0901)

# Xlimits
xlim_0_4=ymd_hms("2020-01-09 00:00:00")
xlim_end_4=ymd_hms("2020-01-10 00:00:00")

ggplot(data = SOC_temp_0901) +
  geom_line(aes(Time, value1/1.76)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge (%)")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "SOC and Temp on 9 JAN 2020") + 
  facet_wrap(.~SOC, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_4,xlim_end_4),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 


# 5. 24-25 JUL 2019
# Select and combine data 
A1802_SOC = A1802 %>% 
  dplyr::select(Time,day, hour, 
                SOC_Batt_1a, SOC_Batt_1b,
                SOC_Batt_2a, SOC_Batt_2b, 
                SOC_Batt_3a, SOC_Batt_3b) %>%
  gather(key = "SOC", value = "value1", -c(Time,day, hour))

B1802_tempMin = B1802 %>% 
  dplyr::select(day,hour,
                TempMinBatt1a, TempMinBatt1b,
                TempMinBatt2a, TempMinBatt2b, 
                TempMinBatt3a, TempMinBatt3b) %>%
  gather(key = "MinTemp", value = "value2",-c(day, hour))

B1802_tempMax = B1802 %>% 
  dplyr::select(day, hour,
                TempMaxBatt1a, TempMaxBatt1b, 
                TempMaxBatt2a, TempMaxBatt2b,
                TempMaxBatt3a, TempMaxBatt3b) %>%
  gather(key = "MaxTemp", value = "value3",-c(day, hour))

SOC_temp_1802 = as.data.frame(c(A1802_SOC, B1802_tempMin, B1802_tempMax))
head(SOC_temp_1802)

# Xlimits
xlim_0_5=ymd_hms("2020-02-18 00:00:00")
xlim_end_5=ymd_hms("2020-02-20 00:00:00")

ggplot(data = SOC_temp_1802) +
  geom_line(aes(Time, value1/1.76)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge (%)")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "SOC and Temperature on 18-19 FEB 2020") + 
  facet_wrap(.~SOC, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_5,xlim_end_5),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 

############################################################

# PLOTTING BUS VOLTS AND TEMPS BY HOUR

# 1. 24-25 JUL 2019
# Select and combine data 
A2507_V = A2507 %>% 
  dplyr::select(Time, day, hour, 
                vtBusVoltsBatt1a, vtBusVoltsBatt1b,
                vtBusVoltsBatt2a, vtBusVoltsBatt2b,
                vtBusVoltsBatt3a, vtBusVoltsBatt3b) %>%
  gather(key = "Volts", value = "value1", -c(Time, day, hour))

Volts_temp_2507 = as.data.frame(c(A2507_V, B2507_tempMin, B2507_tempMax))
head(Volts_temp_2507)

ggplot(data = Volts_temp_2507) +
  geom_line(aes(Time, value1/15)) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts (V)")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "Bus Volts and Temp on 24-25 JUL 2019") + 
  facet_wrap(.~Volts, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0,xlim_end),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 


# 2. 9 DEC 2019
# Select and combine data 
A0912_V = A0912 %>% 
  dplyr::select(Time, day, hour, 
                vtBusVoltsBatt1a, vtBusVoltsBatt1b,
                vtBusVoltsBatt2a, vtBusVoltsBatt2b,
                vtBusVoltsBatt3a, vtBusVoltsBatt3b) %>%
  gather(key = "Volts", value = "value1", -c(Time, day, hour))

Volts_temp_0912 = as.data.frame(c(A0912_V, B0912_tempMin, B0912_tempMax))
head(Volts_temp_0912)

ggplot(data = Volts_temp_0912) +
  geom_line(aes(Time, value1/15)) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts (V)")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "Bus Volts and Temp on 9 DEC 2019") + 
  facet_wrap(.~Volts, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_2,xlim_end_2),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 



# 3. 12 DEC 2019
# Select and combine data 
A1212_V = A1212 %>% 
  dplyr::select(Time, day, hour, 
                vtBusVoltsBatt1a, vtBusVoltsBatt1b,
                vtBusVoltsBatt2a, vtBusVoltsBatt2b,
                vtBusVoltsBatt3a, vtBusVoltsBatt3b) %>%
  gather(key = "Volts", value = "value1", -c(Time, day, hour))

Volts_temp_1212 = as.data.frame(c(A1212_V, B1212_tempMin, B1212_tempMax))
head(Volts_temp_1212)

ggplot(data = Volts_temp_1212) +
  geom_line(aes(Time, value1/15)) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts (V)")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "Bus Volts and Temp on 12 DEC 2019") + 
  facet_wrap(.~Volts, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_3,xlim_end_3),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines"))  

# 4. 9 JAN 2019
# Select and combine data 
A0901_V = A0901 %>% 
  dplyr::select(Time, day, hour, 
                vtBusVoltsBatt1a, vtBusVoltsBatt1b,
                vtBusVoltsBatt2a, vtBusVoltsBatt2b,
                vtBusVoltsBatt3a, vtBusVoltsBatt3b) %>%
  gather(key = "Volts", value = "value1", -c(Time, day, hour))

Volts_temp_0901 = as.data.frame(c(A0901_V, B0901_tempMin, B0901_tempMax))
head(Volts_temp_0901)

ggplot(data = Volts_temp_0901) +
  geom_line(aes(Time, value1/15)) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts (V)")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Bus Volts and Temp on 9 JAN 2020") + 
  facet_wrap(.~Volts, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_4,xlim_end_4),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 

# 5. 18-19 FEB 2020
# Select and combine data 
A1802_V = A1802 %>% 
  dplyr::select(Time, day, hour, 
                vtBusVoltsBatt1a, vtBusVoltsBatt1b,
                vtBusVoltsBatt2a, vtBusVoltsBatt2b,
                vtBusVoltsBatt3a, vtBusVoltsBatt3b) %>%
  gather(key = "Volts", value = "value1", -c(Time, day, hour))

Volts_temp_1802 = as.data.frame(c(A1802_V, B1802_tempMin, B1802_tempMax))
head(Volts_temp_2507)

ggplot(data = Volts_temp_1802) +
  geom_line(aes(Time, value1/15)) +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts (V)")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "Bus Volts and Temp on 18-19 FEB 2020") + 
  facet_wrap(.~Volts, nrow = 2) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_5,xlim_end_5),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 

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
Avg_SOC1 = P2507 %>% dplyr::select(Time, day, hour, Average_Battery_SOC)

ASOC_2507 = as.data.frame(c(Avg_SOC1, B2507_tempMin, B2507_tempMax))
head(ASOC_2507)

ggplot(data = ASOC_2507) +
  geom_line(aes(Time, Average_Battery_SOC/1.5)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "Avg SOC and Temp on 24-25 JUL 2019") + 
  facet_wrap(.~MaxTemp, nrow = 2, labeller = batt_labeller) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0,xlim_end),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 


# 2. 9 DEC 2019
Avg_SOC2 = P0912 %>% dplyr::select(Time, day, hour, Average_Battery_SOC)

ASOC_0912 = as.data.frame(c(Avg_SOC2, B0912_tempMin, B0912_tempMax))
head(ASOC_0912)

ggplot(data = ASOC_0912) +
  geom_line(aes(Time, Average_Battery_SOC/1.5)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "Avg SOC and Temp on 9 DEC 2019") + 
  facet_wrap(.~MaxTemp, nrow = 2, labeller = batt_labeller) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_2,xlim_end_2),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 


# 3. 12 DEC 2019
Avg_SOC3 = P1212 %>% dplyr::select(Time, day, hour, Average_Battery_SOC)

ASOC_1212 = as.data.frame(c(Avg_SOC3, B1212_tempMin, B1212_tempMax))
head(ASOC_1212)

ggplot(data = ASOC_1212) +
  geom_line(aes(Time, Average_Battery_SOC/1.5)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "Avg SOC and Temp on 12 DEC 2019") + 
  facet_wrap(.~MaxTemp, nrow = 2, labeller = batt_labeller) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_3,xlim_end_3),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines"))  

# 4. 9 JAN 2019
Avg_SOC4 = P0901 %>% dplyr::select(Time, day, hour, Average_Battery_SOC)

ASOC_0901 = as.data.frame(c(Avg_SOC4, B0901_tempMin, B0901_tempMax))
head(ASOC_0901)

ggplot(data = ASOC_0901) +
  geom_line(aes(Time, Average_Battery_SOC/1.5)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC")) +
  labs(x = "Time (hour)", y = "Temperature (C)", 
       title = "Avg SOC and Temp on 9 JAN 2020") + 
  facet_wrap(.~MaxTemp, nrow = 2, labeller = batt_labeller) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_4,xlim_end_4),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines"))   

# 5. 18-19 FEB 2020
Avg_SOC5 = P1802 %>% dplyr::select(Time, day, hour, Average_Battery_SOC)

ASOC_1802 = as.data.frame(c(Avg_SOC5, B1802_tempMin, B1802_tempMax))
head(ASOC_1802)

ggplot(data = ASOC_1802) +
  geom_line(aes(Time, Average_Battery_SOC/1.5)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC")) +
  labs(x = "Time of Day (hour)", y = "Temperature (C)", 
       title = "Avg SOC and Temp on 18-19 FEB 2020") + 
  facet_wrap(.~MaxTemp, nrow = 2, labeller = batt_labeller) +
  geom_line(aes(Time, value2, color="Min Temp")) +
  geom_line(aes(Time, value3, color="Max Temp")) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_5,xlim_end_5),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 

############################################################

# PLOTTING POWER FLOW AGAINST TIME

# 1. 24-25 JUL 2019
power_flow = P2507 %>% dplyr::select(Time, day, hour, Channel_1_Power,
                                     Channel_2_Power, Channel_3_Power) %>%
    gather(key = "channel", value = "value", -c(Time, day, hour))


ggplot(data = power_flow) +
  geom_line(aes(Time, value)) +
  labs(x = "Time of Day (hour)", y = "Power Flow", 
       title = "Power flow on 24-25 JUL 2019") + 
  facet_wrap(.~channel, nrow = 3) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0,xlim_end),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 

# 2. 9 DEC 2019
power_flow1 = P0912 %>% dplyr::select(Time, day, hour, Channel_1_Power,
                                     Channel_2_Power, Channel_3_Power) %>%
  gather(key = "channel", value = "value", -c(Time, day, hour))


ggplot(data = power_flow1) +
  geom_line(aes(Time, value)) +
  labs(x = "Time (hour)", y = "Power Flow", 
       title = "Power flow on 9 DEC 2019") + 
  facet_wrap(.~channel, nrow = 3) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_2,xlim_end_2),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines"))  

# 3. 12 DEC 2019
power_flow2 = P1212 %>% dplyr::select(Time, day, hour, Channel_1_Power,
                                      Channel_2_Power, Channel_3_Power) %>%
  gather(key = "channel", value = "value", -c(Time, day, hour))


ggplot(data = power_flow2) +
  geom_line(aes(Time, value)) +
  labs(x = "Time of Day (hour)", y = "Power Flow", 
       title = "Power flow on 12 DEC 2019") + 
  facet_wrap(.~channel, nrow = 3) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_3,xlim_end_3),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines"))  

# 4. 9 JAN 2020
power_flow3 = P0901 %>% dplyr::select(Time, day, hour, Channel_1_Power,
                                      Channel_2_Power, Channel_3_Power) %>%
  gather(key = "channel", value = "value", -c(Time, day, hour))


ggplot(data = power_flow3) +
  geom_line(aes(Time, value)) +
  labs(x = "Time (hour)", y = "Power Flow", 
       title = "Power flow on 9 JAN 2019") + 
  facet_wrap(.~channel, nrow = 3) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_4,xlim_end_4),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines"))

# 5. 18-19 FEB 2020
power_flow5 = P1802 %>% dplyr::select(Time, day, hour, Channel_1_Power,
                                     Channel_2_Power, Channel_3_Power) %>%
  gather(key = "channel", value = "value", -c(Time, day, hour))


ggplot(data = power_flow5) +
  geom_line(aes(Time, value)) +
  labs(x = "Time of Day (hour)", y = "Power Flow", 
       title = "Power flow on 18-19 FEB 2020") + 
  facet_wrap(.~channel, nrow = 3) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%a-%d\n%H:%M")+
  coord_cartesian(xlim = c(xlim_0_5,xlim_end_5),expand=FALSE)+
  guides(color=guide_legend(title="Temperature"))+
  theme(legend.position = "right",panel.spacing.x =unit(2.5,"lines")) 
########################################################################
ggplot(data = B) +
  geom_line(aes(Time, TempMaxBatt1a/12, color="Temp-Max-1a")) +
  geom_line(aes(Time, TempMaxBatt1b/12, color="Temp-Max-1b")) +
  geom_line(aes(Time, TempMaxBatt2a/12, color="Temp-Max-2a")) +
  geom_line(aes(Time, TempMaxBatt2b/12, color="Temp-Max-2b")) +
  geom_line(aes(Time, TempMaxBatt3a/12, color="Temp-Max-3a")) +
  geom_line(aes(Time, TempMaxBatt3b/12, color="Temp-Max-3b")) +
  geom_line(aes(Time, AIRCON_REQUEST_NODE_1, color="Node 1")) +
  geom_line(aes(Time, AIRCON_REQUEST_NODE_2, color="Node 2")) +
  geom_line(aes(Time, AIRCON_REQUEST_NODE_3, color="Node 3")) +
  geom_line(aes(Time, AIRCON_REQUEST_NODE_4, color="Node 4")) +
  geom_line(aes(Time, AIRCON_REQUEST_NODE_5, color="Node 5")) +
  geom_line(aes(Time, AIRCON_REQUEST_NODE_6, color="Node 6")) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Aircon Request (1 = Request)") +
  scale_x_datetime(breaks = "2 weeks", labels = date_format("%d-%m-%y")) +
  scale_y_continuous(sec.axis = sec_axis(~.*12, name = "Temperature (°C)"))
