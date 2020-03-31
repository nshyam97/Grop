library(ggplot2)
library(scales)
options(digits = 6)

# Summarise Battery A by day
A_day_df <- A_df %>%
  group_by(day = date(A_df$Time)) %>%
  summarise_all(list(mean))

# Summarise Battery A by hour
A_hour_df <- A_df %>%
  group_by(day = date(A_df$Time), hour = hour(A_df$Time)) %>%
  summarise_all(list(mean))

# Summarise Battery B by day
B_day_df <- B_df %>%
  group_by(day = date(B_df$Time)) %>%
  summarise_all(list(mean))

# Summarise Battery B by hour
B_hour_df <- B_df %>%
  group_by(day = date(B_df$Time), hour = hour(B_df$Time)) %>%
  summarise_all(list(mean))

# Summarise Power by day
power_day_df <- Power_df %>%
  group_by(day = date(Power_df$Time)) %>%
  summarise_all(list(mean))

# Summarise Power by hour
power_hour_df <- Power_df %>%
  group_by(day = date(Power_df$Time), hour = hour(Power_df$Time)) %>%
  summarise_all(list(mean))

# Dates with significant spikes
dates_to_monitor = list(as.Date("2019-07-24"), 
                        as.Date("2019-07-25"),
                        as.Date("2019-12-09"), 
                        as.Date("2019-12-12"), 
                        as.Date("2020-01-09"))

# 24/25th July
A2507 = filter(A_hour_df, day == dates_to_monitor[1] | day == dates_to_monitor[2])
B2507 = filter(B_hour_df, day == dates_to_monitor[1] | day == dates_to_monitor[2])
P2507 = filter(power_hour_df, day == dates_to_monitor[1] | day == dates_to_monitor[2])

# 9th December
A0912 = filter(A_hour_df, day == dates_to_monitor[3])
B0912 = filter(B_hour_df, day == dates_to_monitor[3])
P0912 = filter(power_hour_df, day == dates_to_monitor[3])

# 12th December
A1212 = filter(A_hour_df, day == dates_to_monitor[4])
B1212 = filter(B_hour_df, day == dates_to_monitor[4])
P1212 = filter(power_hour_df, day == dates_to_monitor[4])

# 9th January
A0901 = filter(A_hour_df, day == dates_to_monitor[5])
B0901 = filter(B_hour_df, day == dates_to_monitor[5])
P0901 = filter(power_hour_df, day == dates_to_monitor[5])

# Plotting SOC and Temps 1a against hour
ggplot(data = A2507) +
  geom_line(aes(Time, SOC_Batt_1a/1.76, color="SOC-a")) +
  geom_line(data=B2507, aes(Time, TempMinBatt1a, color="Temp-Min")) +
  geom_line(data=B2507, aes(Time, TempMaxBatt1a, color="Temp-Max")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge"))

ggplot(data = A0912) +
  geom_line(aes(Time, SOC_Batt_1a/1.76, color="SOC-a")) +
  geom_line(data=B0912, aes(Time, TempMinBatt1a, color="Temp-Min-a")) +
  geom_line(data=B0912, aes(Time, TempMaxBatt1a, color="Temp-Max-a")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge"))

ggplot(data = A1212) +
  geom_line(aes(Time, SOC_Batt_1a/1.76, color="SOC-a")) +
  geom_line(data=B1212, aes(Time, TempMinBatt1a, color="Temp-Min-a")) +
  geom_line(data=B1212, aes(Time, TempMaxBatt1a, color="Temp-Max-a")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "State of Charge"))

ggplot(data = A0901) +
  geom_line(aes(Time, SOC_Batt_1a/1.76, color="Bus Volts")) +
  geom_line(data=B0901, aes(Time, TempMinBatt1a, color="Temp-Min-a")) +
  geom_line(data=B0901, aes(Time, TempMaxBatt1a, color="Temp-Max-a")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*1.76, name = "Bus Volts"))

# Plotting bus volts and Temps 1a against hour
ggplot(data = A2507) +
  geom_line(aes(Time, vtBusVoltsBatt1a/15, color="Bus Volts")) +
  geom_line(data=B2507, aes(Time, TempMinBatt1a, color="Temp-Min")) +
  geom_line(data=B2507, aes(Time, TempMaxBatt1a, color="Temp-Max")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts"))

ggplot(data = A0912) +
  geom_line(aes(Time, vtBusVoltsBatt1a/15, color="Bus Volts")) +
  geom_line(data=B0912, aes(Time, TempMinBatt1a, color="Temp-Min-a")) +
  geom_line(data=B0912, aes(Time, TempMaxBatt1a, color="Temp-Max-a")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts"))

ggplot(data = A1212) +
  geom_line(aes(Time, vtBusVoltsBatt1a/15, color="Bus Volts")) +
  geom_line(data=B1212, aes(Time, TempMinBatt1a, color="Temp-Min-a")) +
  geom_line(data=B1212, aes(Time, TempMaxBatt1a, color="Temp-Max-a")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts"))

ggplot(data = A0901) +
  geom_line(aes(Time, vtBusVoltsBatt1a/15, color="Bus Volts")) +
  geom_line(data=B0901, aes(Time, TempMinBatt1a, color="Temp-Min-a")) +
  geom_line(data=B0901, aes(Time, TempMaxBatt1a, color="Temp-Max-a")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Bus Volts"))

# Plotting average SOC and Temps 1a against hour
ggplot(data = P2507) +
  geom_line(aes(Time, Average_Battery_SOC/1.5, color="Avg SOC")) +
  geom_line(data=B2507, aes(Time, TempMinBatt1a, color="Temp-Min")) +
  geom_line(data=B2507, aes(Time, TempMaxBatt1a, color="Temp-Max")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC"))

ggplot(data = P0912) +
  geom_line(aes(Time, Average_Battery_SOC/1.5, color="Avg SOC")) +
  geom_line(data=B0912, aes(Time, TempMinBatt1a, color="Temp-Min-a")) +
  geom_line(data=B0912, aes(Time, TempMaxBatt1a, color="Temp-Max-a")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC"))

ggplot(data = P1212) +
  geom_line(aes(Time, Average_Battery_SOC/1.5, color="Avg SOC")) +
  geom_line(data=B1212, aes(Time, TempMinBatt1a, color="Temp-Min-a")) +
  geom_line(data=B1212, aes(Time, TempMaxBatt1a, color="Temp-Max-a")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC"))

ggplot(data = P0901) +
  geom_line(aes(Time, Average_Battery_SOC/1.5, color="Avg SOC")) +
  geom_line(data=B0901, aes(Time, TempMinBatt1a, color="Temp-Min-a")) +
  geom_line(data=B0901, aes(Time, TempMaxBatt1a, color="Temp-Max-a")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Avg SOC"))
