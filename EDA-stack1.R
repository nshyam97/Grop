library(ggplot2)
library(scales)
options(digits = 6)

# Plot lines of all minimum voltages
ggplot(data = A_df) + geom_line(aes(Time, vtCellMinBatt1a, color='1a')) + 
  geom_line(aes(Time, vtCellMinBatt1b, color='1b')) +
  geom_line(aes(Time, vtCellMinBatt2a, color='2a')) +
  geom_line(aes(Time, vtCellMinBatt2b, color='2b')) +
  geom_line(aes(Time, vtCellMinBatt3a, color='3a')) +
  geom_line(aes(Time, vtCellMinBatt3b, color='3b')) +
  scale_x_datetime(labels = date_format("%H:%M:%S")) +
  theme(legend.position="right")

# Plot lines of all maximum voltages
ggplot(data = A_df) + geom_line(aes(day(A_df$Time), vtCellMaxBatt1a), color='steelblue') + 
  geom_line(aes(day(A_df$Time), vtCellMaxBatt1b), color='red') +
  geom_line(aes(day(A_df$Time), vtCellMaxBatt2a), color='yellow') +
  geom_line(aes(day(A_df$Time), vtCellMaxBatt2b), color='black') +
  geom_line(aes(day(A_df$Time), vtCellMaxBatt3a), color='orange') +
  geom_line(aes(day(A_df$Time), vtCellMaxBatt3b), color='green')

# Plot line for min and max of 1a
ggplot(data = A_df) + geom_line(aes(Time, vtCellMaxBatt1a, color='Max')) + 
  geom_line(aes(A_df$Time, vtCellMinBatt1a, color='Min')) +
  scale_x_datetime(labels = date_format("%H:%M:%S")) +
  theme(legend.position="right")

# Plot point and line of min/max 2a and soc 2a
ggplot(data = A_df) + geom_point(aes(A_df$Time, SOC_Batt_2a/12), alpha=0.2) +
  geom_line(aes(Time, vtCellMaxBatt2a, color='Max')) + 
  geom_line(aes(A_df$Time, vtCellMinBatt2a, color='Min')) +
  scale_x_datetime(labels = date_format("%H:%M:%S")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~.*12, name = "State of Charge"))

# Plot point and line of bus volts 2a and soc 2a
ggplot(data = A_df) + geom_point(aes(A_df$Time, SOC_Batt_2a*8), alpha=0.2) +
  geom_line(aes(Time, vtBusVoltsBatt2a, color='Max')) +
  scale_x_datetime(labels = date_format("%b")) +
  theme(legend.position="right") +
  scale_y_continuous(sec.axis = sec_axis(~./8, name = "State of Charge"))

# Summarise Battery A by day
day_df <- A_df %>%
  group_by(day = date(A_df$Time)) %>%
  summarise_all(list(mean))

# Plot minimum of Battery 1a per day
ggplot(data=day_df) + geom_line(aes(day, vtCellMinBatt1a))

