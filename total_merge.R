library(lubridate)
library(tidyverse)
library(tsibble)
library(plyr)

Battery_A_files = list.files(path="Stack_1", pattern="*A_K2.csv", full.names=TRUE)
Battery_B_files = list.files(path="Stack_1", pattern="*B_K2.csv", full.names=TRUE)
Power_files = list.files(path="Stack_1", pattern="*POWER.csv", full.names=TRUE)

Battery_A <- lapply(Battery_A_files, read.csv)
Battery_B <- lapply(Battery_B_files, read.csv)
Power <- lapply(Power_files, read.csv)

Battery_A <- lapply(Battery_A, function(x) {
  x$Time <- dmy_hms(x$Time)
  x <- arrange(x, Time)
  x
})

Battery_B <- lapply(Battery_B, function(x) {
  x$Time <- dmy_hms(x$Time)
  x <- arrange(x, Time)
  x
})

Power <- lapply(Power, function(x) {
  x$Time <- dmy_hms(x$Time)
  x <- arrange(x, Time)
  x
})

Battery_A <- do.call(rbind, Battery_A)
Battery_A <- unique(Battery_A)
Battery_A <- Battery_A[,-1]

Battery_B <- do.call(rbind, Battery_B)
Battery_B <- unique(Battery_B)
Battery_B <- Battery_B[,-1]

Power <- do.call(rbind, Power)
Power <- unique(Power)
Power <- Battery_A[,-1]


