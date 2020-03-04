## Load the dataset
setwd("C:/Users/HP/OneDrive - Newcastle University/Group project")
library(magrittr)
# load the files
A= read.csv("code/A.csv")
B= read.csv("code/B.csv")
P= read.csv("code/Power.csv")

A$time = format(as.POSIXct(A$Time,format ="%d/%m/%Y %H:%M:%S"),"%H:%M:%S")
 
A$Date <- format(as.Date(A$Time, format ="%d/%m/%Y"), "%d/%m/%Y")

View(A)
