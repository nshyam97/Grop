# Connected Energy Temperature Analysis

This repository stores our working files for our analysis on the data provided by Connected Energy.

The data-combining.R file contains the code that will help convert the multiple csv files into a single dataframe ordered by the timestamps provided and removing all duplicate values. The result of this file are separate csv files for Battery files A and B, Power and P demand. These will form the basis of the resulting EDA.

The other files found in this repository contain a combination of a number of approaches towards our EDA from different stacks. Each one produces hourly and daily dataframes from the original csv files and produces graphs for various variables comparing them to the temperature as well as focusing on specific dates (mentioned in the group report) that produced significant temperature spikes.