# Name: Ben Watto
# Date: October 12th, 2018

# Run the below only if the library is not already installed.
# install.packages(dslabs)

library(dslabs)
library(dplyr)
library(tidyverse)
data(us_contagious_diseases)

#Question 1.
# creates the object dat and uses filter to find Measles without including Alaska and Hawaii
dat <- filter(us_contagious_diseases, disease == "Measles", state != "Alaska" & state != "Hawaii")
# function to calculate per100000rate
dat <- mutate(dat, per100000rate = ((count*100000)/population)* (weeks_reporting/52))

#Question 2.
# creates the object dat_cal and uses filter to find Measles in California
dat_cal <- filter(dat, disease == "Measles", state == "California")
# creates a graph with a vericle line at 1965
ggplot(data = dat_cal, mapping = aes(x = year, y = per100000rate)) + geom_line() + geom_vline(xintercept = 1965)

#Question 3.
# creates object to show us_contagious_diseases in California
dat_caliFocus <- filter(us_contagious_diseases, state == "California")
# Blocks all years besides 1950, 1960, 1970
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1950] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1960] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1970] <- "1970’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1980] <- "NA"
# Filters out all NA
dat_caliFocus <- filter(dat_caliFocus, yearBlock != "NA")
# creates a bar graph
ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))
# creates a bar graph and square roots count
ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))
# The 1970's seems to have the lowest rate of similar variability across years, while the 1950's has the most. This is most likely because the data from the 1950's is older and less reliable.


#Question 4.
# creates an object with us_contagious_diseases
dat_Focus <- filter(us_contagious_diseases)
# blocks all years besides 1950, 1960, 1970
dat_Focus$yearBlock[dat_Focus$year >= 1950] <- "1950’s"
dat_Focus$yearBlock[dat_Focus$year >= 1960] <- "1960’s"
dat_Focus$yearBlock[dat_Focus$year >= 1970] <- "1970’s"
dat_Focus$yearBlock[dat_Focus$year >= 1980] <- "NA"
# filters out all NA
dat_Focus <- filter(dat_Focus, yearBlock != "NA")
# plots a graph showing all the states
ggplot(data = dat_Focus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#Question 5.
# creates an object with us_contagious_diseases
dat_Focus <- filter(us_contagious_diseases)
# blocks all years besides 1950, 1960, 1970
dat_Focus$yearBlock[dat_Focus$year >= 1950] <- "1950’s"
dat_Focus$yearBlock[dat_Focus$year >= 1960] <- "1960’s"
dat_Focus$yearBlock[dat_Focus$year >= 1970] <- "1970’s"
dat_Focus$yearBlock[dat_Focus$year >= 1980] <- "NA"
# filters out all NA
dat_Focus <- filter(dat_Focus, yearBlock != "NA")
# function to calculate per100000rate
dat_Focus <- mutate(dat_Focus, per100000rate = ((count*100000)/population)* (weeks_reporting/52))
#plots a graph using geom_tile, set x = state, y = year, fill = rate, and color = per100000rate
ggplot(data = dat_Focus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + geom_tile(mapping = aes(x = state, y = count, color = per100000rate)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#Question 6.
autismData <- read.csv("~/cs301F2018/04-i-lab-cs301-fall-2018-wattob/autismData.csv", comment.char="#")
View(autismData)
ggplot(data = autismData) + geom_line(mapping = aes(x = Year.., y = Net.Growth))
#The plot created shows an increase in autism from 1985 onward, but this does not relate to vaccine use which started in the 1960's. In this situation autism appears to increase from the improvements to medicine as well as the lenient diagnoses for autism. The increase in autism does not coincide with the introduction of vaccines.

