# Name: Ben Watto
# Date: October 10th, 2018

# Run the below only if the library is not already installed.
# install.packages(dslabs)

library(dslabs)
library(dplyr)
library(tidyverse)
data(us_contagious_diseases)

#Question 1.
dat <- filter(us_contagious_diseases, disease == "Measles", state != "Alaska" & state != "Hawaii")
dat <- mutate(dat, per100000rate = ((count*100000)/population)* (weeks_reporting/52))

#Question 2.
dat_cal <- filter(dat, disease == "Measles", state == "California")
ggplot(data = dat_cal, mapping = aes(x = year, y = per100000rate)) + geom_line() + geom_vline(xintercept = 1965)

#Question 3.
dat_caliFocus <- filter(us_contagious_diseases, state == "California")
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1950] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1960] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1970] <- "1970’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1980] <- "NA"
dat_caliFocus <- filter(dat_caliFocus, yearBlock != "NA")
ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))
ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))
# The 1950's seems to have more similar variability across years. I think this is because the data collected is much older and tends to be less reliable.


#Question 4.
dat_Focus <- filter(us_contagious_diseases)
dat_Focus$yearBlock[dat_Focus$year >= 1950] <- "1950’s"
dat_Focus$yearBlock[dat_Focus$year >= 1960] <- "1960’s"
dat_Focus$yearBlock[dat_Focus$year >= 1970] <- "1970’s"
dat_Focus$yearBlock[dat_Focus$year >= 1980] <- "NA"
dat_Focus <- filter(dat_Focus, yearBlock != "NA")
ggplot(data = dat_Focus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#Question 5.
dat_Focus <- filter(us_contagious_diseases)
dat_Focus$yearBlock[dat_Focus$year >= 1950] <- "1950’s"
dat_Focus$yearBlock[dat_Focus$year >= 1960] <- "1960’s"
dat_Focus$yearBlock[dat_Focus$year >= 1970] <- "1970’s"
dat_Focus$yearBlock[dat_Focus$year >= 1980] <- "NA"
dat_Focus <- filter(dat_Focus, yearBlock != "NA")
dat_Focus <- mutate(dat_Focus, per100000rate = ((count*100000)/population)* (weeks_reporting/52))
ggplot(data = dat_Focus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + geom_tile(mapping = aes(x = state, y = count, color = per100000rate)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#Question 6.





