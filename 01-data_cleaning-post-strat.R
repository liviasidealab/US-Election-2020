#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/index.shtml
# Author: Huiyan Li; Wenyu Qu; Bingzhen Wan; Tongxin Zeng
# Data: Nov 2, 2020
# Contact: vialivia.ct@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to 304PS3 folder
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data.
setwd("/Users/liviali/Downloads/304PS3")
raw_data <- read_dta("usa_00002.dta")

#Add 2 to the age (because the data was collected in 2018)
raw_data <- raw_data %>% 
  mutate(age = age + 2) 

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest
reduced_data <- 
  raw_data %>% 
  select(age, 
         sex,
         empstat,
         race,
         citizen)

#vote cleaning
reduced_data <-
  reduced_data %>% 
  filter(age >= 18)

reduced_data <-
  reduced_data %>% 
  filter(citizen == "naturalized citizen") 

#age grouping and mapping
reduced_data$age <- as.numeric(reduced_data$age)
reduced_data<-
  reduced_data %>% 
  mutate(age_group = case_when(age <=  20 ~ '20 or less',
                               age > 20  & age <= 40 ~ '21 to 40',
                               age > 40  & age <= 60 ~ '41 to 60',
                               age > 60  & age <= 80 ~ '61 to 80',
                               age > 80 ~ 'above 80')) 

#sex mapping
reduced_data <- reduced_data %>%
  rename(gender = sex) %>% 
  mutate(gender = ifelse(gender == "male", "Male", "Female"))

#race mapping
reduced_data$race <- sub('other race, nec', 'some other race', reduced_data$race)
reduced_data$race <- sub('two major races', 'some other race', reduced_data$race) 
reduced_data$race <- sub('three or more major races', 'some other race', reduced_data$race)

#employment status mapping
reduced_data <- reduced_data %>%
  rename(employment = empstat)

#grouping
reduced_data <- 
  reduced_data %>%
  count(age, gender, employment, race) %>%
  group_by(age, gender, employment, race) 

#filter NAs 
reduced_data <- na.omit(reduced_data)
rm(raw_data)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")



         