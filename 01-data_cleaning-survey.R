#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set
# Author: Huiyan Li; Wenyu Qu; Bingzhen Wan; Tongxin Zeng
# Data: Nov 2, 2020
# Contact: vialivia.ct@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the Nationscape Data Set and save the folder that you're interested in to 304PS3 folder
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data 
setwd("/Users/liviali/Downloads/304PS3")
raw_data <- read_dta("ns20200625/ns20200625.dta")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(vote_intention,
         registration,
         vote_2020,
         employment,
         gender,
         race_ethnicity,
         age)

#vote cleaning
reduced_data <-
  reduced_data %>% 
  filter(age >= 18)

reduced_data <-
  reduced_data %>% 
  filter(registration == "Registered"&
         vote_intention != "No, I am not eligible to vote" &
         vote_intention != "No, I will not vote but I am eligible" &
         (vote_2020 == "Donald Trump"| vote_2020 == "Joe Biden" | vote_2020 == "I am not sure/don't know"))

reduced_data <-
  reduced_data %>%
  filter(vote_2020 != "I would not vote") %>% 
  mutate(vote_trump = 
           ifelse(vote_2020 == "Donald Trump", 1, 0)) %>% 
  mutate(vote_biden = 
           ifelse(vote_2020 == "Joe Biden", 1, 0)) %>% 
  mutate(uncertain_vote = 
           ifelse(vote_2020 == "I am not sure/don't know", 1, 0))

#age grouping and mapping
reduced_data$age <- as.numeric(reduced_data$age)

reduced_data <-
  reduced_data %>% 
  mutate(age_group = case_when(age <=  20 ~ '20 or less',
                               age > 20  & age <= 40 ~ '21 to 40',
                               age > 40  & age <= 60 ~ '41 to 60',
                               age > 60  & age <= 80 ~ '61 to 80',
                               age > 80 ~ 'above 80')) 

#race mapping
reduced_data <- 
  reduced_data %>%
  rename(race = race_ethnicity)

reduced_data$race <- sub('White', 'white', reduced_data$race) 
reduced_data$race <- sub('Some other race', 'some other race', reduced_data$race) 
reduced_data$race <- sub('American Indian or Alaska Native', 'american indian or alaska native', reduced_data$race) 
reduced_data$race <- sub('Black, or African American', 'black/african american/negro', reduced_data$race) 
reduced_data$race <- sub('Asian \\(Chinese\\)', 'chinese', reduced_data$race)
reduced_data$race <- sub('Asian \\(Japanese\\)', 'japanese', reduced_data$race)
reduced_data$race <- sub('Asian \\(Asian Indian\\)', 'other asian or pacific islander', reduced_data$race)
reduced_data$race <- sub('Asian \\(Filipino\\)', 'other asian or pacific islander', reduced_data$race)
reduced_data$race <- sub('Asian \\(Korean\\)', 'other asian or pacific islander', reduced_data$race)
reduced_data$race <- sub('Asian \\(Vietnamese\\)', 'other asian or pacific islander', reduced_data$race)
reduced_data$race <- sub('Asian \\(Other\\)', 'other asian or pacific islander', reduced_data$race)
reduced_data$race <- sub('Pacific Islander \\(Native Hawaiian\\)', 'other asian or pacific islander', reduced_data$race)
reduced_data$race <- sub('Pacific Islander \\(Guamanian\\)', 'other asian or pacific islander', reduced_data$race)
reduced_data$race <- sub('Pacific Islander \\(Samoan\\)', 'other asian or pacific islander', reduced_data$race)
reduced_data$race <- sub('Pacific Islander \\(Other\\)', 'other asian or pacific islander', reduced_data$race)

#employment mapping
reduced_data$employment <- sub('Full-time employed', 'employed', reduced_data$employment)
reduced_data$employment <- sub('Homemaker', 'not in labor force', reduced_data$employment)
reduced_data$employment <- sub('Retired', 'not in labor force', reduced_data$employment)
reduced_data$employment <- sub('Unemployed or temporarily on layoff', 'unemployed', reduced_data$employment)
reduced_data$employment <- sub('Part-time employed', 'employed', reduced_data$employment)
reduced_data$employment <- sub('Permanently disabled', 'not in labor force', reduced_data$employment)
reduced_data$employment <- sub('Student', 'not in labor force', reduced_data$employment)
reduced_data$employment <- sub('Self-employed', 'employed', reduced_data$employment)
reduced_data$employment <- sub('Other', 'n/a', reduced_data$employment)

#filter NAs 
reduced_data <-na.omit(reduced_data)
rm(raw_data)
  
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")

