# Filename..: 4.2_descriptin_CMR
# Study.....: ODIN
# Author....: Taalke Maria Sitter
# Date......: 10/09/2026
# Updated...: 
# Purpose...: Calculating Crude Mortality Rates 
# Note......: 
# Data used.: Validation_file.csv


# Opening library
library(tidyverse)
library(dplyr)
library(Matrix)
library(splines)


############################
# Read in data
###########################
setwd(".../Data")
df <- read.csv(".../Validation_file.csv", sep = ",")


###############################
# Calculate Person Years
###############################

df$daystillend_y <- df$days_between_release_mortality
df$daystillend_y[is.na(df$daystillend_y)] <- 365.25
df$daystillend_y[df$daystillend_y > 365] <- 365.25

df$daystillend_m <- df$days_between_release_mortality
df$daystillend_m[is.na(df$daystillend_m)] <- 30
df$daystillend_m[df$daystillend_m > 30] <- 30

total_person_years_y <- as.numeric(sum(df$daystillend_y))/ 365.25
total_person_years_m <- as.numeric(sum(df$daystillend_m))/ 365.25

#############################
# Total number of event 
##############################

total_overdose_death_m <- sum(df$overdose_30d==1, na.rm = TRUE)
total_allcause_death_m <- sum(df$all_cause_mort_30d==1, na.rm = TRUE)
total_overdose_death_y <- sum(df$overdose_1y==1, na.rm = TRUE)
total_allcause_death_y <- sum(df$all_cause_mort_1y==1, na.rm = TRUE)



##########################
# Crude Mortality Rates
##########################

# 1-year overdose mortality
pt_overdose_y <- poisson.test(
  total_overdose_death_y,
  T = total_person_years_y
)
cmr_overdose_y <- pt_overdose_y$estimate * 100000
ci_overdose_y <- pt_overdose_y$conf.int * 100000


# 1-year all-cause mortality
pt_allcause_y <- poisson.test(
  total_allcause_death_y,
  T = total_person_years_y
)
cmr_allcause_y <- pt_allcause_y$estimate * 100000
ci_allcause_y <- pt_allcause_y$conf.int * 100000


# 30-day overdose mortality
pt_overdose_m <- poisson.test(
  total_overdose_death_m,
  T = total_person_years_m
)
cmr_overdose_m <- pt_overdose_m$estimate * 100000
ci_overdose_m <- pt_overdose_m$conf.int * 100000


# 30-day all-cause mortality
pt_allcause_m <- poisson.test(
  total_allcause_death_m,
  T = total_person_years_m
)
cmr_allcause_m <- pt_allcause_m$estimate * 100000
ci_allcause_m <- pt_allcause_m$conf.int * 100000




