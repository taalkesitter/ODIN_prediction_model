# Filename..: 4.1_descirption_sample_evaluation
# Study.....: ODIN
# Author....: Taalke Maria Sitter
# Date......: 22/05/2026
# Updated...: 08/09/2026
# Purpose...:  Describing the sample
# Note......: 
# Data used.: df_validation.RDS


# Opening library
library(tidyverse)
library(dplyr)
library(tableone)



############################
# Read in data
###########################
setwd(".../Data")
df <- readRDS("df_validation.RDS")

#################
# DATA PREPERATION
##################

df[c(3:21)] <- lapply(df[c(3:21)], as.factor)
df[c(28:50)] <- lapply(df[c(28:50)], as.factor)
df$age_at_release <- as.numeric(df$age_at_release)


####################
# DESCRIPTION
###################
tab <- CreateTableOne(data = df)

###################
# SAVING RESULTS
####################

setwd(".../Output")
table_norway <- print(tab, printToggle = FALSE, quote = FALSE, noSpaces = TRUE)
write.csv2(table_norway, file = "table_norway.csv")


