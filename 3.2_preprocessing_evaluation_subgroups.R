# Filename..: 3.2_preprocessing_evaluation_subgroups
# Study.....: ODIN
# Author....: Taalke Maria Sitter
# Date......: 22/05/2026
# Updated...: 
# Purpose...:  Data prepeeration for model evaluation in subgroups 
# Note......: 
# Data used.: Validation_file_sensitivity.csv


# Opening library
library(tidyverse)
library(dplyr)
library(Matrix)
library(splines)


############################
# Read in data
###########################
setwd(".../Data")
df <- read.csv(".../Data/Validation_file_sensitivity.csv", sep = ";")

#################
# DATA PREPERATION
##################


df$release_date <- as.Date(df$release_date, format= "%m/%d/%Y")


#make diagnosis variables "pre" and "during" to overall diagnosis variables
vars <- c("aud", "oud", "odud", "schiz", "bd", "dep", "nss", "behasyn", "pd", "adhd", "otherF", "selfharm", "intox")

for(v in vars){
  
  pre <- paste0(v, "_pre")
  during <- paste0(v, "_during")
  
  df[[v]] <- ifelse(df[[pre]] == 1 | df[[during]] == 1, 1, 0)
  
  df[[pre]] <- NULL
  df[[during]] <- NULL
}


#Age in Splines 
knots <- c(30, 45)

df <- df %>%
  mutate(
    age_at_release_ns1 = ns(age_at_release, knots = knots)[, 1],
    age_at_release_ns2 = ns(age_at_release, knots = knots)[, 2],
    age_at_release_ns3 = ns(age_at_release, knots = knots)[, 3]
  ) 

df <- df %>%
  mutate(
    interaction_age_sex_1 = age_at_release_ns1 * sex,
    interaction_age_sex_2 = age_at_release_ns2 * sex,
    interaction_age_sex_3 = age_at_release_ns3 * sex
  )

# order

df <- df[, c("release_date",
             "remand_status",
              "LOPENR",
             "age_at_release",
             "all_cause_mort_1y", 
             "all_cause_mort_30d",
             "overdose_1y",
             "overdose_30d",
             "sex",
             "aud",
             "oud",
             "odud",
             "schiz",
             "bd",
             "dep",
             "nss",
             "behasyn",
             "pd",
             "adhd",
             "otherF",
             "selfharm",
             "intox",
             "addict_treat",
             "age_at_release_ns1",
             "age_at_release_ns2",
             "age_at_release_ns3",
             "interaction_age_sex_1",
             "interaction_age_sex_2",
             "interaction_age_sex_3",
             "born_not_inside_country",
             "length_6to12m",
             "length_12to24m",
             "length_above24m",
             "security_high",
             "security_other",
             "married_yes",
             "married_missing",
             "edu_under9y",
             "edu_9to11y",
             "edu_missing",
             "employ_yes",
             "employ_missing",
             "inc_low",
             "inc_missing",
             "dcrime_inc_yes",
             "dcrime_inc_missing",
             "dcrime_bp_yes",
             "dcrime_bp_missing",
             "vcrime_inc_yes",
             "vcrime_inc_missing",
             "vcrime_bp_yes",
             "vcrime_bp_missing"
)]

df_remand <- subset(df, df$remand_status==1)
df_noremand <- subset(df, df$remand_status==0)
df_precovid <- subset(df, df$release_date< "2019-03-01")


#saving file
saveRDS(df_remand, file="df_validation_remand.RDS")
saveRDS(df_noremand, file="df_validation_noremand.RDS")
saveRDS(df_precovid, file="df_validation_precovid.RDS")