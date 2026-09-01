# Filename..: 1.1_preprocessing
# Study.....: ODIN
# Author....: Taalke Maria Sitter
# Date......: 10/09/2025
# Updated...: 27/04/2026 
# Purpose...: Preparing datasets for model development
# Note......: 
# Data used.: previously prepared data from Swedish registry data


############################
# Opening library
############################
library(tidyverse)
library(haven)
library(splines)

############################
# Read in data
###########################

#load data as df

###########################
# Exclude individuals, that died before or on day of release from prison
###########################
df <- df %>%
  filter(end_date < death_date | is.na(death_date))

#############################
# Age at release limited to 18-75 and as natural cubic splines
#############################

#Restric to age 18 to 75 at release
df <- df %>%
  filter(age_at_release >= 18 & age_at_release <= 75)

#identify knots for natural cubic splines
age_spline_model <- ns(df$age_at_release, df = 3)
knots <- attr(age_spline_model, "knots")
knots
# knots at 30 and 45 (IMPORTANT FOR EVALUATION)

#apply knots to dataset
df <- df %>%
  mutate(
    age_at_release_ns1 = ns(df$age_at_release, knots = knots)[, 1],
    age_at_release_ns2 = ns(df$age_at_release, knots = knots)[, 2],
    age_at_release_ns3 = ns(df$age_at_release, knots = knots)[, 3]
  )



#############################################
# Include Interaction term between age splines and sex
#############################################

df <- df %>%
  mutate(
    interaction_age_sex_1 = age_at_release_ns1 * sex,
    interaction_age_sex_2 = age_at_release_ns2 * sex,
    interaction_age_sex_3 = age_at_release_ns3 * sex
  )

############################################
## Outcome Variables 
############################################

df$all_cause_mort <- ifelse(df$ORSAK == "",   0, 1)
df$overdose <- ifelse(!is.na(df$overdose) & df$overdose == 1, 1, 0)


df$all_cause_mort_30d <- ifelse(df$all_cause_mort  == 1 & df$death_date-df$end_date <= 30,   1, 0)
df$all_cause_mort_30d <- factor(df$all_cause_mort_30d, 
                          levels = c(0,1), 
                          labels = c("No", "Yes"))


df$all_cause_mort_1y <- ifelse(df$all_cause_mort  == 1 & df$death_date-df$end_date <= 365.25,   1, 0)
df$all_cause_mort_1y <- factor(df$all_cause_mort_1y, 
                                levels = c(0,1), 
                                labels = c("No", "Yes"))


df$overdose_30d <- ifelse(df$overdose  == 1 & df$death_date-df$end_date <= 30,   1, 0)
df$overdose_30d <- factor(df$overdose_30d, 
                          levels = c(0,1), 
                          labels = c("No", "Yes"))

df$overdose_1y <- ifelse(df$overdose  == 1 & df$death_date-df$end_date <= 365.25,   1, 0)
df$overdose_1y <- factor(df$overdose_1y, 
                          levels = c(0,1), 
                          labels = c("No", "Yes"))


####################################################
## Dummy encoding of all predictors
###################################################

  #Born Outside Sweden (born inside sweden = reference); Missingness at birth = coded as born outside Sweden
df$born_not_inside_country <- ifelse(is.na(df$born_out_swe) | df$born_out_swe == 1, 1, 0)
df <- subset(df, select = -born_out_swe)


 #Lenghts of stay (under 6 month= reference) (and remove those with 0 days of prison time)
df <- df %>%
  filter(length_num > 0)

df$length_6to12m <- ifelse(df$length == 1,   1, 0)
df$length_12to24m <- ifelse(df$length == 2,   1, 0)
df$length_above24m <- ifelse(df$length == 3,   1, 0)
df <- subset(df, select = -length)

 #Security Level at Release (low = reference)
df$security_high <- ifelse(!is.na(df$security_slut) & df$security_slut == 1, 1, 0)
df$security_other <- ifelse(is.na(df$security_slut), 1, 0)
df <- subset(df, select = -security_slut)

#addict_treat (we are only interested if there is any, but not which type, so only addict_treat is kept)
df <- subset(df, select = -addict_treat1)
df <- subset(df, select = -addict_treat2)

  #married (no= reference) 
df$married_yes <- ifelse(!is.na(df$married) & df$married == 1, 1, 0)
df$married_missing <- ifelse(is.na(df$married), 1, 0)
df <- subset(df, select = -married)

  #eudcation (above 11 as reference)
df$edu_under9y <- ifelse(!is.na(df$edu_cat) & df$edu_cat == 0, 1, 0)
df$edu_9to11y <- ifelse(!is.na(df$edu_cat) & df$edu_cat == 1, 1, 0)
df$edu_missing <- ifelse(is.na(df$edu_cat), 1, 0)
df <- subset(df, select = -edu_cat)

  #employment (no as reference)
df$employ_yes <- ifelse(!is.na(df$employ_bi) & df$employ_bi == 1, 1, 0)
df$employ_missing <- ifelse(is.na(df$employ_bi), 1, 0)
df <- subset(df, select = -employ_bi)

  #inc_cat (stable as reference)
df$inc_low <- ifelse(!is.na(df$inc_cat) & df$inc_cat == 1, 1, 0)
df$inc_missing <- ifelse(is.na(df$inc_cat), 1, 0)
df <- subset(df, select = -inc_cat)

  #dcrime_inc (no as reference)
df$dcrime_inc_yes <- ifelse(!is.na(df$dcrime_inc) & df$dcrime_inc == 1, 1, 0)
df$dcrime_inc_missing <- ifelse(is.na(df$dcrime_inc), 1, 0)
df <- subset(df, select = -dcrime_inc)

  #dcrime_bp (no as reference)
df$dcrime_bp_yes <- ifelse(!is.na(df$dcrime_bp) & df$dcrime_bp == 1, 1, 0)
df$dcrime_bp_missing <- ifelse(is.na(df$dcrime_bp), 1, 0)
df <- subset(df, select = -dcrime_bp)

  #vcrime_inc (no as reference)
df$vcrime_inc_yes <- ifelse(!is.na(df$vcrime_inc) & df$vcrime_inc == 1, 1, 0)
df$vcrime_inc_missing <- ifelse(is.na(df$vcrime_inc), 1, 0)
df <- subset(df, select = -vcrime_inc)

  #vcrime_bp (no as reference)
df$vcrime_bp_yes <- ifelse(!is.na(df$vcrime_bp) & df$vcrime_bp == 1, 1, 0)
df$vcrime_bp_missing <- ifelse(is.na(df$vcrime_bp), 1, 0)
df <- subset(df, select = -vcrime_bp)



#######################################################
# Preparing dataframes for the 4 different prediction models
######################################################

df_overdose_30d <- df %>%  
  select(-c(LOPNR,HUVUDBROTT, PAFOLJD, VERKSTALLEBORJ, VERKSTALLESLUT, start_date, end_date, X_ID, AR,
            ICD, NR, ORSAK, death_date, overdose, by,length_num, all_cause_mort, all_cause_mort_30d,
            all_cause_mort_1y, overdose_1y, age_at_release)) 

df_overdose_1y <- df %>%  
  select(-c(LOPNR,HUVUDBROTT, PAFOLJD, VERKSTALLEBORJ, VERKSTALLESLUT, start_date, end_date, X_ID, AR,
            ICD, NR, ORSAK, death_date, overdose, by,length_num, all_cause_mort, all_cause_mort_30d,
            all_cause_mort_1y, overdose_30d, age_at_release)) 

df_all_cause_mort_30d <- df %>%  
  select(-c(LOPNR,HUVUDBROTT, PAFOLJD, VERKSTALLEBORJ, VERKSTALLESLUT, start_date, end_date, X_ID, AR,
            ICD, NR, ORSAK, death_date, overdose, by,length_num, all_cause_mort, overdose_30d,
            all_cause_mort_1y, overdose_1y,  age_at_release)) 


df_all_cause_mort_1y <- df %>%  
  select(-c(LOPNR,HUVUDBROTT, PAFOLJD, VERKSTALLEBORJ, VERKSTALLESLUT, start_date, end_date, X_ID, AR,
            ICD, NR, ORSAK, death_date, overdose, by,length_num, all_cause_mort, overdose_30d,
            all_cause_mort_30d, overdose_1y,  age_at_release)) 


######################################################
# Saving dfs to use in model development step (2.1-2.4)
######################################################
setwd(".../Data")
saveRDS(df, file="prediction_df_full.rds")
save(df_overdose_30d, df_overdose_1y, df_all_cause_mort_30d, df_all_cause_mort_1y, file = "outcome_dfs.RData")

