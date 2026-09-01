# Filename..: 1.2_description
# Study.....: ODIN
# Author....: Taalke Maria Sitter
# Date......: 10/09/2025
# Updated...: 25/07/2026  
# Purpose...: Estimate frequency of predictors and outcomes in the sample, and estimate Crude Mortality Rate
# Note......: 
# Data used.: prediction_df_full.rds


library(dplyr)
library(purrr)
library(officer)
library(tinytable)
library(tidyr)

############################
# Read in data
###########################
setwd(".../Data")
df <- readRDS("prediction_df_full.rds")


####################################################
## Frequency of Predictor and Outcomes
####################################################

df_var <- df %>%  
  select(-c(LOPNR,HUVUDBROTT, PAFOLJD, VERKSTALLEBORJ, VERKSTALLESLUT, start_date, end_date, X_ID, AR,
            ICD, NR, ORSAK, death_date, overdose, by,length_num, all_cause_mort)) 

df_var$overdose_30d <- as.numeric(df_var$overdose_30d) - 1
df_var$overdose_1y<- as.numeric(df_var$overdose_1y) - 1
df_var$all_cause_mort_30d <- as.numeric(df_var$all_cause_mort_30d) - 1
df_var$all_cause_mort_1y <- as.numeric(df_var$all_cause_mort_1y) - 1


#creating function to describe variables based on variable type
summarise_variable <- function(x, varname) {
  x <- x[!is.na(x)]
  discrete_numeric <- is.numeric(x) && length(unique(x)) <= 5 
  if (discrete_numeric) {
    
    tibble(value = sort(unique(x))) %>%
      mutate(
        count = sapply(value, function(v) sum(x == v)),
        perc  = round(count / length(x), 5) * 100,
        variable = varname,
        statistic = as.character(value)
      ) %>%
      select(variable, statistic, count, perc)
    
  } else {
    
    tibble(
      variable  = varname,
      statistic = "mean",
      count     = round(mean(x), 2),
      perc      = NA
    )
  }
}

#create summary table
summary_table <- bind_rows(
  lapply(names(df_var), function(v) summarise_variable(df_var[[v]], v))
)


#Save as word document
setwd(".../Output")

doc2 <- read_docx() %>%
  body_add_par("Dataset Overview", style = "heading 1") %>%
  body_add_par(
    "Counts and frequencies for categorical variables, and means for continuous variables.",
    style = "Normal"
  ) %>%
  body_add_table(summary_table, style = "table_template")

print(doc2, target = "variable_overview.docx")




##################################################
#CRUDE MORTALITY RATE per 100.000 person-years
##################################################

df$daystillend_y <- df$death_date - df$end_date
df$daystillend_y[is.na(df$daystillend_y)] <- 365.25
df$daystillend_y[df$daystillend_y > 365] <- 365.25

df$daystillend_m <- df$death_date - df$end_date
df$daystillend_m[is.na(df$daystillend_m)] <- 30
df$daystillend_m[df$daystillend_m > 30] <- 30


total_person_years_y <- as.numeric(sum(df$daystillend_y))/ 365.25
total_person_years_m <- as.numeric(sum(df$daystillend_m))/ 365.25

total_overdose_death_m <- sum(df$overdose_30d=="Yes", na.rm = TRUE)
total_allcause_death_m <- sum(df$all_cause_mort_30d=="Yes", na.rm = TRUE)
total_overdose_death_y <- sum(df$overdose_1y=="Yes", na.rm = TRUE)
total_allcause_death_y <- sum(df$all_cause_mort_1y=="Yes", na.rm = TRUE)



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

