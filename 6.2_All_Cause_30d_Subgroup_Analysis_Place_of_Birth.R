# Filename..: 6.2_All_Cause_30d_Subgroup_Analysis_Place_of_Birth
# Study.....: ODIN
# Author....: Taalke Maria Sitter
# Date......: 08/06/2026
# Updated...: 
# Purpose...:  External Evaluation of recalibrated model among (1) individuals born outside Norway and (2) individuals born inside Norway
# Note......: 
# Data used.: df_validation.RDS

# Opening library
library(tidyverse)
library(dplyr)
library(pROC)
library(ggplot2)
library(Matrix)
library(splines)
library(glmnet)
library(PRROC)
library(patchwork)



############################
# Read in data
###########################
setwd(".../Data")
df <- readRDS("df_validation.RDS")

df_migrant <- subset(df, born_not_inside_country==1)
df_nomigrant <- subset(df, born_not_inside_country==0)



###########################################
# AMONG MIGRANTS
###########################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df_migrant %>%
  select(-"all_cause_mort_1y", -"overdose_1y", -"overdose_30d", -"age_at_release", -"LOPENR") 
xr <- xry %>%
  select(-"all_cause_mort_30d") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df_migrant$all_cause_mort_30d
yr <- as.numeric(yr)





######################
# LOAD MODEL & applied model on dataset
######################

load("allcause30d_model.RData")

lp_link <- predict(finalmodel, xr,  type="link")
recal_model <- 3.88 + 1.74 * lp_link

lp <- plogis(recal_model)



########################
#  Prepare eval dataset
########################
predictions <- as.data.frame(lp)
predictions <- predictions %>%
  rename(pred = s0)



#SUMMARY OF PREDICTIONS
eval_df_migrant <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df_migrant$estimate[eval_df_migrant$truth == 0])
summary(eval_df_migrant$estimate[eval_df_migrant$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve.migrant <- pr.curve(scores.class0 = eval_df_migrant$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve.migrant, main = "30-days All-Cause Mortality Simple: Born outside Norway")

#AUPRC & 95%CI
auprc <- pr.curve.migrant$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df_migrant$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))

###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_All_Cause_30d_migrant <- roc(yr, eval_df_migrant$estimate, direction = "<")
plot(AUCROC_All_Cause_30d_migrant, print.auc = TRUE, main = "30-days All-Cause mortality: Born outside Norway")
auc(AUCROC_All_Cause_30d_migrant)
ci.auc(AUCROC_All_Cause_30d_migrant, method = "delong")



##############
# BRIER SCORE
##############
brier_score_migrant <- mean((eval_df_migrant$estimate - (yr == 1))^2)
brier_score_migrant




###########################################
# AMONG NoN-MIGRANTS
###########################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df_nomigrant %>%
  select(-"all_cause_mort_1y", -"overdose_1y", -"overdose_30d", -"age_at_release", -"LOPENR") 
xr <- xry %>%
  select(-"all_cause_mort_30d") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df_nomigrant$all_cause_mort_30d
yr <- as.numeric(yr)




######################
# apply model on dataset
######################
lp_link <- predict(finalmodel, xr,  type="link")
recal_model <-3.88 + 1.74 * lp_link

lp <- plogis(recal_model)




########################
#  Prepare eval dataset
########################
predictions <- as.data.frame(lp)
predictions <- predictions %>%
  rename(pred = s0)



#SUMMARY OF PREDICTIONS
eval_df_nomigrant <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df_nomigrant$estimate[eval_df_nomigrant$truth == 0])
summary(eval_df_nomigrant$estimate[eval_df_nomigrant$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve.nomigrant <- pr.curve(scores.class0 = eval_df_nomigrant$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve.nomigrant, main = "30-days All-Cause Mortality Simple: Born inside Norway")

#AUPRC & 95%CI
auprc <- pr.curve.nomigrant$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df_nomigrant$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))


###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_All_Cause_30d_nomigrant <- roc(yr, eval_df_nomigrant$estimate, direction = "<")
plot(AUCROC_All_Cause_30d_nomigrant, print.auc = TRUE, main = "30-days All-Cause mortality: Born inside Norway")
auc(AUCROC_All_Cause_30d_nomigrant)
ci.auc(AUCROC_All_Cause_30d_nomigrant, method = "delong")



##############
# BRIER SCORE
##############
brier_score_nomigrant <- mean((eval_df_nomigrant$estimate - (yr == 1))^2)
brier_score_nomigrant


