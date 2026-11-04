# Filename..: 7_Subgroup_Analysis_Remand
# Study.....: prisud
# Author....: Taalke Maria Sitter
# Date......: 11/06/2026
# Updated...: 
# Purpose...:  External Evaluation of recalibrated model among (1) individuals released from remand and (2) released from other places as remand 
# Note......: 
# Data used.: df_validation_remand.RDS

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

################################################################################################################################
# RELEASED FROM REMAND
#################################################################################################################################


############################
# Read in data
###########################
setwd(".../Data")
df <- readRDS("df_validation_remand.RDS")





#################################################################################
#  1 year all cause
#################################################################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df %>%
  select(-"all_cause_mort_30d", -"overdose_1y", -"overdose_30d", -"age_at_release", -"LOPENR", -"release_date", -"remand_status") 
xr <- xry %>%
  select(-"all_cause_mort_1y") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df$all_cause_mort_1y
yr <- as.numeric(yr)


######################
# LOAD MODEL & applied model on dataset
######################

load("allcause1y_model.RData")

lp_link <- predict(finalmodel, xr,  type="link")
recal_model <- -0.85 + 0.81 * lp_link

lp <- plogis(recal_model)



########################
#  Prepare eval dataset
########################
predictions <- as.data.frame(lp)
predictions <- predictions %>%
  rename(pred = s0)



#SUMMARY OF PREDICTIONS
eval_df <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df$estimate[eval_df$truth == 0])
summary(eval_df$estimate[eval_df$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve <- pr.curve(scores.class0 = eval_df$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve, main = "1-year All-Cause Mortality: Released from remand")

#AUPRC & 95%CI
auprc <- pr.curve$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))

###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_All_Cause_1y <- roc(yr, eval_df$estimate, direction = "<")
plot(AUCROC_All_Cause_1y, print.auc = TRUE, main = "1-year All-Cause mortality: Released from remand")
auc(AUCROC_All_Cause_1y)
ci.auc(AUCROC_All_Cause_1y, method = "delong")



##############
# BRIER SCORE
##############
brier_score <- mean((eval_df$estimate - (yr == 1))^2)
brier_score


#################################################################################
#  30 days all cause
#################################################################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df %>%
  select(-"all_cause_mort_1y", -"overdose_1y", -"overdose_30d", -"age_at_release", -"LOPENR", -"release_date", -"remand_status") 
xr <- xry %>%
  select(-"all_cause_mort_30d") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df$all_cause_mort_30d
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
eval_df <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df$estimate[eval_df$truth == 0])
summary(eval_df$estimate[eval_df$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve <- pr.curve(scores.class0 = eval_df$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve, main = "30-days All-Cause Mortality: Released from remand")


#AUPRC & 95%CI
auprc <- pr.curve$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))

###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_All_Cause_30d <- roc(yr, eval_df$estimate, direction = "<")
plot(AUCROC_All_Cause_30d, print.auc = TRUE, main = "30-days All-Cause mortality: Released from remand")
auc(AUCROC_All_Cause_30d)
ci.auc(AUCROC_All_Cause_30d, method = "delong")



##############
# BRIER SCORE
##############
brier_score <- mean((eval_df$estimate - (yr == 1))^2)
brier_score




#################################################################################
#  1 year overdose
#################################################################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df %>%
  select(-"overdose_30d", -"all_cause_mort_1y", -"all_cause_mort_30d", -"age_at_release", -"LOPENR", -"release_date", -"remand_status") 
xr <- xry %>%
  select(-"overdose_1y") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df$overdose_1y
yr <- as.numeric(yr)





######################
# apply model on dataset
######################
load("overdose1y_model.RData")

lp_link <- predict(finalmodel, xr,  type="link")
recal_model <- 1.73 + 1.4 * lp_link

lp <- plogis(recal_model)



########################
#  Prepare eval dataset
########################
predictions <- as.data.frame(lp)
predictions <- predictions %>%
  rename(pred = s0)



#SUMMARY OF PREDICTIONS
eval_df <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df$estimate[eval_df$truth == 0])
summary(eval_df$estimate[eval_df$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve <- pr.curve(scores.class0 = eval_df$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve, main = "1-year overdose Mortality: Released from remand")


#AUPRC & 95%CI
auprc <- pr.curve$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))

###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_overdose_1y <- roc(yr, eval_df$estimate, direction = "<")
plot(AUCROC_overdose_1y, print.auc = TRUE, main = "1-year Overdose mortality: Released from remand")
auc(AUCROC_overdose_1y)
ci.auc(AUCROC_overdose_1y, method = "delong")



##############
# BRIER SCORE
##############
brier_score <- mean((eval_df$estimate - (yr == 1))^2)
brier_score



#################################################################################
#  30 days overdose
#################################################################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df %>%
  select(-"overdose_1y", -"all_cause_mort_30d", -"all_cause_mort_1y", -"age_at_release", -"LOPENR", -"release_date", -"remand_status") 
xr <- xry %>%
  select(-"overdose_30d") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df$overdose_30d
yr <- as.numeric(yr)




######################
# LOAD MODEL & applied model on dataset
######################

load("overdose30d_model.RData")

lp_link <- predict(finalmodel, xr,  type="link")
recal_model <-7.96 + 2.36 * lp_link

lp <- plogis(recal_model)




########################
#  Prepare eval dataset
########################
predictions <- as.data.frame(lp)
predictions <- predictions %>%
  rename(pred = s0)



#SUMMARY OF PREDICTIONS
eval_df <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df$estimate[eval_df$truth == 0])
summary(eval_df$estimate[eval_df$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve <- pr.curve(scores.class0 = eval_df$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve, main = "30-days Overdose Mortality: Released from remand")


#AUPRC & 95%CI
auprc <- pr.curve$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))


###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_overdose_30d <- roc(yr, eval_df$estimate, direction = "<")
plot(AUCROC_overdose_30d, print.auc = TRUE, main = "30-days Overdose mortality: Released from remand")
auc(AUCROC_overdose_30d)
ci.auc(AUCROC_overdose_30d, method = "delong")



##############
# BRIER SCORE
##############
brier_score <- mean((eval_df$estimate - (yr == 1))^2)
brier_score




################################################################################################################################
# RELEASED FROM PLACE OTHER THAN REMAND
#################################################################################################################################


############################
# Read in data
###########################
setwd("N:/durable/project/Taalke/Data")
df <- readRDS("df_validation_noremand.RDS")





#################################################################################
#  1 year all cause
#################################################################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df %>%
  select(-"all_cause_mort_30d", -"overdose_1y", -"overdose_30d", -"age_at_release", -"LOPENR", -"release_date", -"remand_status") 
xr <- xry %>%
  select(-"all_cause_mort_1y") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df$all_cause_mort_1y
yr <- as.numeric(yr)


######################
# LOAD MODEL & applied model on dataset
######################

load("allcause1y_model.RData")

lp_link <- predict(finalmodel, xr,  type="link")
recal_model <- -0.85 + 0.81 * lp_link

lp <- plogis(recal_model)



########################
#  Prepare eval dataset
########################
predictions <- as.data.frame(lp)
predictions <- predictions %>%
  rename(pred = s0)



#SUMMARY OF PREDICTIONS
eval_df <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df$estimate[eval_df$truth == 0])
summary(eval_df$estimate[eval_df$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve <- pr.curve(scores.class0 = eval_df$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve, main = "1-year All-Cause Mortality: Released from other than remand")


#AUPRC & 95%CI
auprc <- pr.curve$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))

###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_All_Cause_1y <- roc(yr, eval_df$estimate, direction = "<")
plot(AUCROC_All_Cause_1y, print.auc = TRUE, main = "1-year All-Cause mortality: Released from other than remand")
auc(AUCROC_All_Cause_1y)
ci.auc(AUCROC_All_Cause_1y, method = "delong")



##############
# BRIER SCORE
##############
brier_score <- mean((eval_df$estimate - (yr == 1))^2)
brier_score


#################################################################################
#  30 days all cause
#################################################################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df %>%
  select(-"all_cause_mort_1y", -"overdose_1y", -"overdose_30d", -"age_at_release", -"LOPENR", -"release_date", -"remand_status") 
xr <- xry %>%
  select(-"all_cause_mort_30d") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df$all_cause_mort_30d
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
eval_df <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df$estimate[eval_df$truth == 0])
summary(eval_df$estimate[eval_df$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve <- pr.curve(scores.class0 = eval_df$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve, main = "30-days All-Cause Mortality: Released from other than remand")


#AUPRC & 95%CI
auprc <- pr.curve$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))


###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_All_Cause_30d <- roc(yr, eval_df$estimate, direction = "<")
plot(AUCROC_All_Cause_30d, print.auc = TRUE, main = "30-days All-Cause mortality: Released from other than remand")
auc(AUCROC_All_Cause_30d)
ci.auc(AUCROC_All_Cause_30d, method = "delong")



##############
# BRIER SCORE
##############
brier_score <- mean((eval_df$estimate - (yr == 1))^2)
brier_score




#################################################################################
#  1 year overdose
#################################################################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df %>%
  select(-"overdose_30d", -"all_cause_mort_1y", -"all_cause_mort_30d", -"age_at_release", -"LOPENR", -"release_date", -"remand_status") 
xr <- xry %>%
  select(-"overdose_1y") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df$overdose_1y
yr <- as.numeric(yr)





######################
# apply model on dataset
######################
load("overdose1y_model.RData")

lp_link <- predict(finalmodel, xr,  type="link")
recal_model <- 1.73 + 1.4 * lp_link

lp <- plogis(recal_model)



########################
#  Prepare eval dataset
########################
predictions <- as.data.frame(lp)
predictions <- predictions %>%
  rename(pred = s0)



#SUMMARY OF PREDICTIONS
eval_df <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df$estimate[eval_df$truth == 0])
summary(eval_df$estimate[eval_df$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve <- pr.curve(scores.class0 = eval_df$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve, main = "1-year overdose Mortality: Released from other than remand")




#AUPRC & 95%CI
auprc <- pr.curve$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3)) 

###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_overdose_1y <- roc(yr, eval_df$estimate, direction = "<")
plot(AUCROC_overdose_1y, print.auc = TRUE, main = "1-year Overdose mortality: Released from other than remand")
auc(AUCROC_overdose_1y)
ci.auc(AUCROC_overdose_1y, method = "delong")



##############
# BRIER SCORE
##############
brier_score <- mean((eval_df$estimate - (yr == 1))^2)
brier_score



#################################################################################
#  30 days overdose
#################################################################################

#################
# DATA PREPERATION
##################

#formating of data for evalutation
xry <- df %>%
  select(-"overdose_1y", -"all_cause_mort_30d", -"all_cause_mort_1y", -"age_at_release", -"LOPENR", -"release_date", -"remand_status") 
xr <- xry %>%
  select(-"overdose_30d") 
xr[] <- lapply(xr, as.numeric)
xr_df <- xr
xr <- as.matrix(xr)
yr <- df$overdose_30d
yr <- as.numeric(yr)




######################
# LOAD MODEL & applied model on dataset
######################

load("overdose30d_model.RData")

lp_link <- predict(finalmodel, xr,  type="link")
recal_model <- 7.96 + 2.36 * lp_link

lp <- plogis(recal_model)




########################
#  Prepare eval dataset
########################
predictions <- as.data.frame(lp)
predictions <- predictions %>%
  rename(pred = s0)



#SUMMARY OF PREDICTIONS
eval_df <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df$estimate[eval_df$truth == 0])
summary(eval_df$estimate[eval_df$truth == 1])


########################
# Precision-Recall Curve
########################
# Plot the PRROC curve using the predictions from the cross-validation
pr.curve <- pr.curve(scores.class0 = eval_df$estimate, weights.class0 = yr, curve = TRUE)
plot(pr.curve, main = "30-days Overdose Mortality: Released from other than remand")


#AUPRC & 95%CI
auprc <- pr.curve$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3)) 


###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_overdose_30d <- roc(yr, eval_df$estimate, direction = "<")
plot(AUCROC_overdose_30d, print.auc = TRUE, main = "30-days Overdose mortality: Released from other than remand")
auc(AUCROC_overdose_30d)
ci.auc(AUCROC_overdose_30d, method = "delong")



##############
# BRIER SCORE
##############
brier_score <- mean((eval_df$estimate - (yr == 1))^2)
brier_score




