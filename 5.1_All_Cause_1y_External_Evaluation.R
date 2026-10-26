# Filename..: 5.1_All_Cause_1y_External_Evaluation
# Study.....: ODIN
# Author....: Taalke Maria Sitter
# Date......: 22/05/2026
# Updated...: 
# Purpose...: External evaluation of (1) the model, and (2) the recalibrated model; ; 1-year all-cause model
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
library(dcurves)
library(gtsummary)
library(scales)

############################
# Read in data
###########################
setwd("..../Data")
df <- readRDS("df_validation.RDS")


#################
# DATA PREPERATION
##################

#formatting of data for evaluation
xry <- df %>%
      select(-"all_cause_mort_30d", -"overdose_1y", -"overdose_30d", -"age_at_release", -"LOPENR") 
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

lp <- predict(finalmodel, xr,  type="response")



########################
# 1. Simple Validation
########################
predictions <- as.data.frame(lp)
predictions <- predictions %>%
  rename(pred = s0)



#SUMMARY OF PREDICTIONS
eval_df_1 <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred,
  truth_num = yr
)

summary(eval_df_1$estimate[eval_df_1$truth == 0])
summary(eval_df_1$estimate[eval_df_1$truth == 1])


########################
# Precision-Recall Curve
########################
setwd(".../Output")


# Plot the PRROC curve using the predictions from the cross-validation
pr.curve.V1 <- pr.curve(scores.class0 = eval_df_1$estimate, weights.class0 = yr, curve = TRUE)

#save it as a png with today's date
today <- format(Sys.Date(), "%Y%m%d")
PRCurve_Name <- paste0(today, "V1_PRCurve_All_Cause_1year.png")
png(PRCurve_Name)                     
plot(pr.curve.V1, main = "1-year All-Cause Mortality Simple Validation")  # Make the plot
dev.off()

plot(pr.curve.V1, main = "1-year All-Cause Mortality Simple Validation")

#save it as a data file (but it is kind of useless wihtout also loading the data)
save(pr.curve.V1, file= "V1_PRCurve_All_Cause_1y.RData")

#AUPRC & 95%CI
auprc <- pr.curve.V1$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df_1$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))



###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_All_Cause_1y_V1 <- roc(yr, eval_df_1$estimate, direction = "<")
plot(AUCROC_All_Cause_1y_V1, print.auc = TRUE, main = "1-year All-Cause Mortality Simple Validation")
save(AUCROC_All_Cause_1y_V1, file= "V1_AUCROC_All_Cause_1y.RData")
auc(AUCROC_All_Cause_1y_V1)
ci.auc(AUCROC_All_Cause_1y_V1, method = "delong")

#save it as a png with today's date
today <- format(Sys.Date(), "%Y%m%d")
AUROC_Name <- paste0(today, "V1_AUROC_All_Cause_1year.png")
png(AUROC_Name)                     
plot(AUCROC_All_Cause_1y_V1, main = "1-year All-Cause Mortality Simple Validation")  # Make the plot
dev.off()

##############
# BRIER SCORE
##############
brier_score_V1 <- mean((eval_df_1$estimate - (yr == 1))^2)
brier_score_V1


#####################
#Calibration plot 
#####################

g1_V1 <- mutate(eval_df_1, bin = ntile(estimate, 10)) %>% 
  # Bin prediction into 10ths
  group_by(bin) %>%
  mutate(n = n(), # Get ests and CIs
         bin_pred = mean(estimate), 
         bin_prob = mean(truth_num), 
         se = sqrt((bin_prob * (1 - bin_prob)) / n), 
         ul = bin_prob + 1.96 * se, 
         ll = bin_prob - 1.96 * se) %>%
  ungroup() %>%
  ggplot(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul)) +
  geom_pointrange(size = 0.5, color = "forestgreen") +
  scale_y_continuous(breaks = seq(0, 0.2, by = 0.01)) +
  scale_x_continuous(breaks = seq(0, 0.2, by = 0.01)) +
  coord_cartesian(xlim = c(0, 0.2), ylim = c(0, 0.2)) +
  geom_abline() + 
  xlab("") +
  ylab("Observed Probability") +
  theme_minimal() +
  ggtitle("1-year All-Cause Mortality Simple Validation")


# The distribution plot        
g2_V1 <- ggplot(eval_df_1, aes(x = estimate)) +
  geom_histogram(fill = "royalblue4", binwidth = 0.001) +
  scale_x_continuous(breaks = seq(0, 0.2, by = 0.01))  +
  coord_cartesian(xlim = c(0, 0.2)) +
  xlab("Predicted Probability") +
  ylab("Frequency") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 3500, by = 1000)) +
  theme(panel.grid.minor = element_blank())

# Combine them
g_V1 <- g1_V1 + g2_V1 + plot_layout(heights = c(4, 1)) 
g_V1

save(g_V1, file= "V1_Calibration_All_Cause_1y.RData")


###########################
#DECISION CURVE ANALYSIS 
##########################
dca_1 <- dca(truth_num ~ estimate, data = eval_df_1,
           thresholds = seq(0, 0.15, by = 0.001),
           label = list(estimate = "Prediction Model")) %>%
  plot() + labs(title = "1-year All-Cause Mortality")


dca_1 +
  scale_x_continuous(
    breaks = seq(0, 0.15, by = 0.01),
    labels = label_percent(accuracy = 1)
  )















########################
# 2. Validation with udpated baseline risk
########################

#lp_link = predictors of original model

lp_link <- predict(finalmodel, xr,  type="link")


recal_model <- glm(
  yr ~ lp_link,
  family = binomial(),
  data = xr_df
)

recalibraiton_inc <- coef(recal_model)[1]
recalibraiton_b <- coef(recal_model)[2]


predictions$pred_new <- predict(
  recal_model,
  type = "response"
)


#SUMMARY OF PREDICTIONS
eval_df_2 <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred_new,
  truth_num = yr
)

eval_df_oud_2 <- data.frame(
  truth = factor(yr),
  estimate = predictions$pred_new,
  truth_num = yr,
  OUD = xry$oud
)


summary(eval_df_2$estimate[eval_df_2$truth == 0])
summary(eval_df_2$estimate[eval_df_2$truth == 1])


########################
# Precision-Recall Curve
########################

# Plot the PRROC curve using the predictions from the cross-validation
pr.curve.V2 <- pr.curve(scores.class0 = eval_df_2$estimate, weights.class0 = yr, curve = TRUE)

#save it as a png with today's date
today <- format(Sys.Date(), "%Y%m%d")
PRCurve_Name <- paste0(today, "_V2_PRCurve_All_Cause_1year.png")
png(PRCurve_Name)                     
plot(pr.curve.V2, main = "1-year All-Cause Mortality")  # Make the plot
dev.off()

plot(pr.curve.V2, main = "1-year All-Cause Mortality")  # Make the plot

#save it as a data file (but it is kind of useless wihtout also loading the data)
save(pr.curve.V2, file= "V2_PRCurve_All_Cause_1y.RData")



#AUPRC & 95%CI
auprc <- pr.curve.V2$auc.integral

#Bootstrap CI
set.seed(123)
B <- 1000 
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n),size =n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = eval_df_2$estimate[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC=", round(auprc,3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2],3))



###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_All_Cause_1y_V2 <- roc(yr, eval_df_2$estimate, direction = "<")
plot(AUCROC_All_Cause_1y_V2, print.auc = TRUE, main = "1-year All-Cause Mortality")
save(AUCROC_All_Cause_1y_V2, file= "V2_AUCROC_All_Cause_1y.RData")
auc(AUCROC_All_Cause_1y_V2)
ci.auc(AUCROC_All_Cause_1y_V2, method = "delong")

#save it as a png with today's date
today <- format(Sys.Date(), "%Y%m%d")
AUCROC_Name <- paste0(today, "V2_AUCROC_All_Cause_1year.png")
png(AUCROC_Name)                     
plot(AUCROC_All_Cause_1y_V2, print.auc = TRUE, main = "1-year All-Cause Mortality")  # Make the plot
dev.off()

##############
# BRIER SCORE
##############
brier_score_V2 <- mean((eval_df_2$estimate - (yr == 1))^2)
brier_score_V2


#####################
#Calibration plot 
#####################

g1_V2 <- mutate(eval_df_2, bin = ntile(estimate, 10)) %>% 
  # Bin prediction into 10ths
  group_by(bin) %>%
  mutate(n = n(), # Get ests and CIs
         bin_pred = mean(estimate), 
         bin_prob = mean(truth_num), 
         se = sqrt((bin_prob * (1 - bin_prob)) / n), 
         ul = bin_prob + 1.96 * se, 
         ll = bin_prob - 1.96 * se) %>%
  ungroup() %>%
  ggplot(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul)) +
  geom_pointrange(size = 0.5, color = "forestgreen") +
  scale_y_continuous(breaks = seq(0, 0.1, by = 0.01)) +
  scale_x_continuous(breaks = seq(0, 0.1, by = 0.01)) +
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 0.1)) +
  geom_abline() + 
  xlab("") +
  ylab("Observed Probability") +
  theme_minimal() +
  ggtitle("1-year All-Cause Mortality")


# The distribution plot        
g2_V2 <- ggplot(eval_df_2, aes(x = estimate)) +
  geom_histogram(fill = "royalblue4", binwidth = 0.001) +
  scale_x_continuous(breaks = seq(0, 0.1, by = 0.01))  +
  coord_cartesian(xlim = c(0, 0.1)) +
  xlab("Predicted Probability") +
  ylab("Frequency") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000)) +
  theme(panel.grid.minor = element_blank())

# Combine them
g_V2 <-  g1_V2 + g2_V2 + plot_layout(heights = c(4, 1)) 
g_V2

save(g_V2, file= "V2_Calibration_All_Cause_1y.RData")

ggsave("V2_Calibration_All_Cause_1y.png", g_V2, width = 7, height = 7)



###########################
#DECISION CURVE ANALYSIS 
##########################

dca_2 <- dca(truth_num ~ estimate + OUD, data = eval_df_oud_2,
           thresholds = seq(0, 0.15, by = 0.001),
           label = list(estimate = "Prediction Model")) %>%
  plot() + labs(title = "1-year All-Cause Mortality")


dca_2_plot <- dca_2 +
  scale_x_continuous(
    breaks = seq(0, 0.15, by = 0.01),
    labels = label_percent(accuracy = 1)
  )
plot(dca_2_plot)

ggsave("V2_DCA_All_Cause_1y.png", dca_2_plot, width = 9, height = 5)


