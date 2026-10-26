# Filename..: 2.1_All_Cause_1y_development_and_internal_validation
# Study.....: ODIN
# Author....: Taalke Maria Sitter
# Date......: 03/12/2025
# Updated...: 26/10/2026 
#Purpose...:  Model tuning and internal model performance  for prediction
#             of 1-year all-cause mortality after prison release
# Note......: 
# Data used.: outcome_dfs.RData


# Opening library
library(dplyr)
library(caret)      
library(glmnet)
library(PRROC)
library(MLmetrics)
library(smotefamily) 
library(themis)
library(pROC)
library(ggplot2)
library(rmda)
library(yardstick)
library(splines)
library(xfun)
require(tidyverse)
require(viridis)
require(gridExtra)
library(patchwork)


############################
# Read in data
###########################
setwd(".../Data")
load("outcome_dfs.RData")

#################
# DATA PREPERATION
##################

xry <- df_all_cause_mort_1y
xr <- subset(df_all_cause_mort_1y, select = -all_cause_mort_1y)
xr <- as.matrix(xr)
yr <- df_all_cause_mort_1y$all_cause_mort_1y
yr <- as.numeric(yr)-1



######################
# DOUBLE LOOP FUNCTION
######################
cv2.glmnet <- function(x, y, folds, lambda, alpha_values) {
  preds <- vector(length = nrow(x))  # To store predictions for all data
  selected.lambda <- vector(length = length(unique(folds)))  # Store selected lambda values for each fold
  selected.alpha <- vector(length = length(unique(folds)))  # Store selected alpha values for each fold
  
  # Loop over folds in the outer loop
  for (i in unique(folds)) {
    # Handle the x matrix
    x.train <- x[folds != i, ]
    tmp <- apply(x.train, 2, function(col) var(col, na.rm = TRUE)) != 0  # Remove features with zero variance
    x.train <- x.train[, tmp]
    x.test <- x[folds == i, tmp]  
    
    # Handle the y matrix
    y.train <- y[folds != i]
    y.test <- y[folds == i]
    
    # Loop over all values in lambda and alpha
    folds.inner <- folds[folds != i]
    aucs.inner <- matrix(0, nrow = length(alpha_values), ncol = length(lambda))
    
    for (a in 1:length(alpha_values)) {
      for (l in 1:length(lambda)) {
        preds.inner <- numeric(length = nrow(x.train))  # Vector for storing predictions in the inner loop
        
        # Loop over folds in the inner loop
        for (j in unique(folds.inner)) {
          # Handle the x matrix for inner loop
          x.train.inner <- x.train[folds.inner != j, ]
          tmp <- apply(x.train.inner, 2, function(col) var(col, na.rm = TRUE)) != 0  # Remove features with zero variance
          x.train.inner <- x.train.inner[, tmp]
          x.test.inner <- x.train[folds.inner == j, tmp] 
          
          # Handle the y matrix for inner loop
          y.train.inner <- y.train[folds.inner != j]
          y.test.inner <- y.train[folds.inner == j]
          
          
          # Train the model with Elastic Net (alpha and lambda)
          M.glmnet.inner <- glmnet(x.train.inner, y.train.inner, family = "binomial", 
                                   alpha = alpha_values[a], lambda = lambda[l])
          preds.inner[folds.inner == j] <- predict(M.glmnet.inner, x.test.inner, type = "response")  # Get predictions
        }
        
        # Ensure preds.inner is numeric for Precision-Recall calculation
        preds.inner <- as.numeric(preds.inner)
        
        # Calculate Precision and Recall for this alpha, lambda combination
        pr.curve <- pr.curve(scores.class0 = preds.inner, weights.class0 = y.train)
        
        # Store the AUC of the Precision-Recall curve (average precision)
        aucs.inner[a, l] <- pr.curve$auc.integral
      }
    }
    
    # Find the best alpha and lambda combination based on AUC of the Precision-Recall curve
    max.auc.index <- which(aucs.inner == max(aucs.inner), arr.ind = TRUE)
    best.alpha <- alpha_values[max.auc.index[1,1]] 
    best.lambda <- lambda[max.auc.index[1,2]]
    
    # Fit the final model with the best alpha and lambda
    M.glmnet <- glmnet(x.train, y.train, family = "binomial", alpha = best.alpha, lambda = best.lambda)
    preds[folds == i] <- predict(M.glmnet, x.test, type = "response")  # Get final predictions for the outer fold
    
    selected.lambda[i] <- best.lambda
    selected.alpha[i] <- best.alpha
  }
  
  return(list("preds" = preds, "selected.lambda" = selected.lambda, "selected.alpha" = selected.alpha, "coefficients" = coef(M.glmnet)))
}


#############################
#APPLICATION OF THE FUNCTION
###########################
set.seed(123456)

# Create Grid for lambda and aplha values
L <- seq(0, 1, 0.02)  # Grid of lambda values
alpha_values <- seq(0, 1, 0.02)  # Grid of alpha values 

# Create folds 
folds <- createFolds(yr, k = 10, returnTrain = FALSE, list = FALSE)

# Runnning the function
cv2.res <- cv2.glmnet(xr, yr, folds, lambda=L, alpha_values=alpha_values)  

# Access the selected lambda and alpha values for each fold
cv2.res$selected.lambda  # Selected lambda for each fold
cv2.res$selected.alpha   # Selected alpha for each fold

#SAVE MODEL (with today's date)
today <- format(Sys.Date(), "%Y%m%d")  
filename <- paste0(today, "_model_cv2_allcausemort_1y.RData")
save(cv2.res, file= filename)


######################
# PERFORMANCE MEASURES
######################

load("20260428_model_cv2_allcausemort_1y.RData")
setwd(".../Output")

#######################
#SUMMARY OF PREDICTIONS
#######################
eval_df <- data.frame(
  truth = factor(yr),
  estimate = cv2.res$preds,
  truth_num = yr
)

summary(eval_df$estimate[eval_df$truth == 0])
summary(eval_df$estimate[eval_df$truth == 1])


########################
# Precision-Recall Curve
########################

# Plot the AUPRC curve using the predictions from the cross-validation
pr.curve.final <- pr.curve(scores.class0 = cv2.res$preds, weights.class0 = yr, curve = TRUE)

#save it as a png with today's date
today <- format(Sys.Date(), "%Y%m%d")
PRCurve_Name <- paste0(today, "PRCurve_All_Cause_1year.png")
png(PRCurve_Name)                     
plot(pr.curve.final, main = "1-year All-Cause Mortality")  # Make the plot
dev.off()


#AURPC & 95% Confidence Intervall
auprc <- pr.curve.final$auc.integral

# Bootstrap CI
set.seed(123)
B <- 1000 #number of bootstraps
boot.auprc <- numeric(B)

n <- length(yr)

for (b in 1:B) {
  idx <- sample(seq_len(n), size = n, replace = TRUE)
  
  boot.auprc[b] <- pr.curve(
    scores.class0 = cv2.res$preds[idx],
    weights.class0 = yr[idx],
    curve = FALSE
  )$auc.integral
}

prc_ci <- quantile(boot.auprc, c(0.025, 0.975))
cat("AUPRC =", round(auprc, 3), "; 95% CI =", round(prc_ci[1], 3), "-", round(prc_ci[2], 3), "\n")


###########################
# AUC-ROC with DeLong's test
###########################

AUCROC_All_Cause_1y <- roc(yr, cv2.res$preds, direction = "<")
plot(AUCROC_All_Cause_1y, print.auc = TRUE, main = "1-year All-Cause Mortality")
save(AUCROC_All_Cause_1y, file= "AUCROC_All_Cause_1y.RData")
auc(AUCROC_All_Cause_1y)
ci.auc(AUCROC_All_Cause_1y, method = "delong")

##############
# BRIER SCORE
##############
brier_score <- mean((cv2.res$preds - (yr == 1))^2)
brier_score


#####################
#Calibration plot 
#####################

g1 <- mutate(eval_df, bin = ntile(estimate, 10)) %>% 
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


#mark region i want to zoom in
g1 <- g1 +
  annotate("rect",
           xmin = 0, xmax = 0.01,
           ymin = 0, ymax = 0.01,
           fill = NA, color = "red", linetype = "dashed")


#zoom in to certain region
g1_zoom <- g1 +
  coord_cartesian(xlim = c(0, 0.01), ylim = c(0, 0.01)) +
  scale_x_continuous(breaks = c(0, 0.005, 0.01)) +
  scale_y_continuous(breaks = c(0, 0.005, 0.01)) +
  labs(x = NULL, y = NULL) +   # remove axis titles
  theme_minimal(base_size = 0) +
  theme(
    axis.text = element_text(size = 6),
    plot.title = element_blank(),
    plot.background = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )


# add it to the main plot with marks, from where it came
g1_with_zoom <- g1  +
  geom_path(aes(x,y,group=grp), 
            data=data.frame(x = c(0.01,0.065,0.01,0.065), y=c(0.00, 0.005, 0.01,0.0475),grp=c(1,1,2,2)),
            linetype='dashed',
            color = "red",
            inherit.aes = FALSE) + 
  inset_element(
    g1_zoom,
    left = 0.6, bottom = 0.05,
    right = 1, top = 0.5) 



# The distribution plot        
g2 <- ggplot(eval_df, aes(x = estimate)) +
  geom_histogram(fill = "royalblue4", binwidth = 0.001) +
  scale_x_continuous(breaks = seq(0, 0.1, by = 0.01))  +
  coord_cartesian(xlim = c(0, 0.1)) +
  xlab("Predicted Probability") +
  ylab("Frequency") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 3500, by = 1000)) +
  theme(panel.grid.minor = element_blank())

# Combine them    
g <-  g1_with_zoom + g2 + plot_layout(heights = c(4, 1)) 
g

save(g, file= "Calibration_All_Cause_1y.RData")


###########################
#DECISION CURVE ANALYSIS 
##########################
library(dcurves)
library(gtsummary)
library(scales)


#without oud as comparator 

dca <- dca(truth_num ~ estimate, data = eval_df,
           thresholds = seq(0, 0.15, by = 0.001),
           label = list(estimate = "Prediction Model")) %>%
  plot() + labs(title = "1-year All-Cause Mortality")


dca_plot <- dca +
  scale_x_continuous(
    breaks = seq(0, 0.15, by = 0.01),
    labels = label_percent(accuracy = 1)
  )

ggsave("DCA_All_Cause_1y.png",dca_plot, width = 9, height = 5)




#with oud as comparator 
eval_df_oud <- data.frame(
  truth = factor(yr),
  estimate = cv2.res$preds,
  truth_num = yr,
  OUD = xry$oud
)


dca_oud <- dca(truth_num ~ estimate + OUD , data = eval_df_oud,
               thresholds = seq(0, 0.15, by = 0.001),
               label = list(estimate = "Prediction Model")) %>%
  plot() + labs(title = "1-year All-Cause Mortality")


dca_plot_oud <- dca_oud +
  scale_x_continuous(
    breaks = seq(0, 0.15, by = 0.01),
    labels = label_percent(accuracy = 1)
  )

dca_plot_oud
ggsave("DCA_oud_All_Cause_1y.png",dca_plot_oud, width = 9, height = 5)


#############################
# FINAL MODEL 
############################

finalmodel <- glmnet(xr, yr, family = "binomial", alpha = 0, lambda =0)

save(finalmodel, file= "allcause1y_model.RData")
final_predictions <- predict(finalmodel, xr, type = "response")

#####################
#COEFFICIENT LIST
#####################
coef_OR <- as.matrix(exp(coef(finalmodel)))
coef_b <- as.matrix(coef(finalmodel))

coef_df <- data.frame(
  b  = coef_b[, 1],
  OR = coef_OR[, 1]
)


###########################
# COEFFICIENT PLOT
#########################
fp <- data.frame(
  predictor = rownames(coef_b),
  estimate = as.numeric(coef_b[, 1])
)
                       
# Remove intercept and predictors with zero coefficients
fp <- subset(fp, predictor != "(Intercept)" & estimate != 0)

#plot
fp_pred <- ggplot(fp, aes(x = estimate, y = reorder(predictor, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 2.5) +
  labs(
    title = "1-year All-Cause Mortality",
    x = "Regression coefficient",
    y = NULL
  ) +
  theme_minimal()


ggsave("fp_pred_All_Cause_1y.png",fp_pred, width = 4, height = 6)


#######################
#SHAP VALUE PLOT
######################
library(fastshap)
library(shapviz)


#tells fastshap how to obtain predictions
pred_fun <- function(object, newdata) {
  predict(
    object,
    newx = newdata,
    type = "response"
  )[, 1]
}

#get shap values
set.seed(123)

shap_values <- fastshap::explain(
  object = finalmodel,
  X = xr,
  pred_wrapper = pred_fun,
  nsim = 50,
  adjust = TRUE
)

#create the plot & save it
shap_plot <- shapviz(shap_values, X = xr) |>
  sv_importance(kind = "bee", max_display = 60) +
  labs(title = "1-year All-Cause Mortality")

ggsave(
  filename = "SHAP_all_cause_1y.png",
  plot = shap_plot,
  width = 5,
  height = 10,
  units = "in",
  dpi = 300
)









