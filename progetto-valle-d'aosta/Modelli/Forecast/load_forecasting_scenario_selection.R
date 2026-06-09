# --- PACKAGES ---
library(dplyr); library(tidyr); library(lubridate)
library(readr); library(slider); library(tibble)
library(glmnet); library(ranger)
library(xgboost); library(gbm)
library(mgcv); library(prophet)

set.seed(20260526)

# --- OUTPUT DIR ---
output_dir <- "report_assets"
dir.create(output_dir, showWarnings = FALSE)

# --- LOAD DATA ---
data_path <- c("../data_full.csv","data_full.csv","../Modelli/data_full.csv","Modelli/data_full.csv")[file.exists(c("../data_full.csv","data_full.csv","../Modelli/data_full.csv","Modelli/data_full.csv"))][1]
if (is.na(data_path)) stop("data_full.csv not found")
data <- read_csv(data_path, show_col_types = FALSE)

# --- PREPROCESSING ---
data <- data %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    dow = wday(date),
    comune_key = factor(comune_key),
    kwh = ifelse(kwh < 0, NA, kwh),
    log_kwh = log1p(kwh),
    temp = temperatura,
    log_arrivi = ifelse("totale_arrivi" %in% names(.), log1p(totale_arrivi), 0),
    precipitazione = ifelse("precipitazione" %in% names(.), precipitazione, 0)
  ) %>%
  arrange(comune_key, date)

# --- FEATURE ENGINEERING ---
data <- data %>%
  group_by(comune_key) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    
    # -----------------------------------
    # SAFE LAGS FOR 9-MONTH FORECAST
    # -----------------------------------
    lag9  = lag(log_kwh, 9),
    lag12 = lag(log_kwh, 12),
    lag24 = lag(log_kwh, 24),
    
    # -----------------------------------
    # SAFE ROLLING FEATURES
    # (past-only / causal windows)
    # -----------------------------------
    roll_mean_12 = slide_dbl(
      lag9,
      mean,
      .before = 11,
      .complete = TRUE,
      na_rm = TRUE       # Fixed here (slide_dbl accepts na_rm)
    ),
    
    roll_sd_12 = slide_dbl(
      lag9,
      sd,
      .before = 11,
      .complete = TRUE,
      na.rm = TRUE        # Fixed here: changed .na_rm to na.rm for the sd function
    ),
    
    # -----------------------------------
    # SEASONAL FEATURES
    # -----------------------------------
    sin1 = sin(2 * pi * month / 12),
    cos1 = cos(2 * pi * month / 12),
    
    sin2 = sin(2 * pi * 2 * month / 12),
    cos2 = cos(2 * pi * 2 * month / 12),
    
    sin3 = sin(2 * pi * 3 * month / 12),
    cos3 = cos(2 * pi * 3 * month / 12),
    
    # -----------------------------------
    # WEATHER FEATURES
    # -----------------------------------
    temp_sq = temp^2,
    HDD = pmax(18 - temp, 0),
    CDD = pmax(temp - 22, 0)
    
  ) %>%
  ungroup()

#
# --- MODEL DATASET ---
model_df <- data %>%
  select(
    date,
    comune_key,
    year,
    
    # target
    log_kwh,
    
    # weather
    temp,
    temp_sq,
    HDD,
    CDD,
    
    # tourism
    log_arrivi,
    
    # SAFE LAGS FOR 9-MONTH FORECAST
    lag9,
    lag12,
    lag24,
    
    # SAFE ROLLING FEATURES
    roll_mean_12,
    roll_sd_12,
    
    # calendar / seasonality
    month,
    dow,
    sin1, cos1,
    sin2, cos2,
    sin3, cos3
  ) %>%
  
  # remove rows without required history
  filter(
    !is.na(log_kwh),
    !is.na(lag9),
    !is.na(lag12),
    !is.na(lag24),
    !is.na(roll_mean_12),
    !is.na(roll_sd_12)
  )

train_df <- model_df %>% filter(year >= 2021 & year < 2025)

valid_df <- model_df %>% filter(year == 2025) 



if (nrow(valid_df) == 0) valid_df <- model_df %>% filter(date >= as.Date("2025-01-01"))
if (nrow(valid_df) == 0) stop("No validation data found")

# --- FORMULA ---
predictor_formula <- log_kwh ~ comune_key + temp + temp_sq + HDD + CDD + 
  lag9 + lag12 + lag24 + 
  roll_mean_12 + roll_sd_12 + 
  sin1 + cos1 + sin2 + cos2 + sin3 + cos3

x_train <- model.matrix(predictor_formula, train_df)[,-1]
x_valid <- model.matrix(predictor_formula, valid_df)[,-1]
y_train <- train_df$log_kwh
y_valid <- valid_df$log_kwh

metrics <- function(a, p) {
  tibble(
    MAE = mean(abs(a - p), na.rm = TRUE),
    RMSE = sqrt(mean((a - p)^2, na.rm = TRUE)),
    MAPE = mean(abs(a - p) / pmax(abs(a), 1e-6), na.rm = TRUE),
    R2 = cor(a, p, use = "complete.obs")^2
  )
}

metrics_list <- list()
predictions <- valid_df %>% select(date, comune_key)

# --- LM ---
lm_fit <- lm(predictor_formula, train_df)
lm_pred <- predict(lm_fit, valid_df)
metrics_list$LM <- metrics(y_valid, lm_pred)
predictions$LM <- expm1(lm_pred)



# --- Mixed Effects Model (M4) ---
library(lme4)
library(lmerTest)

# 1. Strip comune_key out of the fixed effects, then add it as a random intercept
mixed_formula <- update(predictor_formula, . ~ . - comune_key + (1 | comune_key))

# 2. Fit the model using lmer
m4_fit <- lmerTest::lmer(mixed_formula, data = train_df, REML = FALSE)

# 3. Generate predictions
m4_pred <- predict(m4_fit, newdata = valid_df, allow.new.levels = TRUE)

# 4. Save your metrics and exponentiate (assuming your target was logged)
metrics_list$MixedModel <- metrics(y_valid, m4_pred)
predictions$MixedModel  <- expm1(m4_pred)

# --- Random Forest ---
rf_fit <- ranger(predictor_formula, train_df, num.trees = 500)
rf_pred <- predict(rf_fit, valid_df)$predictions
metrics_list$RF <- metrics(y_valid, rf_pred)
predictions$RF <- expm1(rf_pred)

# --- XGBoost ---
# --- DEFINING EXPLICIT FEATURES (NO STRUCTURAL COLUMNS OR TARGETS LEAKED) ---
feature_cols <- c("temp", "temp_sq", "HDD", "CDD", "log_arrivi", 
                  "lag9", "lag12", "lag24", "roll_mean_12", "roll_sd_12", 
                  "month", "dow", "sin1", "cos1", "sin2", "cos2", "sin3", "cos3")

# Prepare separate dataframes by year
xgb_train_df <- model_df %>% filter(year >= 2021 & year <= 2023)
xgb_valid_df <- model_df %>% filter(year == 2024)
xgb_test_df  <- model_df %>% filter(year == 2025) # This matches your standard valid_df

# Convert dataframes into matrices and encode 'comune_key' as a numeric ID
make_boosting_matrix <- function(df, reference_df = xgb_train_df) {
  df %>%
    mutate(comune_id = as.numeric(factor(comune_key, levels = levels(factor(reference_df$comune_key))))) %>%
    select(comune_id, all_of(feature_cols)) %>%
    as.matrix()
}

x_train_boost <- make_boosting_matrix(xgb_train_df)
x_valid_boost <- make_boosting_matrix(xgb_valid_df)
x_test_boost  <- make_boosting_matrix(xgb_test_df)

y_train_boost <- xgb_train_df$log_kwh
y_valid_boost <- xgb_valid_df$log_kwh
y_test_boost  <- xgb_test_df$log_kwh # This is your y_valid target

library(xgboost)

# 1. Construct safe DMatrices
dtrain <- xgb.DMatrix(data = x_train_boost, label = y_train_boost)
dvalid <- xgb.DMatrix(data = x_valid_boost, label = y_valid_boost)
dtest  <- xgb.DMatrix(data = x_test_boost, label = y_test_boost)

# 2. Define the evaluation watchlist
watchlist <- list(train = dtrain, validation = dvalid)

# 3. Train the model using 2024 to find the optimal stopping point
xgb_fit <- xgb.train(
  data = dtrain,
  nrounds = 500,                  # Raised limit; early stopping will handle the rest
  watchlist = watchlist,
  early_stopping_rounds = 20,     # Stops if 2024 validation error hasn't dropped in 20 rounds
  objective = "reg:squarederror",
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 1                     # Set to 1 to see the validation progress print out
)

# 4. Predict on your final 2025 Test data
xgb_pred <- predict(xgb_fit, dtest)

# 5. Store metrics and save exponentiated predictions
metrics_list$XGB <- metrics(y_test_boost, xgb_pred)
predictions$XGB  <- expm1(xgb_pred)

#QGB
library(gbm)

# 1. Combine Train and Validation (2021-2024) to maximize history for gbm fitting
#    while keeping features aligned.
gbm_train_pool <- bind_rows(xgb_train_df, xgb_valid_df) %>%
  mutate(comune_id = as.numeric(factor(comune_key, levels = levels(factor(xgb_train_df$comune_key)))))

# Create a clean matrix version for the 2025 testing frame
gbm_test_frame <- as.data.frame(x_test_boost)

# 2. Build explicit formula for gbm (using the numeric comune_id)
gbm_formula <- as.formula(paste("log_kwh ~ comune_id +", paste(feature_cols, collapse = " + ")))

# 3. Train Quantile GBM (alpha = 0.5 represents Median Regression / MAD equivalent)
gbm_fit <- gbm(
  formula = gbm_formula,
  distribution = list(name = "quantile", alpha = 0.5), 
  data = gbm_train_pool,
  n.trees = 300,
  interaction.depth = 4,
  shrinkage = 0.05,
  bag.fraction = 0.8,
  train.fraction = 0.75, # Internal validation split to choose trees stably
  verbose = FALSE
)

# 4. Predict on your final 2025 Test data
#    Using best tree estimate calculated during the train fraction split
best_iter <- gbm.perf(gbm_fit, method = "OOB", plot.it = FALSE)
if(best_iter < 1) best_iter <- 300 # Safety fallback

gbm_pred <- predict(gbm_fit, newdata = gbm_test_frame, n.trees = best_iter)

# 5. Store metrics and save exponentiated predictions
metrics_list$QuantileGBM <- metrics(y_test_boost, gbm_pred)
predictions$QuantileGBM  <- expm1(gbm_pred)



# --- GAM ---

train_df <- model_df %>% filter(year >= 2021 & year < 2025) 
valid_df <- model_df %>% filter(year == 2025) 



# 1. Update the formula to add non-linear smoothing splines to continuous variables
#    and treat comune_key as a random effect smooth 're'
gam_formula <- log_kwh ~ s(comune_key, bs = "re") + 
  s(temp, k = 5) + 
  s(HDD, k = 5) + 
  s(CDD, k = 5) + 
  s(lag9, k = 10) + 
  s(lag12, k = 10) + 
  s(lag24, k = 10) +
  s(roll_mean_12, k = 10) +
  s(roll_sd_12, k = 10) +
  sin1 + cos1 + sin2 + cos2 + sin3 + cos3 # Keep Fourier terms linear

# 2. Fit the true GAM model
gam_fit <- mgcv::gam(
  gam_formula,
  data = train_df,
  method = "REML" # REML is highly recommended for stable spline estimation
)

# 3. Predict on your validation set
gam_pred <- predict(gam_fit, newdata = valid_df)

# 4. Save metrics and transform your target back
metrics_list$GAM <- metrics(y_valid, gam_pred)
predictions$GAM <- expm1(gam_pred)
# --- Prophet ---
prophet_train <- train_df %>%
  group_by(date) %>% summarise(y = mean(log_kwh), .groups = "drop") %>%
  rename(ds = date)
prophet_valid <- valid_df %>%
  group_by(date) %>% summarise(y = mean(log_kwh), .groups = "drop") %>%
  rename(ds = date)
m <- prophet(prophet_train)
future <- make_future_dataframe(m, periods = nrow(prophet_valid), freq = "month")
forecast <- predict(m, future)
prophet_pred <- tail(forecast$yhat, nrow(prophet_valid))
metrics_list$Prophet <- metrics(prophet_valid$y, prophet_pred)
plot(prophet_pred, type="l")
lines(prediction)

################
#Plots
#############

# ==========================================================
# FINAL GLOBAL PLOT (2021–2025, TRAIN vs TEST SPLIT)
# ==========================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ---------------------------
# 1. SAFE ALIGNMENT (CRITICAL FIX)
# ---------------------------
valid_df_plot <- model_df %>%
  filter(year >= 2021)

# rebuild predictions safely from valid_df only
predictions_plot <- predictions

# ensure same ordering
predictions_plot <- predictions_plot %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

valid_df_plot <- valid_df_plot %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# ---------------------------
# 2. AGGREGATE GLOBAL LOAD (NO MUNICIPALITY SPLIT)
# ---------------------------
actual_global <- valid_df_plot %>%
  group_by(date) %>%
  summarise(actual = sum(expm1(log_kwh), na.rm = TRUE), .groups = "drop")

preds_global <- predictions_plot %>%
  group_by(date) %>%
  summarise(
    LM          = sum(LM, na.rm = TRUE),
    MixedModel  = sum(MixedModel, na.rm = TRUE),
    RF          = sum(RF, na.rm = TRUE),
    XGB         = sum(XGB, na.rm = TRUE),
    QuantileGBM = sum(QuantileGBM, na.rm = TRUE), # Added
    GAM         = sum(GAM, na.rm = TRUE)
  )

plot_df <- actual_global %>%
  left_join(preds_global, by = "date") %>%
  arrange(date)

# ---------------------------
# 3. TRAIN / TEST SPLIT MARKER
# ---------------------------
split_date <- as.Date("2025-01-01")

# ---------------------------
# 4. LONG FORMAT FOR GGPLOT
# ---------------------------
plot_long <- plot_df %>%
  pivot_longer(
    cols = c(LM, MixedModel, RF, XGB, QuantileGBM, GAM), # Added QuantileGBM
    names_to = "model",
    values_to = "prediction"
  )

# ---------------------------
# 5. PLOT
# ---------------------------
ggplot() +
  
  # actual
  geom_line(
    data = plot_df,
    aes(x = date, y = actual),
    color = "black",
    linewidth = 1
  ) +
  
  # predictions
  geom_line(
    data = plot_long,
    aes(x = date, y = prediction, color = model),
    alpha = 0.7
  ) +
  
  # vertical split line (TRAIN vs TEST)
  geom_vline(
    xintercept = as.numeric(split_date),
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  
  annotate(
    "text",
    x = as.Date("2024-06-01"),
    y = max(plot_df$actual, na.rm = TRUE),
    label = "TRAIN",
    color = "black"
  ) +
  
  annotate(
    "text",
    x = as.Date("2025-06-01"),
    y = max(plot_df$actual, na.rm = TRUE),
    label = "TEST (2025)",
    color = "red"
  ) +
  
  labs(
    title = "Global Electricity Load Forecast (2021–2025)",
    subtitle = "Black = Actual | Colored = Model Predictions | Red line = Forecast start",
    x = "Date",
    y = "Total kWh",
    color = "Model"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# ==========================================================
# MUNICIPALITY-LEVEL METRICS (MAE + MAPE)
# ==========================================================

library(dplyr)
library(tidyr)

# ---------------------------
# 1. BUILD SAFE EVALUATION FRAME
# ---------------------------
eval_df <- predictions %>%
  mutate(
    date = as.Date(date)
  ) %>%
  left_join(
    model_df %>%
      select(date, comune_key, log_kwh),
    by = c("date", "comune_key")
  )

# convert actual back to original scale
eval_df <- eval_df %>%
  mutate(actual = expm1(log_kwh))

# ---------------------------
# 2. METRIC FUNCTION
# ---------------------------
mae_fun <- function(a, p) mean(abs(a - p), na.rm = TRUE)

mape_fun <- function(a, p) mean(abs(a - p) / pmax(abs(a), 1e-6), na.rm = TRUE)

# ---------------------------
# 3. RESHAPE TO LONG FORMAT
# ---------------------------
long_df <- eval_df %>%
  pivot_longer(
    cols = -c(date, comune_key, actual, log_kwh),
    names_to = "model",
    values_to = "predicted"
  )

# ---------------------------
# 4. MAE BY MUNICIPALITY
# ---------------------------
mae_table <- long_df %>%
  group_by(comune_key, model) %>%
  summarise(
    MAE = mae_fun(actual, predicted),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = model,
    values_from = MAE
  ) %>%
  arrange(comune_key)

# ---------------------------
# 5. MAPE BY MUNICIPALITY
# ---------------------------
mape_table <- long_df %>%
  group_by(comune_key, model) %>%
  summarise(
    MAPE = mape_fun(actual, predicted),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = model,
    values_from = MAPE
  ) %>%
  arrange(comune_key)

# ---------------------------
# 6. PRINT RESULTS
# ---------------------------
cat("\n========================\nMAE BY MUNICIPALITY\n========================\n")
print(mae_table)

cat("\n========================\nMAPE BY MUNICIPALITY\n========================\n")
print(mape_table)


# ==========================================================
# INDIVIDUAL MUNICIPALITY PLOTS (PAGINATED CHUNKS OF 6)
# ==========================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce) # Ensures 6 plots per page automatically

# 1. SETUP PATHS
root_dir <- "C:/Users/2692812C/OneDrive - University of Glasgow/Desktop/Progetto-NODES-main/Progetto-NODES-main/progetto-valle-d'aosta/Modelli/Forecast"
output_dir <- file.path(root_dir, "Municipality_Plots")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. COMBINE DATA WITHOUT SQUASHING IT
# Transform actuals back from log scale, keeping dates and municipality keys
actual_muni <- valid_df_plot %>%
  mutate(
    date = as.Date(date),
    actual = expm1(log_kwh),
    comune_key = tolower(trimws(as.character(comune_key)))
  ) %>%
  select(date, comune_key, actual)

# Align predictions dataframe keys
predictions_muni <- predictions %>%
  mutate(
    date = as.Date(date),
    comune_key = tolower(trimws(as.character(comune_key)))
  )

# Join them together so we have actuals and predictions side-by-side per town
plot_data <- actual_muni %>%
  left_join(predictions_muni, by = c("date", "comune_key")) %>%
  select(date, comune_key, actual, LM, GAM, MixedModel, XGB, QuantileGBM, RF) %>% # Added QuantileGBM
  pivot_longer(cols = c(LM, GAM, MixedModel, XGB, QuantileGBM, RF), names_to = "model", values_to = "pred")

# 3. CALCULATE TOTAL PAGES (6 towns per page)
all_comuni <- unique(plot_data$comune_key)
total_pages <- ceiling(length(all_comuni) / 6)

# 4. PLOT AND SAVE PAGE BY PAGE
for (page in 1:total_pages) {
  
  p <- ggplot(plot_data, aes(x = date)) +
    geom_line(aes(y = actual), color = "black", linewidth = 0.9) +
    geom_line(aes(y = pred, color = model), alpha = 0.8) +
    
    # Simple, clean pagination layout (3 rows x 2 columns = 6 per page)
    facet_wrap_paginate(~ comune_key, scales = "free_y", ncol = 2, nrow = 3, page = page) +
    
    labs(
      title = paste("Municipality Forecasts - Page", page, "of", total_pages),
      x = "Date", y = "kWh", color = "Model"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save directly to your folder
  file_name <- sprintf("plot_page_%02d.png", page)
  ggsave(filename = file.path(output_dir, file_name), plot = p, width = 11, height = 8.5)
}

cat("Success! All individual municipality plots are saved in:", output_dir, "\n")

# --- SAVE METRICS AND DATA OUT TO LOCAL STORAGE ---
write_csv(mae_table, file.path(output_dir, "mae_metrics_by_muni.csv"))
write_csv(mape_table, file.path(output_dir, "mape_metrics_by_muni.csv"))
write_csv(predictions, file.path(output_dir, "preds_2025.csv"))

cat("DONE\n")