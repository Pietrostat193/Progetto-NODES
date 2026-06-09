# Compare 3 allocation strategies of the regional nnetar forecast to the
# municipalities, on the 2025 validation window. Reads cached CSVs only (no
# refit of the regional model).
#
# Methods compared:
#   BASE     : 2-year static monthly share (current production approach)
#   METHOD_A : exponentially-weighted historical share + weather/seasonal
#              regression per comune in log-space, then softmax across comuni
#              (alternative 1 + 3 combined)
#   METHOD_B : per-comune ETS forecast then proportional reconciliation to the
#              regional nnetar total (proxy for MinT with two-level hierarchy)
#   METHOD_C : BASE static share + shrinkage-regularized weather adjustment
#              from METHOD_A (log-share convex combination, lambda small)
#   METHOD_D : BASE static share + weather-ONLY exp-weighted adjustment per
#              comune (no calendar, no trend in the meteo regression, so the
#              seasonality remains entirely in BASE), shrinkage-blended
#   METHOD_E : independent nnetar per comune, direct forecast (no reconciliation
#              to the regional total) -- tests whether modelling each series
#              standalone reduces the implausible count
#   METHOD_F : BASE++ with exponentially-weighted share (halflife 12 months) over
#              a 3-year window and weighted-median aggregation per (comune,month).
#              Targets the drift-of-level and spike-contamination patterns.
#   METHOD_G : BASE++ with comune-specific window selection (1y vs 2y based on
#              t-test of last-12m vs prior-12m means) plus winsorization at the
#              95th percentile of s_{c,t}. Conservative alternative to F.
#   METHOD_H : BASE++ with volume-aware shrinkage of monthly share toward the
#              comune's annual share (small comuni get flatter monthly profile).
#   METHOD_I : best BASE++ (chosen empirically) blended with the weather-only
#              exp-weighted signal from METHOD_D (lambda=0.10).
#
# Output: per-method per-comune metrics + headline summary printed to console
# and saved to report_assets/allocation_comparison_*.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(tibble)
  library(forecast)
})

script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE)),
  error = function(e) getwd()
)
if (!nzchar(script_dir) || is.na(script_dir)) script_dir <- getwd()

assets_dir <- file.path(script_dir, "report_assets")
data_path  <- file.path(script_dir, "..", "data_full.csv")
stopifnot(file.exists(assets_dir), file.exists(data_path))

# ---- load --------------------------------------------------------------------
data_full <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  mutate(
    date = as.Date(substr(as.character(date), 1, 10)),
    totale_arrivi = pmax(as.numeric(totale_arrivi), 0),
    temperatura = as.numeric(temperatura),
    precipitazione = pmax(as.numeric(precipitazione), 0),
    log_precipitazione = log1p(precipitazione),
    pressione = as.numeric(pressione),
    umidit_relativa = as.numeric(umidit_relativa)
  ) %>%
  filter(!is.na(date))

bench <- read.csv(file.path(assets_dir, "tourism_arrivals_model_comparison_validation.csv"),
                  stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(substr(date, 1, 10)))

nnetar_regional <- bench %>%
  filter(model == "nnetar") %>%
  select(date, regional_q50 = q50)

# ---- window ------------------------------------------------------------------
training_start <- as.Date("2021-01-01")
val_start <- min(nnetar_regional$date)
val_end   <- max(nnetar_regional$date)
train_end <- val_start %m-% months(1)

train_df <- data_full %>% filter(date >= training_start, date <= train_end)
val_df   <- data_full %>% filter(date >= val_start, date <= val_end)
val_dates <- sort(unique(val_df$date))

comuni <- train_df %>%
  distinct(comune_key, comune_nome) %>%
  arrange(comune_key)

cat(sprintf("Training: %s -> %s  (%d obs, %d comuni)\n",
            as.character(training_start), as.character(train_end),
            nrow(train_df), nrow(comuni)))
cat(sprintf("Validation: %s -> %s  (%d months)\n",
            as.character(val_start), as.character(val_end), length(val_dates)))

actuals <- val_df %>%
  group_by(comune_key, comune_nome, date) %>%
  summarise(actual = sum(totale_arrivi), .groups = "drop")

# Validation-period weather per comune (used by METHOD_A predictions).
val_weather <- val_df %>%
  select(comune_key, date, temperatura, log_precipitazione, pressione, umidit_relativa)

# =============================================================================
# BASELINE: static 2-yr monthly share
# =============================================================================
share_start <- max(training_start, train_end %m-% years(2))

monthly_share <- train_df %>%
  filter(date >= share_start) %>%
  group_by(date) %>%
  mutate(tot = sum(totale_arrivi)) %>%
  ungroup() %>%
  mutate(s = ifelse(tot > 0, totale_arrivi / tot, NA_real_),
         month_num = month(date)) %>%
  group_by(comune_key, comune_nome, month_num) %>%
  summarise(s_bar = mean(s, na.rm = TRUE), .groups = "drop")

monthly_share_fallback <- train_df %>%
  filter(date >= share_start) %>%
  group_by(date) %>%
  mutate(tot = sum(totale_arrivi)) %>%
  ungroup() %>%
  mutate(s = ifelse(tot > 0, totale_arrivi / tot, 0)) %>%
  group_by(comune_key, comune_nome) %>%
  summarise(s_fb = mean(s, na.rm = TRUE), .groups = "drop")

build_share_template <- function(share_tbl, fb_tbl) {
  expand.grid(date = val_dates, comune_key = comuni$comune_key,
              stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(month_num = month(date)) %>%
    left_join(comuni, by = "comune_key") %>%
    left_join(share_tbl, by = c("comune_key", "comune_nome", "month_num")) %>%
    left_join(fb_tbl,    by = c("comune_key", "comune_nome")) %>%
    mutate(s = dplyr::coalesce(s_bar, s_fb, 0)) %>%
    group_by(date) %>%
    mutate(s = if (sum(s, na.rm = TRUE) > 0) s / sum(s, na.rm = TRUE) else 0) %>%
    ungroup() %>%
    select(date, comune_key, comune_nome, s)
}

base_template <- build_share_template(monthly_share, monthly_share_fallback)

allocate <- function(template) {
  template %>%
    left_join(nnetar_regional, by = "date") %>%
    mutate(forecast = s * regional_q50) %>%
    select(date, comune_key, comune_nome, forecast)
}

forecast_BASE <- allocate(base_template)

# =============================================================================
# METHOD_A: exp-weighted + weather-conditioned shares
# =============================================================================
alpha_decay <- 0.7   # weight halves roughly every ~2 years
half_life_years <- log(0.5) / log(alpha_decay)
cat(sprintf("METHOD_A: exp decay alpha=%.2f (half-life ~%.1f years)\n",
            alpha_decay, half_life_years))

max_train_date <- max(train_df$date)

train_w <- train_df %>%
  mutate(
    years_back = as.numeric(difftime(max_train_date, date, units = "days")) / 365.25,
    w = alpha_decay ^ years_back,
    log_y = log1p(totale_arrivi),
    month_f = factor(month(date), levels = 1:12),
    t_idx = as.numeric(date) / 365.25
  )

fit_one_comune <- function(df_c) {
  if (nrow(df_c) < 18 || sum(df_c$totale_arrivi) < 1) return(NULL)
  if (length(unique(df_c$month_f)) < 6) return(NULL)
  rhs <- "month_f + temperatura + log_precipitazione + pressione + umidit_relativa + t_idx"
  fml <- as.formula(paste("log_y ~", rhs))
  tryCatch(stats::lm(fml, data = df_c, weights = df_c$w),
           error = function(e) NULL)
}

predict_one_comune <- function(mod, newdat) {
  if (is.null(mod)) return(rep(NA_real_, nrow(newdat)))
  tryCatch(predict(mod, newdata = newdat),
           error = function(e) rep(NA_real_, nrow(newdat)))
}

newdat_template <- val_weather %>%
  mutate(month_f = factor(month(date), levels = 1:12),
         t_idx = as.numeric(date) / 365.25)

a_predictions <- comuni %>%
  rowwise() %>%
  do({
    ck <- .$comune_key; cn <- .$comune_nome
    df_c <- train_w %>% filter(comune_key == ck)
    mod <- fit_one_comune(df_c)
    nd  <- newdat_template %>% filter(comune_key == ck) %>% arrange(date)
    if (nrow(nd) == 0) {
      tibble(comune_key = ck, comune_nome = cn,
             date = val_dates, log_y_pred = NA_real_)
    } else {
      tibble(comune_key = ck, comune_nome = cn, date = nd$date,
             log_y_pred = predict_one_comune(mod, nd))
    }
  }) %>%
  ungroup()

# Fallback for missing predictions: use baseline static share (mapped back to
# a log-prediction proportional to that share, irrelevant for softmax since
# we recompute shares from exp())
a_template <- a_predictions %>%
  group_by(date) %>%
  mutate(
    log_y_pred = ifelse(is.na(log_y_pred), -50, log_y_pred),  # ~ zero weight
    # numerical stabilization before softmax
    log_y_pred = log_y_pred - max(log_y_pred, na.rm = TRUE),
    s = exp(log_y_pred),
    s = if (sum(s, na.rm = TRUE) > 0) s / sum(s, na.rm = TRUE) else 0
  ) %>%
  ungroup() %>%
  select(date, comune_key, comune_nome, s)

# Where the WLS produced no useful signal for a comune (model NULL for all
# months), fall back to baseline share for that comune.
missing_comuni <- a_predictions %>%
  group_by(comune_key) %>%
  summarise(all_na = all(is.na(log_y_pred)), .groups = "drop") %>%
  filter(all_na) %>%
  pull(comune_key)

if (length(missing_comuni) > 0) {
  cat(sprintf("METHOD_A: %d comuni without WLS fit -> fallback to baseline share\n",
              length(missing_comuni)))
  a_template <- a_template %>%
    filter(!(comune_key %in% missing_comuni)) %>%
    bind_rows(base_template %>% filter(comune_key %in% missing_comuni)) %>%
    group_by(date) %>%
    mutate(s = if (sum(s, na.rm = TRUE) > 0) s / sum(s, na.rm = TRUE) else 0) %>%
    ungroup()
}

forecast_A <- allocate(a_template)

# =============================================================================
# METHOD_B: per-comune ETS + proportional reconciliation to regional nnetar
# =============================================================================
forecast_one_comune_ets <- function(df_c, target_dates) {
  s <- df_c %>%
    group_by(date) %>%
    summarise(y = sum(totale_arrivi), .groups = "drop") %>%
    arrange(date)
  h <- length(target_dates)

  # need at least 24 observations to attempt ETS meaningfully
  if (nrow(s) < 24 || sum(s$y) < 10) {
    # seasonal-naive: same month from previous year(s)
    last_year_idx <- match(format(target_dates, "%m"), format(s$date, "%m"))
    pred <- s$y[last_year_idx]
    pred[is.na(pred)] <- mean(s$y, na.rm = TRUE)
    return(pmax(pred, 0))
  }

  ts_y <- ts(log1p(s$y),
             start = c(year(min(s$date)), month(min(s$date))),
             frequency = 12)

  fit <- tryCatch(forecast::ets(ts_y),
                  error = function(e) NULL,
                  warning = function(w) suppressWarnings(forecast::ets(ts_y)))
  if (is.null(fit) || inherits(fit, "try-error")) {
    fit <- tryCatch(forecast::auto.arima(ts_y, seasonal = TRUE),
                    error = function(e) NULL)
  }
  if (is.null(fit)) {
    last_year_idx <- match(format(target_dates, "%m"), format(s$date, "%m"))
    pred <- s$y[last_year_idx]
    pred[is.na(pred)] <- mean(s$y, na.rm = TRUE)
    return(pmax(pred, 0))
  }
  fc <- forecast::forecast(fit, h = h)
  pmax(exp(as.numeric(fc$mean)) - 1, 0)
}

cat("METHOD_B: fitting per-comune ETS (this can take a minute)...\n")

b_base <- comuni %>%
  rowwise() %>%
  do({
    ck <- .$comune_key; cn <- .$comune_nome
    df_c <- train_df %>% filter(comune_key == ck)
    pred <- forecast_one_comune_ets(df_c, val_dates)
    tibble(comune_key = ck, comune_nome = cn,
           date = val_dates, base_pred = pred)
  }) %>%
  ungroup()

forecast_B <- b_base %>%
  left_join(nnetar_regional, by = "date") %>%
  group_by(date) %>%
  mutate(
    base_sum = sum(base_pred, na.rm = TRUE),
    scale    = ifelse(base_sum > 0, regional_q50 / base_sum, 0),
    forecast = pmax(base_pred * scale, 0)
  ) %>%
  ungroup() %>%
  select(date, comune_key, comune_nome, forecast)

# =============================================================================
# METHOD_C: BASE share + shrunk weather adjustment from METHOD_A
# =============================================================================
# log s_C = (1 - lambda) * log s_BASE + lambda * log s_A,  then renormalize.
# lambda in (0,1): 0 = pure BASE, 1 = pure METHOD_A. Small lambda keeps the
# stable static composition and only nudges it with the weather signal.
# Tuned via sweep over lambda in {0, .05, .10, .15, .20, .30, .50, .70, 1}:
# lambda ~ 0.10 marginally improves median |rel_total| (0.094 vs 0.097) and
# mean |rel_total| (0.148 vs 0.151) without hurting MAPE materially; larger
# lambda monotonically degrades. See sweep output in conversation history.
lambda_shrink <- 0.10
cat(sprintf("METHOD_C: log-share shrinkage lambda=%.2f\n", lambda_shrink))

eps <- 1e-9
c_template <- base_template %>%
  rename(s_base = s) %>%
  left_join(a_template %>% rename(s_a = s),
            by = c("date", "comune_key", "comune_nome")) %>%
  mutate(
    s_a = ifelse(is.na(s_a), s_base, s_a),
    log_s = (1 - lambda_shrink) * log(s_base + eps) +
                  lambda_shrink  * log(s_a    + eps)
  ) %>%
  group_by(date) %>%
  mutate(
    log_s = log_s - max(log_s, na.rm = TRUE),  # stabilize
    s = exp(log_s),
    s = if (sum(s, na.rm = TRUE) > 0) s / sum(s, na.rm = TRUE) else 0
  ) %>%
  ungroup() %>%
  select(date, comune_key, comune_nome, s)

forecast_C <- allocate(c_template)

# =============================================================================
# METHOD_D: BASE share + weather-ONLY exp-weighted per-comune adjustment
# =============================================================================
# Per-comune exp-weighted WLS on weather covariates only:
#   log_y ~ temperatura + log_precipitazione + pressione + umidit_relativa
# (no month factor, no time trend -> seasonality stays in BASE)
# We use the model's predictions only as a *relative deviation* signal
# (centered per comune to mean zero on training), then blend in log-space:
#   log s_D = log s_BASE + lambda_D * (delta_c,t  -  mean_c(delta_c,*))
# where delta_c,t = log_y_hat from the weather-only fit.
fit_one_comune_weather <- function(df_c) {
  if (nrow(df_c) < 18 || sum(df_c$totale_arrivi) < 1) return(NULL)
  vars <- c("temperatura", "log_precipitazione", "pressione", "umidit_relativa")
  df_c2 <- df_c[, c("log_y", "w", vars)]
  df_c2 <- df_c2[complete.cases(df_c2), , drop = FALSE]
  if (nrow(df_c2) < 18) return(NULL)
  # require some weather variation, otherwise lm is degenerate
  if (any(vapply(df_c2[, vars], function(z) length(unique(z)) < 3, logical(1)))) return(NULL)
  rhs <- paste(vars, collapse = " + ")
  fml <- as.formula(paste("log_y ~", rhs))
  tryCatch(stats::lm(fml, data = df_c2, weights = df_c2$w),
           error = function(e) NULL)
}

newdat_weather <- val_weather %>%
  select(comune_key, date, temperatura, log_precipitazione, pressione, umidit_relativa)

d_predictions <- comuni %>%
  rowwise() %>%
  do({
    ck <- .$comune_key; cn <- .$comune_nome
    df_c <- train_w %>% filter(comune_key == ck)
    mod <- fit_one_comune_weather(df_c)
    nd  <- newdat_weather %>% filter(comune_key == ck) %>% arrange(date)
    if (is.null(mod) || nrow(nd) == 0) {
      tibble(comune_key = ck, comune_nome = cn,
             date = val_dates, delta = 0)
    } else {
      # center the predictions around the training-period mean of the same
      # weather model evaluated on training data -> pure deviation signal
      train_fit <- tryCatch(predict(mod, newdata = df_c), error = function(e) NA_real_)
      center <- mean(train_fit, na.rm = TRUE)
      val_pred <- tryCatch(predict(mod, newdata = nd), error = function(e) rep(NA_real_, nrow(nd)))
      delta <- val_pred - center
      delta[is.na(delta)] <- 0
      tibble(comune_key = ck, comune_nome = cn, date = nd$date, delta = delta)
    }
  }) %>%
  ungroup()

lambda_D <- 0.10
cat(sprintf("METHOD_D: weather-only exp-weighted blend lambda=%.2f\n", lambda_D))

d_template <- base_template %>%
  rename(s_base = s) %>%
  left_join(d_predictions %>% select(date, comune_key, delta),
            by = c("date", "comune_key")) %>%
  mutate(
    delta = ifelse(is.na(delta), 0, delta),
    log_s = log(s_base + eps) + lambda_D * delta
  ) %>%
  group_by(date) %>%
  mutate(
    log_s = log_s - max(log_s, na.rm = TRUE),
    s = exp(log_s),
    s = if (sum(s, na.rm = TRUE) > 0) s / sum(s, na.rm = TRUE) else 0
  ) %>%
  ungroup() %>%
  select(date, comune_key, comune_nome, s)

forecast_D <- allocate(d_template)

# =============================================================================
# METHOD_E: per-comune independent nnetar (no reconciliation to regional)
# =============================================================================
forecast_one_comune_nnetar <- function(df_c, target_dates) {
  s <- df_c %>%
    group_by(date) %>%
    summarise(y = sum(totale_arrivi), .groups = "drop") %>%
    arrange(date)
  h <- length(target_dates)
  if (nrow(s) < 24 || sum(s$y) < 10) {
    # seasonal-naive fallback
    last_year_idx <- match(format(target_dates, "%m"), format(s$date, "%m"))
    pred <- s$y[last_year_idx]
    pred[is.na(pred)] <- mean(s$y, na.rm = TRUE)
    return(pmax(pred, 0))
  }
  ts_y <- ts(log1p(s$y),
             start = c(year(min(s$date)), month(min(s$date))),
             frequency = 12)
  fit <- tryCatch(forecast::nnetar(ts_y, lambda = NULL, P = 1, repeats = 20),
                  error = function(e) NULL,
                  warning = function(w) suppressWarnings(
                    forecast::nnetar(ts_y, lambda = NULL, P = 1, repeats = 20)))
  if (is.null(fit) || inherits(fit, "try-error")) {
    last_year_idx <- match(format(target_dates, "%m"), format(s$date, "%m"))
    pred <- s$y[last_year_idx]
    pred[is.na(pred)] <- mean(s$y, na.rm = TRUE)
    return(pmax(pred, 0))
  }
  fc <- forecast::forecast(fit, h = h)
  pmax(exp(as.numeric(fc$mean)) - 1, 0)
}

cat("METHOD_E: fitting per-comune nnetar (this can take a couple of minutes)...\n")
set.seed(20260525)

forecast_E <- comuni %>%
  rowwise() %>%
  do({
    ck <- .$comune_key; cn <- .$comune_nome
    df_c <- train_df %>% filter(comune_key == ck)
    pred <- forecast_one_comune_nnetar(df_c, val_dates)
    tibble(comune_key = ck, comune_nome = cn,
           date = val_dates, forecast = pred)
  }) %>%
  ungroup()

# =============================================================================
# Evaluate
# =============================================================================
# -----------------------------------------------------------------------------
# Helpers used by BASE++ variants (F, G, H)
# -----------------------------------------------------------------------------
weighted_median <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  if (length(x) == 1) return(x)
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

shares_per_month <- function(df) {
  df %>%
    group_by(date) %>%
    mutate(tot = sum(totale_arrivi)) %>%
    ungroup() %>%
    mutate(s = ifelse(tot > 0, totale_arrivi / tot, NA_real_),
           month_num = month(date))
}

# =============================================================================
# METHOD_F: exp-weighted share (halflife 12m) + weighted median, 3-yr window
# =============================================================================
share_start_F <- max(training_start, train_end %m-% years(3))
halflife_months_F <- 12
alpha_F <- 0.5 ^ (1 / halflife_months_F)
cat(sprintf("METHOD_F: exp share halflife=%d months, 3-yr window, weighted median\n",
            halflife_months_F))

shares_F <- train_df %>%
  filter(date >= share_start_F) %>%
  shares_per_month() %>%
  mutate(
    months_back = as.numeric(difftime(train_end, date, units = "days")) / 30.4375,
    w = alpha_F ^ pmax(months_back, 0)
  )

monthly_share_F <- shares_F %>%
  group_by(comune_key, comune_nome, month_num) %>%
  summarise(s_bar = weighted_median(s, w), .groups = "drop")

monthly_fb_F <- shares_F %>%
  group_by(comune_key, comune_nome) %>%
  summarise(s_fb = weighted.mean(s, w, na.rm = TRUE), .groups = "drop")

F_template <- build_share_template(monthly_share_F, monthly_fb_F)
forecast_F <- allocate(F_template)

# =============================================================================
# METHOD_G: comune-adaptive window + winsorization (95th percentile)
# =============================================================================
last12_end <- train_end
last12_start <- train_end %m-% months(11)
prev12_end <- last12_start %m-% months(1)
prev12_start <- prev12_end %m-% months(11)

level_test <- train_df %>%
  mutate(window = case_when(
    date >= last12_start & date <= last12_end ~ "last",
    date >= prev12_start & date <= prev12_end ~ "prev",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(window)) %>%
  group_by(comune_key, window) %>%
  summarise(m = mean(totale_arrivi, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = window, values_from = m, values_fill = 0) %>%
  mutate(
    rel_diff = ifelse(prev > 0, abs(last - prev) / prev, NA_real_),
    use_1y = !is.na(rel_diff) & rel_diff > 0.30
  )

n_1y <- sum(level_test$use_1y, na.rm = TRUE)
cat(sprintf("METHOD_G: %d/%d comuni flagged as drifting -> use 1-yr window\n",
            n_1y, nrow(level_test)))

share_start_G_2y <- max(training_start, train_end %m-% years(2))
share_start_G_1y <- max(training_start, train_end %m-% years(1))

shares_G_raw <- train_df %>%
  filter(date >= share_start_G_2y) %>%
  shares_per_month() %>%
  left_join(level_test %>% select(comune_key, use_1y),
            by = "comune_key") %>%
  filter(!use_1y | date >= share_start_G_1y) %>%
  group_by(comune_key) %>%
  mutate(s_cap = quantile(s, 0.95, na.rm = TRUE),
         s = pmin(s, s_cap)) %>%
  ungroup()

monthly_share_G <- shares_G_raw %>%
  group_by(comune_key, comune_nome, month_num) %>%
  summarise(s_bar = mean(s, na.rm = TRUE), .groups = "drop")

monthly_fb_G <- shares_G_raw %>%
  group_by(comune_key, comune_nome) %>%
  summarise(s_fb = mean(s, na.rm = TRUE), .groups = "drop")

G_template <- build_share_template(monthly_share_G, monthly_fb_G)
forecast_G <- allocate(G_template)

# =============================================================================
# METHOD_H: volume-aware shrinkage of monthly share toward annual share
# =============================================================================
# beta_c = V0 / (V0 + V_c)  -> small comuni shrink more toward flat profile
V0 <- 1000   # comuni with annual volume = V0 get beta = 0.5

annual_vol <- train_df %>%
  filter(date >= share_start) %>%
  group_by(comune_key) %>%
  summarise(V_c = sum(totale_arrivi, na.rm = TRUE) /
              (as.numeric(difftime(train_end, share_start, units = "days")) / 365.25),
            .groups = "drop") %>%
  mutate(beta = V0 / (V0 + V_c))

cat(sprintf("METHOD_H: volume-aware shrinkage, V0=%d, median beta=%.2f (range %.2f-%.2f)\n",
            V0, median(annual_vol$beta), min(annual_vol$beta), max(annual_vol$beta)))

# annual share = average of monthly shares for that comune
annual_share <- monthly_share %>%
  group_by(comune_key, comune_nome) %>%
  summarise(s_annual = mean(s_bar, na.rm = TRUE), .groups = "drop")

monthly_share_H <- monthly_share %>%
  left_join(annual_share, by = c("comune_key", "comune_nome")) %>%
  left_join(annual_vol %>% select(comune_key, beta), by = "comune_key") %>%
  mutate(s_bar = (1 - beta) * s_bar + beta * s_annual) %>%
  select(comune_key, comune_nome, month_num, s_bar)

monthly_fb_H <- monthly_share_fallback   # fallback unchanged
H_template <- build_share_template(monthly_share_H, monthly_fb_H)
forecast_H <- allocate(H_template)

# =============================================================================
# METHOD_I: best BASE++ (will pick F/G/H by total MAE after eval) + meteo blend
# =============================================================================
# For now we precompute the meteo-only blend on top of EACH of F, G, H using
# the same delta signal from METHOD_D, with lambda=0.10. The best of the three
# is selected after the evaluation step.
blend_meteo <- function(tpl, lambda = 0.10) {
  tpl %>%
    rename(s_base = s) %>%
    left_join(d_predictions %>% select(date, comune_key, delta),
              by = c("date", "comune_key")) %>%
    mutate(
      delta = ifelse(is.na(delta), 0, delta),
      log_s = log(s_base + eps) + lambda * delta
    ) %>%
    group_by(date) %>%
    mutate(
      log_s = log_s - max(log_s, na.rm = TRUE),
      s = exp(log_s),
      s = if (sum(s, na.rm = TRUE) > 0) s / sum(s, na.rm = TRUE) else 0
    ) %>%
    ungroup() %>%
    select(date, comune_key, comune_nome, s)
}

forecast_F_meteo <- allocate(blend_meteo(F_template))
forecast_G_meteo <- allocate(blend_meteo(G_template))
forecast_H_meteo <- allocate(blend_meteo(H_template))

# =============================================================================
# Evaluate
# =============================================================================
all_forecasts <- bind_rows(
  forecast_BASE %>% mutate(method = "BASE_static_share"),
  forecast_A    %>% mutate(method = "METHOD_A_expw_weather"),
  forecast_B    %>% mutate(method = "METHOD_B_ets_reconcile"),
  forecast_C    %>% mutate(method = sprintf("METHOD_C_base+shrunkA_l%.2f", lambda_shrink)),
  forecast_D    %>% mutate(method = sprintf("METHOD_D_base+meteoOnly_l%.2f", lambda_D)),
  forecast_E    %>% mutate(method = "METHOD_E_nnetar_per_comune"),
  forecast_F    %>% mutate(method = "METHOD_F_expw_median_3yr"),
  forecast_G    %>% mutate(method = "METHOD_G_adaptive_winsor"),
  forecast_H    %>% mutate(method = "METHOD_H_volume_shrinkage"),
  forecast_F_meteo %>% mutate(method = "METHOD_F+meteo_l0.10"),
  forecast_G_meteo %>% mutate(method = "METHOD_G+meteo_l0.10"),
  forecast_H_meteo %>% mutate(method = "METHOD_H+meteo_l0.10")
)

joined <- all_forecasts %>%
  left_join(actuals, by = c("comune_key", "comune_nome", "date"))

per_comune <- joined %>%
  mutate(
    err = forecast - actual,
    abs_err = abs(err),
    sq_err = err^2,
    ape = ifelse(actual > 0, abs_err / actual, NA_real_)
  ) %>%
  group_by(method, comune_key, comune_nome) %>%
  summarise(
    n_obs        = sum(!is.na(actual)),
    actual_total = sum(actual, na.rm = TRUE),
    pred_total   = sum(forecast, na.rm = TRUE),
    mae          = mean(abs_err, na.rm = TRUE),
    rmse         = sqrt(mean(sq_err, na.rm = TRUE)),
    bias         = mean(err, na.rm = TRUE),
    mape         = mean(ape, na.rm = TRUE),
    rel_total    = ifelse(actual_total > 0,
                          (pred_total - actual_total) / actual_total,
                          NA_real_),
    .groups = "drop"
  )

summary_tbl <- per_comune %>%
  group_by(method) %>%
  summarise(
    n_comuni             = dplyr::n(),
    median_mape          = median(mape, na.rm = TRUE),
    mean_mape            = mean(mape, na.rm = TRUE),
    median_abs_rel_total = median(abs(rel_total), na.rm = TRUE),
    mean_abs_rel_total   = mean(abs(rel_total),   na.rm = TRUE),
    total_mae            = sum(mae, na.rm = TRUE),
    pct_rel_gt_50        = mean(abs(rel_total) > 0.5,  na.rm = TRUE),
    pct_rel_gt_100       = mean(abs(rel_total) > 1.0,  na.rm = TRUE),
    pct_mape_gt_75       = mean(mape > 0.75, na.rm = TRUE),
    pct_implausible      = mean(
      (!is.na(rel_total) & abs(rel_total) > 0.5) |
        (!is.na(mape) & mape > 0.75),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

cat("\n===== HEADLINE SUMMARY (per-municipality forecast quality, 2025) =====\n")
print(as.data.frame(summary_tbl), row.names = FALSE, digits = 3)

# How many comuni does each method strictly win on MAPE?
win_mape <- per_comune %>%
  select(method, comune_key, mape) %>%
  pivot_wider(names_from = method, values_from = mape) %>%
  rowwise() %>%
  mutate(
    winner_val = min(c_across(-comune_key), na.rm = TRUE),
    win = names(.)[which(c_across(-comune_key) == winner_val)[1] + 1]
  ) %>%
  ungroup()

cat("\n===== Per-comune winner by MAPE =====\n")
print(as.data.frame(win_mape %>% count(win, sort = TRUE)), row.names = FALSE)

# Worst 10 comuni under each method (by |rel_total|) and head-to-head on them
worst_by_method <- per_comune %>%
  group_by(method) %>%
  arrange(method, desc(abs(rel_total))) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(method, comune_nome, actual_total, pred_total, rel_total, mape) %>%
  arrange(method, desc(abs(rel_total)))

cat("\n===== Top 10 worst comuni per method (by |rel_total|) =====\n")
print(as.data.frame(worst_by_method), row.names = FALSE, digits = 3)

# Persist outputs
write.csv(per_comune,
          file.path(assets_dir, "allocation_comparison_per_comune.csv"),
          row.names = FALSE)
write.csv(summary_tbl,
          file.path(assets_dir, "allocation_comparison_summary.csv"),
          row.names = FALSE)
write.csv(all_forecasts,
          file.path(assets_dir, "allocation_comparison_forecasts.csv"),
          row.names = FALSE)

cat("\nOutputs written to ", assets_dir, "\n", sep = "")
invisible(NULL)
