# =====================================================================
# nnetar_deep_comparison.R
#
# INDEPENDENT model-selection script.
#
# Goal: check whether moving from the single-hidden-layer NNAR (the
# production regional backbone, see `tourism_arrivals_forecast.R`) to a
# multi-hidden-layer feed-forward neural network improves the regional
# tourism-arrivals forecast on the 2025 validation window.
#
# `forecast::nnetar` is hard-wired to a single hidden layer, so we
# replicate its setup (lagged inputs on log1p(arrivals), ensemble of
# random restarts, recursive multi-step forecast) using `nnet` (1 hidden
# layer, used to reproduce the baseline) and `neuralnet` (1, 2 or 3
# hidden layers).
#
# Metrics: MAE, RMSE, MAPE, bias, empirical 90% coverage on the same
# nine-month validation window (2025-01 -> 2025-09) used in the paper.
#
# Usage:
#   Rscript nnetar_deep_comparison.R
#   Rscript nnetar_deep_comparison.R --repeats=30 --boot=200
# =====================================================================

get_script_dir <- function() {
  frame_files <- Filter(
    Negate(is.null),
    lapply(sys.frames(), function(frame) frame$ofile)
  )
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[[length(frame_files)]], winslash = "/", mustWork = FALSE)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }
  getwd()
}

parse_arg <- function(args, key, default) {
  hit <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", key, "="), "", hit[[1]])
}

required_pkgs <- c("dplyr", "forecast", "lubridate", "tibble", "tidyr", "nnet", "neuralnet")
to_install <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages(to_install)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(forecast)
  library(lubridate)
  library(tibble)
  library(tidyr)
  library(nnet)
  library(neuralnet)
})

args <- commandArgs(trailingOnly = TRUE)
repeats   <- as.integer(parse_arg(args, "repeats", "20"))   # ensemble size per architecture
boot      <- as.integer(parse_arg(args, "boot",    "200"))  # residual-bootstrap reps for coverage
seed_base <- as.integer(parse_arg(args, "seed",    "20260525"))
validation_start <- as.Date(parse_arg(args, "validation_start", "2025-01-01"))
validation_end   <- as.Date(parse_arg(args, "validation_end",   "2025-09-01"))
# Target transform: "log1p" (previous default, equivalent to fixed BoxCox
# lambda=0 on (arrivals+1)) or "boxcox" (lambda estimated from training data
# via forecast::BoxCox.lambda).
transform <- tolower(parse_arg(args, "transform", "boxcox"))
if (!transform %in% c("log1p", "boxcox")) {
  stop("--transform must be 'log1p' or 'boxcox'.", call. = FALSE)
}

cat(sprintf("Repeats per architecture: %d | Bootstrap reps: %d\n", repeats, boot))
cat(sprintf("Target transform: %s\n", transform))
cat(sprintf("Validation window: %s -> %s\n", validation_start, validation_end))

# ---------------------------------------------------------------------
# Load data (same source as the production script)
# ---------------------------------------------------------------------
script_dir <- get_script_dir()
output_dir <- file.path(script_dir, "report_assets")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data_candidates <- c(
  file.path(script_dir, "data_full.csv"),
  file.path(script_dir, "..", "data_full.csv")
)
data_path <- normalizePath(data_candidates[file.exists(data_candidates)][1],
                           winslash = "/", mustWork = TRUE)
cat("Reading data from:", data_path, "\n")

data <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(totale_arrivi))

regional <- data %>%
  group_by(date) %>%
  summarise(actual_arrivals = sum(totale_arrivi, na.rm = TRUE), .groups = "drop") %>%
  arrange(date) %>%
  mutate(month = month(date),
         trend = as.integer(date - min(date)) / 30.4375)

train_end       <- validation_start - 1
validation_dates <- seq.Date(validation_start, validation_end, by = "month")

train_df <- regional %>% filter(date <= train_end)
valid_df <- regional %>% filter(date %in% validation_dates)

# ---------------------------------------------------------------------
# Target transform (uniform across every model so the comparison stays
# apples-to-apples).
#   transform = "log1p":  y = log1p(arrivals)             (lambda fixed = 0)
#   transform = "boxcox": y = BoxCox(arrivals + 1, lambda),
#                         with lambda estimated by Guerrero on training data
# ---------------------------------------------------------------------
if (transform == "boxcox") {
  lambda_y <- forecast::BoxCox.lambda(train_df$actual_arrivals + 1, method = "guerrero")
  y_transform   <- function(x) forecast::BoxCox(x + 1, lambda_y)
  y_untransform <- function(z) pmax(forecast::InvBoxCox(z, lambda_y) - 1, 0)
  cat(sprintf("BoxCox lambda (Guerrero, on arrivals+1): %.4f\n", lambda_y))
} else {
  lambda_y <- 0
  y_transform   <- function(x) log1p(x)
  y_untransform <- function(z) pmax(expm1(z), 0)
}

regional$log_arrivals <- y_transform(regional$actual_arrivals)
train_df$log_arrivals <- y_transform(train_df$actual_arrivals)
valid_df$log_arrivals <- y_transform(valid_df$actual_arrivals)

if (nrow(valid_df) != length(validation_dates)) {
  warning(sprintf("Only %d validation rows found in the data (expected %d).",
                  nrow(valid_df), length(validation_dates)))
}

# ---------------------------------------------------------------------
# Feature engineering: sin/cos seasonality + lags on log_arrivals
# ---------------------------------------------------------------------
build_features <- function(df) {
  df %>%
    arrange(date) %>%
    mutate(
      sin_m = sin(2 * pi * month / 12),
      cos_m = cos(2 * pi * month / 12),
      lag1  = lag(log_arrivals, 1),
      lag2  = lag(log_arrivals, 2),
      lag3  = lag(log_arrivals, 3),
      lag12 = lag(log_arrivals, 12)
    )
}

feature_cols <- c("sin_m", "cos_m", "trend", "lag1", "lag2", "lag3", "lag12")

full_feat <- build_features(regional)
train_feat <- full_feat %>% filter(date <= train_end) %>% tidyr::drop_na(all_of(feature_cols))

# Standardise inputs and target -> stable training for neuralnet
y_mean <- mean(train_feat$log_arrivals)
y_sd   <- sd(train_feat$log_arrivals)
x_mean <- vapply(feature_cols, function(c) mean(train_feat[[c]]), numeric(1))
x_sd   <- vapply(feature_cols, function(c) sd(train_feat[[c]]),   numeric(1))

scale_x <- function(mat) {
  out <- mat
  for (c in feature_cols) out[[c]] <- (out[[c]] - x_mean[[c]]) / x_sd[[c]]
  out
}
scale_y <- function(y) (y - y_mean) / y_sd
unscale_y <- function(z) z * y_sd + y_mean

train_scaled <- scale_x(train_feat)
train_scaled$y_scaled <- scale_y(train_feat$log_arrivals)

# ---------------------------------------------------------------------
# Trainers: one per architecture. Each returns a function
#   predict_one(x_new_unscaled) -> log_arrivals on original (un-logged?)
# We work in log1p space throughout.
# ---------------------------------------------------------------------
train_nnet <- function(size, decay = 0.01, maxit = 200, seed) {
  set.seed(seed)
  fit <- nnet::nnet(
    x = as.matrix(train_scaled[feature_cols]),
    y = train_scaled$y_scaled,
    size = size, linout = TRUE, decay = decay,
    maxit = maxit, trace = FALSE, MaxNWts = 5000
  )
  function(x_new) {
    x_s <- scale_x(as.data.frame(x_new))[feature_cols]
    z <- as.numeric(predict(fit, as.matrix(x_s)))
    unscale_y(z)
  }
}

train_neuralnet <- function(hidden, seed) {
  set.seed(seed)
  form <- as.formula(paste("y_scaled ~", paste(feature_cols, collapse = " + ")))
  fit <- tryCatch(
    neuralnet::neuralnet(
      form, data = train_scaled, hidden = hidden,
      linear.output = TRUE, threshold = 0.05,
      stepmax = 2e5, lifesign = "none", rep = 1,
      algorithm = "rprop+"
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || is.null(fit$net.result)) return(NULL)
  function(x_new) {
    x_s <- scale_x(as.data.frame(x_new))[feature_cols]
    z <- as.numeric(neuralnet::compute(fit, as.matrix(x_s))$net.result)
    unscale_y(z)
  }
}

# Recursive multi-step forecast given a single predictor function.
# Returns log_arrivals on validation_dates.
recursive_forecast <- function(predict_fun) {
  history <- regional %>% filter(date <= train_end) %>% arrange(date) %>% pull(log_arrivals)
  hist_dates <- regional %>% filter(date <= train_end) %>% arrange(date) %>% pull(date)
  preds <- numeric(length(validation_dates))
  for (i in seq_along(validation_dates)) {
    d <- validation_dates[i]
    m <- month(d)
    tr <- as.integer(d - min(regional$date)) / 30.4375
    feats <- tibble(
      sin_m = sin(2 * pi * m / 12),
      cos_m = cos(2 * pi * m / 12),
      trend = tr,
      lag1  = tail(history, 1),
      lag2  = tail(history, 2)[1],
      lag3  = tail(history, 3)[1],
      lag12 = tail(history, 12)[1]
    )
    yhat <- predict_fun(feats)
    if (!is.finite(yhat)) yhat <- tail(history, 1)
    preds[i] <- yhat
    history <- c(history, yhat)
  }
  preds
}

# Ensemble of `repeats` random restarts -> mean point forecast +
# per-step residual std-dev used for the bootstrap interval.
ensemble_forecast <- function(trainer_factory, label) {
  cat(sprintf("  fitting %s (%d restarts)...\n", label, repeats))
  mat <- matrix(NA_real_, nrow = repeats, ncol = length(validation_dates))
  in_sample_resid <- vector("list", repeats)
  for (r in seq_len(repeats)) {
    fun <- trainer_factory(seed_base + r)
    if (is.null(fun)) next
    mat[r, ] <- recursive_forecast(fun)
    # one-step in-sample residuals -> base for bootstrap
    yhat_train <- fun(train_feat[feature_cols])
    in_sample_resid[[r]] <- train_feat$log_arrivals - yhat_train
  }
  ok <- apply(mat, 1, function(row) all(is.finite(row)))
  if (sum(ok) == 0) {
    return(list(point = rep(NA_real_, length(validation_dates)),
                resid = numeric(0)))
  }
  point <- colMeans(mat[ok, , drop = FALSE])
  resid_pool <- unlist(in_sample_resid[ok])
  list(point = point, resid = resid_pool)
}

# Bootstrap 90% interval on log scale, then exp back.
make_interval <- function(point_log, resid_pool) {
  if (length(resid_pool) == 0) {
    return(list(q05 = rep(NA, length(point_log)), q95 = rep(NA, length(point_log))))
  }
  h <- length(point_log)
  draws <- matrix(NA_real_, nrow = boot, ncol = h)
  for (b in seq_len(boot)) {
    draws[b, ] <- point_log + sample(resid_pool, h, replace = TRUE)
  }
  list(
    q05 = apply(draws, 2, quantile, probs = 0.05, na.rm = TRUE),
    q95 = apply(draws, 2, quantile, probs = 0.95, na.rm = TRUE)
  )
}

# ---------------------------------------------------------------------
# Models to compare
# ---------------------------------------------------------------------
arch_list <- list(
  list(label = "nnetar_baseline (forecast::nnetar)", builder = "nnetar"),
  list(label = "nnet_1L_8u   (1 hidden layer, 8u)",  builder = "nnet",      hidden = 8),
  list(label = "nnet_1L_16u  (1 hidden layer, 16u)", builder = "nnet",      hidden = 16),
  list(label = "deep_2L_8_4  (2 hidden layers)",     builder = "neuralnet", hidden = c(8, 4)),
  list(label = "deep_2L_12_6 (2 hidden layers)",     builder = "neuralnet", hidden = c(12, 6)),
  list(label = "deep_3L_16_8_4 (3 hidden layers)",   builder = "neuralnet", hidden = c(16, 8, 4))
)

results <- list()
for (m in arch_list) {
  cat(sprintf("\n>> %s\n", m$label))
  if (m$builder == "nnetar") {
    # exact reproduction of the production baseline (same target transform
    # as all other models thanks to y_transform applied above).
    ts_y <- ts(train_df$log_arrivals,
               start = c(year(min(train_df$date)), month(min(train_df$date))),
               frequency = 12)
    set.seed(seed_base)
    fit <- forecast::nnetar(ts_y, repeats = repeats)
    fc  <- forecast::forecast(fit, h = length(validation_dates), PI = TRUE, npaths = boot, level = 90)
    point_log <- as.numeric(fc$mean)
    intv <- list(q05 = as.numeric(fc$lower), q95 = as.numeric(fc$upper))
  } else if (m$builder == "nnet") {
    ens <- ensemble_forecast(function(seed) train_nnet(size = m$hidden, seed = seed),
                             m$label)
    point_log <- ens$point
    intv <- make_interval(point_log, ens$resid)
  } else { # neuralnet
    ens <- ensemble_forecast(function(seed) train_neuralnet(hidden = m$hidden, seed = seed),
                             m$label)
    point_log <- ens$point
    intv <- make_interval(point_log, ens$resid)
  }

  point <- y_untransform(point_log)
  q05   <- y_untransform(intv$q05)
  q95   <- y_untransform(intv$q95)
  results[[m$label]] <- tibble(
    date = validation_dates,
    actual_arrivals = valid_df$actual_arrivals[match(validation_dates, valid_df$date)],
    mean_arrivals = point,
    q05 = q05,
    q95 = q95
  )
}

# ---------------------------------------------------------------------
# Metrics
# ---------------------------------------------------------------------
metrics <- bind_rows(lapply(names(results), function(nm) {
  df <- results[[nm]] %>% filter(!is.na(actual_arrivals))
  err <- df$mean_arrivals - df$actual_arrivals
  tibble(
    model       = nm,
    mae         = mean(abs(err), na.rm = TRUE),
    rmse        = sqrt(mean(err^2, na.rm = TRUE)),
    mape        = mean(abs(err) / pmax(df$actual_arrivals, 1), na.rm = TRUE),
    bias        = mean(err, na.rm = TRUE),
    coverage_90 = mean(df$actual_arrivals >= df$q05 & df$actual_arrivals <= df$q95, na.rm = TRUE)
  )
})) %>% arrange(mae)

cat("\n========================================================\n")
cat("Regional NN architecture comparison (validation 2025-01..2025-09)\n")
cat("========================================================\n")
print(metrics, n = Inf, width = 120)

predictions <- bind_rows(lapply(names(results), function(nm) {
  results[[nm]] %>% mutate(model = nm)
})) %>% select(model, date, actual_arrivals, mean_arrivals, q05, q95)

metrics_path <- file.path(output_dir, "nnetar_deep_comparison_metrics.csv")
preds_path   <- file.path(output_dir, "nnetar_deep_comparison_predictions.csv")
write.csv(metrics,     metrics_path, row.names = FALSE)
write.csv(predictions, preds_path,   row.names = FALSE)

cat("\nSaved:\n")
cat("  -", metrics_path, "\n")
cat("  -", preds_path,   "\n")

best <- metrics$model[1]
cat(sprintf("\nBest architecture by MAE: %s (MAE = %.0f, MAPE = %.3f)\n",
            best, metrics$mae[1], metrics$mape[1]))
