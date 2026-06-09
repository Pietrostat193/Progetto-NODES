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

script_dir <- get_script_dir()

csv_path <- file.path(script_dir, "data_full.csv")

if (!file.exists(csv_path)) {
  stop("File CSV non trovato: ", csv_path, call. = FALSE)
}

data <- read.csv(csv_path)

# ==========================================================
# MODELLO A BLOCCHI PER CONSUMO ENERGETICO COMUNALE
# Versione candidate-blocks:
# - usa SOLO le candidate variables scritte nella sezione 5
# - include tutte le candidate variables nel dataset di modellazione
# - controlla collinearitÃ  prima di ogni modello a blocchi
# - fit sequenziale con comune_key come random intercept
# - nessun fallback a lm
# ==========================================================

# ==========================================================
# 0. Pacchetti
# ==========================================================
required_pkgs <- c(
  "dplyr", "lubridate", "slider", "lme4", "lmerTest",
  "purrr", "tibble", "performance"
)

to_install <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(lubridate)
library(slider)
library(lme4)
library(lmerTest)
library(purrr)
library(tibble)
library(performance)

# ==========================================================
# 1. Preparazione dati di base
# ==========================================================
data <- data %>%
  mutate(
    date = as.Date(date),
    comune_key = as.factor(comune_key),
    comune_nome = as.factor(comune_nome),
    year = as.integer(year),
    month_num = as.integer(month_num)
  ) %>%
  arrange(comune_key, date)

# ==========================================================
# 2. Feature engineering
#    Creo i lag prima del filtro 2018, cosÃ¬ gennaio 2018
#    puÃ² usare il lag 12 del 2017.
# ==========================================================
data_fe <- data %>%
  group_by(comune_key) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    kwh_model = dplyr::if_else(!is.na(kwh) & kwh < 0, NA_real_, as.numeric(kwh)),

    # Variabile risposta
    log_kwh = log1p(kwh_model),
    log_residenti = log1p(residenti),
    log_residenti_mean = if (all(is.na(log_residenti))) NA_real_ else mean(log_residenti, na.rm = TRUE),
    log_residenti_dev = log_residenti - log_residenti_mean,

    # Indice temporale interno al comune
    time_index = row_number(),

    # --------------------------
    # StagionalitÃ  Fourier
    # --------------------------
    sin_m1 = sin(2 * pi * month_num / 12),
    cos_m1 = cos(2 * pi * month_num / 12),
    sin_m2 = sin(2 * pi * 2 * month_num / 12),
    cos_m2 = cos(2 * pi * 2 * month_num / 12),
    sin_m3 = sin(2 * pi * 3 * month_num / 12),
    cos_m3 = cos(2 * pi * 3 * month_num / 12),

    # --------------------------
    # Lags consumo in livello
    # --------------------------
    kwh_lag1  = lag(kwh_model, 1),
    kwh_lag2  = lag(kwh_model, 2),
    kwh_lag3  = lag(kwh_model, 3),
    kwh_lag6  = lag(kwh_model, 6),
    kwh_lag12 = lag(kwh_model, 12),

    # --------------------------
    # Lags consumo in log
    # --------------------------
    log_kwh_lag1  = lag(log_kwh, 1),
    log_kwh_lag2  = lag(log_kwh, 2),
    log_kwh_lag3  = lag(log_kwh, 3),
    log_kwh_lag6  = lag(log_kwh, 6),
    log_kwh_lag12 = lag(log_kwh, 12),

    # --------------------------
    # Rolling means
    # --------------------------
    log_kwh_roll3  = slide_dbl(log_kwh, mean, .before = 2,  .complete = TRUE),
    log_kwh_roll6  = slide_dbl(log_kwh, mean, .before = 5,  .complete = TRUE),
    log_kwh_roll12 = slide_dbl(log_kwh, mean, .before = 11, .complete = TRUE),

    # --------------------------
    # Differenze dinamiche
    # Non usate nei modelli sotto, ma lasciate disponibili.
    # --------------------------
    d_log_kwh_1  = log_kwh - log_kwh_lag1,
    d_log_kwh_12 = log_kwh - log_kwh_lag12,

    # --------------------------
    # Turismo
    # --------------------------
    log_totale_presenze = log1p(totale_presenze),
    log_totale_arrivi   = log1p(totale_arrivi),

    days_in_month = lubridate::days_in_month(date),
    occupazione_alberghiera_proxy = case_when(
      !is.na(numero_letti) & numero_letti > 0 ~ totale_presenze / (numero_letti * days_in_month),
      TRUE ~ NA_real_
    ),
    log_occupazione_alberghiera_proxy = log1p(occupazione_alberghiera_proxy),

    # --------------------------
    # Meteo derivate
    # --------------------------
    log_precipitazione = log1p(precipitazione),
    temp_sq = temperatura^2,

    # HDD / CDD standard semplici
    HDD = pmax(18 - temperatura, 0),
    CDD = pmax(temperatura - 22, 0),

    # Fattore anno
    year_factor = factor(year)
  ) %>%
  ungroup()

# ==========================================================
# 3. Tieni solo i dati dal 2018 in poi
# ==========================================================
data_fe <- data_fe %>%
  filter(year >= 2018)

# ==========================================================
# 4. Rimuovi colonne a varianza zero
#    Include:
#    - tutte NA
#    - un solo valore non-NA
# ==========================================================
is_zero_variance <- function(x) {
  x_non_na <- x[!is.na(x)]
  if (length(x_non_na) == 0) return(TRUE)
  length(unique(x_non_na)) <= 1
}

zero_var_cols <- names(data_fe)[vapply(data_fe, is_zero_variance, logical(1))]

data_model <- data_fe %>%
  select(-all_of(zero_var_cols))

cat("Numero colonne rimosse per varianza zero:", length(zero_var_cols), "\n")
if (length(zero_var_cols) > 0) print(zero_var_cols)

# ==========================================================
# 5. Definizione blocchi candidate variables
#    NON aggiungere candidate esterne e NON rimuovere candidate
#    automaticamente. Queste sono le sole variabili usate.
# ==========================================================
candidate_structure <- c(
  "log_residenti_mean",
  "log_residenti_dev"
)

candidate_meteo <- c(
  "temperatura",
  "log_precipitazione",
  "pressione",
  "umidit_relativa"
)

candidate_calendar <- c(
  "sin_m1", "cos_m1",
  "sin_m2", "cos_m2",
  "sin_m3", "cos_m3",
  "year_factor"
)

candidate_dynamics <- c(
  "log_kwh_lag1",
  "log_kwh_lag12"
)

candidate_tourism <- c(
  "log_totale_arrivi"
)

# ----------------------------------------------------------
# Controllo che TUTTE le candidate variables siano disponibili
# ----------------------------------------------------------
assert_vars_exist <- function(df, vars, label) {
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    stop(
      "Variabili mancanti nel blocco ", label, ": ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_vars_exist(data_model, candidate_meteo,    "meteo")
assert_vars_exist(data_model, candidate_calendar, "calendar")
assert_vars_exist(data_model, candidate_dynamics, "dynamics")
assert_vars_exist(data_model, candidate_structure, "structure")
assert_vars_exist(data_model, candidate_tourism,  "tourism")

structure_terms_raw <- candidate_structure
meteo_terms_raw    <- candidate_meteo
calendar_terms_raw <- candidate_calendar
dynamic_terms_raw  <- candidate_dynamics
tourism_terms_raw  <- candidate_tourism

cat("\nCandidate variables usate nei modelli:\n")
cat("Structure:", paste(structure_terms_raw, collapse = ", "), "\n")
cat("Meteo    :", paste(meteo_terms_raw, collapse = ", "), "\n")
cat("Calendar :", paste(calendar_terms_raw, collapse = ", "), "\n")
cat("Dynamics :", paste(dynamic_terms_raw, collapse = ", "), "\n")
cat("Tourism  :", paste(tourism_terms_raw, collapse = ", "), "\n")

# ==========================================================
# 6. Dataset comune a tutti i modelli
#    Tutte le candidate variables vengono incluse nel dataset.
# ==========================================================
all_model_vars <- unique(c(
  "log_kwh", "comune_key",
  structure_terms_raw,
  meteo_terms_raw,
  calendar_terms_raw,
  dynamic_terms_raw,
  tourism_terms_raw
))

assert_vars_exist(data_model, all_model_vars, "dataset di modellazione")

n_original_model <- nrow(data_model)

analysis_df <- data_model %>%
  select(all_of(all_model_vars)) %>%
  filter(complete.cases(.))

cat("\nRighe disponibili dopo filtro year >= 2018:", n_original_model, "\n")
cat("Righe nel campione finale comune a tutti i modelli:", nrow(analysis_df), "\n")
cat("Quota campione mantenuta:", round(nrow(analysis_df) / n_original_model, 3), "\n")
cat("Numero comuni nel campione finale:", dplyr::n_distinct(analysis_df$comune_key), "\n")

if (nrow(analysis_df) < 0.50 * n_original_model) {
  stop(
    "Il complete-case dataset scende sotto il 50% del dataset originale post-2018. ",
    "Controllare missingness delle candidate variables.",
    call. = FALSE
  )
}

# ==========================================================
# 7. Standardizzazione predittori numerici
# ==========================================================
numeric_predictors <- names(analysis_df)[sapply(analysis_df, is.numeric)]
numeric_predictors <- setdiff(numeric_predictors, "log_kwh")

analysis_df <- analysis_df %>%
  mutate(across(all_of(numeric_predictors), ~ as.numeric(scale(.)), .names = "{.col}_z"))

z_if_numeric <- function(vars, df) {
  out <- c()
  for (v in vars) {
    if (v %in% names(df) && is.numeric(df[[v]])) {
      out <- c(out, paste0(v, "_z"))
    } else {
      out <- c(out, v)
    }
  }
  out
}

structure_terms <- z_if_numeric(structure_terms_raw, analysis_df)
meteo_terms    <- z_if_numeric(meteo_terms_raw, analysis_df)
calendar_terms <- z_if_numeric(calendar_terms_raw, analysis_df)
dynamic_terms  <- z_if_numeric(dynamic_terms_raw, analysis_df)
tourism_terms  <- z_if_numeric(tourism_terms_raw, analysis_df)

make_fixed_formula <- function(response, fixed_terms) {
  fixed_part <- if (length(fixed_terms) == 0) "1" else paste(fixed_terms, collapse = " + ")
  as.formula(paste0(response, " ~ ", fixed_part))
}

arrivi_destag_fit <- stats::lm(
  make_fixed_formula("log_totale_arrivi_z", c(structure_terms, meteo_terms, calendar_terms)),
  data = analysis_df
)
analysis_df$arrivi_destag_weather <- as.numeric(stats::resid(arrivi_destag_fit))
analysis_df$arrivi_destag_weather_z <- as.numeric(scale(analysis_df$arrivi_destag_weather))
# ==========================================================
# 8. Helper formule e collinearitÃ 
# ==========================================================

make_mixed_formula <- function(response, fixed_terms, group_var = "comune_key") {
  fixed_part <- if (length(fixed_terms) == 0) "1" else paste(fixed_terms, collapse = " + ")
  as.formula(paste0(response, " ~ ", fixed_part, " + (1 | ", group_var, ")"))
}

compute_vif_from_model_matrix <- function(X) {
  if (is.null(X) || ncol(X) == 0) {
    return(tibble::tibble(predictor_column = character(), VIF = numeric()))
  }
  
  if (ncol(X) == 1) {
    return(tibble::tibble(
      predictor_column = colnames(X),
      VIF = NA_real_
    ))
  }
  
  X_df <- as.data.frame(X)
  
  vifs <- purrr::map_dbl(seq_along(X_df), function(j) {
    y <- X_df[[j]]
    others <- X_df[-j]
    
    if (stats::var(y, na.rm = TRUE) == 0) return(NA_real_)
    
    fit <- tryCatch(
      stats::lm(y ~ ., data = others),
      error = function(e) NULL
    )
    
    if (is.null(fit)) return(NA_real_)
    
    r2 <- tryCatch(
      suppressWarnings(summary(fit)$r.squared),
      error = function(e) NA_real_
    )
    
    if (is.na(r2)) return(NA_real_)
    if (r2 >= 0.999999) return(Inf)
    
    1 / (1 - r2)
  })
  
  tibble::tibble(
    predictor_column = names(X_df),
    VIF = vifs
  ) %>%
    arrange(desc(VIF))
}

check_collinearity <- function(df, fixed_terms, label, corr_cutoff = 0.70, vif_cutoff = 5) {
  cat("\n==========================================================\n")
  cat("COLLINEARITY CHECK:", label, "\n")
  cat("==========================================================\n")
  
  if (length(fixed_terms) == 0) {
    cat("Nessun predittore fisso: collinearitÃ  non applicabile.\n")
    return(invisible(NULL))
  }
  
  missing_terms <- setdiff(fixed_terms, names(df))
  if (length(missing_terms) > 0) {
    stop(
      "Questi predittori non sono presenti nel dataframe: ",
      paste(missing_terms, collapse = ", ")
    )
  }
  
  cat("Predittori inclusi:\n")
  print(fixed_terms)
  
  numeric_terms <- fixed_terms[
    vapply(df[fixed_terms], is.numeric, logical(1))
  ]
  
  factor_terms <- fixed_terms[
    vapply(df[fixed_terms], function(x) is.factor(x) || is.character(x) || is.logical(x), logical(1))
  ]
  
  # ----------------------------------------------------------
  # Diagnostica fattori
  # Non calcoliamo correlazioni/VIF sui fattori dummy-expanded.
  # ----------------------------------------------------------
  if (length(factor_terms) > 0) {
    cat("\nPredittori fattoriali/categorici esclusi da correlazione e VIF:\n")
    print(factor_terms)
    
    for (ft in factor_terms) {
      cat("\nDistribuzione livelli per", ft, ":\n")
      print(table(df[[ft]], useNA = "ifany"))
    }
  }
  
  # ----------------------------------------------------------
  # Correlazione Pearson solo tra predittori numerici
  # ----------------------------------------------------------
  corr_mat <- NULL
  
  if (length(numeric_terms) >= 2) {
    corr_mat <- stats::cor(
      df[, numeric_terms, drop = FALSE],
      use = "pairwise.complete.obs"
    )
    
    cat("\nMatrice di correlazione Pearson tra predittori numerici:\n")
    print(round(corr_mat, 3))
    
    high_corr_idx <- which(
      abs(corr_mat) > corr_cutoff & upper.tri(corr_mat),
      arr.ind = TRUE
    )
    
    if (nrow(high_corr_idx) > 0) {
      high_corr_tbl <- tibble::tibble(
        var1 = rownames(corr_mat)[high_corr_idx[, "row"]],
        var2 = colnames(corr_mat)[high_corr_idx[, "col"]],
        correlation = corr_mat[high_corr_idx]
      ) %>%
        arrange(desc(abs(correlation)))
      
      cat("\nCoppie numeriche con |correlazione| >", corr_cutoff, ":\n")
      print(high_corr_tbl)
    } else {
      cat("\nNessuna coppia numerica con |correlazione| >", corr_cutoff, "\n")
    }
  } else {
    cat("\nMeno di due predittori numerici: matrice di correlazione non calcolata.\n")
  }
  
  # ----------------------------------------------------------
  # VIF solo su predittori numerici
  # ----------------------------------------------------------
  vif_tbl <- tibble::tibble(predictor_column = character(), VIF = numeric())
  
  if (length(numeric_terms) >= 1) {
    mm_num <- stats::model.matrix(
      stats::reformulate(numeric_terms),
      data = df
    )
    
    mm_num <- mm_num[, colnames(mm_num) != "(Intercept)", drop = FALSE]
    
    keep_nonconstant_num <- apply(mm_num, 2, function(x) {
      stats::var(x, na.rm = TRUE) > 0
    })
    
    mm_num <- mm_num[, keep_nonconstant_num, drop = FALSE]
    
    vif_tbl <- compute_vif_from_model_matrix(mm_num)
    
    cat("\nVIF calcolato solo sui predittori numerici:\n")
    print(vif_tbl, n = nrow(vif_tbl))
    
    high_vif_tbl <- vif_tbl %>%
      filter(is.finite(VIF), VIF > vif_cutoff)
    
    inf_vif_tbl <- vif_tbl %>%
      filter(is.infinite(VIF))
    
    if (nrow(inf_vif_tbl) > 0) {
      cat("\nATTENZIONE: VIF infinito tra predittori numerici. Possibile collinearitÃ  perfetta:\n")
      print(inf_vif_tbl, n = nrow(inf_vif_tbl))
    }
    
    if (nrow(high_vif_tbl) > 0) {
      cat("\nATTENZIONE: VIF numerico >", vif_cutoff, ":\n")
      print(high_vif_tbl, n = nrow(high_vif_tbl))
    } else if (nrow(inf_vif_tbl) == 0) {
      cat("\nNessun VIF numerico finito >", vif_cutoff, "\n")
    }
  } else {
    cat("\nNessun predittore numerico: VIF non calcolato.\n")
  }
  
  # ----------------------------------------------------------
  # Rank check sul design matrix completo
  # Qui i fattori sono inclusi, perchÃ© vogliamo solo verificare
  # se il modello complessivo Ã¨ stimabile.
  # ----------------------------------------------------------
  mm_full <- stats::model.matrix(
    stats::reformulate(fixed_terms),
    data = df
  )
  
  mm_full <- mm_full[, colnames(mm_full) != "(Intercept)", drop = FALSE]
  
  if (ncol(mm_full) > 0) {
    keep_nonconstant_full <- apply(mm_full, 2, function(x) {
      stats::var(x, na.rm = TRUE) > 0
    })
    
    mm_full <- mm_full[, keep_nonconstant_full, drop = FALSE]
  }
  
  qr_rank <- if (ncol(mm_full) > 0) qr(mm_full)$rank else 0
  n_cols <- ncol(mm_full)
  rank_deficient <- qr_rank < n_cols
  
  cat("\nDesign matrix completo, inclusi eventuali fattori:\n")
  cat("Design matrix columns:", n_cols, "\n")
  cat("Design matrix rank:", qr_rank, "\n")
  cat("Rank deficient:", rank_deficient, "\n")
  
  if (n_cols >= 2) {
    cat("Condition number kappa:", round(kappa(mm_full, exact = TRUE), 3), "\n")
  }
  
  if (rank_deficient) {
    cat("\nATTENZIONE: il design matrix completo Ã¨ rank-deficient.\n")
    cat("Questo segnala collinearitÃ  perfetta o ridondanza esatta nel modello fisso.\n")
  }
  
  invisible(list(
    numeric_terms = numeric_terms,
    factor_terms = factor_terms,
    numeric_correlations = corr_mat,
    vif_numeric = vif_tbl,
    rank_deficient = rank_deficient,
    rank = qr_rank,
    n_columns = n_cols
  ))
}
# ==========================================================
# 9. Formule a blocchi sequenziali
# ==========================================================
f_m0 <- make_mixed_formula("log_kwh", character(0))
f_m0_pop <- make_mixed_formula("log_kwh", structure_terms)
f_m1 <- make_mixed_formula("log_kwh", c(structure_terms, meteo_terms))
f_m2 <- make_mixed_formula("log_kwh", c(structure_terms, meteo_terms, calendar_terms))
f_m3 <- make_mixed_formula("log_kwh", c(structure_terms, meteo_terms, calendar_terms, dynamic_terms))
f_m4 <- make_mixed_formula("log_kwh", c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, tourism_terms))

cat("\nFormule usate:\n")
print(f_m0)
print(f_m0_pop)
print(f_m1)
print(f_m2)
print(f_m3)
print(f_m4)

# ==========================================================
# 10. Fit sequenziale mixed models
#     comune_key Ã¨ sempre random intercept.
#     Prima di ogni modello con predittori, stampo il check
#     di collinearitÃ  tra variabili indipendenti.
# ==========================================================
ctrl <- lme4::lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 2e5)
)

cat("\n==========================================================\n")
cat("FIT M0: baseline random-intercept model\n")
cat("==========================================================\n")
check_collinearity(analysis_df, character(0), "M0 baseline")
m0 <- lmerTest::lmer(f_m0, data = analysis_df, REML = FALSE, control = ctrl)

cat("\n==========================================================\n")
cat("FIT M0_pop: + struttura (popolazione comunale)\n")
cat("==========================================================\n")
check_collinearity(analysis_df, structure_terms, "M0_pop cumulative predictors: structure")
m0_pop <- lmerTest::lmer(f_m0_pop, data = analysis_df, REML = FALSE, control = ctrl)

cat("\n==========================================================\n")
cat("FIT M1: + meteo\n")
cat("==========================================================\n")
check_collinearity(analysis_df, meteo_terms, "M1 added block only: meteo")
check_collinearity(analysis_df, c(structure_terms, meteo_terms), "M1 cumulative predictors: structure + meteo")
m1 <- lmerTest::lmer(f_m1, data = analysis_df, REML = FALSE, control = ctrl)

cat("\n==========================================================\n")
cat("FIT M2: + calendar\n")
cat("==========================================================\n")
check_collinearity(analysis_df, calendar_terms, "M2 added block only: calendar")
check_collinearity(analysis_df, c(structure_terms, meteo_terms, calendar_terms), "M2 cumulative predictors: structure + meteo + calendar")
m2 <- lmerTest::lmer(f_m2, data = analysis_df, REML = FALSE, control = ctrl)

cat("\n==========================================================\n")
cat("FIT M3: + dynamics\n")
cat("==========================================================\n")
check_collinearity(analysis_df, dynamic_terms, "M3 added block only: dynamics")
check_collinearity(analysis_df, c(structure_terms, meteo_terms, calendar_terms, dynamic_terms), "M3 cumulative predictors: structure + meteo + calendar + dynamics")
m3 <- lmerTest::lmer(f_m3, data = analysis_df, REML = FALSE, control = ctrl)

cat("\n==========================================================\n")
cat("FIT M4: + tourism\n")
cat("==========================================================\n")
check_collinearity(analysis_df, tourism_terms, "M4 added block only: tourism")
check_collinearity(analysis_df, c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, tourism_terms), "M4 cumulative predictors: structure + meteo + calendar + dynamics + tourism")
m4 <- lmerTest::lmer(f_m4, data = analysis_df, REML = FALSE, control = ctrl)

f_m4_arrivi_destag <- make_mixed_formula(
  "log_kwh",
  c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, "arrivi_destag_weather_z")
)

cat("\n==========================================================\n")
cat("FIT M4_id: + arrivi destagionalizzati e depurati dal meteo\n")
cat("==========================================================\n")
check_collinearity(
  analysis_df,
  "arrivi_destag_weather_z",
  "M4_id added block only: arrivals residualized on structure + meteo + calendar"
)
check_collinearity(
  analysis_df,
  c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, "arrivi_destag_weather_z"),
  "M4_id cumulative predictors: structure + meteo + calendar + dynamics + residualized arrivals"
)
m4_arrivi_destag <- lmerTest::lmer(f_m4_arrivi_destag, data = analysis_df, REML = FALSE, control = ctrl)

# ----------------------------------------------------------
# Singularity diagnostics only. No refit as lm.
# ----------------------------------------------------------
cat("\nDiagnostica singolaritÃ :\n")
cat("M0 singular:", lme4::isSingular(m0, tol = 1e-5), "\n")
cat("M0_pop singular:", lme4::isSingular(m0_pop, tol = 1e-5), "\n")
cat("M1 singular:", lme4::isSingular(m1, tol = 1e-5), "\n")
cat("M2 singular:", lme4::isSingular(m2, tol = 1e-5), "\n")
cat("M3 singular:", lme4::isSingular(m3, tol = 1e-5), "\n")
cat("M4 singular:", lme4::isSingular(m4, tol = 1e-5), "\n")
cat("M4_id singular:", lme4::isSingular(m4_arrivi_destag, tol = 1e-5), "\n")

model_list <- list(
  M0_baseline   = m0,
  M0_popolazione = m0_pop,
  M1_meteo      = m1,
  M2_calendario = m2,
  M3_dinamica   = m3,
  M4_turismo    = m4
)

# ==========================================================
# 11. R2 helper
# ==========================================================
safe_r2_generic <- function(model) {
  if (requireNamespace("performance", quietly = TRUE)) {
    out <- performance::r2_nakagawa(model, tolerance = 1e-10)
    return(c(
      R2_marginal = unname(out$R2_marginal),
      R2_conditional = unname(out$R2_conditional)
    ))
  }

  c(R2_marginal = NA_real_, R2_conditional = NA_real_)
}

# ==========================================================
# 12. Tabella confronto modelli
# ==========================================================
comparison_table <- purrr::imap_dfr(model_list, function(mod, nm) {
  r2vals <- safe_r2_generic(mod)

  tibble(
    model = nm,
    engine = "lmerTest",
    nobs = stats::nobs(mod),
    AIC = AIC(mod),
    BIC = BIC(mod),
    logLik = as.numeric(logLik(mod)),
    R2_marginal = unname(r2vals["R2_marginal"]),
    R2_conditional = unname(r2vals["R2_conditional"]),
    singular = lme4::isSingular(mod, tol = 1e-5)
  )
})

cat("\nTabella confronto modelli:\n")
print(comparison_table)

# ==========================================================
# 13. Confronti annidati sequenziali
# ==========================================================
cat("\nConfronto annidato M0 -> M0_pop -> M1 -> M2 -> M3 -> M4:\n")
print(anova(m0, m0_pop, m1, m2, m3, m4))

cat("\nDelta AIC sequenziali:\n")
cat("M0 - M0_pop:", AIC(m0) - AIC(m0_pop), "\n")
cat("M0_pop - M1:", AIC(m0_pop) - AIC(m1), "\n")
cat("M1 - M2:", AIC(m1) - AIC(m2), "\n")
cat("M2 - M3:", AIC(m2) - AIC(m3), "\n")
cat("M3 - M4:", AIC(m3) - AIC(m4), "\n")

cat("\nDelta BIC sequenziali:\n")
cat("M0 - M0_pop:", BIC(m0) - BIC(m0_pop), "\n")
cat("M0_pop - M1:", BIC(m0_pop) - BIC(m1), "\n")
cat("M1 - M2:", BIC(m1) - BIC(m2), "\n")
cat("M2 - M3:", BIC(m2) - BIC(m3), "\n")
cat("M3 - M4:", BIC(m3) - BIC(m4), "\n")

# ==========================================================
# 14. Estrazione fixed effects con p-value
# ==========================================================
extract_fixed_effects <- function(mod, model_name) {
  sm <- as.data.frame(coef(summary(mod)))
  sm$term <- rownames(sm)
  rownames(sm) <- NULL

  names(sm) <- sub("^Estimate$", "estimate", names(sm))
  names(sm) <- sub("^Std\\. Error$", "std.error", names(sm))
  names(sm) <- sub("^t value$", "statistic", names(sm))
  names(sm) <- sub("^Pr\\(>\\|t\\|\\)$", "p.value", names(sm))

  if (!"df" %in% names(sm)) {
    sm$df <- df.residual(mod)
  }
  if (!"p.value" %in% names(sm)) {
    sm$p.value <- NA_real_
  }

  sm %>%
    mutate(
      model = model_name,
      engine = "lmerTest"
    ) %>%
    select(model, engine, term, estimate, std.error, df, statistic, p.value)
}

fixed_effects_table <- purrr::imap_dfr(model_list, function(mod, nm) {
  extract_fixed_effects(mod, nm)
})

cat("\nFixed effects table:\n")
print(as.data.frame(fixed_effects_table), row.names = FALSE)

# ==========================================================
# 15. Random effects
# ==========================================================
cat("\nRandom effects:\n")
for (nm in names(model_list)) {
  cat("\nVarCorr ", nm, ":\n", sep = "")
  print(VarCorr(model_list[[nm]]))
}

random_intercepts_m4 <- ranef(m4)$comune_key %>%
  tibble::rownames_to_column("comune_key") %>%
  dplyr::rename(random_intercept = `(Intercept)`) %>%
  dplyr::arrange(desc(random_intercept))

cat("\nTop 10 comuni per random intercept M4:\n")
print(head(random_intercepts_m4, 10))

cat("\nBottom 10 comuni per random intercept M4:\n")
print(tail(random_intercepts_m4, 10))

# ==========================================================
# 16. Summary del modello finale
# ==========================================================
cat("\nSummary modello finale (M4):\n")
print(summary(m4))

# ==========================================================
# 17. Diagnostica residui modello finale
# ==========================================================
analysis_df <- analysis_df %>%
  mutate(
    fitted_final = fitted(m4),
    resid_final  = resid(m4)
  )

par(mfrow = c(2, 2))

plot(
  analysis_df$fitted_final, analysis_df$resid_final,
  xlab = "Fitted", ylab = "Residuals",
  main = "Residuals vs Fitted - final model"
)
abline(h = 0, lty = 2)

qqnorm(analysis_df$resid_final, main = "QQ plot residuals - final model")
qqline(analysis_df$resid_final)

hist(
  analysis_df$resid_final, breaks = 40,
  main = "Histogram residuals - final model",
  xlab = "Residuals"
)

acf(analysis_df$resid_final, main = "ACF residuals - final model")

par(mfrow = c(1, 1))

# ==========================================================
# 18. Diagnostica identificativa sul turismo
#     Confronto tra arrivi osservati e arrivi depurati da
#     struttura, meteo e stagionalitÃ .
# ==========================================================
if (all(c("log_totale_arrivi_z", "arrivi_destag_weather_z") %in% names(analysis_df))) {
  cat("\n==========================================================\n")
  cat("CHECK TURISMO: arrivi osservati vs arrivi residualizzati\n")
  cat("==========================================================\n")

  check_collinearity(
    analysis_df,
    c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, "log_totale_arrivi_z"),
    "Tourism observed arrivals cumulative predictors"
  )

  check_collinearity(
    analysis_df,
    c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, "arrivi_destag_weather_z"),
    "Tourism residualized arrivals cumulative predictors"
  )

  cat("\nConfronto versioni turismo (mixed effects):\n")
  print(AIC(m3, m4, m4_arrivi_destag))
  print(BIC(m3, m4, m4_arrivi_destag))
  print(anova(m3, m4, m4_arrivi_destag))

  cat("\nSingularity tourism variants:\n")
  cat("m4_arrivi singular:", lme4::isSingular(m4, tol = 1e-5), "\n")
  cat("m4_arrivi_destag singular:", lme4::isSingular(m4_arrivi_destag, tol = 1e-5), "\n")
}

# ==========================================================
# 19. Oggetti finali utili
# ==========================================================
# analysis_df             -> dataset comune ai modelli
# m0, m1, m2, m3, m4      -> modelli mixed effects sequenziali
# comparison_table        -> confronto AIC/BIC/R2/singolaritÃ 
# fixed_effects_table     -> coefficienti con p-value
# random_intercepts_m4    -> random intercept dei comuni nel modello finale
# ==========================================================
