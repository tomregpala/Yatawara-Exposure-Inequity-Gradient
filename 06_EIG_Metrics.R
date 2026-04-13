# =============================================================================
# 06_EIG_Metrics.R
#
# EXPOSURE INEQUITY GRADIENT (EIG) — New Three-Group Disparity Metric
#
# WHAT IT COMPUTES (for each State × Pollutant × Year):
#   1. EIG_abs   — Absolute EIG (exposure units: µg/m³ or ppb)
#   2. EIG_rel   — Relative EIG (dimensionless, cross-pollutant comparable)
#   3. MGR       — Middle-Group Residual (curvature diagnostic)
#   4. Gap_pct   — Traditional low-high percentage gap (legacy)
#   5. Bootstrap 95% CIs for EIG_rel
#   6. Permutation p-value for H0: EIG = 0
#
# INPUTS:
#   - master_county_year.csv  (from 02_MasterCSVCreator.R)
#   - epa_data/               (raw EPA AQS daily CSVs)
#
# OUTPUTS:
#   - eig_annual_{STATE}.csv  (annual EIG metrics + CIs + p-values)
#   - eig_trend_{STATE}.csv   (trend in EIG_rel over time)
#
# MATHEMATICAL REFERENCE:
#   EIG_abs = Cov_π(R, μ) / Var_π(R)
#   where R_G are cumulative-rank midpoints and π_G are population shares.
#   See the technical report (EIG_Technical_Report.pdf) for full derivation.
#
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(nlme)



# =============================================================================
# CONFIG
# =============================================================================

STATE       <- "Nebraska"
MASTER_FILE <- "master_county_year.csv"
EPA_DIR     <- "epa_data"
YEARS       <- 2000:2023
POLL_DIRS   <- c(PM2.5 = "PM2.5", NO2 = "NO2", O3 = "O3", SO2 = "SO2", CO = "CO")
N_BOOT      <- 2000
N_PERM      <- 5000
SEED        <- 42
YEAR_CENTER <- mean(YEARS)   # 2011.5

state_slug <- gsub(" ", "_", STATE)
out_dir    <- file.path("States", STATE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_eig   <- file.path(out_dir, paste0("eig_annual_", state_slug, ".csv"))
out_trend <- file.path(out_dir, paste0("eig_trend_",  state_slug, ".csv"))

# =============================================================================
# HELPERS
# =============================================================================

normalize_county <- function(x) {
  x <- str_trim(x)
  # Replace n-tilde using R Unicode escapes (safe on all platforms).
  # Needed so "Dona Ana County" (EPA) matches "Do\u00f1a Ana County" (MHI).
  x <- gsub("\u00f1", "n", x)   # \u00f1 = n-tilde
  x <- gsub("\u00d1", "N", x)   # \u00d1 = N-tilde
  x %>%
    str_remove_all(regex(
      "\\s+(County|Parish|Borough|Census Area|Municipality|City and Borough|city|City)$",
      ignore_case = FALSE
    )) %>%
    str_trim() %>%
    str_to_upper()
}

parse_epa_date <- function(x) {
  lubridate::parse_date_time(x, orders = c("Ymd", "mdY"), quiet = TRUE) %>%
    as.Date()
}
# EPA-specific county name fixes applied after normalize_county.
# Handles cases where EPA spelling differs from Census/MHI files.
EPA_COUNTY_FIXES <- c(
  "SAINT CLAIR"           = "ST. CLAIR",        # Illinois: EPA "Saint Clair" vs MHI "St. Clair"
  "SKAGWAY-HOONAH-ANGOON" = "HOONAH-ANGOON"     # Alaska: pre-2007 combined borough
)



# =============================================================================
# CORE FUNCTION: compute_eig()
#
# Takes a data frame: County_Norm, Income_Group, Population, Concentration
# Returns a single-row tibble with all EIG metrics.
# =============================================================================

compute_eig <- function(df) {
  
  group_stats <- df %>%
    group_by(Income_Group) %>%
    summarise(
      W_G        = sum(Population,                         na.rm = TRUE),
      mu_G       = sum(Concentration * Population,         na.rm = TRUE) /
        sum(Population,                         na.rm = TRUE),
      n_counties = n(),
      .groups    = "drop"
    )
  
  na_row <- tibble(
    pi_L=NA, pi_M=NA, pi_H=NA,
    mu_L=NA, mu_M=NA, mu_H=NA, mu_bar=NA,
    R_H=NA, R_M=NA, R_L=NA,
    Cov_R_mu=NA, Var_R=NA,
    EIG_abs=NA, EIG_rel=NA, MGR=NA, Gap_pct=NA,
    n_cty_L=NA, n_cty_M=NA, n_cty_H=NA,
    note=NA_character_
  )
  
  has_low   <- "low"    %in% group_stats$Income_Group
  has_mid   <- "middle" %in% group_stats$Income_Group
  has_high  <- "high"   %in% group_stats$Income_Group
  has_all   <- has_low & has_mid & has_high
  has_lo_hi <- has_low & has_high
  
  get_val <- function(g, col) {
    v <- group_stats[[col]][group_stats$Income_Group == g]
    if (length(v) == 0) NA_real_ else v
  }
  
  n_L <- get_val("low",    "n_counties")
  n_M <- get_val("middle", "n_counties")
  n_H <- get_val("high",   "n_counties")
  
  if (!has_lo_hi)
    return(mutate(na_row,
                  n_cty_L=n_L, n_cty_M=n_M, n_cty_H=n_H,
                  note = "missing low or high group"))
  
  W_total <- sum(group_stats$W_G)
  pi_H <- get_val("high",   "W_G") / W_total
  pi_M <- if (has_mid) get_val("middle", "W_G") / W_total else 0
  pi_L <- get_val("low",    "W_G") / W_total
  mu_H <- get_val("high",   "mu_G")
  mu_M <- if (has_mid) get_val("middle", "mu_G") else NA_real_
  mu_L <- get_val("low",    "mu_G")
  
  # Gap_pct always computable with low + high
  Gap_pct <- ifelse(mu_H > 0, (mu_L - mu_H) / mu_H * 100, NA_real_)
  
  # Three-group metrics require all three groups
  mu_bar <- NA_real_
  R_H <- R_M <- R_L <- NA_real_
  Cov_R_mu <- Var_R <- NA_real_
  EIG_abs <- EIG_rel <- MGR <- NA_real_
  
  if (has_all) {
    mu_bar <- pi_L*mu_L + pi_M*mu_M + pi_H*mu_H
    
    if (!is.na(mu_bar) && mu_bar > 0) {
      R_H <- pi_H / 2
      R_M <- pi_H + pi_M / 2
      R_L <- 1 - pi_L / 2
      
      Cov_R_mu <- (pi_H*R_H*mu_H + pi_M*R_M*mu_M + pi_L*R_L*mu_L) - 0.5*mu_bar
      Var_R    <- (pi_H*R_H^2    + pi_M*R_M^2    + pi_L*R_L^2)    - 0.25
      
      EIG_abs <- Cov_R_mu / Var_R
      EIG_rel <- EIG_abs / mu_bar
      
      mu_M_pred <- mu_H + EIG_abs * (R_M - R_H)
      MGR       <- (mu_M - mu_M_pred) / mu_bar
    }
  }
  
  note_val <- if (has_all) "ok" else "middle group unmonitored — Gap_pct only"
  
  tibble(
    pi_L=pi_L, pi_M=pi_M, pi_H=pi_H,
    mu_L=mu_L, mu_M=mu_M, mu_H=mu_H, mu_bar=mu_bar,
    R_H=R_H, R_M=R_M, R_L=R_L,
    Cov_R_mu=Cov_R_mu, Var_R=Var_R,
    EIG_abs=EIG_abs, EIG_rel=EIG_rel, MGR=MGR, Gap_pct=Gap_pct,
    n_cty_L=n_L, n_cty_M=n_M, n_cty_H=n_H,
    note=note_val
  )
}

# =============================================================================
# BOOTSTRAP: compute_eig_boot_fast()
#
# VECTORISED — replaces the for-loop version with rmultinom + matrix multiply.
#
# For each group g with n_g counties, rmultinom() generates an n_g × B
# integer count matrix in one call. Each column is one bootstrap resample
# (county draw counts). Weighted group means across all B replicates are
# then computed via two matrix multiplications — no R loop over replicates.
#
#   mu_g_b[b] = ( K[,b] · (pop * conc) ) / ( K[,b] · pop )
#             = t(K) %*% (pop*conc)  /  t(K) %*% pop
#
# ~50-100x faster than the sequential for-loop.
# =============================================================================

compute_eig_boot_fast <- function(df, n_boot = 2000, seed_val = 42) {
  
  set.seed(seed_val)
  groups <- split(df, df$Income_Group)
  
  fail <- tibble(EIG_rel_boot_mean=NA, EIG_rel_boot_se=NA,
                 EIG_rel_ci_lo=NA,    EIG_rel_ci_hi=NA,
                 n_boot_valid=0L)
  
  if (!all(c("low","middle","high") %in% names(groups))) return(fail)
  
  # Generate n_g × n_boot count matrix and compute B weighted means per group
  get_boot_stats <- function(grp) {
    n  <- nrow(grp)
    cp <- grp$Concentration * grp$Population
    p  <- grp$Population
    K  <- rmultinom(n_boot, n, rep(1/n, n))   # n × n_boot
    mu <- as.vector(t(K) %*% cp) / as.vector(t(K) %*% p)
    W  <- as.vector(t(K) %*% p)
    list(mu = mu, W = W)
  }
  
  s_H <- get_boot_stats(groups[["high"]])
  s_M <- get_boot_stats(groups[["middle"]])
  s_L <- get_boot_stats(groups[["low"]])
  
  W_total <- s_H$W + s_M$W + s_L$W
  pi_H <- s_H$W / W_total
  pi_M <- s_M$W / W_total
  pi_L <- s_L$W / W_total
  
  mu_bar <- pi_H*s_H$mu + pi_M*s_M$mu + pi_L*s_L$mu
  
  # Rank midpoints (vectors of length n_boot)
  R_H <- pi_H / 2
  R_M <- pi_H + pi_M / 2
  R_L <- 1 - pi_L / 2
  
  Cov_b <- (pi_H*R_H*s_H$mu + pi_M*R_M*s_M$mu + pi_L*R_L*s_L$mu) - 0.5*mu_bar
  Var_b <- (pi_H*R_H^2      + pi_M*R_M^2      + pi_L*R_L^2)      - 0.25
  
  eig_b <- (Cov_b / Var_b) / mu_bar
  eig_b <- eig_b[is.finite(eig_b) & !is.na(mu_bar) & mu_bar > 0]
  
  if (length(eig_b) < 100) return(mutate(fail, n_boot_valid = length(eig_b)))
  
  tibble(
    EIG_rel_boot_mean = mean(eig_b),
    EIG_rel_boot_se   = sd(eig_b),
    EIG_rel_ci_lo     = quantile(eig_b, 0.025, names = FALSE),
    EIG_rel_ci_hi     = quantile(eig_b, 0.975, names = FALSE),
    n_boot_valid      = length(eig_b)
  )
}

# =============================================================================
# PERMUTATION TEST: permutation_test_eig_fast()
#
# =============================================================================

permutation_test_eig_fast <- function(df, n_perm = 5000, seed_val = 42) {
  
  set.seed(seed_val)
  
  obs     <- compute_eig(df)
  obs_eig <- abs(obs$EIG_rel)
  
  if (is.na(obs_eig)) return(tibble(perm_p_value = NA_real_, n_perm = n_perm))
  
  # Vectorised permutations via indicator matrix multiplication
  labels <- match(df$Income_Group, c("high","middle","low"))
  pop    <- df$Population
  wc     <- df$Concentration * df$Population
  n      <- nrow(df)
  
  # n × n_perm label matrix — each column is one permutation
  P   <- replicate(n_perm, sample(labels))
  I_H <- (P == 1L); I_M <- (P == 2L); I_L <- (P == 3L)
  
  W_H <- as.vector(pop %*% I_H);  WC_H <- as.vector(wc %*% I_H)
  W_M <- as.vector(pop %*% I_M);  WC_M <- as.vector(wc %*% I_M)
  W_L <- as.vector(pop %*% I_L);  WC_L <- as.vector(wc %*% I_L)
  
  W_tot_b <- W_H + W_M + W_L
  pH_b <- W_H/W_tot_b;  pM_b <- W_M/W_tot_b;  pL_b <- W_L/W_tot_b
  mH_b <- WC_H/W_H;     mM_b <- WC_M/W_M;     mL_b <- WC_L/W_L
  mb_b <- pH_b*mH_b + pM_b*mM_b + pL_b*mL_b
  
  rH_b <- pH_b/2;  rM_b <- pH_b + pM_b/2;  rL_b <- 1 - pL_b/2
  cov_b <- (pH_b*rH_b*mH_b + pM_b*rM_b*mM_b + pL_b*rL_b*mL_b) - 0.5*mb_b
  var_b <- (pH_b*rH_b^2    + pM_b*rM_b^2    + pL_b*rL_b^2)    - 0.25
  
  perm_eig <- abs((cov_b / var_b) / mb_b)
  valid    <- is.finite(perm_eig) & !is.na(mb_b) & mb_b > 0
  
  tibble(
    perm_p_value = sum(valid & perm_eig >= obs_eig, na.rm = TRUE) / n_perm,
    n_perm       = n_perm
  )
}

# =============================================================================
# SECTION 1: LOAD DATA
# =============================================================================

cat("== Loading master data for", STATE, "==\n")

master <- read_csv(MASTER_FILE, show_col_types = FALSE) %>%
  filter(State == STATE)

mhi_ref <- master %>% distinct(County, MHI) %>% filter(!is.na(MHI))

q30 <- quantile(mhi_ref$MHI, 0.30, na.rm = TRUE)
q70 <- quantile(mhi_ref$MHI, 0.70, na.rm = TRUE)

county_groups <- mhi_ref %>%
  mutate(
    County_Norm  = normalize_county(County),
    Income_Group = case_when(
      MHI <= q30 ~ "low",
      MHI <= q70 ~ "middle",
      TRUE       ~ "high"
    )
  ) %>%
  select(County_Norm, Income_Group)

pop_lookup <- master %>%
  mutate(County_Norm = normalize_county(County)) %>%
  select(County_Norm, Year, Population) %>%
  filter(!is.na(Population))

cat(sprintf("  q30=$%s | q70=$%s\n",
            format(round(q30), big.mark=","),
            format(round(q70), big.mark=",")))
cat(sprintf("  low: %d | middle: %d | high: %d\n",
            sum(county_groups$Income_Group=="low"),
            sum(county_groups$Income_Group=="middle"),
            sum(county_groups$Income_Group=="high")))

# =============================================================================
# SECTION 2: LOAD EPA DAILY DATA
# =============================================================================

cat("\n== Loading EPA daily data ==\n")

epa_col_types <- cols_only(
  `State Name`      = col_character(),
  `County Name`     = col_character(),
  `Date Local`      = col_character(),
  `Arithmetic Mean` = col_double()
)

load_state_epa <- function(poll_dir, poll_label) {
  files <- list.files(file.path(EPA_DIR, poll_dir), pattern="\\.csv$", full.names=TRUE)
  files      <- files[str_detect(basename(files), "(?<=_)\\d{4}(?=\\.csv$)")]
  file_years <- as.integer(str_extract(basename(files), "(?<=_)\\d{4}(?=\\.csv$)"))
  files      <- files[file_years %in% YEARS]
  if (length(files) == 0) { warning("No files for ", poll_label); return(NULL) }
  cat(sprintf("  %s: %d files\n", poll_label, length(files)))
  raw <- map_dfr(files, function(f) {
    read_csv(f, col_types = epa_col_types, show_col_types = FALSE) %>%
      filter(`State Name` == STATE,
             !str_detect(`County Name`, regex("mobile monitor", ignore_case = TRUE)))
  })
  
  if (nrow(raw) == 0) {
    return(tibble(
      Pollutant   = character(),
      County_Norm = character(),
      Year        = integer(),
      Concentration = double()
    ))
  }
  
  raw %>%
    mutate(
      Pollutant   = poll_label,
      County_Norm = as.character({
        nm <- normalize_county(`County Name`)
        ifelse(nm %in% names(EPA_COUNTY_FIXES), EPA_COUNTY_FIXES[nm], nm)
      }),
      Date        = parse_epa_date(`Date Local`),
      Year        = year(Date)
    ) %>%
    select(Pollutant, County_Norm, Year, Concentration = `Arithmetic Mean`)
}

epa_daily <- imap_dfr(POLL_DIRS, ~ load_state_epa(.x, .y))

# =============================================================================
# SECTION 3: ANNUAL COUNTY-LEVEL EXPOSURES
# =============================================================================

cat("\n== Computing annual county-level exposures ==\n")

annual_enriched <- epa_daily %>%
  filter(!is.na(Concentration)) %>%
  group_by(Pollutant, County_Norm, Year) %>%
  summarise(
    Concentration = mean(Concentration, na.rm = TRUE),
    N_Days        = n(),
    .groups = "drop"
  ) %>%
  left_join(county_groups, by = "County_Norm") %>%
  left_join(pop_lookup,    by = c("County_Norm", "Year")) %>%
  filter(!is.na(Income_Group), !is.na(Population), !is.na(Concentration))

cat(sprintf("  Enriched rows: %s\n", format(nrow(annual_enriched), big.mark=",")))

poll_years <- annual_enriched %>%
  distinct(Pollutant, Year) %>%
  arrange(Pollutant, Year)

# =============================================================================
# SECTION 4: COMPUTE EIG POINT ESTIMATES
# =============================================================================

cat("\n== Computing EIG point estimates ==\n")

eig_point <- map2_dfr(
  poll_years$Pollutant, poll_years$Year,
  function(poll, yr) {
    df_sub <- annual_enriched %>% filter(Pollutant == poll, Year == yr)
    eig    <- compute_eig(df_sub)
    bind_cols(tibble(State = STATE, Pollutant = poll, Year = yr), eig)
  }
) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA_real_, .)))

cat(sprintf("  Point estimates: %d rows\n", nrow(eig_point)))

# =============================================================================
# SECTION 5: BOOTSTRAP CIs
# =============================================================================

cat("\n== Bootstrap CIs for EIG_rel ==\n")

boot_results <- map2_dfr(
  poll_years$Pollutant, poll_years$Year,
  function(poll, yr) {
    df_sub <- annual_enriched %>% filter(Pollutant == poll, Year == yr)
    ci     <- compute_eig_boot_fast(df_sub, n_boot = N_BOOT, seed_val = SEED + yr)
    bind_cols(tibble(Pollutant = poll, Year = yr), ci)
  }
)

cat(sprintf("  Bootstrap complete: %d rows\n", nrow(boot_results)))

# =============================================================================
# SECTION 6: PERMUTATION TESTS
# =============================================================================

cat("\n== Permutation tests for H0: EIG = 0 ==\n")

perm_results <- map2_dfr(
  poll_years$Pollutant, poll_years$Year,
  function(poll, yr) {
    df_sub <- annual_enriched %>% filter(Pollutant == poll, Year == yr)
    pv     <- permutation_test_eig_fast(df_sub, n_perm = N_PERM,
                                        seed_val = SEED + yr + 1000L)
    bind_cols(tibble(Pollutant = poll, Year = yr), pv)
  }
)

cat(sprintf("  Permutation tests complete: %d rows\n", nrow(perm_results)))

# =============================================================================
# SECTION 7: MERGE AND WRITE
# =============================================================================

cat("\n== Merging and writing outputs ==\n")

eig_results <- eig_point %>%
  left_join(boot_results, by = c("Pollutant", "Year")) %>%
  left_join(perm_results, by = c("Pollutant", "Year"))

write_csv(eig_results, out_eig, na = "")
cat(sprintf("  Saved: %s (%d rows)\n", basename(out_eig), nrow(eig_results)))

# =============================================================================
# SECTION 8: TREND MODEL ON EIG_rel
# =============================================================================

cat("\n== Fitting trend models on EIG_rel ==\n")

fit_eig_trend <- function(df_poll) {
  poll   <- unique(df_poll$Pollutant)
  df_fit <- df_poll %>% filter(!is.na(EIG_rel)) %>% mutate(Year_c = Year - YEAR_CENTER)
  
  if (nrow(df_fit) < 5) {
    cat(sprintf("  %-7s SKIP (< 5 years)\n", poll))
    return(tibble(State=STATE, Pollutant=poll,
                  trend_intercept=NA, trend_slope=NA,
                  trend_se=NA, trend_p=NA, n_years=nrow(df_fit)))
  }
  
  fit <- lm(EIG_rel ~ Year_c, data = df_fit)
  tt  <- summary(fit)$coefficients
  cat(sprintf("  %-7s slope=%.6f p=%.4f\n",
              poll, tt["Year_c","Estimate"], tt["Year_c","Pr(>|t|)"]))
  
  tibble(
    State           = STATE,
    Pollutant       = poll,
    trend_intercept = tt["(Intercept)","Estimate"],
    trend_slope     = tt["Year_c","Estimate"],
    trend_se        = tt["Year_c","Std. Error"],
    trend_p         = tt["Year_c","Pr(>|t|)"],
    n_years         = nrow(df_fit)
  )
}

trend_results <- eig_results %>%
  group_by(Pollutant) %>%
  group_split() %>%
  map_dfr(fit_eig_trend)

write_csv(trend_results, out_trend, na = "")
cat(sprintf("  Saved: %s (%d rows)\n", basename(out_trend), nrow(trend_results)))


# =============================================================================
# SUMMARY
# =============================================================================

cat("\n== EIG SUMMARY ==\n\n")

eig_results %>%
  group_by(Pollutant) %>%
  summarise(
    Years_total    = n(),
    Years_valid    = sum(!is.na(EIG_rel)),
    Coverage_note  = ifelse(
      sum(!is.na(EIG_rel)) == 0,
      "NO VALID YEARS — all groups missing monitor data",
      ifelse(sum(!is.na(EIG_rel)) < 5, "SPARSE — too few years for trend", "OK")
    ),
    Mean_EIG_rel   = round(mean(EIG_rel,      na.rm=TRUE), 4),
    SD_EIG_rel     = round(sd(EIG_rel,        na.rm=TRUE), 4),
    Mean_MGR       = round(mean(MGR,           na.rm=TRUE), 4),
    Pct_Positive   = round(100 * mean(EIG_rel > 0, na.rm=TRUE), 1),
    Med_perm_p     = round(median(perm_p_value, na.rm=TRUE), 4),
    .groups = "drop"
  ) %>%
  print(n=Inf, width=Inf)

cat("\n-- Trend results --\n\n")
trend_results %>%
  select(Pollutant, trend_slope, trend_se, trend_p, n_years) %>%
  mutate(across(c(trend_slope, trend_se, trend_p), ~ round(., 5))) %>%
  print(n=Inf, width=Inf)

cat("\nDone.\n")