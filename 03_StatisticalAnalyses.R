# =============================================================================
# 03_StatisticalAnalyses.R — COMPREHENSIVE DISPARITY METRICS
#
# Computes ALL disparity metrics for one state.
#
#   EXISTING METRICS:
#     Eq. 4  Absolute Disparity    A_t = Y_low - Y_high         [Harper & Lynch 2006]
#     Eq. 5  Relative Ratio        R_t = Y_low / Y_high         [WHO HEAT 2023]
#     Eq. 7  Concentration Index   RCI_t                         [Wagstaff et al. 1991]
#     Eq. 8  Slope Index           SII_t                         [Mackenbach & Kunst 1997]
#     Eq. 9  Between-Group Var     BGV_t, BGSD_t                [WHO HEAT 2023]
#
#   NEW METRICS:
#     Eq. 10 EIG (absolute)        EIG_abs = Cov_pi(R,mu)/Var_pi(R)   [This work]
#     Eq. 11 EIG (relative)        EIG_rel = EIG_abs / mu_bar         [This work]
#     Eq. 12 Middle-Group Residual MGR                                 [This work]
#
#   INFERENCE:
#     Bootstrap 95% CIs for EIG_rel (stratified, B = 2000)
#     Permutation p-value for H0: EIG = 0 (B = 5000)
#     Trend models on EIG_rel and daily D_d over time
#
# INPUTS:
#   master_county_year.csv   (from 02_MasterCSVCreator.R)
#   epa_data/                (raw EPA AQS daily CSVs, from 01_ExtractEPAData.R)
#
# OUTPUTS (written to States/[StateName]/):
#   daily_disparity_{STATE}.csv
#   annual_all_metrics_{STATE}.csv
#   trend_summary_{STATE}.csv
#   coverage_diagnostic_{STATE}.csv
#
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(nlme)


# Detect available cores and set up parallel backend.
# Uses all physical cores minus 1 to leave the OS responsive.

# =============================================================================
# CONFIG
# =============================================================================

STATE       <- "Nebraska"
EPA_DIR     <- "epa_data"
MASTER_FILE <- "master_county_year.csv"
YEARS       <- 2000:2023

POLL_DIRS <- c(PM2.5 = "PM2.5", NO2 = "NO2", O3 = "O3", SO2 = "SO2", CO = "CO")

N_BOOT  <- 2000
N_PERM  <- 5000
SEED    <- 42
MIN_CTY_PER_GROUP <- 2L  # Minimum counties per income group for valid inference.
# Groups with fewer counties get NA metrics/CIs.
# Affects states with very few counties (e.g. Rhode Island).

state_slug <- gsub(" ", "_", STATE)
out_dir    <- file.path("States", STATE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_daily    <- file.path(out_dir, paste0("daily_disparity_",     state_slug, ".csv"))
out_annual   <- file.path(out_dir, paste0("annual_all_metrics_",  state_slug, ".csv"))
out_trend    <- file.path(out_dir, paste0("trend_summary_",       state_slug, ".csv"))
out_coverage <- file.path(out_dir, paste0("coverage_diagnostic_", state_slug, ".csv"))

YEAR_CENTER <- mean(YEARS)   # 2011.5

# =============================================================================
# HELPER FUNCTIONS
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
# SECTION 1: INCOME GROUP ASSIGNMENT
# =============================================================================

cat("==========================================================\n")
cat("  COMPREHENSIVE DISPARITY METRICS\n")
cat(sprintf("  State: %s\n", STATE))
cat("==========================================================\n\n")

cat("== SECTION 1: Income group assignment ==\n")

master <- read_csv(MASTER_FILE, show_col_types = FALSE) %>%
  filter(State == STATE)

if (nrow(master) == 0) stop("State '", STATE, "' not found in master CSV.")

mhi_ref <- master %>%
  distinct(County, MHI) %>%
  filter(!is.na(MHI))

q30 <- quantile(mhi_ref$MHI, 0.30, na.rm = TRUE)
q70 <- quantile(mhi_ref$MHI, 0.70, na.rm = TRUE)

cat(sprintf("  Counties with MHI: %d\n", nrow(mhi_ref)))
cat(sprintf("  30th pct: $%s | 70th pct: $%s\n",
            format(round(q30), big.mark = ","),
            format(round(q70), big.mark = ",")))

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

cat(sprintf("  low: %d | middle: %d | high: %d\n",
            sum(county_groups$Income_Group == "low"),
            sum(county_groups$Income_Group == "middle"),
            sum(county_groups$Income_Group == "high")))

pop_lookup <- master %>%
  mutate(County_Norm = normalize_county(County)) %>%
  select(County_Norm, Year, Population) %>%
  filter(!is.na(Population))

# =============================================================================
# SECTION 2: LOAD RAW EPA DAILY DATA
# =============================================================================

cat("\n== SECTION 2: Loading raw EPA daily data for", STATE, "==\n")

epa_col_types <- cols_only(
  `State Name`      = col_character(),
  `County Name`     = col_character(),
  `Date Local`      = col_character(),
  `Arithmetic Mean` = col_double()
)

load_state_epa <- function(poll_dir, poll_label) {
  files <- list.files(file.path(EPA_DIR, poll_dir), pattern = "\\.csv$", full.names = TRUE)
  files      <- files[str_detect(basename(files), "(?<=_)\\d{4}(?=\\.csv$)")]
  file_years <- as.integer(str_extract(basename(files), "(?<=_)\\d{4}(?=\\.csv$)"))
  files      <- files[file_years %in% YEARS]
  
  if (length(files) == 0) { warning("No files found for ", poll_label); return(NULL) }
  cat(sprintf("  %s: %d files\n", poll_label, length(files)))
  
  raw <- map_dfr(files, function(f) {
    read_csv(f, col_types = epa_col_types, show_col_types = FALSE) %>%
      filter(`State Name` == STATE,
             !str_detect(`County Name`, regex("mobile monitor", ignore_case = TRUE)))
  })
  
  # Guard: return a typed empty tibble when this state has no rows in the
  # EPA files for this pollutant.  Without this, the mutate + ifelse chain
  # over a 0-row input can infer County_Norm as <logical> instead of
  # <character>, causing a type clash when imap_dfr binds all pollutants.
  if (nrow(raw) == 0) {
    return(tibble(
      Pollutant   = character(),
      County_Norm = character(),
      Date        = as.Date(character()),
      Year        = integer(),
      DOY         = integer(),
      Concentration = double()
    ))
  }
  
  raw %>%
    mutate(
      Pollutant   = poll_label,
      County_Norm = as.character({   # explicit coercion guards against
        nm <- normalize_county(`County Name`)  # logical type on empty input
        ifelse(nm %in% names(EPA_COUNTY_FIXES), EPA_COUNTY_FIXES[nm], nm)
      }),
      Date        = parse_epa_date(`Date Local`),
      Year        = year(Date),
      DOY         = yday(Date)
    ) %>%
    select(Pollutant, County_Norm, Date, Year, DOY,
           Concentration = `Arithmetic Mean`)
}

epa_daily <- imap_dfr(POLL_DIRS, ~ load_state_epa(.x, .y))
cat(sprintf("  Total daily rows: %s\n", format(nrow(epa_daily), big.mark = ",")))

# =============================================================================
# SECTION 3: JOIN EPA WITH POPULATION AND INCOME GROUPS
# =============================================================================

cat("\n== SECTION 3: Joining EPA with population and income groups ==\n")

epa_enriched <- epa_daily %>%
  left_join(county_groups, by = "County_Norm") %>%
  left_join(pop_lookup,    by = c("County_Norm", "Year")) %>%
  filter(!is.na(Income_Group), !is.na(Population), !is.na(Concentration))

cat(sprintf("  Retained: %s | Dropped: %s\n",
            format(nrow(epa_enriched), big.mark = ","),
            format(nrow(epa_daily) - nrow(epa_enriched), big.mark = ",")))

# =============================================================================
# SECTION 4: MONITOR COVERAGE DIAGNOSTIC
# =============================================================================

cat("\n== SECTION 4: Monitor coverage diagnostic ==\n")

group_totals <- county_groups %>% count(Income_Group, name = "N_Counties_in_Group")

coverage <- epa_enriched %>%
  group_by(Pollutant, Income_Group) %>%
  summarise(
    N_Counties_Monitored = n_distinct(County_Norm),
    N_County_Days        = n(),
    Date_Min             = min(Date),
    Date_Max             = max(Date),
    .groups = "drop"
  ) %>%
  complete(
    Pollutant    = names(POLL_DIRS),
    Income_Group = c("low", "middle", "high"),
    fill = list(N_Counties_Monitored = 0L, N_County_Days = 0L,
                Date_Min = NA, Date_Max = NA)
  ) %>%
  left_join(group_totals, by = "Income_Group") %>%
  mutate(
    Pct_Monitored = round(100 * N_Counties_Monitored / N_Counties_in_Group, 1),
    Flag = case_when(
      N_Counties_Monitored == 0 ~ "NO DATA",
      Pct_Monitored < 20        ~ "SPARSE",
      TRUE                      ~ "OK"
    )
  ) %>%
  arrange(Pollutant, factor(Income_Group, levels = c("low", "middle", "high")))

write_csv(coverage, out_coverage, na = "")
cat(sprintf("  Saved: %s\n", basename(out_coverage)))

# =============================================================================
# SECTION 5: DAILY POPULATION-WEIGHTED MEAN EXPOSURE (Eq. 1)
# =============================================================================

cat("\n== SECTION 5: Daily PW mean exposure (Eq. 1) ==\n")

daily_pw <- epa_enriched %>%
  group_by(Pollutant, Income_Group, Date, Year, DOY) %>%
  summarise(
    PW_Mean    = sum(Concentration * Population) / sum(Population),
    N_Counties = n(),
    .groups    = "drop"
  )

daily_wide <- daily_pw %>%
  pivot_wider(
    id_cols     = c(Pollutant, Date, Year, DOY),
    names_from  = Income_Group,
    values_from = c(PW_Mean, N_Counties)
  ) %>%
  rename_with(~ str_replace(., "PW_Mean_",    "X_bar_"), starts_with("PW_Mean_")) %>%
  rename_with(~ str_replace(., "N_Counties_", "N_cty_"), starts_with("N_Counties_"))

# Guarantee all 6 columns exist even when a group has zero monitored counties
for (col in c("X_bar_low","X_bar_middle","X_bar_high",
              "N_cty_low","N_cty_middle","N_cty_high")) {
  if (!col %in% names(daily_wide)) daily_wide[[col]] <- NA_real_
}

daily_wide <- daily_wide %>%
  mutate(State = STATE, D_d = X_bar_low - X_bar_high) %>%
  select(State, Pollutant, Date, Year, DOY,
         X_bar_low, X_bar_middle, X_bar_high, D_d,
         N_cty_low, N_cty_middle, N_cty_high) %>%
  arrange(Pollutant, Date)

write_csv(daily_wide, out_daily, na = "")
cat(sprintf("  Daily rows: %s | Saved: %s\n",
            format(nrow(daily_wide), big.mark = ","), basename(out_daily)))

# =============================================================================
# SECTION 6: ANNUAL COUNTY-LEVEL SUMMARIES
# =============================================================================

cat("\n== SECTION 6: Annual county-level summaries ==\n")

annual_county <- epa_enriched %>%
  group_by(Pollutant, County_Norm, Year) %>%
  summarise(Ann_Conc = mean(Concentration, na.rm = TRUE), N_Days = n(), .groups = "drop") %>%
  left_join(county_groups, by = "County_Norm") %>%
  left_join(pop_lookup,    by = c("County_Norm", "Year")) %>%
  filter(!is.na(Income_Group), !is.na(Population), !is.na(Ann_Conc))

cat(sprintf("  County-year-pollutant rows: %s\n", format(nrow(annual_county), big.mark = ",")))

# =============================================================================
# SECTION 7: COMPUTE ALL ANNUAL DISPARITY METRICS
#
#   Eq. 4  A_t     = mu_L - mu_H
#   Eq. 5  R_t     = mu_L / mu_H
#   Eq. 7  RCI     = 4 * Cov_pi(R,mu) / mu_bar
#   Eq. 8  SII     = Cov_pi(R,mu) / Var_pi(R)
#   Eq. 9  BGV     = sum_G pi_G * (mu_G - mu_bar)^2
#   Eq. 10 EIG_abs = SII
#   Eq. 11 EIG_rel = EIG_abs / mu_bar
#   Eq. 12 MGR     = (mu_M - mu_M_predicted) / mu_bar
# =============================================================================

cat("\n== SECTION 7: Computing ALL annual disparity metrics ==\n")

compute_all_metrics <- function(county_df, daily_df_year) {
  
  grp <- county_df %>%
    group_by(Income_Group) %>%
    summarise(
      W_G  = sum(Population, na.rm = TRUE),
      mu_G = sum(Ann_Conc * Population, na.rm = TRUE) / sum(Population, na.rm = TRUE),
      n_cty = n(),
      .groups = "drop"
    )
  
  na_row <- tibble(
    mu_L=NA, mu_M=NA, mu_H=NA, mu_bar=NA,
    pi_L=NA, pi_M=NA, pi_H=NA,
    R_H=NA, R_M=NA, R_L=NA,
    A_t=NA, R_t=NA, P_t=NA, Gap_pct=NA,
    BGV=NA, BGSD=NA, RCI=NA, SII=NA,
    EIG_abs=NA, EIG_rel=NA, MGR=NA,
    Cov_R_mu=NA, Var_R=NA,
    n_cty_L=NA, n_cty_M=NA, n_cty_H=NA, note=NA_character_
  )
  
  # Identify which groups have monitor data this year
  has_low    <- "low"    %in% grp$Income_Group
  has_mid    <- "middle" %in% grp$Income_Group
  has_high   <- "high"   %in% grp$Income_Group
  has_all    <- has_low & has_mid & has_high
  has_lo_hi  <- has_low & has_high   # minimum for two-group metrics
  
  get <- function(g, col) {
    v <- grp[[col]][grp$Income_Group == g]
    if (length(v) == 0) NA_real_ else v
  }
  
  n_L <- get("low",    "n_cty")
  n_M <- get("middle", "n_cty")
  n_H <- get("high",   "n_cty")
  
  # Cannot compute anything without at least low and high
  if (!has_lo_hi)
    return(mutate(na_row,
                  n_cty_L = n_L, n_cty_M = n_M, n_cty_H = n_H,
                  note = "missing low or high group"))
  
  W_total <- sum(grp$W_G)
  pi_H <- get("high",   "W_G") / W_total
  pi_M <- if (has_mid) get("middle", "W_G") / W_total else 0
  pi_L <- get("low",    "W_G") / W_total
  mu_H <- get("high",   "mu_G")
  mu_M <- if (has_mid) get("middle", "mu_G") else NA_real_
  mu_L <- get("low",    "mu_G")
  
  # Two-group metrics — always computable when low + high present
  A_t     <- mu_L - mu_H
  R_t     <- ifelse(mu_H > 0, mu_L / mu_H, NA_real_)
  Gap_pct <- ifelse(mu_H > 0, (mu_L - mu_H) / mu_H * 100, NA_real_)
  
  P_t <- NA_real_
  if (!is.null(daily_df_year) && nrow(daily_df_year) > 0) {
    d_vals <- daily_df_year$D_d[!is.na(daily_df_year$D_d)]
    if (length(d_vals) > 0) P_t <- 100 * mean(d_vals > 0)
  }
  
  # Three-group metrics require all three groups
  mu_bar <- NA_real_
  BGV <- BGSD <- RCI <- SII <- EIG_abs <- EIG_rel <- MGR <- NA_real_
  Cov_R_mu <- Var_R <- NA_real_
  R_H_val <- R_M_val <- R_L_val <- NA_real_
  
  if (has_all) {
    mu_bar <- pi_L*mu_L + pi_M*mu_M + pi_H*mu_H
    
    if (!is.na(mu_bar) && mu_bar > 0) {
      BGV  <- pi_L*(mu_L-mu_bar)^2 + pi_M*(mu_M-mu_bar)^2 + pi_H*(mu_H-mu_bar)^2
      BGSD <- sqrt(BGV)
      
      R_H_val  <- pi_H / 2
      R_M_val  <- pi_H + pi_M / 2
      R_L_val  <- 1 - pi_L / 2
      
      Cov_R_mu <- (pi_H*R_H_val*mu_H + pi_M*R_M_val*mu_M + pi_L*R_L_val*mu_L) - 0.5*mu_bar
      Var_R    <- (pi_H*R_H_val^2    + pi_M*R_M_val^2    + pi_L*R_L_val^2)    - 0.25
      
      RCI     <- 4 * Cov_R_mu / mu_bar
      SII     <- Cov_R_mu / Var_R
      EIG_abs <- SII
      EIG_rel <- EIG_abs / mu_bar
      
      mu_M_pred <- mu_H + EIG_abs * (R_M_val - R_H_val)
      MGR       <- (mu_M - mu_M_pred) / mu_bar
    }
  }
  
  # Note records which groups were available
  note_val <- if (has_all) "ok" else "middle group unmonitored — two-group metrics only"
  
  tibble(
    mu_L=mu_L, mu_M=mu_M, mu_H=mu_H, mu_bar=mu_bar,
    pi_L=pi_L, pi_M=pi_M, pi_H=pi_H,
    R_H=R_H_val, R_M=R_M_val, R_L=R_L_val,
    A_t=A_t, R_t=R_t, P_t=P_t, Gap_pct=Gap_pct,
    BGV=BGV, BGSD=BGSD,
    RCI=RCI, SII=SII,
    EIG_abs=EIG_abs, EIG_rel=EIG_rel, MGR=MGR,
    Cov_R_mu=Cov_R_mu, Var_R=Var_R,
    n_cty_L=n_L, n_cty_M=n_M, n_cty_H=n_H,
    note=note_val
  )
}

poll_years <- annual_county %>% distinct(Pollutant, Year) %>% arrange(Pollutant, Year)

# Parallelise across pollutant-year combinations
annual_metrics <- map2_dfr(
  poll_years$Pollutant, poll_years$Year,
  function(poll, yr) {
    cty_df <- annual_county %>% filter(Pollutant == poll, Year == yr)
    day_df <- daily_wide   %>% filter(Pollutant == poll, Year == yr)
    metrics <- compute_all_metrics(cty_df, day_df)
    bind_cols(tibble(State = STATE, Pollutant = poll, Year = yr), metrics)
  }
) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA_real_, .)))

cat(sprintf("  Annual metric rows: %d\n", nrow(annual_metrics)))

# =============================================================================
# SECTION 8: BOOTSTRAP CONFIDENCE INTERVALS FOR EIG_rel
# =============================================================================

cat("\n== SECTION 8: Bootstrap CIs for EIG_rel ==\n")

compute_boot_ci_fast <- function(county_df, n_boot, seed_val) {
  
  set.seed(seed_val)
  groups <- split(county_df, county_df$Income_Group)
  
  fail <- tibble(EIG_rel_ci_lo=NA, EIG_rel_ci_hi=NA,
                 EIG_rel_boot_se=NA, n_boot_valid=0L)
  
  if (!all(c("low","middle","high") %in% names(groups))) return(fail)
  
  # Need at least MIN_CTY_PER_GROUP counties per group for bootstrap to vary
  if (any(sapply(groups, nrow) < MIN_CTY_PER_GROUP)) return(fail)
  
  get_boot_stats <- function(grp) {
    n  <- nrow(grp)
    cp <- grp$Ann_Conc * grp$Population
    p  <- grp$Population
    # rmultinom: each column is a multinomial draw of size n from uniform probs
    K  <- rmultinom(n_boot, n, rep(1/n, n))   # n × n_boot
    mu <- as.vector(t(K) %*% cp) / as.vector(t(K) %*% p)   # B-vector of means
    W  <- as.vector(t(K) %*% p)                              # B-vector of total pop
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
  
  # Rank midpoints (vectors of length B)
  R_H <- pi_H / 2
  R_M <- pi_H + pi_M / 2
  R_L <- 1 - pi_L / 2
  
  Cov_b <- (pi_H*R_H*s_H$mu + pi_M*R_M*s_M$mu + pi_L*R_L*s_L$mu) - 0.5*mu_bar
  Var_b <- (pi_H*R_H^2      + pi_M*R_M^2      + pi_L*R_L^2)      - 0.25
  
  eig_b <- (Cov_b / Var_b) / mu_bar
  eig_b <- eig_b[is.finite(eig_b) & !is.na(mu_bar) & mu_bar > 0]
  
  if (length(eig_b) < 100) return(mutate(fail, n_boot_valid = length(eig_b)))
  
  tibble(
    EIG_rel_ci_lo   = quantile(eig_b, 0.025, names = FALSE),
    EIG_rel_ci_hi   = quantile(eig_b, 0.975, names = FALSE),
    EIG_rel_boot_se = sd(eig_b),
    n_boot_valid    = length(eig_b)
  )
}

boot_results <- map2_dfr(
  poll_years$Pollutant, poll_years$Year,
  function(poll, yr) {
    ci <- compute_boot_ci_fast(
      annual_county %>% filter(Pollutant == poll, Year == yr),
      N_BOOT, SEED + yr
    )
    bind_cols(tibble(Pollutant = poll, Year = yr), ci)
  }
)

cat(sprintf("  Bootstrap complete: %d rows\n", nrow(boot_results)))

# =============================================================================
# SECTION 9: PERMUTATION TEST FOR H0: EIG = 0
# =============================================================================

cat("\n== SECTION 9: Permutation test for H0: EIG = 0 ==\n")

compute_perm_p_fast <- function(county_df, n_perm, seed_val) {
  
  set.seed(seed_val)
  
  # Observed EIG
  grp <- county_df %>%
    group_by(Income_Group) %>%
    summarise(W=sum(Population), mu=sum(Ann_Conc*Population)/sum(Population), .groups="drop")
  
  if (nrow(grp) < 3) return(tibble(perm_p_value = NA_real_))
  
  # Need at least MIN_CTY_PER_GROUP per group for permutation to be meaningful
  grp_sizes <- table(county_df$Income_Group)
  if (any(grp_sizes < MIN_CTY_PER_GROUP)) return(tibble(perm_p_value = NA_real_))
  
  W_tot <- sum(grp$W)
  get_o <- function(g, col) grp[[col]][grp$Income_Group == g]
  pi_H <- get_o("high","W")/W_tot;   pi_M <- get_o("middle","W")/W_tot;  pi_L <- get_o("low","W")/W_tot
  mu_H <- get_o("high","mu");         mu_M <- get_o("middle","mu");        mu_L <- get_o("low","mu")
  mb   <- pi_L*mu_L + pi_M*mu_M + pi_H*mu_H
  rH   <- pi_H/2;  rM <- pi_H + pi_M/2;  rL <- 1 - pi_L/2
  cov_o <- (pi_H*rH*mu_H + pi_M*rM*mu_M + pi_L*rL*mu_L) - 0.5*mb
  var_o <- (pi_H*rH^2    + pi_M*rM^2    + pi_L*rL^2)    - 0.25
  obs   <- abs((cov_o / var_o) / mb)
  
  if (is.na(obs)) return(tibble(perm_p_value = NA_real_))
  
  # Vectorised permutations via indicator matrix multiplication
  # labels: integer vector 1=high, 2=middle, 3=low
  labels <- match(county_df$Income_Group, c("high","middle","low"))
  pop    <- county_df$Population
  wc     <- county_df$Ann_Conc * county_df$Population
  n      <- nrow(county_df)
  
  # P: n x n_perm — each column is one permutation of labels
  P <- replicate(n_perm, sample(labels))   # n × n_perm integer matrix
  
  # Indicator matrices: I_g[i,b] = 1 if county i assigned to group g in replicate b
  I_H <- (P == 1L)
  I_M <- (P == 2L)
  I_L <- (P == 3L)
  
  # Group total populations and weighted concentrations for all replicates at once
  W_H <- as.vector(pop %*% I_H);   WC_H <- as.vector(wc %*% I_H)
  W_M <- as.vector(pop %*% I_M);   WC_M <- as.vector(wc %*% I_M)
  W_L <- as.vector(pop %*% I_L);   WC_L <- as.vector(wc %*% I_L)
  
  W_tot_b <- W_H + W_M + W_L
  pL_b <- W_L/W_tot_b;  pM_b <- W_M/W_tot_b;  pH_b <- W_H/W_tot_b
  mL_b <- WC_L/W_L;     mM_b <- WC_M/W_M;     mH_b <- WC_H/W_H
  mb_b <- pH_b*mH_b + pM_b*mM_b + pL_b*mL_b
  
  rH_b <- pH_b/2;  rM_b <- pH_b + pM_b/2;  rL_b <- 1 - pL_b/2
  cov_b <- (pH_b*rH_b*mH_b + pM_b*rM_b*mM_b + pL_b*rL_b*mL_b) - 0.5*mb_b
  var_b <- (pH_b*rH_b^2    + pM_b*rM_b^2    + pL_b*rL_b^2)    - 0.25
  
  perm_eig <- abs((cov_b / var_b) / mb_b)
  valid    <- is.finite(perm_eig) & !is.na(mb_b) & mb_b > 0
  
  tibble(perm_p_value = sum(valid & perm_eig >= obs, na.rm = TRUE) / n_perm)
}

perm_results <- map2_dfr(
  poll_years$Pollutant, poll_years$Year,
  function(poll, yr) {
    pv <- compute_perm_p_fast(
      annual_county %>% filter(Pollutant == poll, Year == yr),
      N_PERM, SEED + yr + 10000L
    )
    bind_cols(tibble(Pollutant = poll, Year = yr), pv)
  }
)

cat(sprintf("  Permutation test complete: %d rows\n", nrow(perm_results)))

# =============================================================================
# SECTION 10: MERGE ALL RESULTS
# =============================================================================

cat("\n== SECTION 10: Merging all results ==\n")

final <- annual_metrics %>%
  left_join(boot_results, by = c("Pollutant", "Year")) %>%
  left_join(perm_results, by = c("Pollutant", "Year"))

write_csv(final, out_annual, na = "")
cat(sprintf("  Saved: %s (%d rows)\n", basename(out_annual), nrow(final)))

# =============================================================================
# SECTION 11: TREND MODELS
#
# (A) OLS trend in EIG_rel over years
# (B) Mixed-effects ARMA trend in daily D_d (Eq. 6)
# =============================================================================

cat("\n== SECTION 11: Trend models ==\n")

# --- (A) EIG_rel trend ---

fit_eig_trend <- function(df_poll) {
  poll  <- unique(df_poll$Pollutant)
  df_fit <- df_poll %>% filter(!is.na(EIG_rel)) %>% mutate(Year_c = Year - YEAR_CENTER)
  
  if (nrow(df_fit) < 5) {
    cat(sprintf("  %-7s EIG trend: SKIP (< 5 years)\n", poll))
    return(tibble(State=STATE, Pollutant=poll, metric="EIG_rel",
                  beta0=NA, beta1=NA, SE_beta1=NA, p_value=NA, n_years=nrow(df_fit)))
  }
  
  fit <- lm(EIG_rel ~ Year_c, data = df_fit)
  tt  <- summary(fit)$coefficients
  cat(sprintf("  %-7s EIG trend: slope=%.6f p=%.4f\n",
              poll, tt["Year_c","Estimate"], tt["Year_c","Pr(>|t|)"]))
  
  tibble(State=STATE, Pollutant=poll, metric="EIG_rel",
         beta0=tt["(Intercept)","Estimate"], beta1=tt["Year_c","Estimate"],
         SE_beta1=tt["Year_c","Std. Error"], p_value=tt["Year_c","Pr(>|t|)"],
         n_years=nrow(df_fit))
}

eig_trends <- final %>%
  group_by(Pollutant) %>% group_split() %>%
  map_dfr(fit_eig_trend)

# --- (B) Daily D_d trend ---

fit_daily_trend <- function(df, poll_label) {
  df_fit <- df %>%
    filter(!is.na(D_d)) %>%
    mutate(Year_c  = Year - YEAR_CENTER,
           sin_doy = sin(2*pi*DOY/365.25),
           cos_doy = cos(2*pi*DOY/365.25),
           Year_f  = factor(Year)) %>%
    arrange(Year_f, DOY)
  
  if (nrow(df_fit) < 100 || n_distinct(df_fit$Year) < 5) {
    cat(sprintf("  %-7s D_d trend: SKIP\n", poll_label))
    return(tibble(State=STATE, Pollutant=poll_label, metric="D_d",
                  beta0=NA, beta1=NA, SE_beta1=NA, p_value=NA,
                  n_years=n_distinct(df_fit$Year)))
  }
  
  safe_lme <- function(p, q) {
    tryCatch(
      lme(D_d ~ Year_c + sin_doy + cos_doy, random = ~ 1 | Year_f,
          correlation = corARMA(p=p, q=q, form = ~ 1 | Year_f),
          data=df_fit, method="ML", na.action=na.omit,
          control=lmeControl(opt="optim", maxIter=200, msMaxIter=200)),
      error=function(e) NULL, warning=function(w) NULL
    )
  }
  
  fits <- Filter(Negate(is.null), list(`1,0`=safe_lme(1,0), `1,1`=safe_lme(1,1)))
  
  if (length(fits) == 0) {
    cat(sprintf("  %-7s D_d trend: FAILED\n", poll_label))
    return(tibble(State=STATE, Pollutant=poll_label, metric="D_d",
                  beta0=NA, beta1=NA, SE_beta1=NA, p_value=NA,
                  n_years=n_distinct(df_fit$Year)))
  }
  
  best <- fits[[names(which.min(sapply(fits, AIC)))]]
  tt   <- summary(best)$tTable
  cat(sprintf("  %-7s D_d trend: slope=%.5f p=%.4f\n",
              poll_label, tt["Year_c","Value"], tt["Year_c","p-value"]))
  
  tibble(State=STATE, Pollutant=poll_label, metric="D_d",
         beta0=tt["(Intercept)","Value"], beta1=tt["Year_c","Value"],
         SE_beta1=tt["Year_c","Std.Error"], p_value=tt["Year_c","p-value"],
         n_years=n_distinct(df_fit$Year))
}

daily_trends <- daily_wide %>%
  group_by(Pollutant) %>% group_split() %>%
  map_dfr(~ fit_daily_trend(.x, unique(.x$Pollutant)))

all_trends <- bind_rows(eig_trends, daily_trends)
write_csv(all_trends, out_trend, na = "")
cat(sprintf("  Saved: %s (%d rows)\n", basename(out_trend), nrow(all_trends)))


# =============================================================================
# SECTION 12: SUMMARY
# =============================================================================

cat("\n==========================================================\n")
cat("  SUMMARY\n")
cat("==========================================================\n\n")

cat("-- Annual metrics by pollutant (averages across years) --\n\n")
final %>%
  group_by(Pollutant) %>%
  summarise(
    Years        = n(),
    Mean_A_t     = round(mean(A_t,      na.rm=TRUE), 3),
    Mean_R_t     = round(mean(R_t,      na.rm=TRUE), 3),
    Mean_RCI     = round(mean(RCI,      na.rm=TRUE), 4),
    Mean_SII     = round(mean(SII,      na.rm=TRUE), 3),
    Mean_BGSD    = round(mean(BGSD,     na.rm=TRUE), 3),
    Mean_EIG_rel = round(mean(EIG_rel,  na.rm=TRUE), 4),
    Mean_MGR     = round(mean(MGR,      na.rm=TRUE), 4),
    Pct_EIG_pos  = round(100 * mean(EIG_rel > 0, na.rm=TRUE), 1),
    Med_perm_p   = round(median(perm_p_value, na.rm=TRUE), 4),
    .groups = "drop"
  ) %>%
  print(n=Inf, width=Inf)

cat("\n-- Trend results --\n\n")
all_trends %>%
  select(Pollutant, metric, beta1, SE_beta1, p_value, n_years) %>%
  mutate(across(c(beta1, SE_beta1, p_value), ~ round(., 5))) %>%
  print(n=Inf, width=Inf)

cat("\n-- Output files --\n")
cat(sprintf("  %s\n", basename(out_daily)))
cat(sprintf("  %s\n", basename(out_annual)))
cat(sprintf("  %s\n", basename(out_trend)))
cat(sprintf("  %s\n", basename(out_coverage)))

cat("\nDone.\n")