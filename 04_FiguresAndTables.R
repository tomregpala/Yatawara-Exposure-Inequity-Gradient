# ======================================================================
# YOUR JOURNEY ENDS HERE, AT THE FIGURES AND TABLES SCRIPT!
# Generates all publication figures and tables for a specified US state.
# Must be run AFTER 03_StatisticalAnalyses.R has produced its outputs.

# OUTPUTS (written to States/[StateName]/figures/):
#   fig1_county_income_map_{STATE}.pdf/.png
#   fig2_pm25_daily_series_{STATE}.pdf/.png
#   fig2_pm25_daily_disparity_{STATE}.pdf/.png
#   tbl3_pm25_annual_disparity_{STATE}.csv
#   tbl4_trend_summary_{STATE}.csv
# ======================================================================

suppressPackageStartupMessages({
  library(readr);    library(dplyr);    library(tidyr);  library(stringr)
  library(lubridate);library(scales);   library(ggplot2);library(patchwork)
  library(sf);       library(tigris)
})

options(tigris_use_cache = TRUE)   # cache shapefile downloads after first run

# =============================================================================
# CONFIG — change STATE to run for any US state
# =============================================================================

STATE       <- "Rhode Island"
MASTER_FILE <- "master_county_year.csv"
YEARS       <- 2000:2023
YEAR_CENTER <- mean(YEARS)            # 2011.5, same as analysis script

# Derived paths  — mirror 03_StatisticalAnalyses.R exactly
state_slug  <- gsub(" ", "_", STATE)
ana_dir     <- file.path("States", STATE)
fig_dir     <- file.path("States", STATE, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Input files produced by 03_StatisticalAnalyses.R
f_daily  <- file.path(ana_dir, paste0("daily_disparity_",  state_slug, ".csv"))
f_annual <- file.path(ana_dir, paste0("annual_all_metrics_", state_slug, ".csv"))
f_trend  <- file.path(ana_dir, paste0("trend_summary_",    state_slug, ".csv"))

for (f in c(f_daily, f_annual, f_trend)) {
  if (!file.exists(f))
    stop("Required file not found: ", f,
         "\n  Run 03_StatisticalAnalyses.R for '", STATE, "' first.")
}

# THEME

theme_disparity <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5, size = base_size + 2,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(hjust = 0.5, size = base_size - 1,
                                      colour = "grey40", margin = margin(b = 4)),
      plot.caption     = element_text(size = base_size - 2, colour = "grey55", hjust = 1),
      legend.position  = "top",
      legend.text      = element_text(size = base_size),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(8, 8, 8, 8)
    )
}

# SHARED DATA: income groups + population from master CSV

cat("== Loading master CSV ==\n")

normalize_county <- function(x) {
  x %>%
    str_trim() %>%
    str_remove_all(regex(
      "\\s+(County|Parish|Borough|Census Area|Municipality|City and Borough|city|City)$",
      ignore_case = FALSE
    )) %>%
    str_trim() %>%
    str_to_upper()
}

master <- read_csv(MASTER_FILE, show_col_types = FALSE) %>%
  filter(State == STATE)

if (nrow(master) == 0) stop("State '", STATE, "' not found in ", MASTER_FILE)

# Replicate EXACT income-group assignment from 03_StatisticalAnalyses.R
mhi_ref <- master %>%
  distinct(County, FIPS, MHI) %>%
  filter(!is.na(MHI))

q30 <- quantile(mhi_ref$MHI, 0.30, na.rm = TRUE)
q70 <- quantile(mhi_ref$MHI, 0.70, na.rm = TRUE)

county_groups <- mhi_ref %>%
  mutate(
    County_Norm  = normalize_county(County),
    Income_Group = case_when(
      MHI <= q30 ~ "low",
      MHI <= q70 ~ "middle",
      TRUE       ~ "high"
    ),
    # Zero-pad FIPS to 5 characters to match tigris GEOID
    FIPS5 = str_pad(as.character(FIPS), width = 5, side = "left", pad = "0")
  )

cat(sprintf("  q30 = $%s | q70 = $%s\n",
            format(round(q30), big.mark = ","),
            format(round(q70), big.mark = ",")))
cat(sprintf("  low: %d | middle: %d | high: %d | no MHI: %d\n",
            sum(county_groups$Income_Group == "low"),
            sum(county_groups$Income_Group == "middle"),
            sum(county_groups$Income_Group == "high"),
            nrow(master %>% distinct(County, FIPS)) - nrow(mhi_ref)))

# =============================================
# SECTION 1 — FIGURE 1: County income-group map
# =============================================

cat("\n== Figure 1: County income-group map ==\n")

# Download county shapefile for target state from the Census Bureau
state_counties_sf <- tryCatch(
  counties(state = STATE, cb = TRUE, year = 2020, progress_bar = FALSE),
  error = function(e) stop("tigris::counties() failed. Check internet connection. ", e$message)
)

# Join income groups by 5-digit FIPS (GEOID in tigris, FIPS5 in our data)
map_data <- state_counties_sf %>%
  left_join(
    county_groups %>% select(FIPS5, Income_Group, MHI),
    by = c("GEOID" = "FIPS5")
  ) %>%
  mutate(
    Income_Group = factor(
      replace_na(Income_Group, NA_character_),
      levels = c("low", "middle", "high")
    )
  )

map_colors <- c(
  low       = "#E07B39",   # warm orange
  middle    = "#A8D4E6",   # light blue
  high      = "#2A9D8F"    # teal
)

map_labels <- c(
  low       = "Low",
  middle    = "Middle",
  high      = "High"
)

fig1 <- ggplot(map_data) +
  geom_sf(aes(fill = Income_Group), colour = "white", linewidth = 0.25) +
  scale_fill_manual(
    values       = map_colors,
    labels       = map_labels,
    name         = "Income Group",
    drop         = TRUE,
    na.translate = FALSE   # counties with no MHI get na.value colour but no legend key
  ) +
  theme_void() +
  theme(
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 11),
    legend.text      = element_text(size = 10),
    legend.key.size  = unit(0.5, "cm"),
    plot.margin      = margin(4, 4, 4, 4)
  ) +
  guides(fill = guide_legend(
    ncol         = 1,
    override.aes = list(fill = c("#E07B39", "#A8D4E6", "#2A9D8F"))
  ))

out_map_pdf <- file.path(fig_dir, paste0("fig1_county_income_map_", state_slug, ".pdf"))
out_map_png <- file.path(fig_dir, paste0("fig1_county_income_map_", state_slug, ".png"))
ggsave(out_map_pdf, fig1, width = 8, height = 6, units = "in")
ggsave(out_map_png, fig1, width = 8, height = 6, units = "in", dpi = 300)
cat(sprintf("  Saved: %s\n", basename(out_map_pdf)))

# =====================================================================
# SECTION 2 — FIGURE 2: Daily population-weighted PM2.5 by income group
#
# Three Figures:
#   1. Wide daily series
#   2. Daily disparity series (D_d = low - high)
#   3. Annual percent of days low > high
# =====================================================================

cat("\n== Figure 2: Daily PM2.5 series ==\n")

daily_all <- read_csv(f_daily, show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date))

# All PM2.5 rows — each line uses its own non-NA subset so a coverage gap in
# one group never suppresses the other group's line.
pm25_all <- daily_all %>%
  filter(Pollutant == "PM2.5")

if (nrow(pm25_all) == 0)
  stop("No PM2.5 rows in daily output for ", STATE,
       ". Check coverage_diagnostic_", state_slug, ".csv.")

pm25_low  <- pm25_all %>% filter(!is.na(X_bar_low))
pm25_high <- pm25_all %>% filter(!is.na(X_bar_high))

# Guard: skip Figure 2 entirely if either group has no PM2.5 data
# (e.g. Rhode Island — too few monitors to compute group-level means)
if (nrow(pm25_low) == 0 || nrow(pm25_high) == 0) {
  missing <- c(
    if (nrow(pm25_low)  == 0) "low-income",
    if (nrow(pm25_high) == 0) "high-income"
  )
  cat(sprintf("  SKIPPED: no PM2.5 data for %s group(s) in %s.\n",
              paste(missing, collapse = " and "), STATE))
  cat(sprintf("  See coverage_diagnostic_%s.csv for details.\n", state_slug))
} else {
  
  # Complete cases (both groups valid) — used for disparity series and % labels
  pm25_daily <- pm25_all %>%
    filter(!is.na(X_bar_low), !is.na(X_bar_high)) %>%
    mutate(disparity = D_d)
  
  # Annual % days low > high (only over days where both groups are valid)
  ann_pct <- pm25_daily %>%
    mutate(year = year(Date), low_gt_high = X_bar_low > X_bar_high) %>%
    group_by(year) %>%
    summarise(pct_low_gt_high = mean(low_gt_high, na.rm = TRUE) * 100, .groups = "drop")
  
  ymax_pm25 <- max(c(pm25_low$X_bar_low, pm25_high$X_bar_high), na.rm = TRUE)
  n_years   <- length(YEARS)
  width_in  <- max(16, 0.60 * n_years)
  
  col_low   <- "#E6194B"   # red  — Low-income
  col_high  <- "#0571B0"   # blue — High-income
  fill_band <- "#DDEEFF"   # uniform light blue panel wash
  
  # % label y-position: just inside the top of the panel
  pct_label_y <- ymax_pm25 * 0.97
  
  # ---- 2A: Wide daily series — uniform shading + % labels inside panel ----
  fig2a <- ggplot() +
    # Uniform light blue wash pinned to the full study period
    annotate("rect",
             xmin  = as.Date("2000-01-01"), xmax = as.Date("2023-12-31"),
             ymin  = -Inf,                  ymax = Inf,
             fill  = fill_band, alpha = 0.6) +
    # Each line drawn from its own non-NA subset — gaps in one do not cut the other
    geom_line(data = pm25_low,
              aes(Date, X_bar_low,  colour = "Low-income"),  linewidth = 0.4) +
    geom_line(data = pm25_high,
              aes(Date, X_bar_high, colour = "High-income"), linewidth = 0.4) +
    scale_colour_manual(
      values = c("High-income" = col_high, "Low-income" = col_low),
      name   = NULL,
      # Force legend key order: High first, Low second
      breaks = c("High-income", "Low-income")
    ) +
    # % labels inside the panel at the top
    geom_text(
      data = ann_pct,
      aes(x = make_date(year, 7, 1), y = pct_label_y,
          label = sprintf("%.0f%%", pct_low_gt_high)),
      size = 3.2, fontface = "bold", colour = "grey20", vjust = 1
    ) +
    scale_x_date(
      breaks      = make_date(YEARS, 1, 1),   # all years 2000-2023
      date_labels = "%Y",
      expand      = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
    coord_cartesian(xlim = as.Date(c("2000-01-01", "2023-12-31"))) +
    labs(
      title    = paste0("Daily population-weighted PM2.5 by income group (", STATE, ")"),
      subtitle = "Blue shade shows percent of days low-income exceeds high-income per year",
      x        = NULL,
      y        = "PM2.5 (ug/m3)",
      caption  = "Population weights from county-year population"
    ) +
    theme_disparity(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0,   size = 13),
      plot.subtitle    = element_text(hjust = 0, size = 10, colour = "grey30"),
      plot.caption     = element_text(colour = "#C0392B", hjust = 1, size = 9),
      legend.position  = "top",
      legend.justification = "center",
      legend.direction = "horizontal",
      axis.text.x        = element_text(size = 10),
      panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.3),
      plot.margin        = margin(6, 10, 6, 6)
    )
  
  ggsave(file.path(fig_dir, paste0("fig2_pm25_daily_series_", state_slug, ".pdf")),
         fig2a, width = width_in, height = 5.8, units = "in")
  ggsave(file.path(fig_dir, paste0("fig2_pm25_daily_series_", state_slug, ".png")),
         fig2a, width = width_in, height = 5.8, units = "in", dpi = 300)
  cat("  Saved: fig2_pm25_daily_series\n")
  
  # ---- 2B: Daily disparity (low - high) ----
  fig2b <- ggplot(pm25_daily, aes(Date, disparity)) +
    geom_line(colour = "#08306B", linewidth = 0.4) +
    # Dashed red zero line, matching reference
    geom_hline(yintercept = 0, colour = "red", linewidth = 0.6, linetype = "dashed") +
    scale_x_date(
      breaks      = make_date(YEARS, 1, 1),   # all years 2000-2023
      date_labels = "%Y",
      expand      = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(xlim = as.Date(c("2000-01-01", "2023-12-31"))) +
    labs(
      title = "Population-weighted daily PM2.5 difference (low - high)",
      x     = NULL,
      y     = "PM2.5 (ug/m3)"
    ) +
    theme_disparity(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", hjust = 0.5, size = 13),
      panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.3),
      plot.margin        = margin(10, 10, 6, 6)
    )
  
  ggsave(file.path(fig_dir, paste0("fig2_pm25_daily_disparity_", state_slug, ".pdf")),
         fig2b, width = max(14, 0.55 * n_years), height = 4.0, units = "in")
  ggsave(file.path(fig_dir, paste0("fig2_pm25_daily_disparity_", state_slug, ".png")),
         fig2b, width = max(14, 0.55 * n_years), height = 4.0, units = "in", dpi = 300)
  cat("  Saved: fig2_pm25_daily_disparity\n")
  
} # end Figure 2 sparse-data guard

# ===================================================
# SECTION 3 — TABLE 3: Annual PM2.5 disparity metrics
# ===================================================

cat("\n== Table 3: Annual PM2.5 disparity ==\n")

annual_all <- read_csv(f_annual, show_col_types = FALSE)

# CSV only — columns: Year | Abs disparity | Rel disparity | % days low > high
tbl3 <- annual_all %>%
  filter(Pollutant == "PM2.5") %>%
  arrange(Year) %>%
  select(
    Year,
    `Abs disparity (ug/m3)` = A_t,
    `Rel disparity`         = R_t,
    `% days low > high`     = P_t
  )

out_tbl3 <- file.path(fig_dir, paste0("tbl3_pm25_annual_disparity_", state_slug, ".csv"))
write_csv(tbl3, out_tbl3, na = "")
cat(sprintf("  Saved: %s\n", basename(out_tbl3)))

# ========================================
# SECTION 4 — TABLE 4: Trend model summary
# ========================================

cat("\n== Table 4: Trend model summary ==\n")

# trend_summary now contains two rows per pollutant (metric = EIG_rel and D_d)
# from 03_StatisticalAnalyses.R. Columns: State, Pollutant, metric, beta0,
# beta1, SE_beta1, p_value, n_years. No note/ARMA columns.

trend_raw <- read_csv(f_trend, show_col_types = FALSE)

poll_order <- c("PM2.5", "NO2", "O3", "SO2", "CO")

tbl4 <- trend_raw %>%
  mutate(
    Pollutant = factor(Pollutant, levels = poll_order),
    CI_lo     = beta1 - 1.96 * SE_beta1,
    CI_hi     = beta1 + 1.96 * SE_beta1,
    # BH adjustment within each metric type separately
    q_value   = NA_real_
  ) %>%
  arrange(metric, Pollutant) %>%
  # Apply BH per metric group
  group_by(metric) %>%
  mutate(q_value = p.adjust(p_value, method = "BH")) %>%
  ungroup() %>%
  select(
    Pollutant,
    Metric        = metric,
    beta1,
    `95% CI low`  = CI_lo,
    `95% CI high` = CI_hi,
    p             = p_value,
    `q (BH)`      = q_value,
    `N years`     = n_years
  )

out_tbl4 <- file.path(fig_dir, paste0("tbl4_trend_summary_", state_slug, ".csv"))
write_csv(tbl4, out_tbl4, na = "")
cat(sprintf("  Saved: %s\n", basename(out_tbl4)))

# SUMMARY

cat(sprintf("\n== Done -- all outputs written to: %s ==\n", normalizePath(fig_dir)))
cat("  Figure 1:  County income-group map\n")
cat("  Figure 2A: PM2.5 daily series (low + high) with annual shading\n")
cat("  Figure 2B: PM2.5 daily disparity (D_d)\n")
cat("  Table 3:   Annual PM2.5 disparity metrics (CSV)\n")
cat("  Table 4:   5-pollutant trend model summary with BH-adjusted p (CSV)\n")
cat("\n  All tables saved as .csv\n")