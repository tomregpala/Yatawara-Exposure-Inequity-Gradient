# =============================================================================
# 07_NationalSummary.R
#
# Produces national-level summary outputs across all 50 states:
#
#   FIGURE: County choropleth of state EIG_rel for a selected year + pollutant
#   TABLE:  For each pollutant, how many states show EIG_rel > 0 with p < 0.05?
#   FIGURE: Heatmap of mean EIG_rel (across years) by State x Pollutant.
#   FIGURE: PM2.5 pairwise correlation matrix across all disparity metrics.
#
# INPUTS:
#   States/[State]/eig_annual_{STATE}.csv        (from 06_EIG_Metrics.R)
#   States/[State]/eig_trend_{STATE}.csv         (from 06_EIG_Metrics.R)
#   States/[State]/annual_all_metrics_{STATE}.csv (from 03_StatisticalAnalyses.R)
#
# OUTPUTS (written to National_Summary/):
#   fig_choropleth_eig_{POLLUTANT}_{YEAR}.pdf / .png
#   tbl_national_eig_summary.csv
#   fig_eig_heatmap.pdf / .png
#   tbl_pm25_correlation_matrix.csv
#   fig_pm25_correlation_matrix.pdf / .png
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(scales)
library(sf)
library(tigris)

# =============================================================================
# CONFIG
# =============================================================================

ALL_STATES <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California",
  "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
  "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
  "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
  "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
)

POLL_ORDER  <- c("PM2.5", "NO2", "O3", "SO2", "CO")
PVAL_THRESH <- 0.05

# Choropleth settings — change these to produce maps for different years/pollutants
MAP_YEAR      <- 2020    # year to display in the county choropleth
MAP_POLLUTANT <- "PM2.5" # pollutant to display

out_dir      <- "National_Summary"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_table    <- file.path(out_dir, "tbl_national_eig_summary.csv")
out_heat_pdf <- file.path(out_dir, "fig_eig_heatmap.pdf")
out_heat_png <- file.path(out_dir, "fig_eig_heatmap.png")

# =============================================================================
# SECTION 0: COUNTY CHOROPLETH — EIG_rel BY STATE FOR SELECTED YEAR
#
# EIG_rel is a state-level metric (computed across income groups of counties),
# so all counties within a state share the same colour for the selected year.
# This shows the geographic distribution of income-based air quality inequality.
#
# States with no valid EIG for the selected year are shown in grey.
# Alaska and Hawaii are included via tigris shift_geometry.
# =============================================================================

cat(sprintf("== SECTION 0: County choropleth (%s, %d) ==\n", MAP_POLLUTANT, MAP_YEAR))

# Load EIG data for the selected year + pollutant from all states
load_eig_year <- function(state) {
  slug <- gsub(" ", "_", state)
  path <- file.path("States", state, paste0("eig_annual_", slug, ".csv"))
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) %>%
    filter(Pollutant == MAP_POLLUTANT, Year == MAP_YEAR) %>%
    mutate(State = state)
}

eig_year <- map_dfr(ALL_STATES, load_eig_year) %>%
  select(State, EIG_rel, note)

cat(sprintf("  States with EIG data for %s %d: %d / %d\n",
            MAP_POLLUTANT, MAP_YEAR,
            sum(!is.na(eig_year$EIG_rel)), length(ALL_STATES)))

# Download county shapefiles — contiguous 48 states only.
# shift_geometry() insets leave an unreliable bounding box that shrinks the map;
# using Albers Equal Area (EPSG:5070) with hardcoded CONUS limits is more reliable.
# Alaska (STATEFP=02) and Hawaii (STATEFP=15) are excluded and noted in caption.
cat("  Downloading county shapefiles...\n")
options(tigris_use_cache = TRUE)

CONUS_FIPS <- as.character(
  setdiff(sprintf("%02d", 1:56), c("02", "15", "43", "52", "53", "57",
                                   "14", "03", "07", "11", "14", "43",
                                   "52", "14"))
)

counties_sf <- counties(cb = TRUE, year = 2020, progress_bar = FALSE) %>%
  filter(!STATEFP %in% c("02", "15") & as.integer(STATEFP) < 57 &
           !STATEFP %in% c("60","66","69","72","74","78")) %>%
  st_transform(5070) %>%               # Albers Equal Area Conic — standard for CONUS
  select(GEOID, STATEFP, NAME, geometry)

states_sf <- states(cb = TRUE, year = 2020, progress_bar = FALSE) %>%
  as_tibble() %>%
  filter(!STATEFP %in% c("02", "15") & as.integer(STATEFP) < 57 &
           !STATEFP %in% c("60","66","69","72","74","78")) %>%
  select(STATEFP, State = NAME)

# Join EIG onto county sf via state name
choropleth_data <- counties_sf %>%
  left_join(states_sf, by = "STATEFP") %>%
  left_join(eig_year %>% select(State, EIG_rel), by = "State")

# Discrete bins matching the state heatmap colour scheme
choro_breaks <- c(-Inf, -0.35, -0.20, -0.10, -0.05,
                  0.05,  0.10,  0.20,  0.35, Inf)
choro_labels <- c(
  "< -0.35",
  "-0.35 to -0.20",
  "-0.20 to -0.10",
  "-0.10 to -0.05",
  "-0.05 to 0.05",
  "0.05 to 0.10",
  "0.10 to 0.20",
  "0.20 to 0.35",
  "> 0.35"
)
choro_colours <- c(
  "#7F0000",
  "#D6604D",
  "#F4A582",
  "#FDDBC7",
  "#F7F7F7",
  "#D1E5F0",
  "#92C5DE",
  "#4393C3",
  "#053061"
)

choropleth_data <- choropleth_data %>%
  mutate(
    EIG_bin = cut(EIG_rel, breaks = choro_breaks, labels = choro_labels,
                  include.lowest = TRUE, right = FALSE)
  )

fig_choro <- ggplot(choropleth_data) +
  geom_sf(aes(fill = EIG_bin), colour = "white", linewidth = 0.05) +
  # Let ggplot fit to the actual data extent — no hardcoded limits needed
  # since the counties are already filtered to CONUS and reprojected to EPSG:5070
  coord_sf(crs = st_crs(5070), expand = TRUE) +
  scale_fill_manual(
    values       = setNames(choro_colours, choro_labels),
    na.value     = "#CCCCCC",
    name         = "EIG_rel",
    drop         = FALSE,
    na.translate = FALSE,
    guide        = guide_legend(
      reverse      = TRUE,
      keywidth     = unit(0.5, "cm"),
      keyheight    = unit(0.42, "cm"),
      label.theme  = element_text(size = 8),
      title.theme  = element_text(size = 9, face = "plain"),
      override.aes = list(colour = "white", linewidth = 0.2)
    )
  ) +
  labs(
    title    = sprintf("%s Income-Based Air Quality Inequality (EIG_rel), %d",
                       MAP_POLLUTANT, MAP_YEAR),
    subtitle = paste0("Orange: low-income counties more exposed (EIG > 0).  ",
                      "Blue: less exposed (EIG < 0).  Grey: no valid data."),
    caption  = paste0(
      "Contiguous 48 states shown. Alaska and Hawaii excluded.  ",
      "EIG_rel is a state-level metric; all counties within a state share the same value.  ",
      "Source: EPA AQS daily data, ACS 2019-2023 MHI."
    )
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, colour = "grey30",
                                 margin = margin(b = 4)),
    plot.caption  = element_text(size = 7, colour = "grey45", hjust = 0,
                                 margin = margin(t = 6)),
    legend.position  = "right",
    legend.margin    = margin(l = 6),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(6, 6, 6, 6)
  )

choro_slug    <- sprintf("fig_choropleth_eig_%s_%d",
                         gsub("\\.", "", MAP_POLLUTANT), MAP_YEAR)
out_choro_pdf <- file.path(out_dir, paste0(choro_slug, ".pdf"))
out_choro_png <- file.path(out_dir, paste0(choro_slug, ".png"))
ggsave(out_choro_pdf, fig_choro, width = 11, height = 7, units = "in")
ggsave(out_choro_png, fig_choro, width = 11, height = 7, units = "in", dpi = 300)
cat(sprintf("  Saved: %s\n", basename(out_choro_pdf)))
cat(sprintf("  Saved: %s\n", basename(out_choro_png)))

# =============================================================================
# SECTION 1: LOAD ALL STATE EIG ANNUAL FILES
# =============================================================================

cat("== SECTION 1: Loading annual EIG data ==\n")

load_eig_annual <- function(state) {
  slug <- gsub(" ", "_", state)
  path <- file.path("States", state, paste0("eig_annual_", slug, ".csv"))
  if (!file.exists(path)) { warning(sprintf("Missing: %s", path)); return(NULL) }
  read_csv(path, show_col_types = FALSE) %>% mutate(State = state)
}

annual_all <- map_dfr(ALL_STATES, load_eig_annual)
cat(sprintf("  Loaded: %s rows across %d states\n",
            format(nrow(annual_all), big.mark = ","),
            n_distinct(annual_all$State)))

# =============================================================================
# SECTION 2: LOAD ALL STATE EIG TREND FILES
# =============================================================================

cat("\n== SECTION 2: Loading trend data ==\n")

load_eig_trend <- function(state) {
  slug <- gsub(" ", "_", state)
  path <- file.path("States", state, paste0("eig_trend_", slug, ".csv"))
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) %>% mutate(State = state)
}

trend_all <- map_dfr(ALL_STATES, load_eig_trend)
cat(sprintf("  Loaded: %d trend rows across %d states\n",
            nrow(trend_all), n_distinct(trend_all$State)))

# =============================================================================
# SECTION 3: SUMMARY TABLE
#
# For each pollutant:
#   N_states_data         states with >= 1 valid EIG_rel year
#   N_states_EIGpos_sig   states where mean EIG_rel > 0 AND
#                         median permutation p < PVAL_THRESH
#   Pct_states_EIGpos_sig as a percentage of states with data
#   Median_EIG_rel        median of per-state mean EIG_rel
#   IQR_EIG_rel           [Q25, Q75] of per-state mean EIG_rel
#   N_states_trend_pos_sig states with significant positive trend
#   N_states_trend_data   states with enough years to fit trend
# =============================================================================

cat("\n== SECTION 3: Computing summary table ==\n")

# Per-state per-pollutant summaries
state_poll_summary <- annual_all %>%
  filter(!is.na(EIG_rel)) %>%
  group_by(State, Pollutant) %>%
  summarise(
    n_valid_years  = n(),
    mean_EIG_rel   = mean(EIG_rel,        na.rm = TRUE),
    mean_MGR       = mean(MGR,            na.rm = TRUE),
    median_perm_p  = median(perm_p_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    EIG_pos_sig = mean_EIG_rel > 0 &
      !is.na(median_perm_p) &
      median_perm_p < PVAL_THRESH
  )

# Trend significance
trend_sig <- trend_all %>%
  filter(!is.na(trend_slope), !is.na(trend_p)) %>%
  mutate(trend_pos_sig = trend_slope > 0 & trend_p < PVAL_THRESH) %>%
  select(State, Pollutant, trend_slope, trend_p, n_years, trend_pos_sig)

state_poll_full <- state_poll_summary %>%
  left_join(trend_sig, by = c("State", "Pollutant"))

# Aggregate to pollutant level
summary_table <- state_poll_full %>%
  group_by(Pollutant) %>%
  summarise(
    N_states_data          = n(),
    N_states_EIGpos_sig    = sum(EIG_pos_sig,    na.rm = TRUE),
    Pct_states_EIGpos_sig  = round(100 * N_states_EIGpos_sig / N_states_data, 1),
    Median_EIG_rel         = round(median(mean_EIG_rel, na.rm = TRUE), 4),
    Q25_EIG_rel            = round(quantile(mean_EIG_rel, 0.25, na.rm = TRUE), 4),
    Q75_EIG_rel            = round(quantile(mean_EIG_rel, 0.75, na.rm = TRUE), 4),
    N_states_trend_pos_sig = sum(trend_pos_sig,  na.rm = TRUE),
    N_states_trend_data    = sum(!is.na(trend_p)),
    .groups = "drop"
  ) %>%
  mutate(
    Pollutant   = factor(Pollutant, levels = POLL_ORDER),
    IQR_EIG_rel = sprintf("[%.4f, %.4f]", Q25_EIG_rel, Q75_EIG_rel)
  ) %>%
  arrange(Pollutant) %>%
  select(
    Pollutant,
    N_states_data,
    N_states_EIGpos_sig,
    Pct_states_EIGpos_sig,
    Median_EIG_rel,
    IQR_EIG_rel,
    N_states_trend_pos_sig,
    N_states_trend_data
  )

write_csv(summary_table, out_table, na = "")
cat(sprintf("  Saved: %s\n\n", basename(out_table)))
print(summary_table, n = Inf, width = Inf)

# =============================================================================
# SECTION 4: HEATMAP OF MEAN EIG_rel BY STATE x POLLUTANT
#
# Styled after Liu et al. (2021) Fig. 2 — discrete diverging colour bins,
# orange for EIG > 0 (low-income MORE exposed), blue for EIG < 0 (less exposed).
#
# Bin breaks are chosen to reflect proportional EIG_rel magnitude:
#   < -0.35 | -0.35–-0.20 | -0.20–-0.10 | -0.10–-0.05 | -0.05–0.05 (near-zero)
#   0.05–0.10 | 0.10–0.20 | 0.20–0.35 | > 0.35
#
# States ordered top-to-bottom from most negative to most positive mean EIG_rel.
# Grey = insufficient monitor coverage to compute EIG.
# =============================================================================

cat("\n== SECTION 4: Building EIG_rel heatmap ==\n")

heatmap_data <- state_poll_summary %>%
  mutate(Pollutant = factor(Pollutant, levels = POLL_ORDER)) %>%
  complete(
    State     = ALL_STATES,
    Pollutant = POLL_ORDER,
    fill      = list(mean_EIG_rel = NA_real_)
  )

# Order states top-to-bottom: most negative EIG at top, most positive at bottom
state_order <- heatmap_data %>%
  group_by(State) %>%
  summarise(overall = mean(mean_EIG_rel, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall)) %>%   # desc so most positive is at BOTTOM of y-axis
  pull(State)

heatmap_data <- heatmap_data %>%
  mutate(State = factor(State, levels = state_order))

# Discrete colour bins matching Liu et al. break-point style
# Orange family = EIG > 0 (low-income more exposed, adverse)
# Blue family   = EIG < 0 (low-income less exposed)
# Light grey    = near-zero / no meaningful gradient

bin_breaks <- c(-Inf, -0.35, -0.20, -0.10, -0.05,
                0.05,  0.10,  0.20,  0.35, Inf)

bin_labels <- c(
  "< -0.35",
  "-0.35 to -0.20",
  "-0.20 to -0.10",
  "-0.10 to -0.05",
  "-0.05 to 0.05",
  "0.05 to 0.10",
  "0.10 to 0.20",
  "0.20 to 0.35",
  "> 0.35"
)

# Colour palette: mirrors Liu et al. — deep orange to light orange (positive),
# light blue to deep blue (negative), off-white for near-zero
bin_colours <- c(
  "#7F0000",   # deep red-orange  < -0.35
  "#D6604D",   # orange-red       -0.35 to -0.20
  "#F4A582",   # light orange     -0.20 to -0.10
  "#FDDBC7",   # very light orange -0.10 to -0.05
  "#F7F7F7",   # near-white       -0.05 to 0.05
  "#D1E5F0",   # very light blue   0.05 to 0.10
  "#92C5DE",   # light blue        0.10 to 0.20
  "#4393C3",   # medium blue       0.20 to 0.35
  "#053061"    # deep blue         > 0.35
)

heatmap_data <- heatmap_data %>%
  mutate(
    EIG_bin = cut(
      mean_EIG_rel,
      breaks = bin_breaks,
      labels = bin_labels,
      include.lowest = TRUE,
      right = FALSE
    )
  )

fig_heat <- ggplot(heatmap_data,
                   aes(x = Pollutant, y = State, fill = EIG_bin)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  scale_fill_manual(
    values   = setNames(bin_colours, bin_labels),
    na.value = "#CCCCCC",
    name     = "Mean EIG_rel",
    drop     = FALSE,
    guide    = guide_legend(
      reverse       = TRUE,
      keywidth      = unit(0.5, "cm"),
      keyheight     = unit(0.42, "cm"),
      label.theme   = element_text(size = 8),
      title.theme   = element_text(size = 9, face = "plain"),
      override.aes  = list(colour = "white", linewidth = 0.2)
    )
  ) +
  scale_x_discrete(position = "top") +
  labs(
    x       = NULL,
    y       = NULL,
    caption = paste0(
      "Mean EIG_rel averaged across all valid years (2000-2023) ",
      "for each state-pollutant combination.  ",
      "Orange: low-income counties more exposed than high-income (EIG > 0).  ",
      "Blue: low-income less exposed (EIG < 0).  ",
      "Grey: insufficient monitor coverage.  ",
      "States ordered by mean EIG_rel averaged across pollutants."
    )
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.text.y       = element_text(size = 7.5, colour = "grey10"),
    axis.text.x       = element_text(size = 10,  colour = "grey10", face = "bold"),
    axis.line         = element_blank(),
    axis.ticks        = element_blank(),
    legend.position   = "right",
    legend.title      = element_text(size = 9),
    legend.margin     = margin(l = 4),
    panel.border      = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
    plot.caption      = element_text(size = 7, colour = "grey40",
                                     hjust = 0, margin = margin(t = 8)),
    plot.margin       = margin(8, 8, 8, 8)
  )

ggsave(out_heat_pdf, fig_heat, width = 7,  height = 13, units = "in")
ggsave(out_heat_png, fig_heat, width = 7,  height = 13, units = "in", dpi = 300)
cat(sprintf("  Saved: %s\n", basename(out_heat_pdf)))
cat(sprintf("  Saved: %s\n", basename(out_heat_png)))

# =============================================================================
# SECTION 5: CONSOLE SUMMARY
# =============================================================================

cat("\n== NATIONAL SUMMARY ==\n\n")
cat(sprintf("  States with any EIG data : %d / %d\n",
            n_distinct(heatmap_data$State[!is.na(heatmap_data$mean_EIG_rel)]),
            length(ALL_STATES)))
cat(sprintf("  Significance threshold   : p < %.2f (permutation, median across years)\n\n",
            PVAL_THRESH))
cat(sprintf("  %-7s  %14s  %14s  %6s  %14s  %-20s  %14s\n",
            "Poll.", "States w/data", "EIG>0 & p<0.05", "Pct", "Median EIG_rel",
            "IQR", "Trend pos.sig."))
cat(strrep("-", 96), "\n")
for (i in seq_len(nrow(summary_table))) {
  r <- summary_table[i, ]
  cat(sprintf("  %-7s  %14d  %14d  %5.1f%%  %14.4f  %-20s  %d / %d\n",
              as.character(r$Pollutant),
              r$N_states_data,
              r$N_states_EIGpos_sig,
              r$Pct_states_EIGpos_sig,
              r$Median_EIG_rel,
              r$IQR_EIG_rel,
              r$N_states_trend_pos_sig,
              r$N_states_trend_data))
}


# =============================================================================
# SECTION 6: CORRELATION MATRIX — EIG_rel vs. OTHER DISPARITY METRICS
#
# Data source: annual_all_metrics_{STATE}.csv from 03_StatisticalAnalyses.R
# Unit of analysis: one row per State x Pollutant x Year (valid years only).
#
# Metrics compared:
#   EIG_rel  — Relative Exposure Inequity Gradient       [this work]
#   A_t      — Absolute Disparity (low - high)           [Harper & Lynch 2006]
#   R_t      — Relative Ratio (low / high)               [WHO HEAT 2023]
#   RCI      — Relative Concentration Index              [Wagstaff et al. 1991]
#   SII      — Slope Index of Inequality                 [Mackenbach & Kunst 1997]
#   BGV      — Between-Group Variance                    [WHO HEAT 2023]
#   BGSD     — Between-Group Standard Deviation          [WHO HEAT 2023]
#
# Pearson r and Spearman rho reported; both computed within each pollutant
# and pooled across pollutants. Output: CSV table + heatmap figure.
# =============================================================================

cat("\n== SECTION 6: Correlation matrix EIG_rel vs. other metrics ==\n")

# Load annual_all_metrics files (from 03_StatisticalAnalyses.R, not eig_annual)
load_annual_metrics <- function(state) {
  slug <- gsub(" ", "_", state)
  path <- file.path("States", state, paste0("annual_all_metrics_", slug, ".csv"))
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) %>% mutate(State = state)
}

cat("  Loading annual_all_metrics files...\n")
metrics_all <- map_dfr(ALL_STATES, load_annual_metrics)
cat(sprintf("  Loaded: %s rows across %d states\n",
            format(nrow(metrics_all), big.mark = ","),
            n_distinct(metrics_all$State)))

# All metrics in one vector — EIG_rel is the focus but all are correlated
# with each other in the full matrix
ALL_METRICS <- c("EIG_rel", "A_t", "R_t", "RCI", "SII", "BGV", "BGSD")

# PM2.5 only, complete cases across all metrics
corr_base <- metrics_all %>%
  filter(Pollutant == "PM2.5") %>%
  filter(if_all(all_of(ALL_METRICS), ~ !is.na(.) & is.finite(.)))

cat(sprintf("  PM2.5 complete rows for correlation: %s\n",
            format(nrow(corr_base), big.mark = ",")))

# Full pairwise correlation matrix (Pearson)
corr_mat_r <- cor(corr_base[ALL_METRICS], method = "pearson")
corr_mat_p <- matrix(NA_real_, nrow = length(ALL_METRICS), ncol = length(ALL_METRICS),
                     dimnames = list(ALL_METRICS, ALL_METRICS))
for (i in seq_along(ALL_METRICS)) {
  for (j in seq_along(ALL_METRICS)) {
    if (i != j) {
      ct <- cor.test(corr_base[[ALL_METRICS[i]]],
                     corr_base[[ALL_METRICS[j]]],
                     method = "pearson")
      corr_mat_p[i, j] <- ct$p.value
    }
  }
}

# Save as tidy CSV
corr_tidy <- as.data.frame(corr_mat_r) %>%
  tibble::rownames_to_column("Metric_row") %>%
  pivot_longer(-Metric_row, names_to = "Metric_col", values_to = "Pearson_r") %>%
  left_join(
    as.data.frame(corr_mat_p) %>%
      tibble::rownames_to_column("Metric_row") %>%
      pivot_longer(-Metric_row, names_to = "Metric_col", values_to = "Pearson_p"),
    by = c("Metric_row", "Metric_col")
  ) %>%
  mutate(
    Pearson_r = round(Pearson_r, 4),
    Pearson_p = round(Pearson_p, 4),
    N         = nrow(corr_base)
  ) %>%
  filter(Metric_row != Metric_col) %>%
  arrange(Metric_row, Metric_col)

out_corr_csv <- file.path(out_dir, "tbl_pm25_correlation_matrix.csv")
write_csv(corr_tidy, out_corr_csv, na = "")
cat(sprintf("  Saved: %s\n", basename(out_corr_csv)))

# Print correlation matrix to console
cat("\n  PM2.5 Pearson correlation matrix:\n")
print(round(corr_mat_r, 3))

# ── Full pairwise heatmap: rows and columns are all metrics ───────────────
cat("\n  Building PM2.5 correlation matrix heatmap...\n")

# Order: EIG_rel first, then others
metric_order <- ALL_METRICS

corr_heat <- corr_tidy %>%
  mutate(
    Metric_row = factor(Metric_row, levels = rev(metric_order)),
    Metric_col = factor(Metric_col, levels = metric_order),
    r_label    = sprintf("%.3f", Pearson_r),
    sig        = !is.na(Pearson_p) & Pearson_p < 0.05,
    # Diagonal cells (self-correlation = 1.0) not shown — handled by filter above
  )

# Add diagonal tiles (r = 1, grey)
diag_tiles <- tibble(
  Metric_row = factor(metric_order, levels = rev(metric_order)),
  Metric_col = factor(metric_order, levels = metric_order),
  Pearson_r  = NA_real_,
  r_label    = "",
  sig        = FALSE
)

r_max <- max(abs(corr_heat$Pearson_r), na.rm = TRUE)
r_lim <- min(ceiling(r_max * 10) / 10 + 0.05, 1.0)

fig_corr <- ggplot(corr_heat,
                   aes(x = Metric_col, y = Metric_row, fill = Pearson_r)) +
  geom_tile(data = diag_tiles, fill = "#E8E8E8", colour = "white", linewidth = 0.5) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(
    aes(label    = r_label,
        fontface = ifelse(sig, "bold", "plain")),
    size = 3.5, colour = "grey10"
  ) +
  scale_fill_gradient2(
    low      = "#2166AC",
    mid      = "white",
    high     = "#D6604D",
    midpoint = 0,
    limits   = c(-r_lim, r_lim),
    oob      = squish,
    name     = "Pearson r",
    na.value = "#E8E8E8"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    x       = NULL,
    y       = NULL,
    caption = paste0(
      "Pearson r for all pairwise combinations of disparity metrics, ",
      "PM2.5 only, across all state-years with complete data (N = ",
      nrow(corr_base), ").\n",
      "Bold values: p < 0.05.  Blue: negative correlation.  Red: positive.  ",
      "Grey diagonal: self-correlation."
    )
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.text.x       = element_text(size = 9.5, face = "bold", colour = "grey10"),
    axis.text.y       = element_text(size = 9.5, colour = "grey10"),
    axis.line         = element_blank(),
    axis.ticks        = element_blank(),
    legend.position   = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(0.4, "cm"),
    panel.border      = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
    plot.caption      = element_text(size = 7.5, colour = "grey40",
                                     hjust = 0, margin = margin(t = 6)),
    plot.margin       = margin(8, 8, 8, 8)
  )

out_corr_pdf <- file.path(out_dir, "fig_pm25_correlation_matrix.pdf")
out_corr_png <- file.path(out_dir, "fig_pm25_correlation_matrix.png")
ggsave(out_corr_pdf, fig_corr, width = 6, height = 5.5, units = "in")
ggsave(out_corr_png, fig_corr, width = 6, height = 5.5, units = "in", dpi = 300)
cat(sprintf("  Saved: %s\n", basename(out_corr_pdf)))
cat(sprintf("  Saved: %s\n", basename(out_corr_png)))

cat("\nDone.\n")