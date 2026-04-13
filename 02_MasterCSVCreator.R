# install.packages(c("dplyr", "readr", "tidyr", "stringr", "purrr"))

# ===========================================
# MASTER CSV CREATOR
# Combines:
#   1) EPA AQS Daily Data  (2000-2023)
#   2) Population by County (Census Bureau)
#   3) Median Household Income by County (ACS 2019-2023)
# ===========================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# =============================================================================
# CONFIG
# =============================================================================

EPA_DIR  <- "epa_data"
POP_FILE <- "Population_StatesCounty_2000_2023.csv"
MHI_FILE <- "MHIDataUSCounties.csv"
OUT_FILE <- "master_county_year.csv"
YEARS    <- 2000:2023

POLLUTANTS <- list(PM2.5 = "PM2.5", NO2 = "NO2", O3 = "O3", SO2 = "SO2", CO = "CO")

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

pad_fips <- function(x) str_pad(as.character(x), width = 5, side = "left", pad = "0")

# =============================================================================
# COUNTY NAME CROSSWALK
#
# Maps MHI normalized county names -> Population normalized county names.
# Only entries where the two files genuinely differ after normalization.
#
# KEY RULES:
#  1. Virginia independent cities (e.g. "Norton City" vs "Norton city") do NOT
#     need entries here. normalize_county strips both "City" and "city", so both
#     sides already produce the same key (e.g. "NORTON"). Adding them caused the
#     bug where "NORTON" was remapped to "NORTON CITY", breaking the join for
#     unrelated counties named Norton in other states (Kansas, etc.).
#
#  2. Only add entries for names that are structurally different between the two
#     files after normalization.
# =============================================================================

COUNTY_CROSSWALK <- c(
  
  # --- Virginia: MHI uses combined labels for merged jurisdictions -----------
  # MHI "Alleghany County and Clifton Forge City" -> norm -> "ALLEGHANY COUNTY AND CLIFTON FORGE"
  # Population has "Alleghany County"              -> norm -> "ALLEGHANY"
  "ALLEGHANY COUNTY AND CLIFTON FORGE"  = "ALLEGHANY",
  
  # MHI "Halifax County with South Boston City" -> norm -> "HALIFAX COUNTY WITH SOUTH BOSTON"
  # Population has "Halifax County"              -> norm -> "HALIFAX"
  "HALIFAX COUNTY WITH SOUTH BOSTON"    = "HALIFAX",
  
  # MHI "Bedford City and County" -> norm -> "BEDFORD CITY AND"
  # Population has "Bedford County" and "Bedford city" both -> norm -> "BEDFORD"
  "BEDFORD CITY AND"                    = "BEDFORD",
  
  # --- Maryland: MHI drops apostrophes from possessive county names ----------
  # MHI "Prince Georges County"  -> norm -> "PRINCE GEORGES"
  # Population "Prince George's County" -> norm -> "PRINCE GEORGE'S"
  "PRINCE GEORGES"   = "PRINCE GEORGE'S",
  "QUEEN ANNES"      = "QUEEN ANNE'S",
  "ST. MARYS"        = "ST. MARY'S",
  
  # --- New Mexico: MHI merges "De Baca" into "DeBaca" -----------------------
  # MHI "DeBaca County"  -> norm -> "DEBACA"
  # Population "De Baca County" -> norm -> "DE BACA"
  "DEBACA"           = "DE BACA",
  
  # --- Alaska: county renamed in 2015 ----------------------------------------
  # MHI uses new name "Kusilvak Census Area" -> norm -> "KUSILVAK"
  # Population uses old name "Wade Hampton Census Area" -> norm -> "WADE HAMPTON"
  # Both datasets are therefore needed together to cover the full 2000-2023 span
  "KUSILVAK"         = "WADE HAMPTON",
  
  # --- South Dakota: county renamed in 2015 ----------------------------------
  # MHI uses new name "Oglala Lakota County" -> norm -> "OGLALA LAKOTA"
  # Population uses old name "Shannon County" -> norm -> "SHANNON"
  "OGLALA LAKOTA"    = "SHANNON"
  
  # --- KNOWN UNRESOLVABLE GAPS (documented, not fixable via crosswalk) -------
  # Illinois LaSalle County     : present in Population but entirely absent from MHI source file
  # Nevada Esmeralda County     : in both files but MHI = NA (Census-suppressed, pop ~725)
  # Texas Kenedy County         : in both files but MHI = NA (Census-suppressed, pop ~351)
  # Alaska Petersburg Census Area: MHI present, Population NA from 2011 onward
  #                                (split from Wrangell-Petersburg Borough in 2013)
  # Connecticut all 8 counties  : Population NA for 2021-2023
  #                                (CT reorganized county-level census reporting after 2020)
)


# =============================================================================
# SECTION 1: EPA DATA
# =============================================================================

cat("== SECTION 1: Loading EPA data ==\n")

# EPA files pre-2020 use ISO format (2019-12-31); 2020+ switched to M/D/YYYY (1/1/2020).
# Read Date Local as character and parse both formats with parse_date_time().
epa_col_types <- cols_only(
  `State Name`      = col_character(),
  `County Name`     = col_character(),
  `Date Local`      = col_character(),
  `Arithmetic Mean` = col_double(),
  `AQI`             = col_double()
)

parse_epa_date <- function(x) {
  # Try ISO first (pre-2020), then M/D/YYYY (2020+)
  lubridate::parse_date_time(x, orders = c("Ymd", "mdY"), quiet = TRUE) %>%
    as.Date()
}
# EPA-specific county name fixes applied after normalize_county.
EPA_COUNTY_FIXES <- c(
  "SAINT CLAIR"           = "ST. CLAIR",        # Illinois
  "SKAGWAY-HOONAH-ANGOON" = "HOONAH-ANGOON"     # Alaska pre-2007
)



load_epa_pollutant <- function(pollutant_dir, label) {
  files <- list.files(file.path(EPA_DIR, pollutant_dir), pattern = "\\.csv$", full.names = TRUE)
  
  # Use a lookahead anchored to end of filename to extract the year.
  # "\\d{4}" alone grabs the first 4-digit run, which is the parameter code
  # (e.g. "8810" from "daily_88101_2000.csv"). The lookahead pins extraction
  # to the year immediately before ".csv".
  files         <- files[str_detect(basename(files), "(?<=_)\\d{4}(?=\\.csv$)")]
  file_years    <- as.integer(str_extract(basename(files), "(?<=_)\\d{4}(?=\\.csv$)"))
  files         <- files[file_years %in% YEARS]
  
  if (length(files) == 0) {
    warning("No EPA files found for ", label, " in ", file.path(EPA_DIR, pollutant_dir))
    return(NULL)
  }
  
  cat(sprintf("  Loading %s: %d files\\n", label, length(files)))
  
  map_dfr(files, function(f) {
    yr <- as.integer(str_extract(basename(f), "(?<=_)\\d{4}(?=\\.csv$)"))
    read_csv(f, col_types = epa_col_types, show_col_types = FALSE) %>%
      mutate(
        `Date Local` = parse_epa_date(`Date Local`),
        Year = yr, Pollutant = label
      )
  })
}

epa_raw <- imap_dfr(POLLUTANTS, ~ load_epa_pollutant(.x, .y))
cat(sprintf("  Total EPA rows loaded: %s\n\n", format(nrow(epa_raw), big.mark = ",")))

cat("  Aggregating to county-year level...\n")

epa_wide <- epa_raw %>%
  filter(!is.na(`State Name`), !is.na(`County Name`),
         !str_detect(`County Name`, regex("mobile monitor", ignore_case = TRUE))) %>%
  mutate(
    State = str_trim(`State Name`),
    # Normalise county names BEFORE group_by so that different spellings of the
    # same county (e.g. "Saint Clair" and "St. Clair" across file vintages)
    # collapse into one row during summarise rather than creating duplicates.
    County_Norm = {
      nm <- normalize_county(str_trim(`County Name`))
      ifelse(nm %in% names(EPA_COUNTY_FIXES), EPA_COUNTY_FIXES[nm], nm)
    }
  ) %>%
  group_by(State, County_Norm, Year, Pollutant) %>%
  summarise(
    Mean_Concentration = round(mean(`Arithmetic Mean`, na.rm = TRUE), 4),
    Mean_AQI           = round(mean(`AQI`,             na.rm = TRUE), 2),
    N_Daily_Obs        = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols     = c(State, County_Norm, Year),
    names_from  = Pollutant,
    values_from = c(Mean_Concentration, Mean_AQI, N_Daily_Obs)
  ) %>%
  select(State, County_Norm, Year,
         matches("PM2\\.5"), matches("_NO2"), matches("_O3"),
         matches("_SO2"), matches("_CO"))

cat(sprintf("  EPA wide: %s rows, %d columns\n\n",
            format(nrow(epa_wide), big.mark = ","), ncol(epa_wide)))


# =============================================================================
# SECTION 2: POPULATION DATA
# =============================================================================

cat("== SECTION 2: Loading Population data ==\n")

pop_long <- read_csv(POP_FILE, show_col_types = FALSE) %>%
  filter(STNAME != CTYNAME) %>%        # drop state-total rows
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Population") %>%
  mutate(
    Year        = as.integer(Year),
    State       = str_trim(STNAME),
    County_Raw  = str_trim(CTYNAME),
    County_Norm = normalize_county(County_Raw)
  ) %>%
  filter(Year %in% YEARS) %>%
  select(State, County_Raw, County_Norm, Year, Population)

cat(sprintf("  Population rows: %s\n\n", format(nrow(pop_long), big.mark = ",")))


# =============================================================================
# SECTION 3: MHI DATA
# =============================================================================

cat("== SECTION 3: Loading MHI data ==\n")

mhi_clean <- read_csv(MHI_FILE, show_col_types = FALSE) %>%
  # Strip footer metadata rows (blank or non-numeric FIPS)
  filter(!is.na(FIPS), str_detect(as.character(FIPS), "^\\d+$")) %>%
  rename(
    County_Full = County,
    MHI         = `Value (Dollars)`,
    MHI_Rank    = `Rank within US (of 3141 counties)`
  ) %>%
  mutate(
    FIPS        = pad_fips(FIPS),
    State       = str_trim(str_extract(County_Full, "(?<=,)[^,]+$")),
    County_Suf  = str_trim(str_remove(County_Full, ",\\s*[^,]+$")),
    County_Norm = normalize_county(County_Suf),
    MHI         = suppressWarnings(as.numeric(MHI)),   # NA for suppressed values
    # Apply crosswalk: remap MHI normalized names to match Population normalized names
    County_Norm = if_else(
      County_Norm %in% names(COUNTY_CROSSWALK),
      COUNTY_CROSSWALK[County_Norm],
      County_Norm
    )
  ) %>%
  filter(!is.na(State), !is.na(County_Norm)) %>%
  select(FIPS, State, County_Norm, MHI, MHI_Rank) %>%
  # Dedup: crosswalk can map multiple MHI entries to the same County_Norm
  # (e.g. if MHI file contains both old and new names for a renamed county).
  # Keep the row with a valid MHI value; if both valid, keep the first.
  arrange(State, County_Norm, is.na(MHI)) %>%
  distinct(State, County_Norm, .keep_all = TRUE)

cat(sprintf("  MHI rows: %d\n\n", nrow(mhi_clean)))


# =============================================================================
# SECTION 4: BUILD SPINE — Population first
#
# Starting from Population guarantees all 3,142 counties × 24 years appear in
# the master. Counties with no air quality monitor get NA for pollutant columns,
# which is correct. Previously the spine was EPA-first, silently dropping every
# county with no monitor (~1,278 counties).
# =============================================================================

cat("== SECTION 4: Building population spine and joining MHI ==\n")

pop_mhi <- pop_long %>%
  left_join(mhi_clean, by = c("State", "County_Norm"))

cat(sprintf("  Matched to MHI:     %s rows\n", format(sum(!is.na(pop_mhi$MHI)),  big.mark=",")))
cat(sprintf("  Not matched to MHI: %s rows\n\n", format(sum(is.na(pop_mhi$MHI)), big.mark=",")))


# =============================================================================
# SECTION 5: JOIN EPA ONTO SPINE
# =============================================================================

cat("== SECTION 5: Joining EPA onto population spine ==\n")

master <- pop_mhi %>%
  left_join(epa_wide, by = c("State", "County_Norm", "Year"))

cat(sprintf("  Rows with PM2.5 monitor data: %s\n",
            format(sum(!is.na(master$Mean_Concentration_PM2.5)), big.mark=",")))
cat(sprintf("  Rows with no monitor data:    %s\n\n",
            format(sum(is.na(master$Mean_Concentration_PM2.5)),  big.mark=",")))


# =============================================================================
# SECTION 6: FINAL CLEANUP & OUTPUT
# =============================================================================

cat("== SECTION 6: Final cleanup and writing output ==\n")

master_final <- master %>%
  select(
    FIPS,
    State,
    County      = County_Raw,    # restore suffixed name e.g. "Baldwin County"
    Year,
    Population,
    MHI,
    MHI_Rank,
    Mean_Concentration_PM2.5, Mean_AQI_PM2.5, N_Daily_Obs_PM2.5,
    Mean_Concentration_NO2,   Mean_AQI_NO2,   N_Daily_Obs_NO2,
    Mean_Concentration_O3,    Mean_AQI_O3,    N_Daily_Obs_O3,
    Mean_Concentration_SO2,   Mean_AQI_SO2,   N_Daily_Obs_SO2,
    Mean_Concentration_CO,    Mean_AQI_CO,    N_Daily_Obs_CO
  ) %>%
  arrange(State, County, Year)

write_csv(master_final, OUT_FILE, na = "")
cat(sprintf("  Output written: %s\n", OUT_FILE))
cat(sprintf("  Rows: %s | Columns: %d\n\n",
            format(nrow(master_final), big.mark=","), ncol(master_final)))


# =============================================================================
# SECTION 7: MATCH QUALITY REPORT
# =============================================================================

cat("== MATCH QUALITY REPORT ==\n")

total <- nrow(master_final)

tibble(
  Field = c("Population", "MHI", "FIPS",
            "PM2.5", "NO2", "O3", "SO2", "CO"),
  N = c(
    sum(!is.na(master_final$Population)),
    sum(!is.na(master_final$MHI)),
    sum(!is.na(master_final$FIPS)),
    sum(!is.na(master_final$Mean_Concentration_PM2.5)),
    sum(!is.na(master_final$Mean_Concentration_NO2)),
    sum(!is.na(master_final$Mean_Concentration_O3)),
    sum(!is.na(master_final$Mean_Concentration_SO2)),
    sum(!is.na(master_final$Mean_Concentration_CO))
  )
) %>%
  mutate(Pct = round(N / total * 100, 1)) %>%
  print(n = Inf)

cat(sprintf(
  "\nExpected rows (3,142 counties x %d years): %s\nActual rows:                                %s\n",
  length(YEARS),
  format(3142 * length(YEARS), big.mark=","),
  format(nrow(master_final),   big.mark=",")
))

# Save residual unmatched counties for review
still_unmatched <- master_final %>%
  filter(is.na(MHI)) %>%
  distinct(State, County) %>%
  arrange(State, County)

if (nrow(still_unmatched) > 0) {
  cat(sprintf("\nResidual counties missing MHI (%d) -- see unmatched_counties.csv:\n",
              nrow(still_unmatched)))
  print(still_unmatched, n = 30)
  write_csv(still_unmatched, "unmatched_counties.csv")
} else {
  cat("\nAll counties matched to MHI.\n")
}

cat("\nDone.\n")