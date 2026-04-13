# ====================================================================
# WELCOME TO MY EPA DAILY DATA DOWNLOADER
# LOOK UPON MY WORKS AND DESPAIR!
# This script targets: 
# Pollutants: PM2.5, NO2, O3, SO2, and CO
# Years 2000-2024(Variable)
# Sourced from: https://aqs.epa.gov/aqsweb/airdata/download_files.html
# ====================================================================

library(httr)
# Config
BASE_URL <- "https://aqs.epa.gov/aqsweb/airdata"
OUTPUT_DIR <- "epa_data"
YEARS <- 2000:2023

# Pollutant Mappings
POLLUTANTS <- list(
  PM2.5 = "88101",
  NO2   = "42602",
  O3    = "44201",
  SO2   = "42401",
  CO    = "42101"
)

# Setup Directories

for (name in names(POLLUTANTS)) {
  dir.create(file.path(OUTPUT_DIR, name), recursive = TRUE, showWarnings = FALSE)
}
cat("Output directories created:", normalizePath(OUTPUT_DIR), "\n\n")

# Download and Unzip

results <- data.frame(
  pollutant = character(),
  year      = integer(),
  status    = character(),
  stringsAsFactors = FALSE
)

# For each pollutant...
for (name in names(POLLUTANTS)) {
  code    <- POLLUTANTS[[name]]
  pol_dir <- file.path(OUTPUT_DIR, name)
  cat("=== Downloading", name, "===\n")

  # For each year...
  for (yr in YEARS) {
    filename <- sprintf("daily_%s_%d.zip", code, yr)
    url      <- file.path(BASE_URL, filename)
    zip_path <- file.path(pol_dir, filename)
    cat(sprintf("  [%d] %s ... ", yr, filename))
    
    # Skip if CSV already exists for this year
    existing_csv <- list.files(pol_dir, pattern = sprintf("_%d\\.csv$", yr))
    if (length(existing_csv) > 0) {
      cat("already downloaded, skipping.\n")
      results <- rbind(results, data.frame(pollutant = name, year = yr, status = "skipped"))
      next
    }

    # Attempt download
    tryCatch({
      resp <- GET(url, write_disk(zip_path, overwrite = TRUE),
                  timeout(120),
                  progress())
      
      if (http_error(resp)) {
        cat(sprintf("HTTP error %d\n", status_code(resp)))
        file.remove(zip_path)
        results <- rbind(results, data.frame(pollutant = name, year = yr, status = "http_error"))
        next
      }
      
      # Unzip
      unzip(zip_path, exdir = pol_dir)
      file.remove(zip_path)   # Delete ZIP, keep CSV only
      cat(" done.\n")
      results <- rbind(results, data.frame(pollutant = name, year = yr, status = "success"))
      
    }, error = function(e) {
      cat(sprintf(" ERROR: %s\n", conditionMessage(e)))
      if (file.exists(zip_path)) file.remove(zip_path)
      results <<- rbind(results, data.frame(pollutant = name, year = yr, status = "error"))
    })
  }
  
  cat("\n")
}

# Verify and Summarize Downloads

cat("====== Download Summary ======\n")
summary_tbl <- table(results$pollutant, results$status)
print(summary_tbl)

failed <- results[results$status %in% c("http_error", "error"), ]
if (nrow(failed) > 0) {
  cat("\nFailed downloads:\n")
  print(failed)
} else {
  cat("\nAll files downloaded successfully!\n")
}
# Save log
log_path <- file.path(OUTPUT_DIR, "download_log.csv")
write.csv(results, log_path, row.names = FALSE)
cat(sprintf("\nLog saved to: %s\n", normalizePath(log_path)))












