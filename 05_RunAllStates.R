# =============================================================================
# run_all_states.R
#
# Runs 03_StatisticalAnalyses.R, 04_FiguresAndTables.R, and 06_EIG_Metrics.R
# sequentially for all 50 states.
# Must be run from the same working directory as all scripts and master_county_year.csv.
#
# Execution order per state:
#   1. 03_StatisticalAnalyses.R  — disparity metrics + trend models
#   2. 04_FiguresAndTables.R     — figures and tables (skipped if 03 fails)
#   3. 06_EIG_Metrics.R          — EIG metrics + bootstrap + permutation (skipped if 03 fails)
#
# Progress and errors are logged to: run_all_states_log.txt
# A summary table is printed at the end showing pass/fail per state.
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

LOG_FILE <- "run_all_states_log.txt"

# =============================================================================
# SETUP
# =============================================================================

if (!file.exists("03_StatisticalAnalyses.R"))
  stop("03_StatisticalAnalyses.R not found. Run from the correct working directory.")
if (!file.exists("04_FiguresAndTables.R"))
  stop("04_FiguresAndTables.R not found. Run from the correct working directory.")
if (!file.exists("06_EIG_Metrics.R"))
  stop("06_EIG_Metrics.R not found. Run from the correct working directory.")
if (!file.exists("master_county_year.csv"))
  stop("master_county_year.csv not found. Run build_master_csv.R first.")

# Initialise log
writeLines(
  c(paste0("run_all_states.R  —  started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    strrep("=", 72)),
  LOG_FILE
)

log_msg <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  write(msg, LOG_FILE, append = TRUE)
}

# Results accumulator
results <- data.frame(
  State        = ALL_STATES,
  Analysis     = NA_character_,
  Figures      = NA_character_,
  EIG          = NA_character_,
  stringsAsFactors = FALSE
)

# =============================================================================
# HELPER: inject STATE into a script and run it in a fresh environment,
# capturing any errors without stopping the whole loop.
# =============================================================================

run_script_for_state <- function(script_path, state_name) {
  # Read script, replace the STATE <- "..." line with the target state
  lines <- readLines(script_path, warn = FALSE)
  lines <- sub(
    pattern     = '^STATE\\s*<-\\s*"[^"]*"',
    replacement = sprintf('STATE <- "%s"', state_name),
    x           = lines
  )
  # Write to working directory rather than system temp — avoids Windows race
  # condition where tempfile() returns a path in a not-yet-created subdirectory.
  tmp <- file.path(getwd(), paste0("_tmp_", gsub("[^A-Za-z]", "_", state_name), ".R"))
  on.exit(unlink(tmp), add = TRUE)
  writeLines(lines, tmp)
  
  tryCatch({
    source(tmp, local = new.env(parent = globalenv()))
    "OK"
  }, error = function(e) {
    paste0("ERROR: ", conditionMessage(e))
  }, warning = function(w) {
    # Promote warnings that indicate total failure (e.g. no data at all)
    msg <- conditionMessage(w)
    if (grepl("No files found|not found|zero rows|insufficient", msg, ignore.case = TRUE)) {
      paste0("WARN: ", msg)
    } else {
      withCallingHandlers(
        source(tmp, local = new.env(parent = globalenv())),
        warning = function(w) invokeRestart("muffleWarning")
      )
      "OK"
    }
  })
}

# =============================================================================
# MAIN LOOP
# =============================================================================

t_total <- proc.time()

for (i in seq_along(ALL_STATES)) {
  state <- ALL_STATES[i]
  log_msg(sprintf("\n[%2d/%2d] %s", i, length(ALL_STATES), state))
  log_msg(strrep("-", 50))
  
  # ---- Analysis ----
  t0 <- proc.time()
  log_msg("  Running 03_StatisticalAnalyses.R ...")
  ana_result <- run_script_for_state("03_StatisticalAnalyses.R", state)
  elapsed_ana <- round((proc.time() - t0)["elapsed"])
  log_msg(sprintf("  Analysis: %s  (%ds)", ana_result, elapsed_ana))
  results$Analysis[i] <- ana_result
  
  # ---- Figures (only if analysis succeeded) ----
  if (startsWith(ana_result, "OK")) {
    t0 <- proc.time()
    log_msg("  Running 04_FiguresAndTables.R ...")
    fig_result <- run_script_for_state("04_FiguresAndTables.R", state)
    elapsed_fig <- round((proc.time() - t0)["elapsed"])
    log_msg(sprintf("  Figures:  %s  (%ds)", fig_result, elapsed_fig))
    results$Figures[i] <- fig_result
  } else {
    results$Figures[i] <- "SKIPPED (analysis failed)"
    log_msg("  Figures:  SKIPPED (analysis failed)")
  }
  
  # ---- EIG Metrics (only if analysis succeeded) ----
  if (startsWith(ana_result, "OK")) {
    t0 <- proc.time()
    log_msg("  Running 06_EIG_Metrics.R ...")
    eig_result <- run_script_for_state("06_EIG_Metrics.R", state)
    elapsed_eig <- round((proc.time() - t0)["elapsed"])
    log_msg(sprintf("  EIG:      %s  (%ds)", eig_result, elapsed_eig))
    results$EIG[i] <- eig_result
  } else {
    results$EIG[i] <- "SKIPPED (analysis failed)"
    log_msg("  EIG:      SKIPPED (analysis failed)")
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

elapsed_total <- round((proc.time() - t_total)["elapsed"])

log_msg(sprintf("\n%s", strrep("=", 72)))
log_msg(sprintf("COMPLETE  —  %d states  |  total time: %dm %ds",
                length(ALL_STATES),
                elapsed_total %/% 60,
                elapsed_total %%  60))
log_msg(strrep("=", 72))

n_ana_ok  <- sum(startsWith(results$Analysis, "OK"))
n_fig_ok  <- sum(startsWith(results$Figures,  "OK"), na.rm = TRUE)
n_eig_ok  <- sum(startsWith(results$EIG,      "OK"), na.rm = TRUE)
n_ana_err <- sum(!startsWith(results$Analysis, "OK"))
n_fig_err <- sum(!startsWith(results$Figures,  "OK") &
                   results$Figures != "SKIPPED (analysis failed)", na.rm = TRUE)
n_eig_err <- sum(!startsWith(results$EIG,      "OK") &
                   results$EIG     != "SKIPPED (analysis failed)", na.rm = TRUE)

log_msg(sprintf("  Analysis passed : %d / %d", n_ana_ok,  length(ALL_STATES)))
log_msg(sprintf("  Analysis failed : %d / %d", n_ana_err, length(ALL_STATES)))
log_msg(sprintf("  Figures  passed : %d / %d", n_fig_ok,  n_ana_ok))
log_msg(sprintf("  Figures  failed : %d / %d", n_fig_err, n_ana_ok))
log_msg(sprintf("  EIG      passed : %d / %d", n_eig_ok,  n_ana_ok))
log_msg(sprintf("  EIG      failed : %d / %d", n_eig_err, n_ana_ok))

# States with any failure
failures <- results[
  !startsWith(results$Analysis, "OK") |
    (!startsWith(results$Figures, "OK") & results$Figures != "SKIPPED (analysis failed)") |
    (!startsWith(results$EIG,     "OK") & results$EIG     != "SKIPPED (analysis failed)"),
]

if (nrow(failures) > 0) {
  log_msg("\nStates with failures:")
  for (i in seq_len(nrow(failures))) {
    log_msg(sprintf("  %-20s  analysis=%-40s  figures=%-40s  eig=%s",
                    failures$State[i],
                    failures$Analysis[i],
                    failures$Figures[i],
                    failures$EIG[i]))
  }
} else {
  log_msg("\nAll 50 states completed successfully.")
}

log_msg(sprintf("\nFull log: %s", normalizePath(LOG_FILE)))

# Print summary table to console
cat("\n")
print(results, row.names = FALSE)