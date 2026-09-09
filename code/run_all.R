# run_all.R -- pipeline orchestrator
# Project: The U.S. Retirement System: Fast Facts (EIG-Retirement-Fast-Facts)
# Research question: access, participation, and matching in the U.S. retirement savings system
#   for the civilian labor force (SIPP 2025 release, pooled 2022-2025 panels, reference year 2024),
#   the Saver's Match eligibility-vs-access wedge, and the CBO-style retirement tax-expenditure
#   replication.
# Pipeline order: the script_flags list below IS the execution order (deliberately non-numeric;
#   every inversion is a real data dependency documented in code/README.md), then testthat.

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
required_packages <- c(
  "here",
  "arrow",
  "dplyr"
)

## Availability check only: the orchestrator itself calls no arrow/dplyr functions and
## every child script loads its own libraries in a fresh environment, so nothing is attached here.
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Missing package: ", pkg, ". Please install it before running."))
  }
}

#################
### Set paths ###
#################
# Resolve the project root without a hard-coded, user-specific map so the
# script runs out-of-the-box. Prefer here::here() (works from any
# subdirectory inside an RStudio project / git repo); otherwise fall back to
# the current working directory. Run this script from the project root, or
# open the project's .Rproj, and paths resolve automatically.
if (requireNamespace("here", quietly = TRUE)) {
  path_project <- here::here()
} else {
  path_project <- getwd()
  message(
    "Package 'here' not installed; using the working directory as the ",
    "project root:\n  ", path_project,
    "\nRun this script from the project root, or install 'here'."
  )
}

path_data <- file.path(path_project, "data")
path_code <- file.path(path_project, "code")
path_output <- file.path(path_project, "output")

# Optional convenience paths
path_raw <- file.path(path_data, "raw")
path_processed <- file.path(path_data, "processed")
path_figures <- file.path(path_output, "figures")
path_tables <- file.path(path_output, "tables")

###########################
### Simple run logging  ###
###########################
# Creates one log file per run in output/logs
path_logs <- file.path(path_output, "logs")
if (!dir.exists(path_logs)) {
  dir.create(path_logs, recursive = TRUE)
}

run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(path_logs, paste0("run_all_", run_stamp, ".log"))

log_message <- function(message_text) {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", message_text)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

log_message("Starting run_all.R")
log_message(paste0("Project path: ", path_project))

###################
### run scripts ###
###################
# Toggle which scripts run with TRUE/FALSE flags. Each flag maps to a script file in code/;
# flip one to FALSE to skip that stage on a targeted rerun.
run_01_build_dataset  <- TRUE
run_02_analysis       <- TRUE   # pillars incl. I1 match dollars
run_03_savers_match   <- TRUE   # incl. I2 dependent-refined eligibility
run_07_replicate_se   <- TRUE   # I4 replicate-weight standard errors
run_05_make_figures   <- TRUE   # pillar figures (EIG-styled + Tufte)
run_08_tax_units      <- TRUE   # TE replication: CY2024 annual income, deciles, TY2024 tax-filing units
run_09_tax_engine     <- TRUE   # TE replication: tax-computation stage (PolicyEngine-US)
run_11_stage5_imput   <- TRUE   # TE replication: Stage-5 imputations (DB employer, Roth, outside IRA)
run_10_tax_expend     <- TRUE   # TE replication: PV tax expenditure by decile + benchmarks (both rows)
run_12_te_bench_se    <- TRUE   # TE replication: external calibration + replicate-weight SEs
run_13_te_figures     <- TRUE   # TE replication: EIG-styled figures (deciles, CBO comparison)
run_14_te_crosscuts   <- TRUE   # TE cross-cuts with pillars + demographic/socioeconomic breakdowns
run_16_access_gap_tbl <- TRUE   # single Census-style access-gap descriptive table (all subsets)
run_17_fig1_waffle    <- TRUE   # hero: private-sector access-gap waffle (Figure 1)
run_15_chart_specs    <- TRUE   # chart payload specs for every figure/table exhibit
run_tests             <- TRUE   # run the testthat suite after the pipeline

# flag -> script filename. Order here is the execution order, which is deliberately NOT purely
# numeric: 14 before 13, 11 before 10, and 07 before 12 and 05. Every inversion is a real data
# dependency and is documented in code/README.md -- do not "fix" it by renaming scripts.
script_flags <- list(
  "01_build_dataset.R"      = run_01_build_dataset,
  "02_analysis.R"           = run_02_analysis,
  "03_savers_match.R"       = run_03_savers_match,
  "07_replicate_weights.R"  = run_07_replicate_se,
  "05_make_figures.R"       = run_05_make_figures,
  "08_tax_units.R"          = run_08_tax_units,
  "09_tax_engine.R"         = run_09_tax_engine,   # dispatcher -> 09_policyengine_te.py
  "11_stage5_imputations.R" = run_11_stage5_imput,
  "10_tax_expenditure.R"    = run_10_tax_expend,
  "12_te_benchmarks_se.R"   = run_12_te_bench_se,
  "14_te_crosscuts.R"       = run_14_te_crosscuts,   # before 13: te_fig4 reads te_crosscuts.csv
  "13_te_figures.R"         = run_13_te_figures,
  "16_access_gap_table.R"   = run_16_access_gap_tbl, # reads sipp_fastfacts.parquet (from 01)
  "17_fig1_hero_waffle.R"   = run_17_fig1_waffle,    # reads sipp_fastfacts.parquet (from 01)
  "15_publish_datawrapper.R" = run_15_chart_specs    # chart payloads incl. te_fig1b, Table 1
)

scripts_to_run <- names(script_flags)[vapply(script_flags, isTRUE, logical(1))]

if (length(scripts_to_run) == 0) {
  log_message("No scripts selected (all flags FALSE). Exiting.")
} else {
  for (script_name in scripts_to_run) {
    script_path <- file.path(path_code, script_name)

    if (!file.exists(script_path)) {
      log_message(paste0("ERROR: script not found -> ", script_name))
      stop(paste0("Script not found: ", script_path))
    }

    log_message(paste0("Running: ", script_name))
    script_start <- Sys.time()

    tryCatch(
      {
        # Source each script in a fresh environment so scripts do not leak
        # objects into one another or into the orchestrator's workspace.
        source(script_path, local = new.env(parent = globalenv()))
        elapsed <- round(as.numeric(difftime(Sys.time(), script_start, units = "secs")), 2)
        log_message(paste0("Completed: ", script_name, " (", elapsed, " seconds)"))
      },
      error = function(e) {
        log_message(paste0("ERROR in ", script_name, ": ", conditionMessage(e)))
        stop(e)
      }
    )
  }
}

if (isTRUE(run_tests)) {
  log_message("Running testthat suite ...")
  source(file.path(path_code, "tests", "testthat.R"), local = new.env(parent = globalenv()))
  log_message("Tests complete.")
}

log_message("Finished run_all.R")
log_message(paste0("Log saved to: ", log_file))
