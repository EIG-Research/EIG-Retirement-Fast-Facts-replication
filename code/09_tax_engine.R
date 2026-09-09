# code/09_tax_engine.R
# TE Stage 3: run the tax-computation stage (PolicyEngine-US)
# Reads tax_units.parquet (from 08_tax_units.R) and writes taxsim_results.parquet, plus an
# engine-tagged copy taxsim_results_<engine>.parquet. The output schema is fixed, so the
# downstream stages 10-14 do not depend on which engine produced it.
#   system2(PYTHON_BIN, 09_policyengine_te.py)  (PolicyEngine-US, version pinned in params.R)
# The file name 'taxsim_results' is historical and is kept so the downstream contract is stable.

suppressWarnings(suppressMessages({ library(arrow) }))

path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
path_code      <- file.path(path_project, "code")
path_processed <- file.path(path_project, "data", "processed")
canonical      <- file.path(path_processed, "taxsim_results.parquet")

if (!identical(TAX_ENGINE, "policyengine")) {
  stop("09_tax_engine: TAX_ENGINE = '", TAX_ENGINE, "'. This replication package ships only ",
       "the PolicyEngine-US engine, which produced the published estimates. Unset ",
       "SIPP_TAX_ENGINE, or set it to 'policyengine'.")
}
message("09_tax_engine: TAX_ENGINE = '", TAX_ENGINE, "' (tax year ", TAX_YEAR, ")")

{
  ## PolicyEngine-US via an ISOLATED Python subprocess (NOT reticulate). PYTHON_BIN is the
  ## interpreter path resolved in params.R; set SIPP_PYTHON_BIN to choose it explicitly.
  py     <- PYTHON_BIN
  script <- file.path(path_code, "09_policyengine_te.py")
  tax_in <- file.path(path_processed, "tax_units.parquet")
  if (!file.exists(py))     stop("09_tax_engine: PYTHON_BIN not found: ", py,
                                 " (set env SIPP_PYTHON_BIN).")
  if (!file.exists(script)) stop("09_tax_engine: worker not found: ", script)
  if (!file.exists(tax_in)) stop("09_tax_engine: ", tax_in, " not found - run 08_tax_units.R first.")

  args <- c(shQuote(script),
            "--in",  shQuote(tax_in),
            "--out", shQuote(canonical),
            "--chunk", "2000",
            ## The worker ASSERTS the installed policyengine-us equals this pin (fail loud), and
            ## the OASDI cap flows from params.R instead of a duplicated literal.
            "--expect-version", shQuote(POLICYENGINE_VERSION),
            "--oasdi-cap", OASDI_WAGE_CAP)
  message("09_tax_engine: launching PolicyEngine-US (pinned ", POLICYENGINE_VERSION, ") subprocess ...")
  code <- system2(py, args = args, stdout = "", stderr = "")
  if (!identical(code, 0L)) stop("09_tax_engine: PolicyEngine subprocess failed (exit ", code,
                                 "). See stderr above.")
}

## --- Verify output + write engine-tagged copy --------------------------------
if (!file.exists(canonical)) stop("09_tax_engine: expected output not written: ", canonical)
res <- as.data.frame(read_parquet(canonical))
needed <- c("filing_unit_id", "te_ee_arc", "te_er_arc", "frate_base", "ficar_base")
miss <- setdiff(needed, names(res))
if (length(miss)) stop("09_tax_engine: taxsim_results.parquet missing downstream columns: ",
                       paste(miss, collapse = ", "))
if (nrow(res) == 0) stop("09_tax_engine: taxsim_results.parquet is empty.")

tagged <- file.path(path_processed, paste0("taxsim_results_", TAX_ENGINE, ".parquet"))
file.copy(canonical, tagged, overwrite = TRUE)
message("09_tax_engine: wrote ", basename(canonical), " (", nrow(res), " units) and tagged copy ",
        basename(tagged), ".")
