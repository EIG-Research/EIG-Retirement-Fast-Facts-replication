# code/_shared/params.R
# Single source of policy + coding constants for the SIPP Fast Facts pipeline.
# Central constants for the pipeline: vintage, sample frame, statutory parameters, and
# Value codes are Census-verbatim from the SIPP Data Dictionary (see the vintage doc under
# tax-engine settings. Change constants here, not in the analysis scripts.
#
# LIVING-REPO VINTAGE SWITCH: to move to a new annual SIPP release, update the vintage block below
# (SIPP_PANEL, REF_YEAR, RAW_DTA_FILE, EXTRACT_PARQUET, RW_DTA_FILE, RW_EXTRACT_PARQUET) plus the
# TAX_YEAR block; every analysis/figure script derives its filenames and source-line labels from
# these constants, so no per-script filename edits are needed. Updated 2026-07-15: 2024 -> 2025 SIPP
# (reference year 2023 -> 2024).

## --- Vintage / sample scope -------------------------------------------------
SIPP_PANEL      <- 2025L      # SPANEL of the newest panel in the release (file also pools older panels)
SIPP_WAVE       <- 1L         # SWAVE of the newest panel
REF_MONTH       <- 12L        # MONTHCODE == 12 (December reference month cross-section)
REF_YEAR        <- 2024L      # reference calendar year (fielded 2025 -> covers CY2024)
AGE_MIN         <- 18L
AGE_MAX         <- 64L
FT_HOURS_MIN    <- 35L        # usual weekly hours >= 35 => full-time (first non-missing December
                              # job line's TJB1-6_JOBHRS1, coalesced in 01 -- primary-job hours,
                              # not hours summed across concurrent jobs)
## Civilian-labor-force scope (project decision 2026-07-16). When TRUE, the pipeline drops the ENTIRE
## SIPP household wherever any active-duty military member appears (see drop_military_households() in
## helpers.R; applied in 01/08/11). SIPP only captures household-resident military anyway; dropping whole
## households avoids splitting any tax unit. Set FALSE to restore the all-worker (military-in-government)
## frame. The exclusion's effect on sample size/composition is written to output/tables/
## military_exclusion_impact.csv for the draft disclosure footnote.
EXCLUDE_MILITARY <- TRUE

## --- RMESR: "employed in the reference month" -------------------------------
# 1..5 = had a job/business during the month; 6/7/8 = no job all month; -9 = NA.
RMESR_EMPLOYED  <- 1:5

## --- Worker class (EJB1_CLWRK) ----------------------------------------------
# 1 Federal, 2 Active-duty military, 3 State, 4 Local, 5 Private for-profit,
# 6 Private not-for-profit, 7 Self-emp incorporated, 8 Self-emp unincorporated.
# Civilian-labor-force scope (2026-07-16): active-duty military (2) are EXCLUDED at the household
# level (see EXCLUDE_MILITARY), so government = federal-civilian + state + local only.
CLWRK_PRIVATE   <- c(5L, 6L)
CLWRK_GOVERNMENT<- c(1L, 3L, 4L)       # federal-civilian, state, local (active-duty military excluded)
CLWRK_SELFEMP   <- c(7L, 8L)
CLWRK_MILITARY  <- 2L                  # active-duty military; excluded from the civilian labor force

## --- Yes/No + missing sentinels ---------------------------------------------
## Provenance (corrected 2026-09-08): -9 is the dictionary's categorical NA. The dollar
## sentinels below follow the SIPP CSV-format NA conventions; the .dta pipeline receives Stata
## missing as native NA (verified: none of these values occurs anywhere in the pu2025 extract),
## so na_sentinel() cleaning is belt-and-suspenders for a future CSV-format drop-in, not a
## load-bearing step for the current .dta path.
NA_CAT          <- -9L                 # categorical NA
NA_TPTOTINC     <- -10000000999        # TPTOTINC / TFTOTINC NA sentinel (CSV-format convention)
NA_TPEARN       <- -100000998          # TPEARN NA sentinel (TPEARN may be legitimately negative)
NA_DOLLAR_SMALL <- -999                # generic small-dollar / pointer NA sentinel

## --- Education grouping (EEDUC 31..46) --------------------------------------
# <HS: 31-38; HS grad/GED: 39; Some college/assoc: 40-42; Bachelor's+: 43-46.
EDUC_LT_HS      <- 31:38
EDUC_HS         <- 39L
EDUC_SOME_COLL  <- 40:42
EDUC_BA_PLUS    <- 43:46

## --- Race/ethnicity (ERACE 1-4, EORIGIN 1/2). Hispanic takes precedence. ----
# ERACE: 1 White alone, 2 Black alone, 3 Asian alone, 4 Residual.

## --- Age bands ---------------------------------------------------------------
AGE_BREAKS      <- c(18, 25, 35, 45, 55, 65)             # left-closed, right-open
AGE_LABELS      <- c("18-24", "25-34", "35-44", "45-54", "55-64")

## --- Saver's Match statutory thresholds (SECURE 2.0 sec 103 / IRC 6433) ------
# 50% match on up to $2,000 of contributions (max $1,000); linear phaseout between
# the lower (full-match ceiling) and upper (complete phaseout) AGI thresholds.
# NOTE: the match applies to tax years after 2026; applying it to a 2024-reference
# SIPP frame is inherently a projection/counterfactual, and SIPP income is an AGI
# PROXY. The thresholds below are applied UNINDEXED, and EFSTATUS is the PRIOR tax
# year with no offset -- 03_savers_match.R implements neither adjustment; both are
# disclosed as caveats there and in the draft (correction, 2026-08-27).
SM_MATCH_RATE   <- 0.50
SM_CONTRIB_CAP  <- 2000
SM_MAX_MATCH    <- 1000
SM_THRESHOLDS   <- list(          # by filing group: c(lower, upper) AGI
  single_mfs = c(20500, 35500),   # Single / Married-filing-separately
  mfj        = c(41000, 71000),   # Married filing jointly
  hoh        = c(30750, 53250)    # Head of household
)
# EFSTATUS: 1 Single, 2 MFJ, 3 MFS, 4 HoH.

## --- Raw source + reusable extract (01 builds the extract FROM the raw .dta) -
# The pipeline starts from the raw Census SIPP microdata so a fresh SIPP download
# is drop-in. 01_build_dataset.R reads RAW_DTA_FILE, selects EXTRACT_VARS, and
# writes EXTRACT_PARQUET for fast reuse; it never starts from a pre-built parquet.
RAW_DTA_FILE    <- "pu2025.dta"              # under data/raw/ (gitignored; download from Census SIPP)
EXTRACT_PARQUET <- "pu2025_extract.parquet"  # under data/processed/ (built by 01 from the raw .dta)
# Replicate-weight file + its December extract (built by 07 from the raw rw .dta; read by 07 and 12).
RW_DTA_FILE     <- "rw2025.dta"              # under data/raw/ (gitignored; unzip rw2025.dta.zip)
RW_EXTRACT_PARQUET <- "rw2025_extract.parquet"  # under data/processed/ (built by 07)
# TRUE => (re)build the extract from the raw .dta (default; correct for a fresh download).
# FALSE => reuse the cached EXTRACT_PARQUET for fast iteration once it exists.
REBUILD_EXTRACT <- TRUE

## --- Publication source lines (single source; derived from the vintage above) ------
# Every figure/table script pulls its Source: line from here so a vintage bump updates them all.
# The December frame POOLS four SIPP panels (no SPANEL/SWAVE filter anywhere in the pipeline; the
# 2025 panel supplies ~19.5% of the weight), so the citation names the RELEASE and the pooled panel
# range, not a single panel/wave (decision of 2026-08-27). 01 asserts the frame's actual
# panel set equals SIPP_PANELS_POOLED, so a vintage bump that changes the pool fails loud here.
SIPP_PANELS_POOLED <- 2022:2025
SIPP_PANEL_LABEL   <- paste0(min(SIPP_PANELS_POOLED), "–", max(SIPP_PANELS_POOLED))
SIPP_CITATION   <- paste0("U.S. Census Bureau, Survey of Income and Program Participation, ",
                          SIPP_PANEL, " release (pooled ", SIPP_PANEL_LABEL, " panels), ",
                          month.name[REF_MONTH], " ", REF_YEAR, " reference month")
SRC_SIPP        <- paste0(SIPP_CITATION, "; EIG analysis.")
SRC_TE          <- paste0("SIPP ", SIPP_PANEL, " release (pooled ", SIPP_PANEL_LABEL,
                          " panels), reference year ", REF_YEAR, "; EIG calculations.")

# Variables pulled from the raw pu2025.dta into the reusable extract (all verified present in the
# 5,203-variable file). Includes TST_INTV (state) so geography is available for a future extension
# without another raw rebuild. Change this list to widen the extract.
# EXTRACT_VARS is defined in the GENERATED sibling file extract_vars.R (188 vars; see its header).
# It was produced by the completeness pass over the full 5,203-variable SIPP file and re-confirmed
# present in pu2025.dta via the Census 2024->2025 crosswalk (0 variables changed). Regenerate that
# file (not this block) to widen/narrow the extract.
## Resolve the repo root even when sourced from a non-root working directory (e.g. testthat):
## caller-provided path_project first, then here::here() (git-root detection), then getwd().
.ev_root <- if (exists("path_project")) path_project else
            if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(.ev_root, "code", "_shared", "extract_vars.R"))

## =============================================================================
## Retirement tax-expenditure replication (08_tax_units / 09_tax_engine /
## 10_tax_expenditure): CBO-style present-value method.
## =============================================================================

## --- Tax-computation engine (PLUGGABLE) --------------------------------------
## The tax-computation stage is engine-agnostic: both engines read tax_units.parquet and write a
## schema-identical taxsim_results.parquet, so downstream 10-14 never change. The dispatcher is
## 09_tax_engine.R, called by run_all.R.
##   "policyengine" -> 09_policyengine_te.py (PolicyEngine-US; covers the reference year directly)
## The engine sets TAX_YEAR and the year-specific statutory constants below, so it runs correct law.
## Published estimates use the PolicyEngine-US engine, pinned to the version above. EIG also runs an
## NBER TAXSIM-35 cross-check internally (tax-year frontier 2023); that engine is not part of this
## replication package. Cross-engine agreement, measured 2026-08-27 on TY2023 dual-run fixtures:
## aggregate arc TE gap 2.34% (employee leg 2.40%, employer 2.26%; per-unit correlation 0.999).
## (10%/12% tolerances). PolicyEngine simulates true TY2024 (REF_YEAR) tax law directly.
## Only the PolicyEngine engine ships in this replication package; SIPP_TAX_ENGINE exists so the
## value is explicit and asserted, not so it can be switched.
TAX_ENGINE           <- Sys.getenv("SIPP_TAX_ENGINE", "policyengine")   # "policyengine" (only)
stopifnot(TAX_ENGINE %in% c("policyengine"))
POLICYENGINE_VERSION <- "1.772.0"    # PINNED for reproducibility (the PolicyEngine analog of "TAXSIM v35").
## Python interpreter for the PolicyEngine engine: set SIPP_PYTHON_BIN to the absolute path of an
## interpreter that has policyengine-us installed; otherwise the first python3/python on PATH is used.
## 09_tax_engine.R invokes this path via system2 (NOT reticulate) and fails loud at engine launch if
## the interpreter does not exist, rather than silently misfiring. On Windows a bare python/python3 on
## PATH can be a non-functional Microsoft Store stub, so set SIPP_PYTHON_BIN explicitly there.
.py_candidates <- c(Sys.getenv("SIPP_PYTHON_BIN"),
                    Sys.which("python3"), Sys.which("python"))
.py_candidates <- .py_candidates[nzchar(.py_candidates)]
PYTHON_BIN <- if (length(.py_candidates)) .py_candidates[[1]] else ""

## --- Tax-law simulation year + year-specific statutory constants (TE pipeline) -----------------
## PROVISIONAL CAVEAT (taxsim engine only): the NBER TAXSIM-35 engine bundled with usincometaxes accepts
## tax years only through 2023, so under TAX_ENGINE="taxsim" the RY2024 income is simulated under TY2023
## tax law (a ~one-year lag, flagged provisional in 12 and the draft appendix). TAX_ENGINE="policyengine"
## removes the cap and simulates true TY2024 (REF_YEAR) law -- the constants below switch accordingly.
## Contribution DOLLARS and the NIPA calibration in 12 are always the REF_YEAR (2024) values.
if (identical(TAX_ENGINE, "policyengine")) {
  TAX_YEAR          <- REF_YEAR   # 2024: PolicyEngine covers the reference year directly (no lag)
  OASDI_WAGE_CAP    <- 168600     # TY2024 Social Security taxable maximum (SSA)
  DEP_FILING_THRESH <- 14600      # TY2024 dependent filing threshold = single standard deduction
  QREL_INCOME_LIMIT <- 5050       # TY2024 qualifying-relative gross income limit
}
## For reference, the corresponding TY2023 values (used by EIG's internal TAXSIM cross-check, which
## is not part of this package) are: OASDI cap 160200, dependent filing threshold 13850,
## qualifying-relative limit 4700.
FICA_OASDI_RATE     <- 0.124     # combined employer+employee OASDI (statutory; unchanged)
FICA_HI_RATE        <- 0.029     # combined employer+employee Medicare HI (statutory; unchanged)
FICA_EE_ER_GROSSUP  <- 1.0765    # incidence gross-up: employer contribs escape the 7.65% employer share
CHILD_DEP_AGE_MAX   <- 18L       # qualifying child: under 19 ...
STUDENT_DEP_AGE_MAX <- 23L       # ... or under 24 if a full-time student

## --- CBO present-value parameters (CBO 57413/57585 appendix, printed pp. 31-32) ------
PV_RETURN           <- 0.035     # nominal return = discount rate (CBO base case)
PV_RETURN_SENS      <- 0.060     # sensitivity (CBO reports ~ +38% income-tax TE)
PV_RET_AGE          <- 65L       # contributions accumulate to age 65
PV_END_AGE          <- 85L       # withdrawn in equal installments to age 85
PV_MIN_WITHDRAW_YRS <- 1L        # workers already >= PV_RET_AGE: treat as withdrawing over remaining years (floor 1)

## --- Measurement assumptions (each disclosed in the limitations block) -------
## SIPP earnings are GROSS of employee elective deferrals - documented in the 2025 SIPP Users'
## Guide (data/raw/2025_SIPP_Users_Guide.pdf; content unchanged from the 2024 edition verified
## 2026-07-12): (1) earnings are collected as pay rates -
## "hourly wage, annual salary, or gross annual amount" (Glossary, "Wages and Salaries",
## sec. 4.2); (2) the TPEARN/TPEARN_ALT recode is defined as "the sum of GROSS earnings,
## wages, and salary" (income-recode component table, ch. 5); (3) the imputation chapter
## refers to the earnings item as "the monthly gross pay item". SIPP never asks about
## pre-tax payroll deductions, so deferrals are not netted out. Addresses
## (methodology-report 2026-07-12); the 09 arc-MTR baseline algebra stands.
SIPP_EARN_GROSS_OF_DEFERRALS <- TRUE

## --- TAXSIM state input: FIPS (TST_INTV) -> two-letter abbreviation ----------
FIPS_TO_STATE <- c(
  `1`="AL",`2`="AK",`4`="AZ",`5`="AR",`6`="CA",`8`="CO",`9`="CT",`10`="DE",`11`="DC",
  `12`="FL",`13`="GA",`15`="HI",`16`="ID",`17`="IL",`18`="IN",`19`="IA",`20`="KS",
  `21`="KY",`22`="LA",`23`="ME",`24`="MD",`25`="MA",`26`="MI",`27`="MN",`28`="MS",
  `29`="MO",`30`="MT",`31`="NE",`32`="NV",`33`="NH",`34`="NJ",`35`="NM",`36`="NY",
  `37`="NC",`38`="ND",`39`="OH",`40`="OK",`41`="OR",`42`="PA",`44`="RI",`45`="SC",
  `46`="SD",`47`="TN",`48`="TX",`49`="UT",`50`="VT",`51`="VA",`53`="WA",`54`="WV",
  `55`="WI",`56`="WY"
)

## --- NA sentinels for the new income/withdrawal amounts ----------------------
## SIPP dollar amounts are >= 0 by the dictionary coding; any negative value is a
## sentinel except where the dictionary allows losses. Per the 2024 dictionary,
## TJB(n)_MSUM has domain $0-$9,999,999 (job-level wage earnings, NOT profits), so
## its negatives ARE sentinels; self-employment losses live in TPEARN. (
## the earlier version of this list wrongly included TJB*_MSUM.) TPEARN / TPTOTINC /
## TFTOTINC are cleaned with their exact documented sentinels in 08; the remaining
## members are cleaned allow-negative there.
AMT_ALLOW_NEGATIVE <- c("TPEARN", "TINC_RENT", "TINC_OTH", "TPPRPINC",
                        "THTOTINC", "TPTOTINC", "TFTOTINC")

## --- Universal-access counterfactual take-up (retained constants; the counterfactual
## --- stage is not part of this replication package) --------------------------
## MODELED / ex-ante assumptions (not measured). Auto-enrollment take-up ~85% from the 401(k) defaults
## literature (Madrian & Shea 2001; Choi, Laibson, Madrian & Metrick 2004) [user choice 2026-07-10].
## Voluntary (no auto-enrollment) take-up ~15%, anchored to the "active saver" share in Chetty et al.
## (2014) -- DANISH administrative data, transferred to the U.S. setting as an assumption --
## and to low observed voluntary IRA take-up. Levers act on the newly-covered (no qualifying account).
CF_AUTO_TAKEUP      <- 0.85
CF_VOLUNTARY_TAKEUP <- 0.15
