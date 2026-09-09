# code/01_build_dataset.R
# Build the analysis frame for the SIPP Fast Facts (access/participation/matching).
# Reads the existing pu2025 extract, restricts to the working population (18-64, employed,
# December reference month), derives pillar flags + worker class + demographics + income,
# writes data/processed/sipp_fastfacts.parquet, and prints a funnel + weighted distributions
# for review. Implements the 2026-07-10 spec. No Saver's Match eligibility here (see 03).

suppressWarnings(suppressMessages({
  library(haven); library(arrow); library(dplyr)
}))

## --- Resolve project root + load shared config ------------------------------
path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
source(file.path(path_project, "code", "_shared", "helpers.R"))
path_raw       <- file.path(path_project, "data", "raw")
path_processed <- file.path(path_project, "data", "processed")
path_tables    <- file.path(path_project, "output", "tables")
dir.create(path_processed, showWarnings = FALSE, recursive = TRUE)
dir.create(path_tables,    showWarnings = FALSE, recursive = TRUE)

## ===========================================================================
## STAGE 1 - Build the reusable extract FROM THE RAW SIPP microdata (pu2025.dta)
## The pipeline starts from raw so a fresh Census SIPP download is drop-in. The
## extract parquet is a cache for fast reuse, not the starting point.
## ===========================================================================
raw_dta    <- file.path(path_raw, RAW_DTA_FILE)
extract_pq <- file.path(path_processed, EXTRACT_PARQUET)
need_build <- isTRUE(REBUILD_EXTRACT) || !file.exists(extract_pq)

if (need_build) {
  if (!file.exists(raw_dta)) {
    stop("Raw SIPP microdata not found: ", raw_dta,
         "\n  Download ", RAW_DTA_FILE, " from https://www.census.gov/programs-surveys/sipp/data/datasets.html",
         " and place it at data/raw/", RAW_DTA_FILE, " (see data/raw/README.md).")
  }
  t0 <- Sys.time()
  hdr  <- read_dta(raw_dta, n_max = 0)                 # header only, to validate names
  vars <- EXTRACT_VARS[EXTRACT_VARS %in% names(hdr)]
  miss <- setdiff(EXTRACT_VARS, names(hdr))
  if (length(miss)) warning("EXTRACT_VARS not found in ", RAW_DTA_FILE, ": ", paste(miss, collapse = ", "))
  message("STAGE 1: reading ", length(vars), " of ", ncol(hdr), " variables from raw ", RAW_DTA_FILE, " ...")
  ext <- read_dta(raw_dta, col_select = all_of(vars))
  ext <- as.data.frame(zap_labels(ext))                # strip Stata value labels -> plain numeric/char codes
  write_parquet(ext, extract_pq)
  message(sprintf("STAGE 1: wrote %s (%d person-months x %d vars) from raw in %.1f min",
                  EXTRACT_PARQUET, nrow(ext), ncol(ext),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  rm(ext); gc()
} else {
  message("STAGE 1: reusing cached extract (REBUILD_EXTRACT = FALSE): ", EXTRACT_PARQUET)
}

## ===========================================================================
## STAGE 2 - Derive the analysis frame from the extract
## ===========================================================================
raw <- as.data.frame(read_parquet(extract_pq))
funnel <- funnel_row(paste0("00 raw extract from ", RAW_DTA_FILE, " (all person-months)"), raw)

## Households to exclude for the civilian-labor-force frame: any household with an active-duty
## military member (helpers.R). Computed from the full extract so it also catches military members
## outside the 18-64 worker frame; applied to the worker frame after the December-job filter below.
mil_hh <- if (isTRUE(EXCLUDE_MILITARY)) military_household_ids(raw) else character(0)

## --- Restrict to December reference month, ages 18-64, employed --------------
df <- raw[!is.na(raw$MONTHCODE) & raw$MONTHCODE == REF_MONTH, ]
funnel <- rbind(funnel, funnel_row("01 December reference month", df))
df <- df[!is.na(df$TAGE) & df$TAGE >= AGE_MIN & df$TAGE <= AGE_MAX, ]
funnel <- rbind(funnel, funnel_row("02 ages 18-64", df))
df <- df[!is.na(df$RMESR) & df$RMESR %in% RMESR_EMPLOYED, ]
funnel <- rbind(funnel, funnel_row("03 employed (RMESR 1-5)", df))

## --- Worker class, FT/PT from ANY December job line (I3, 2026-07-10) ---------
## Coalesce class / work-arrangement / hours across job lines 1-6 (job line 7 is suppressed in the
## PUF) so workers whose December job is not job 1 are classified rather than dropped. First
## non-missing across lines; documented -9/-999 sentinels -> NA before coalescing.
coalesce_lines <- function(vars, na_val) {
  cols <- lapply(vars, function(v) { x <- df[[v]]; x[x == na_val] <- NA; x })
  Reduce(dplyr::coalesce, cols)
}
clw    <- coalesce_lines(paste0("EJB", 1:6, "_CLWRK"),   NA_CAT)           # -9  -> NA
jborse <- coalesce_lines(paste0("EJB", 1:6, "_JBORSE"),  NA_CAT)           # -9  -> NA
hrs    <- coalesce_lines(paste0("TJB", 1:6, "_JOBHRS1"), NA_DOLLAR_SMALL)  # -999 -> NA
## strict dictionary guard: documented code domains (2025 SIPP Data Dictionary)
stopifnot(all(clw %in% c(1:8, NA)), all(jborse %in% c(1:3, NA)))
## keep RMESR-employed workers who held a December job on ANY line 1-6
keep <- !is.na(jborse)
df <- df[keep, ]; clw <- clw[keep]; hrs <- hrs[keep]
funnel <- rbind(funnel, funnel_row("04 with a December job on any line (EJB1-6 JBORSE 1-3)", df))

## --- Civilian labor force: drop households with any active-duty military member ----------------
## Whole households leave together (project decision 2026-07-16) so no tax unit is ever split; this
## necessarily removes the civilian members of those households too, disclosed via the impact CSV.
if (isTRUE(EXCLUDE_MILITARY) && length(mil_hh) > 0) {
  in_mil  <- hh_key(df) %in% mil_hh
  ## any-job-line test (helpers.R), NOT the coalesced primary-job `clw`: a member whose military
  ## job sits on line 2+ would otherwise be disclosed as a civilian household member.
  is_mil  <- is_military_person(df)
  dec_all <- raw[!is.na(raw$MONTHCODE) & raw$MONTHCODE == REF_MONTH, ]
  mil_impact <- data.frame(
    households_dropped          = length(mil_hh),
    dec_persons_in_mil_hh       = sum(hh_key(dec_all) %in% mil_hh),
    workers_dropped             = sum(in_mil),
    military_members_dropped    = sum(in_mil & is_mil),
    civilian_members_dropped    = sum(in_mil & !is_mil),
    wt_workers_before           = sum(df$WPFINWGT, na.rm = TRUE),
    wt_workers_dropped          = sum(df$WPFINWGT[in_mil], na.rm = TRUE),
    wt_military_members_dropped = sum(df$WPFINWGT[in_mil & is_mil], na.rm = TRUE),
    wt_civilian_members_dropped = sum(df$WPFINWGT[in_mil & !is_mil], na.rm = TRUE),
    stringsAsFactors = FALSE)
  df <- df[!in_mil, ]; clw <- clw[!in_mil]; hrs <- hrs[!in_mil]
  mil_impact$wt_workers_after <- sum(df$WPFINWGT, na.rm = TRUE)
  write.csv(mil_impact, file.path(path_tables, "military_exclusion_impact.csv"), row.names = FALSE)
  funnel <- rbind(funnel, funnel_row("04b civilian labor force (drop households w/ active-duty military)", df))
  rm(dec_all)
}

df$worker_class <- dplyr::case_when(
  clw %in% CLWRK_PRIVATE    ~ "Private",
  clw %in% CLWRK_GOVERNMENT ~ "Government",
  clw %in% CLWRK_SELFEMP    ~ "Self-employed",
  TRUE                      ~ "Unknown"     # residual: class missing on all held lines
)
df$is_self_employed <- df$worker_class == "Self-employed"
## Government sub-class, persisted for the within-government access-gap detail in
## 16_access_gap_table.R (draft footnote 3). NA for non-government workers. Codes per params.R:
## 1 Federal (civilian), 3 State, 4 Local. Active-duty military (code 2) is excluded upstream at the
## household level (EXCLUDE_MILITARY, 2026-07-16), so Federal here is federal-civilian only.
df$gov_subclass <- dplyr::case_when(
  clw == 1L ~ "Federal",
  clw == 3L ~ "State",
  clw == 4L ~ "Local",
  TRUE      ~ NA_character_
)
df$ft_pt <- dplyr::if_else(is.na(hrs), NA_character_,
                           dplyr::if_else(hrs >= FT_HOURS_MIN, "Full-time", "Part-time"))

## --- ACCESS flags (pillar 1) ------------------------------------------------
emp_acct  <- is_yes(df$EMJOB_401) | is_yes(df$EMJOB_IRA) | is_yes(df$EMJOB_PEN)
incl      <- is_yes(df$EINCPENS)   # offered AND included (universe: EPENSNYN==1)
offered   <- is_yes(df$EPENSNYN)   # employer/business offers a plan (backstop universe)
own_dcira <- is_yes(df$EOWN_THR401) | is_yes(df$EOWN_IRAKEO)
own_db    <- is_yes(df$EOWN_PENSION)

df$access_emp_raw   <- emp_acct | incl                 # SIPP-measured employer/business access
df$access_offer_raw <- emp_acct | offered              # offer-only companion
# Headline decision: self-employed count as lacking EMPLOYER-provided access by construction.
df$access_emp   <- ifelse(df$is_self_employed, FALSE, df$access_emp_raw)
df$access_offer <- ifelse(df$is_self_employed, FALSE, df$access_offer_raw)
df$lacks_access <- !df$access_emp                      # HEADLINE (H1)
df$access_ownership <- own_dcira                        # H4: any DC/IRA account (SE not forced)
df$own_db <- own_db
df$se_business_plan <- df$is_self_employed & df$access_emp_raw   # FYI: SE who report a business plan
## Saver's Match qualifying account (author decision 2026-09-08): under IRC sec 6433 the
## match is deposited into a defined-contribution plan or IRA -- a defined-benefit/cash-balance
## pension (EMJOB_PEN) cannot receive it, so DB coverage does NOT count as a claimable vehicle.
## EMJOB_401/EMJOB_IRA are asked only of the corresponding EOWN_* owners, so the DC/IRA-only test
## reduces exactly to own_dcira (401(k)-type or IRA/Keogh ownership from any source).
df$has_qual_acct <- own_dcira                                    # DC/IRA only (Saver's Match wedge; feeds 03/04/05)
## Participation-question universe: ESCNTYN_*/EECNTYN_* are asked only where the matching
## EMJOB_* = 1, so participation is OBSERVED only for plan-type-observed access. Workers whose
## access comes solely through the offer-and-inclusion branch (EINCPENS) were never asked and score
## FALSE by construction. Published P1 uses the observed-universe denominator (decision of
## 2026-08-27); the all-access denominator is retained as a disclosed conservative floor.
df$access_emp_obs <- df$access_emp & emp_acct

## --- PARTICIPATION (pillar 2) & MATCHING (pillar 3) -------------------------
df$participates  <- is_yes(df$ESCNTYN_401) | is_yes(df$ESCNTYN_IRA) | is_yes(df$ESCNTYN_PEN)
df$match_receipt <- is_yes(df$EECNTYN_401) | is_yes(df$EECNTYN_IRA)
## Matching-question universe: EECNTYN_* exists only for the DC/IRA plan types (EMJOB_401/EMJOB_IRA);
## pension-only participants sit outside it. Companion denominator for M1-given-participation.
df$participates_dcira <- df$participates & (is_yes(df$EMJOB_401) | is_yes(df$EMJOB_IRA))
## Employer contribution DOLLARS (I1): TECNTAMT, negatives/sentinels -> NA (amounts are >= 0)
ec <- df$TECNTAMT; ec[is.na(ec) | ec < 0] <- NA
df$emp_contrib_amt <- ec
## Dependent flag (I2) strict dictionary guard: EDEPCLM documented codes are 1/2/-9
stopifnot(all(df$EDEPCLM %in% c(1L, 2L, NA_CAT) | is.na(df$EDEPCLM)))

## --- Income: TRUE calendar-year 2024 from the SIPP panel (full-year; = REF_YEAR) -------------
## SIPP income is monthly; annual = SUM across MONTHCODE 1-12 per person, scaled to 12 months for
## partial-year respondents (Option B). Built from the FULL extract (all months), then joined to the
## December analysis frame. TPEARN may be legitimately negative (business losses); only the exact
## NA-Missing sentinels are set to NA before summing.
ann <- raw |>
  dplyr::transmute(SSUID, PNUM,
                   e = na_sentinel(TPEARN,   NA_TPEARN),
                   p = na_sentinel(TPTOTINC, NA_TPTOTINC),
                   f = na_sentinel(TFTOTINC, NA_TPTOTINC)) |>
  dplyr::group_by(SSUID, PNUM) |>
  dplyr::summarise(n_months = dplyr::n(),
                   earn_obs_months = sum(!is.na(e)),   # months with OBSERVED earnings
                   earn_sum = sum(e, na.rm = TRUE),
                   pinc_sum = sum(p, na.rm = TRUE),
                   finc_sum = sum(f, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(earn_annual = earn_sum * 12 / n_months,      # scale observed months to a 12-month year
                pinc_annual = pinc_sum * 12 / n_months,
                finc_annual = finc_sum * 12 / n_months) |>
  dplyr::select(SSUID, PNUM, n_months, earn_obs_months, earn_annual, pinc_annual, finc_annual)
df <- dplyr::left_join(df, ann, by = c("SSUID", "PNUM"))
df$earn_month <- na_sentinel(df$TPEARN, NA_TPEARN)           # December monthly earnings (reference only)
df$earn_decile <- wtd_decile(df$earn_annual, df$WPFINWGT)
## MFJ Saver's Match base: the spouse pair's combined annual personal income, matched within the
## household via the spouse pointer EPNSPOUSE (-999 -> NA when no spouse is present).
df$sp_pnum <- ifelse(df$EPNSPOUSE == NA_DOLLAR_SMALL, NA, df$EPNSPOUSE)
df <- dplyr::left_join(df, dplyr::transmute(ann, SSUID, sp_pnum = PNUM, spouse_pinc_annual = pinc_annual),
                       by = c("SSUID", "sp_pnum"))
df$sp_pnum <- NULL

## --- Demographics -----------------------------------------------------------
df$sex <- dplyr::case_when(df$ESEX == 1 ~ "Male", df$ESEX == 2 ~ "Female", TRUE ~ NA_character_)
df$race_eth <- dplyr::case_when(
  df$EORIGIN == 1        ~ "Hispanic",
  df$ERACE   == 1        ~ "White (NH)",
  df$ERACE   == 2        ~ "Black (NH)",
  df$ERACE   == 3        ~ "Asian (NH)",
  df$ERACE   == 4        ~ "Other (NH)",
  TRUE                   ~ NA_character_
)
df$educ_grp <- dplyr::case_when(
  df$EEDUC %in% EDUC_LT_HS     ~ "Less than HS",
  df$EEDUC %in% EDUC_HS        ~ "HS grad",
  df$EEDUC %in% EDUC_SOME_COLL ~ "Some college/assoc",
  df$EEDUC %in% EDUC_BA_PLUS   ~ "Bachelor's+",
  TRUE                          ~ NA_character_
)
df$age_band <- as.character(cut(df$TAGE, breaks = AGE_BREAKS, labels = AGE_LABELS, right = FALSE))
df$disability <- dplyr::case_when(df$RDIS == 1 ~ "Disability", df$RDIS == 2 ~ "No disability", TRUE ~ NA_character_)

funnel <- rbind(funnel, funnel_row("05 analysis frame (derived)", df))

## --- Pillar-item missingness audit --------------------------------------------------------------
## Every pillar flag routes item nonresponse (-9/NA) into "No" via is_yes(). This tabulates the
## weighted missing share of each retirement-module input WITHIN its documented question universe,
## so the draft can state (and bound, if needed) the missing-as-No convention. Universes follow the
## 2025 SIPP Data Dictionary skip logic as implemented above: EMJOB_* is asked of the corresponding
## EOWN_* owners; EPENSNYN of workers with no EMJOB_* plan; EINCPENS of EPENSNYN==1; ESCNTYN_*/
## EECNTYN_* of the corresponding EMJOB_*==1 holders. EOWN_* is asked of all frame workers.
miss_item <- function(x, universe, label) {
  u <- universe & !is.na(df$WPFINWGT)
  m <- u & (is.na(x) | x == NA_CAT)
  data.frame(item = label,
             n_universe = sum(u),
             wt_universe = sum(df$WPFINWGT[u]),
             n_missing = sum(m),
             missing_share_wt = ifelse(sum(df$WPFINWGT[u]) > 0,
                                       sum(df$WPFINWGT[m]) / sum(df$WPFINWGT[u]), NA_real_),
             stringsAsFactors = FALSE)
}
all_u <- rep(TRUE, nrow(df))
item_miss <- rbind(
  miss_item(df$EOWN_THR401,  all_u,                      "EOWN_THR401 (owns 401k-type, any source)"),
  miss_item(df$EOWN_IRAKEO,  all_u,                      "EOWN_IRAKEO (owns IRA/Keogh, any source)"),
  miss_item(df$EOWN_PENSION, all_u,                      "EOWN_PENSION (owns DB/cash-balance, any source)"),
  miss_item(df$EMJOB_401,    is_yes(df$EOWN_THR401),     "EMJOB_401 | owns 401k-type"),
  miss_item(df$EMJOB_IRA,    is_yes(df$EOWN_IRAKEO),     "EMJOB_IRA | owns IRA/Keogh"),
  miss_item(df$EMJOB_PEN,    is_yes(df$EOWN_PENSION),    "EMJOB_PEN | owns DB/cash-balance"),
  miss_item(df$EPENSNYN,     !emp_acct,                  "EPENSNYN | no plan through main employer"),
  miss_item(df$EINCPENS,     is_yes(df$EPENSNYN),        "EINCPENS | employer offers a plan"),
  miss_item(df$ESCNTYN_401,  is_yes(df$EMJOB_401),       "ESCNTYN_401 | holds 401k-type via employer"),
  miss_item(df$ESCNTYN_IRA,  is_yes(df$EMJOB_IRA),       "ESCNTYN_IRA | holds IRA/Keogh via employer"),
  miss_item(df$ESCNTYN_PEN,  is_yes(df$EMJOB_PEN),       "ESCNTYN_PEN | holds DB via employer"),
  miss_item(df$EECNTYN_401,  is_yes(df$EMJOB_401),       "EECNTYN_401 | holds 401k-type via employer"),
  miss_item(df$EECNTYN_IRA,  is_yes(df$EMJOB_IRA),       "EECNTYN_IRA | holds IRA/Keogh via employer")
)
write.csv(item_miss, file.path(path_tables, "pillar_item_missingness.csv"), row.names = FALSE)

## --- Persist ----------------------------------------------------------------
out_cols <- c("SSUID","PNUM","MONTHCODE","WPFINWGT","TAGE","worker_class","gov_subclass","is_self_employed",
              "ft_pt","access_emp","access_offer","lacks_access","access_ownership","own_db",
              "access_emp_raw","se_business_plan","participates","match_receipt",
              "access_emp_obs","participates_dcira",
              "earn_month","earn_annual","pinc_annual","finc_annual","spouse_pinc_annual","earn_decile",
              "sex","race_eth","educ_grp","age_band","disability","EFSTATUS","EEDFTPT","has_qual_acct",
              "SPANEL","SWAVE","EDEPCLM","emp_contrib_amt","TECNTAMT_401","TECNTAMT_IRA")
## Source-line integrity: the pooled-panel label printed on every figure must match the
## panels actually present in the frame; a vintage bump updates SIPP_PANELS_POOLED in params.R.
stopifnot(identical(sort(unique(as.integer(df$SPANEL))), as.integer(SIPP_PANELS_POOLED)))
write_parquet(df[, out_cols], file.path(path_processed, "sipp_fastfacts.parquet"))
write.csv(funnel, file.path(path_tables, "sample_funnel.csv"), row.names = FALSE)

## --- Review printout --------------------------------------------------------
pct <- function(x) sprintf("%.1f%%", 100 * x)
cat("\n================ SAMPLE FUNNEL ================\n")
print(funnel, row.names = FALSE)
if (isTRUE(EXCLUDE_MILITARY) && exists("mil_impact")) {
  cat("\n---- Civilian-labor-force exclusion (households with active-duty military) ----\n")
  cat(sprintf("Dropped %d households; %d workers removed (%d military members + %d civilian members); weighted workers %.2fM -> %.2fM.\n",
              mil_impact$households_dropped, mil_impact$workers_dropped,
              mil_impact$military_members_dropped, mil_impact$civilian_members_dropped,
              mil_impact$wt_workers_before / 1e6, mil_impact$wt_workers_after / 1e6))
}

cat("\n================ WORKER CLASS (weighted share of workers) ================\n")
cls <- tapply(df$WPFINWGT, df$worker_class, sum); cls <- cls / sum(cls)
print(round(cls, 3))

cat("\n================ HEADLINE H1: lacks employer-provided access ================\n")
df$.univ <- TRUE
h1 <- breakout(df, "lacks_access", ".univ", by_vars = c("worker_class","ft_pt","sex","race_eth","educ_grp","age_band","disability","earn_decile"))
h1$share_fmt <- pct(h1$share)
print(h1[, c("dimension","group","share_fmt","n_den","wt_den")], row.names = FALSE)

cat("\n================ COMPANION ACCESS DEFINITIONS (overall) ================\n")
cat("H1 employer-provided (offered & eligible) lacks-access:", pct(wtd_share(df$lacks_access, df$.univ, df$WPFINWGT)$share), "\n")
cat("H3 offer-only lacks-access:", pct(wtd_share(!df$access_offer, df$.univ, df$WPFINWGT)$share), "\n")
cat("H4 ownership-based (has DC/IRA) COVERAGE:", pct(wtd_share(df$access_ownership, df$.univ, df$WPFINWGT)$share),
    " => lacks:", pct(wtd_share(!df$access_ownership, df$.univ, df$WPFINWGT)$share), "\n")
cat("FYI self-employed who report a business plan (n):", sum(df$se_business_plan), "\n")

cat("\n================ PILLARS 2-3 ================\n")
p1 <- wtd_share(df$participates, df$access_emp_obs, df$WPFINWGT)
cat("P1 participation | employer access, plan type observed:", pct(p1$share),
    " (denom n=", p1$n_den, ")\n", sep = "")
p1f <- wtd_share(df$participates, df$access_emp, df$WPFINWGT)
cat("P1 conservative floor | all employer access (EINCPENS-only access scored FALSE):",
    pct(p1f$share), " (denom n=", p1f$n_den, ")\n", sep = "")
p2 <- wtd_share(df$participates, df$.univ, df$WPFINWGT)
cat("P2 participation | all workers:", pct(p2$share), "\n")
m1a <- wtd_share(df$match_receipt, df$.univ, df$WPFINWGT)
m1b <- wtd_share(df$match_receipt, df$participates, df$WPFINWGT)
cat("M1 match receipt | all workers:", pct(m1a$share), "  | participants:", pct(m1b$share), "\n")

cat("\n================ DATA-QUALITY FLAGS ================\n")
## Note: earn_annual is a na.rm sum and is never NA by construction; the meaningful check is
## workers with ZERO observed earnings months (all 12 TPEARN values missing), who enter as $0 earners.
cat("Unknown worker class (n):", sum(df$worker_class == "Unknown"),
    " | NA ft_pt:", sum(is.na(df$ft_pt)),
    " | NA race_eth:", sum(is.na(df$race_eth)),
    " | zero observed earnings months (enters as $0):", sum(df$earn_obs_months == 0), "\n")
cat("Pillar-item weighted missing shares (max):",
    sprintf("%.2f%%", 100 * max(item_miss$missing_share_wt, na.rm = TRUE)),
    "-> output/tables/pillar_item_missingness.csv\n")
cat("\nWrote:", file.path(path_processed, "sipp_fastfacts.parquet"), "(", nrow(df), "persons )\n")
