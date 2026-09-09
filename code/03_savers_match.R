# code/03_savers_match.R
# Saver's Match section. S1 = income-eligibility (current-law statutory thresholds);
# S2 = the eligibility x access WEDGE (eligible workers who lack a qualifying account to claim/benefit).
# v1 with documented proxies (see caveats). Implements the 2026-07-10 spec (Saver's Match section).

suppressWarnings(suppressMessages({ library(arrow); library(dplyr) }))
path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
source(file.path(path_project, "code", "_shared", "helpers.R"))
path_processed <- file.path(path_project, "data", "processed")
path_tables    <- file.path(path_project, "output", "tables")

df <- as.data.frame(read_parquet(file.path(path_processed, "sipp_fastfacts.parquet")))
pct <- function(x) sprintf("%.1f%%", 100 * x)

## Filing group from EFSTATUS (1 Single, 2 MFJ, 3 MFS, 4 HoH; -9 NA / non-filer)
df$filing_group <- dplyr::case_when(
  df$EFSTATUS %in% c(1, 3) ~ "single_mfs",
  df$EFSTATUS == 2         ~ "mfj",
  df$EFSTATUS == 4         ~ "hoh",
  TRUE                     ~ NA_character_
)
## Income base for the match: full-year PANEL-SUMMED personal income; for MFJ, the spouse PAIR's
## combined annual personal income (matched via EPNSPOUSE). SIPP income is an AGI PROXY (no
## above-the-line adjustments) and EFSTATUS is the PRIOR tax year -> counts remain approximations/bounds.
df$sm_income <- ifelse(df$filing_group == "mfj",
                       df$pinc_annual + dplyr::coalesce(df$spouse_pinc_annual, 0),
                       df$pinc_annual)
## named-vector lookup: NA filing_group indexes to NA automatically
sm_lower <- vapply(SM_THRESHOLDS, `[`, numeric(1), 1)
sm_upper <- vapply(SM_THRESHOLDS, `[`, numeric(1), 2)
lower <- unname(sm_lower[df$filing_group])
upper <- unname(sm_upper[df$filing_group])

## Statutory exclusions APPLIED to the headline: full-time post-secondary student (EEDFTPT==1) and,
## where OBSERVED, claimed-dependent status (EDEPCLM; the I2 refinement below). EDEPCLM's universe is
## narrow, so dependency is only partly observable and the headline remains an UPPER bound on that
## margin -- but the exclusion IS applied, not skipped (corrected after external RA review 2026-08-27).
ft_student <- is_yes(df$EEDFTPT)
## I2 (observed-only): exclude dependents where OBSERVED. EDEPCLM's universe is narrow (ages 15-25 who
## filed 'single'); outside it dependency is unobserved and NOT imputed (per user, 2026-07-10).
dep_obs <- is_yes(df$EDEPCLM)
## Universe edges (disclosed 2026-09-08). Two further restrictions are embedded in
## sm_in_universe and pushed to "not eligible":
##   (1) workers with NO prior-year filing status (EFSTATUS -9: non-filers + item nonresponse) cannot
##       be placed on a phase-out schedule. Claiming the match requires filing a return, so treating
##       non-filers as ineligible is behaviorally defensible -- but it mixes a statutory income test
##       with a filing-behavior test, and low-income workers are the likeliest non-filers.
##   (2) sm_income >= 0 excludes workers with negative annual income (self-employment losses), though
##       negative AGI would qualify under the statute. Kept for a conservative, well-defined universe.
## Also note EEDFTPT's dictionary universe is POST-SECONDARY enrollment (grades 13-22), so full-time
## K-12 enrollees aged 18 are not caught by the student exclusion. All three shares print below.
df$sm_in_universe    <- !is.na(df$filing_group) & !ft_student & !is.na(df$sm_income) & df$sm_income >= 0
df$sm_in_universe_v2 <- df$sm_in_universe & !dep_obs                 # dependent-refined
df$sm_elig_any_v1 <- df$sm_in_universe    & df$sm_income <  upper    # pre-dependent (comparison)
df$sm_elig_any    <- df$sm_in_universe_v2 & df$sm_income <  upper    # HEADLINE (dependent-refined)
df$sm_elig_full   <- df$sm_in_universe_v2 & df$sm_income <= lower
df$sm_wedge       <- df$sm_elig_any & !df$has_qual_acct              # eligible but NO qualifying account (S2)
df$.univ <- TRUE
df$.filers <- !is.na(df$filing_group)

## Tables
elig_tbl  <- breakout(df, "sm_elig_any", ".univ", c("worker_class","race_eth","earn_decile"))
wedge_tbl <- breakout(df, "sm_wedge", "sm_elig_any", c("worker_class","race_eth"))
write.csv(elig_tbl,  file.path(path_tables, "savers_match_eligibility.csv"), row.names = FALSE)
write.csv(wedge_tbl, file.path(path_tables, "savers_match_wedge.csv"), row.names = FALSE)

## Console summary
s_any  <- wtd_share(df$sm_elig_any,  df$.univ, df$WPFINWGT)
s_full <- wtd_share(df$sm_elig_full, df$.univ, df$WPFINWGT)
s_wedge_of_elig <- wtd_share(df$sm_wedge, df$sm_elig_any, df$WPFINWGT)
s_wedge_of_all  <- wtd_share(df$sm_wedge, df$.univ, df$WPFINWGT)
m <- function(x) sprintf("%.1fM", x / 1e6)
cat("\n=========== SAVER'S MATCH SECTION (weighted; v1, PROXY thresholds) ===========\n")
cat("Filers (has EFSTATUS): ", pct(weighted.mean(df$.filers, df$WPFINWGT)),
    " of workers (weighted)\n", sep = "")
## Universe-edge disclosure shares
neg_inc <- !is.na(df$sm_income) & df$sm_income < 0
cat("Universe edges -> counted NOT eligible: no filing status ",
    pct(weighted.mean(is.na(df$filing_group), df$WPFINWGT)),
    " of workers; negative annual income ", pct(weighted.mean(neg_inc, df$WPFINWGT)),
    " (n=", sum(neg_inc), "); full-time-student exclusion covers post-secondary enrollment only.\n", sep = "")
d_v1 <- wtd_share(df$sm_elig_any_v1, df$.univ, df$WPFINWGT)
cat("I2 dependent exclusion (observed-only via EDEPCLM): observed dependents n=", sum(dep_obs),
    " | any-match eligible v1(no dep)=", pct(d_v1$share), " (~", m(d_v1$wt_num),
    ") -> v2(dep-refined)=", pct(s_any$share), " (~", m(s_any$wt_num), ")\n", sep = "")
cat("S1 income-eligible for ANY match: ", pct(s_any$share), " of workers (~", m(s_any$wt_num), ")\n", sep = "")
cat("   income-eligible for FULL match: ", pct(s_full$share), " of workers (~", m(s_full$wt_num), ")\n", sep = "")
cat("S2 WEDGE (eligible but NO qualifying account):\n")
cat("   - as share of eligible: ", pct(s_wedge_of_elig$share), " (~", m(s_wedge_of_elig$wt_num), " workers)\n", sep = "")
cat("   - as share of all workers: ", pct(s_wedge_of_all$share), "\n", sep = "")
cat("\nCAVEATS: SIPP income is an AGI proxy; EFSTATUS is prior tax year; MFJ uses family-income proxy;\n",
    "dependent-status exclusion applied only where OBSERVED (narrow EDEPCLM universe); statutory\n",
    "thresholds unindexed; match is a post-2026 program applied to a 2024 frame (counterfactual);\n",
    "non-filers and negative-income workers counted NOT eligible (shares above); student exclusion\n",
    "is post-secondary only. Net bias is NOT signed (exclusions push down, unobserved dependents push\n",
    "up) -- treat as an approximation, not a clean bound.\n", sep = "")
write.csv(data.frame(
  metric = c("elig_any_v1_wt", "elig_any_v2_wt", "elig_full_wt", "wedge_wt", "n_dep_obs"),
  value  = c(d_v1$wt_num, s_any$wt_num, s_full$wt_num, s_wedge_of_elig$wt_num, sum(dep_obs))
), file.path(path_tables, "savers_match_summary.csv"), row.names = FALSE)
## Per-row eligibility flags for the counterfactual (04) to join on.
write_parquet(df[, c("SSUID", "PNUM", "filing_group", "sm_elig_any", "sm_elig_full", "sm_wedge")],
              file.path(path_processed, "sipp_savers_match.parquet"))
cat("Wrote savers_match_eligibility.csv, savers_match_wedge.csv, savers_match_summary.csv, sipp_savers_match.parquet\n")
