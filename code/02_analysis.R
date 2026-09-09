# code/02_analysis.R
# Compute the pillar tables (access H1-H5, participation P1-P2, matching M1)
# from data/processed/sipp_fastfacts.parquet, weighted, with unified + class + demographic
# breakouts. Writes CSVs to output/tables/. Implements the 2026-07-10 spec.

suppressWarnings(suppressMessages(library(arrow)))
path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
source(file.path(path_project, "code", "_shared", "helpers.R"))
path_processed <- file.path(path_project, "data", "processed")
path_tables    <- file.path(path_project, "output", "tables")
dir.create(path_tables, showWarnings = FALSE, recursive = TRUE)

df <- as.data.frame(read_parquet(file.path(path_processed, "sipp_fastfacts.parquet")))
df$.univ <- TRUE
DIMS <- c("worker_class","ft_pt","sex","race_eth","educ_grp","age_band","disability","earn_decile")
w <- function(t, name) { write.csv(t, file.path(path_tables, name), row.names = FALSE); t }
pct <- function(x) sprintf("%.1f%%", 100 * x)

## H1/H2/H5 - lacks employer-provided access (offered & eligible), all breakouts
access_hdln <- w(breakout(df, "lacks_access", ".univ", DIMS), "access_headline.csv")

## Access by race/ethnicity x education (four-cell grid for the Figure 3 access variant,
## 2026-08-31). "Other (NH)" is excluded per user decision: it is the smallest and most
## heterogeneous race_eth category, and four cells (Hispanic, White NH, Black NH, Asian NH)
## keep the grid at a clean 2x2 rather than folding a fifth, dissimilar group into one panel.
## Scope is PRIVATE-SECTOR EMPLOYEES ONLY (2026-08-31 decision): government and self-employed
## workers are dropped so every cell and the national benchmark (H1's private-sector row, 49.1%
## lacks access) describe the same population; mixing in self-employed (forced no-access by
## construction, 01) or government (a structurally different access regime) against a
## private-sector benchmark would compare mismatched populations.
## Numerator is lacks_access (2026-08-31: switched from access_emp to match the draft's framing,
## which is built around the SHARE WHO LACK access throughout -- see the H1 headline above and
## Figure 1's waffle/bar chart -- not the share who have it. `share` in the output CSV is
## therefore a lacks-access rate, consistent with access_headline.csv's own convention.
df_priv_race4 <- df[df$worker_class == "Private" &
                    df$race_eth %in% c("Hispanic", "White (NH)", "Black (NH)", "Asian (NH)"), ,
                    drop = FALSE]
access_race_educ <- w(breakout2(df_priv_race4, "lacks_access", ".univ", "race_eth", "educ_grp"),
                      "access_by_race_educ.csv")

## Private-sector one-way marginals (2026-08-31): race-only and education-only lacks-access
## rates, same PRIVATE-SECTOR population as the cross-tab above but collapsed across the other
## dimension (e.g. "Black (NH), any education" rather than a single race x education cell).
## These back the draft prose comparing race/ethnicity and education gaps among private-sector
## employees; kept as their own tables rather than derived from access_by_race_educ.csv so each
## number traces directly to a `breakout()` call, the same pattern as access_headline.csv.
df_priv <- df[df$worker_class == "Private", , drop = FALSE]
access_race_private <- w(breakout(df_priv, "lacks_access", ".univ", "race_eth"),
                         "access_by_race_private.csv")
access_educ_private  <- w(breakout(df_priv, "lacks_access", ".univ", "educ_grp"),
                         "access_by_educ_private.csv")

## Figure 2 income-ladder inputs (2026-08-31): access, participation, and employer
## contribution/match by earnings decile, PRIVATE-SECTOR EMPLOYEES ONLY. Figure 2 previously
## mixed all-worker access (df) with employee-frame match (Private+Government, `de`) -- two
## different populations in one chart. All three series now share the same private-sector-
## employee population as the race/education private-sector tables above.
access_decile_private <- w(breakout(df_priv, "lacks_access",  ".univ", "earn_decile"),
                           "access_by_decile_private.csv")
part_decile_private   <- w(breakout(df_priv, "participates",  ".univ", "earn_decile"),
                           "participation_by_decile_private.csv")
match_decile_private  <- w(breakout(df_priv, "match_receipt", ".univ", "earn_decile"),
                           "matching_by_decile_private.csv")

## Participation despite no employer contribution (2026-08-31): among PRIVATE-SECTOR EMPLOYEES
## who receive no employer contribution, the share who still participate in a plan on their own.
## This is the correct test of the liquidity-constraint hypothesis (higher earners can self-fund
## saving without an employer match) -- unlike the unconditional "participates AND no match" share,
## which is dominated by the fact that high earners participate more overall and does not isolate
## the ability to save UNMATCHED. Denominator is workers without a contribution (`no_match`);
## numerator is participation among that subgroup.
df_priv$no_match <- !df_priv$match_receipt
part_given_no_match_private <- w(breakout(df_priv, "participates", "no_match", "earn_decile"),
                                 "participation_given_no_match_private.csv")
## Bottom-half (deciles 1-5) vs top-half (deciles 6-10) summary of the same measure, for the
## console readout and the draft's liquidity-constraint claim.
df_priv$earn_half <- ifelse(df_priv$earn_decile <= 5, "Bottom half (deciles 1-5)",
                             "Top half (deciles 6-10)")
part_given_no_match_half_private <- w(breakout(df_priv, "participates", "no_match", "earn_half"),
                                      "participation_given_no_match_half_private.csv")

## H3/H4 - alternative access definitions (overall + by worker class)
df$lacks_offer <- !df$access_offer
df$lacks_ownership <- !df$access_ownership
alt <- rbind(
  transform(breakout(df, "lacks_offer",     ".univ", "worker_class"), definition = "H3 offer-only (lacks)"),
  transform(breakout(df, "lacks_ownership", ".univ", "worker_class"), definition = "H4 ownership DC/IRA (lacks)"),
  transform(breakout(df, "lacks_access",    ".univ", "worker_class"), definition = "H1 offered+eligible (lacks)")
)
w(alt, "access_alt_definitions.csv")

## P1 - participation among those WITH employer access. Denominator = plan-type-OBSERVED access
## (the ESCNTYN question universe; decision of 2026-08-27); the all-access denominator is
## kept as a disclosed conservative floor. P2 - among all workers.
part_access <- w(breakout(df, "participates", "access_emp_obs", DIMS), "participation_given_access.csv")
part_floor  <- w(breakout(df, "participates", "access_emp",     DIMS), "participation_given_access_floor.csv")
part_all    <- w(breakout(df, "participates", ".univ",          DIMS), "participation_all_workers.csv")

## M1 - employer-match receipt among all workers and among participants
match_all  <- w(breakout(df, "match_receipt", ".univ",        DIMS), "matching_all_workers.csv")
match_part <- w(breakout(df, "match_receipt", "participates", DIMS), "matching_given_participation.csv")
## Companion: among participants inside the match-question universe (DC/IRA plan types;
## EECNTYN does not exist for pension-only participants).
match_part_dcira <- w(breakout(df, "match_receipt", "participates_dcira", DIMS),
                      "matching_given_participation_dcira.csv")

## M2 - employer-match DOLLARS among matched workers (I1; TECNTAMT via emp_contrib_amt)
matched  <- df$match_receipt & !is.na(df$emp_contrib_amt)
w_m <- df$WPFINWGT[matched]; amt <- df$emp_contrib_amt[matched]
emp_med  <- if (length(amt)) wtd_quantile(amt, w_m, 0.5) else NA_real_
emp_mean <- if (length(amt)) sum(amt * w_m) / sum(w_m) else NA_real_
w(data.frame(measure = c("median_employer_$", "mean_employer_$", "n_matched_with_amt", "wt_matched_with_amt"),
             value   = c(emp_med, emp_mean, sum(matched), sum(w_m))),
  "matching_dollars.csv")

## Employee-only participation & matching (option-2 default, 2026-07-16): participation and matching are
## employer-benefit concepts, so self-employed business-plan saving is reported SEPARATELY rather than
## blended into all-worker rates. Employees = Private + Government (non-self-employed). Access keeps its
## all-worker frame; the all-worker P/M tables above are retained for reference and the tax-expenditure arc.
de <- df[df$worker_class %in% c("Private", "Government"), , drop = FALSE]
se <- df[df$worker_class == "Self-employed", , drop = FALSE]
part_emp  <- w(breakout(de, "participates",  ".univ", DIMS), "participation_employees.csv")
match_emp <- w(breakout(de, "match_receipt", ".univ", DIMS), "matching_employees.csv")
em <- de$match_receipt & !is.na(de$emp_contrib_amt)
emp_med_e  <- if (any(em)) wtd_quantile(de$emp_contrib_amt[em], de$WPFINWGT[em], 0.5) else NA_real_
emp_mean_e <- if (any(em)) sum(de$emp_contrib_amt[em] * de$WPFINWGT[em]) / sum(de$WPFINWGT[em]) else NA_real_
emp_summary <- data.frame(
  metric = c("participation_employees", "participation_employees_with_access",
             "participation_employees_with_access_floor",
             "match_employees", "match_employee_participants",
             "match_employee_participants_dcira",
             "median_employer_$_employees", "mean_employer_$_employees",
             "participation_selfemp", "match_selfemp"),
  value  = c(wtd_share(de$participates,  de$.univ,           de$WPFINWGT)$share,
             wtd_share(de$participates,  de$access_emp_obs,  de$WPFINWGT)$share,
             wtd_share(de$participates,  de$access_emp,      de$WPFINWGT)$share,
             wtd_share(de$match_receipt, de$.univ,           de$WPFINWGT)$share,
             wtd_share(de$match_receipt, de$participates,    de$WPFINWGT)$share,
             wtd_share(de$match_receipt, de$participates_dcira, de$WPFINWGT)$share,
             emp_med_e, emp_mean_e,
             wtd_share(se$participates,  se$.univ, se$WPFINWGT)$share,
             wtd_share(se$match_receipt, se$.univ, se$WPFINWGT)$share))
w(emp_summary, "pillars_employee_summary.csv")

## Compact console summary (overall rows)
ov <- function(t) t$share[t$dimension == "Overall"]
cat("\n=========== PILLAR SUMMARY (overall, weighted) ===========\n")
cat("H1 lacks employer-provided access (offered+eligible):", pct(ov(access_hdln)), "\n")
cat("H3 lacks (offer-only):", pct(wtd_share(df$lacks_offer, df$.univ, df$WPFINWGT)$share),
    " | H4 lacks (ownership DC/IRA):", pct(wtd_share(df$lacks_ownership, df$.univ, df$WPFINWGT)$share), "\n")
cat("P1 participation | access:", pct(ov(part_access)),
    " | P2 participation | all:", pct(ov(part_all)), "\n")
cat("M1 match | all workers:", pct(ov(match_all)),
    " | match | participants:", pct(ov(match_part)), "\n")
cat("M2 employer-match $ (annual, among matched): median $", round(emp_med),
    " | mean $", round(emp_mean), " | matched n=", sum(matched), "\n", sep = "")
cat("\n--- EMPLOYEE-ONLY DEFAULT (self-employed reported separately) ---\n")
ev <- function(m) emp_summary$value[emp_summary$metric == m]   # by name, not position
cat("Participation | employees:", pct(ev("participation_employees")),
    " | of employees with access (plan type observed):", pct(ev("participation_employees_with_access")),
    " | conservative floor (all access):", pct(ev("participation_employees_with_access_floor")), "\n")
cat("Match | employees:", pct(ev("match_employees")),
    " | of employee participants:", pct(ev("match_employee_participants")),
    " | of DC/IRA-universe participants:", pct(ev("match_employee_participants_dcira")), "\n")
cat("Employer-match $ (employees, matched): median $", round(ev("median_employer_$_employees")),
    " | mean $", round(ev("mean_employer_$_employees")), "\n", sep = "")
cat("Self-employed (separate): participation", pct(ev("participation_selfemp")),
    " | match", pct(ev("match_selfemp")), "\n")
cat("\n--- FIGURE 2 INPUTS: PRIVATE-SECTOR EMPLOYEES ONLY ---\n")
cat("Lacks access:", pct(ov(access_decile_private)),
    " | Doesn't participate:", pct(1 - ov(part_decile_private)),
    " | No employer contribution:", pct(1 - ov(match_decile_private)), "\n")
cat("\n--- FIGURE 2B INPUT: PARTICIPATION AMONG THE UNMATCHED, PRIVATE-SECTOR EMPLOYEES ---\n")
half <- part_given_no_match_half_private
p_bot <- half$share[half$group == "Bottom half (deciles 1-5)"]
p_top <- half$share[half$group == "Top half (deciles 6-10)"]
cat("Bottom half (deciles 1-5):", pct(p_bot), " | Top half (deciles 6-10):", pct(p_top),
    " | gap:", sprintf("%.1fpp", 100 * (p_top - p_bot)),
    " | ratio:", sprintf("%.1fx", p_top / p_bot), "\n")
## ===========================================================================
## Pillar external calibration: SIPP pillar estimates vs BLS NCS
## employer-reported benchmarks, on matched populations. The TE side has a full
## calibration table (12); this is the pillar analog for the public headline.
## External values are hand-entered from the BLS March 2024 EBS news release
## (ebs2_09192024; verified 2026-09-08) in data/raw/external_benchmarks/
## pillar_external.csv -- each row carries its source and vintage.
## Concept differences (why SIPP access is EXPECTED to sit below NCS):
##  - reporter: worker/proxy self-report (SIPP) vs establishment report (NCS);
##    W-2-linked studies find household reports understate DC offer/participation
##    (Dushi, Iams & Tamborini, Social Security Bulletin 2011/2017), so the SIPP
##    lacks-access headline is best read as an UPPER bound on that margin;
##  - concept: offered AND personally eligible (SIPP) vs plan available to any
##    employees at the establishment (NCS);
##  - unit/frame: December-employed persons 18-64 (SIPP) vs jobs at surveyed
##    establishments, all ages (NCS).
## ===========================================================================
ext <- read.csv(file.path(path_project, "data", "raw", "external_benchmarks", "pillar_external.csv"),
                stringsAsFactors = FALSE)
sl  <- df$worker_class == "Government" & df$gov_subclass %in% c("State", "Local")
civ <- df$worker_class == "Private" | sl        # BLS civilian scope: private + state/local gov
prv <- df$worker_class == "Private"
sipp_val <- function(pop_mask, series) {
  switch(series,
    access        = wtd_share(df$access_emp,   pop_mask, df$WPFINWGT)$share,
    participation = wtd_share(df$participates, pop_mask, df$WPFINWGT)$share,
    takeup        = wtd_share(df$participates, pop_mask & df$access_emp_obs, df$WPFINWGT)$share)
}
pop_masks <- list("Private industry workers" = prv,
                  "Civilian workers (private + state/local gov)" = civ,
                  "State and local government workers" = sl)
bench <- ext
bench$sipp_estimate <- mapply(function(p, s) sipp_val(pop_masks[[p]], s), ext$population, ext$series)
bench$gap_pp <- round(100 * (bench$sipp_estimate - bench$external_value), 1)
bench$sipp_concept <- ifelse(bench$series == "takeup",
  "participation among workers with plan-type-observed employer access (self/proxy-reported)",
  paste0(bench$series, ", worker-reported, offered-and-eligible concept, December-employed ages 18-64"))
bench <- bench[, c("series", "population", "sipp_estimate", "external_value", "gap_pp",
                   "vintage", "sipp_concept", "source")]
bench$sipp_estimate <- round(bench$sipp_estimate, 4)
w(bench, "pillar_benchmarks.csv")
cat("\n--- PILLAR CALIBRATION vs BLS NCS (see pillar_benchmarks.csv; concept differences in header) ---\n")
print(bench[, c("series", "population", "sipp_estimate", "external_value", "gap_pp")], row.names = FALSE)

cat("\nWrote the pillar tables to output/tables/.\n")
