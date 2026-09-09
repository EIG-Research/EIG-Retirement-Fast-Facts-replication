# code/10_tax_expenditure.R
# TE Stage 4+6: CBO-style PRESENT-VALUE retirement tax expenditure per worker, aggregated to
# income deciles; payroll-tax leg; JCT-style cash-flow appendix; benchmark comparisons.
# Inputs : data/processed/te_persons.parquet, tax_units.parquet, taxsim_results.parquet
# Outputs: output/tables/te_decile.csv, te_quintile_cbo_compare.csv, te_benchmarks.csv,
#          te_decile_cbo_comparable.csv, te_decile_cbo_comparable_r6.csv;
#          data/processed/te_person_results.parquet (console review is also printed)
#
# Estimand 1 (HEADLINE, CBO-faithful PV): for CY2024 contributions only,
#   TE_income = arc exclusion tax difference (from 09, actual TY2024 brackets/credits)
#             + PV future component: untaxed inside buildup on the 2024 contribution vs. a
#               taxable account, net of future withdrawal taxes (CBO parameters: accumulate
#               to 65, equal withdrawals to 85, r = discount = 3.5%, withdrawal MTR = current).
#   TE_payroll = employer DC/IRA contributions x FICA wedge (employee deferrals are already
#                FICA-taxable; no offset for future Social Security benefits, matching CBO).
# Estimand 2 (appendix, JCT-style cash-flow): frate x (contributions + r x year-end balances)
#   - frate x observed withdrawals. Reported as an aggregate only.
# Two rows ship every run: the "observed flows" row (main-employer flows only, no Roth split,
# no employer-DB imputation, federal tax only) and the CBO-COMPARABLE row (Stage-5 imputations
# from 11 layered on; the published headline). 11 must run first -- enforced below, fail-loud.

suppressWarnings(suppressMessages({
  library(arrow); library(dplyr)
}))

path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
source(file.path(path_project, "code", "_shared", "helpers.R"))
path_processed <- file.path(path_project, "data", "processed")
path_tables    <- file.path(path_project, "output", "tables")
dir.create(path_tables, showWarnings = FALSE, recursive = TRUE)

persons <- as.data.frame(read_parquet(file.path(path_processed, "te_persons.parquet")))
units   <- as.data.frame(read_parquet(file.path(path_processed, "tax_units.parquet")))
tsr     <- as.data.frame(read_parquet(file.path(path_processed, "taxsim_results.parquet")))

## =============================================================================
## PV machinery: shared module code/_shared/pv_te.R (CBO 57413 appendix parameters).
## Traditional TE per $1 pre-tax = m0 + fut_trad; Roth TE per $1 after-tax = te_roth1.
## The observed arc legs from 09 replace the linear m0 upfront piece.
## =============================================================================
source(file.path(path_project, "code", "_shared", "pv_te.R"))

## --- Sanity invariants on the PV functions -----------------------------------
stopifnot(abs(te_trad(1000, 40, 0.22, 0.22, r = 0)) < 1e-8)   # r=0, m0=mr -> TE=0
stopifnot(te_trad(1000, 40, 0) == 0, te_roth(1000, 40, 0) == 0)
stopifnot(te_trad(1000, 40, 0.24) > te_trad(1000, 40, 0.12))
stopifnot(te_trad(1000, 30, 0.22) > te_trad(1000, 60, 0.22))
stopifnot(te_roth(1000, 30, 0.22) > 0)

## =============================================================================
## Person-level assembly
## =============================================================================
tsx <- tsr |> select(filing_unit_id, te_ee_arc, te_er_arc, frate_base, ficar_base)

pp <- persons |> left_join(tsx, by = "filing_unit_id")
z <- function(x) ifelse(is.na(x), 0, x)
## A handful of persons are dependents of a NON-FILING dependent (e.g., an infant whose parent
## is themselves a dependent teen); their parent unit never reaches TAXSIM. Their TE is zero.
n_orphan <- sum(is.na(pp$frate_base))
if (n_orphan > 0) message("10: NOTE ", n_orphan,
                          " persons (dependents of non-filing dependents) get TE = 0.")
for (v in c("te_ee_arc","te_er_arc")) pp[[v]] <- z(pp[[v]])
pp$c_ee <- z(pp$c_ee_401) + z(pp$c_ee_ira) + z(pp$c_ee_pen)   # employee pre-tax contribs
pp$c_er <- z(pp$c_er_401) + z(pp$c_er_ira)                    # employer DC/IRA contribs
pp$c_all <- pp$c_ee + pp$c_er
pp$m0 <- z(pp$frate_base) / 100                               # federal MTR as fraction

## Allocate unit-level arc TE to persons in proportion to own contributions.
## Denominators are PERSON-SUMS within the filing unit: the unit-frame
## totals cover adults only, so a contributing non-filing dependent would receive a share
## without being in the denominator, letting shares sum above 1.
pp <- pp |> group_by(filing_unit_id) |>
  mutate(u_c_ee_tot = sum(c_ee), u_c_er_tot = sum(c_er)) |> ungroup() |> as.data.frame()
pp$te_ee_arc_p <- ifelse(pp$u_c_ee_tot > 0, z(pp$te_ee_arc) * pp$c_ee / pp$u_c_ee_tot, 0)
pp$te_er_arc_p <- ifelse(pp$u_c_er_tot > 0, z(pp$te_er_arc) * pp$c_er / pp$u_c_er_tot, 0)

## PV future component (per $1 of contribution, by age x MTR), both legs
contrib <- pp$c_all > 0 & !is.na(pp$age)
fg  <- pv_factor_grid(pp$age[contrib], pp$m0[contrib], r = PV_RETURN)
fg6 <- pv_factor_grid(pp$age[contrib], pp$m0[contrib], r = PV_RETURN_SENS)
pp$te_future  <- 0; pp$te_future6 <- 0
pp$te_future[contrib]  <- pp$c_all[contrib] * fg(pp$age[contrib],  pp$m0[contrib], "fut_trad")
pp$te_future6[contrib] <- pp$c_all[contrib] * fg6(pp$age[contrib], pp$m0[contrib], "fut_trad")

## Income-tax TE (headline) and 6% sensitivity (arc legs held fixed)
pp$te_income  <- pp$te_ee_arc_p + pp$te_er_arc_p + pp$te_future
pp$te_income6 <- pp$te_ee_arc_p + pp$te_er_arc_p + pp$te_future6

## Payroll leg: employer contributions escape FICA; wedge depends on wage position
## relative to the TY2024 OASDI cap; employer-share incidence gross-up. No Additional
## Medicare Tax (0.9%) modeled (disclosed).
wage_p <- z(pp$wages)
pp$fica_wedge <- ifelse(wage_p < OASDI_WAGE_CAP,
                        (FICA_OASDI_RATE + FICA_HI_RATE), FICA_HI_RATE) / FICA_EE_ER_GROSSUP
pp$te_payroll <- pp$c_er * pp$fica_wedge
pp$te_total   <- pp$te_income + pp$te_payroll

## Estimand 2 (JCT-style cash-flow, aggregate appendix): current-year exclusions + tax on
## inside earnings of the ENTIRE stock, minus tax on current withdrawals.
pp$te_cashflow <- pp$te_ee_arc_p + pp$te_er_arc_p +
                  pp$m0 * PV_RETURN * z(pp$bal_ret) -
                  pp$m0 * (z(pp$wd_ira) + z(pp$wd_401))

## =============================================================================
## Row B: CBO-COMPARABLE (Stage-5 imputations: employer DB, Roth split, outside IRA)
## Built only when 11_stage5_imputations.R has produced stage5_imputed.parquet.
## Imputed legs use LINEAR m0 upfronts (arc precision is not meaningful for imputed
## dollars); observed legs keep their arcs, with the employee arc scaled by the
## traditional fraction (Roth 401(k) deferrals are NOT excluded from taxable wages).
## =============================================================================
## Row B requires 11_stage5_imputations.R to have run. This was previously a silent
## file.exists() branch: running the scripts in numeric order (10 before 11) produced a te_decile.csv
## with the CBO-comparable HEADLINE row missing and no error at all (external RA review, 2026-08-27).
## Fail loud instead; run_all.R runs 11 BEFORE 10 by design (see code/README.md).
s5_path <- file.path(path_processed, "stage5_imputed.parquet")
HAS_S5  <- file.exists(s5_path)
if (!HAS_S5 && !nzchar(Sys.getenv("SIPP_ALLOW_NO_STAGE5"))) {
  stop("10_tax_expenditure: ", basename(s5_path), " not found - run 11_stage5_imputations.R first.\n",
       "  The CBO-comparable headline row (Row B) depends on it. For a deliberate\n",
       "  observed-flows-only run, set SIPP_ALLOW_NO_STAGE5=1.")
}
if (HAS_S5) {
  s5 <- as.data.frame(read_parquet(s5_path))
  pp <- left_join(pp, s5[, setdiff(names(s5), "sector")], by = c("SSUID","PNUM"))
  for (v in c("c_er_db_imp","c_er_db_imp_actual","c_ee_401_roth","c_ee_401_trad",
              "c_ira_out_trad_ded","c_ira_out_trad_nonded","c_ira_out_roth"))
    pp[[v]] <- z(pp[[v]])

  pp$c_ee_trad <- pp$c_ee - pp$c_ee_401_roth
  ee_frac <- ifelse(pp$c_ee > 0, pp$c_ee_trad / pp$c_ee, 1)

  trad_C <- pp$c_ee_trad + pp$c_er + pp$c_er_db_imp + pp$c_ira_out_trad_ded
  roth_C <- pp$c_ee_401_roth + pp$c_ira_out_roth + pp$c_ira_out_trad_nonded
  contribB <- (trad_C + roth_C) > 0 & !is.na(pp$age)
  fgB <- pv_factor_grid(pp$age[contribB], pp$m0[contribB], r = PV_RETURN)
  fut_B <- numeric(nrow(pp)); roth_B <- numeric(nrow(pp))
  fut_B[contribB]  <- trad_C[contribB] * fgB(pp$age[contribB], pp$m0[contribB], "fut_trad")
  roth_B[contribB] <- roth_C[contribB] * fgB(pp$age[contribB], pp$m0[contribB], "te_roth1")

  pp$te_income_B  <- pp$te_ee_arc_p * ee_frac + pp$te_er_arc_p +
                     pp$m0 * (pp$c_er_db_imp + pp$c_ira_out_trad_ded) +
                     fut_B + roth_B
  pp$te_payroll_B <- (pp$c_er + pp$c_er_db_imp) * pp$fica_wedge
  pp$te_total_B   <- pp$te_income_B + pp$te_payroll_B

  ## CO-HEADLINE: same composition at r = PV_RETURN_SENS (6%) - the "2024 rate environment"
  ## row (user decision 2026-07-11). CBO's 3.5% is a 2021 low-rate-era parameter; at matched
  ## MTRs and discount = return, the PV benefit is entirely the inside-buildup shelter, which
  ## scales strongly with the return. Arc legs and the payroll wedge are rate-invariant.
  fgB6 <- pv_factor_grid(pp$age[contribB], pp$m0[contribB], r = PV_RETURN_SENS)
  fut_B6 <- numeric(nrow(pp)); roth_B6 <- numeric(nrow(pp))
  fut_B6[contribB]  <- trad_C[contribB] * fgB6(pp$age[contribB], pp$m0[contribB], "fut_trad")
  roth_B6[contribB] <- roth_C[contribB] * fgB6(pp$age[contribB], pp$m0[contribB], "te_roth1")
  pp$te_income_B6 <- pp$te_ee_arc_p * ee_frac + pp$te_er_arc_p +
                     pp$m0 * (pp$c_er_db_imp + pp$c_ira_out_trad_ded) +
                     fut_B6 + roth_B6
  pp$te_total_B6  <- pp$te_income_B6 + pp$te_payroll_B
  ## actual-cash DB sensitivity (payroll and linear income upfront+future recomputed)
  fut_dbA <- numeric(nrow(pp))
  fut_dbA[contribB] <- pp$c_er_db_imp_actual[contribB] *
                       fgB(pp$age[contribB], pp$m0[contribB], "te_trad1")
  ## Masked lookup: fgB is queried ONLY on the contribB rows the grid was built from.
  ## The previous ifelse() form queried the FULL frame and relied on ifelse to discard the silent
  ## NAs from out-of-grid pairs; the grid lookup now fails loud on any unmasked query.
  fut_db_accr <- numeric(nrow(pp))
  fut_db_accr[contribB] <- pp$c_er_db_imp[contribB] *
                           fgB(pp$age[contribB], pp$m0[contribB], "fut_trad")
  te_income_B_dbactual  <- pp$te_income_B -
                           (pp$m0 * pp$c_er_db_imp + fut_db_accr) +
                           fut_dbA
  te_payroll_B_dbactual <- (pp$c_er + pp$c_er_db_imp_actual) * pp$fica_wedge
}

## =============================================================================
## Aggregation
## =============================================================================
W <- pp$WPFINWGT
agg <- function(x, by) tapply(x * W, by, sum, na.rm = TRUE)

decile_table <- function(inc, pay) {
  tab <- data.frame(decile = 1:10)
  tot <- inc + pay
  tab$income_te_B   <- as.numeric(agg(inc, pp$decile)) / 1e9
  tab$payroll_te_B  <- as.numeric(agg(pay, pp$decile)) / 1e9
  tab$total_te_B    <- tab$income_te_B + tab$payroll_te_B
  tab$income_share  <- tab$income_te_B  / sum(tab$income_te_B)
  tab$payroll_share <- tab$payroll_te_B / sum(tab$payroll_te_B)
  tab$total_share   <- tab$total_te_B   / sum(tab$total_te_B)
  tab$pct_benefiting <- as.numeric(tapply(W * (tot > 0), pp$decile,
                                          function(x) sum(x, na.rm = TRUE)) /
                                   tapply(W, pp$decile, sum))
  ben <- !is.na(tot) & tot > 0
  tab$mean_te_benef <- as.numeric(tapply((tot*W)[ben], pp$decile[ben], sum) /
                                  tapply(W[ben], pp$decile[ben], sum))
  tab
}

decile_tab <- decile_table(pp$te_income, pp$te_payroll)

if (HAS_S5) decile_tab_cbo <- decile_table(pp$te_income_B, pp$te_payroll_B)

## Weighting-convention gap: SIPP has no tax-unit weight, so two conventions coexist --
## 09's console prints UNIT totals weighted by the primary filer's person weight, while the
## canonical published path (everything below) allocates unit TE to persons and aggregates with
## WPFINWGT. Quantify the gap once so no draft number is ever quoted from the unit-weighted path.
unit_arcB <- sum((tsr$te_ee_arc + tsr$te_er_arc) * tsr$wgt_unit, na.rm = TRUE) / 1e9
pers_arcB <- sum((pp$te_ee_arc_p + pp$te_er_arc_p) * W, na.rm = TRUE) / 1e9
write.csv(data.frame(
  path = c("09 console (unit-weighted, primary filer's WPFINWGT)",
           "10 canonical (person-allocated, WPFINWGT)"),
  aggregate_arc_te_B = round(c(unit_arcB, pers_arcB), 2),
  note = "Published numbers use the person-weighted path only; the unit-weighted 09 console total is a diagnostic."
), file.path(path_tables, "te_weighting_convention.csv"), row.names = FALSE)
cat(sprintf("10: weighting-convention check: unit-weighted arc TE $%.1fB vs person-weighted $%.1fB (gap %.1f%%). Published = person-weighted.\n",
            unit_arcB, pers_arcB, 100 * abs(unit_arcB - pers_arcB) / pers_arcB))

## CBO quintile comparison (income-tax leg shares)
q <- ceiling(decile_tab$decile / 2)
quint <- data.frame(quintile = 1:5,
  sipp_income_share = as.numeric(tapply(decile_tab$income_te_B,  q, sum)) / sum(decile_tab$income_te_B),
  sipp_payroll_share= as.numeric(tapply(decile_tab$payroll_te_B, q, sum)) / sum(decile_tab$payroll_te_B),
  ## Provenance: the $202B/$74B totals, the 63% top-quintile income share, and the ~0.7%
  ## bottom-quintile share are verified against the CBO 57413 report text; the three interior
  ## income shares and the payroll vector come from the exhibit's underlying values (CBO 57413
  ## supplemental data -- the printed p.20 exhibit is a chart) and have not been independently
  ## re-verified against the supplemental file. Re-confirm on the next vintage bump.
  cbo2019_income_share  = c(0.007, 0.038, 0.089, 0.24, 0.63),
  cbo2019_payroll_share = c(0.029, 0.067, 0.15, 0.30, 0.45))
if (HAS_S5) {
  quint$sipp_cbo_comparable_income  <- as.numeric(tapply(decile_tab_cbo$income_te_B,  q, sum)) /
                                           sum(decile_tab_cbo$income_te_B)
  quint$sipp_cbo_comparable_payroll <- as.numeric(tapply(decile_tab_cbo$payroll_te_B, q, sum)) /
                                           sum(decile_tab_cbo$payroll_te_B)
}

## Benchmarks
totals <- c(
  income_te_B    = sum(decile_tab$income_te_B),
  payroll_te_B   = sum(decile_tab$payroll_te_B),
  total_te_B     = sum(decile_tab$total_te_B),
  income_te6_B   = sum(pp$te_income6 * W, na.rm = TRUE) / 1e9 + sum(decile_tab$payroll_te_B) * 0,
  cashflow_te_B  = sum(pp$te_cashflow * W, na.rm = TRUE) / 1e9,
  contrib_ee_B   = sum(pp$c_ee * W, na.rm = TRUE) / 1e9,
  contrib_er_B   = sum(pp$c_er * W, na.rm = TRUE) / 1e9)
bench <- data.frame(
  quantity = c("SIPP CY2024 income-tax TE (PV, observed flows)",
               "SIPP CY2024 payroll-tax TE (observed employer DC/IRA)",
               "SIPP CY2024 combined TE",
               "SIPP income-tax TE at r=6% sensitivity",
               "SIPP JCT-style cash-flow TE (appendix)",
               "SIPP weighted employee contributions",
               "SIPP weighted employer DC/IRA contributions",
               "CBO CY2019 income-tax TE (PV, incl. DB imputation)",
               "CBO CY2019 payroll-tax TE",
               "Treasury PV of CY2025 activity: DC",
               "Treasury PV of CY2025 activity: DB",
               "JCT cash-flow FY2024: DC"),
  value_B = c(round(unname(totals[c("income_te_B","payroll_te_B","total_te_B",
                                    "income_te6_B","cashflow_te_B",
                                    "contrib_ee_B","contrib_er_B")]), 1),
              202, 74, 215.4, 92.6, 212.0),
  source = c(rep("this pipeline (10_tax_expenditure.R)", 7),
             "CBO 57413 p.20", "CBO 57413 p.20",
             "Treasury FY2027 TE report Table 4", "same", "JCX-48-24 p.33"))

## Person-level results for downstream SE estimation (12) and figures
pl_cols <- c("SSUID","PNUM","WPFINWGT","decile","age","c_ee","c_er",
             "te_income","te_payroll","te_total","te_cashflow")
if (HAS_S5) pl_cols <- c(pl_cols, "te_income_B","te_payroll_B","te_total_B",
                         "te_income_B6","te_total_B6",
                         "c_er_db_imp","c_ee_401_roth","c_ira_out_trad_ded",
                         "c_ira_out_trad_nonded","c_ira_out_roth",
                         "c_ee_401","c_ee_ira","c_ee_pen","c_er_401","c_er_ira")
write_parquet(pp[, pl_cols], file.path(path_processed, "te_person_results.parquet"))

if (HAS_S5) {
  totB_inc <- sum(decile_tab_cbo$income_te_B); totB_pay <- sum(decile_tab_cbo$payroll_te_B)
  decile_tab_cbo6 <- decile_table(pp$te_income_B6, pp$te_payroll_B)
  totB6_inc <- sum(decile_tab_cbo6$income_te_B)
  ## Sensitivity: CBO's design pins the WITHDRAWAL-year MTR to the contribution-year MTR
  ## (mr = m0) for horizons up to 47 years. That is the replication target, not a bug -- but it is
  ## an untested lever, so its leverage is published: rescale the withdrawal rate alone and
  ## recompute the income-tax leg (arc legs and the payroll wedge are unaffected).
  te_inc_B_mr <- vapply(c(0.9, 0.8, 0.7), function(k) {
    fgk   <- pv_factor_grid(pp$age[contribB], pp$m0[contribB], r = PV_RETURN, mr_scale = k)
    fut_k <- numeric(nrow(pp))
    fut_k[contribB] <- trad_C[contribB] * fgk(pp$age[contribB], pp$m0[contribB], "fut_trad")
    sum((pp$te_ee_arc_p * ee_frac + pp$te_er_arc_p +
         pp$m0 * (pp$c_er_db_imp + pp$c_ira_out_trad_ded) + fut_k + roth_B) * W, na.rm = TRUE) / 1e9
  }, numeric(1))
  ## Headline decomposition: upfront (arc + linear imputed-upfront) legs vs the PV future component
  ## -- the net of large offsetting terms behind the income-tax total.
  upfront_B <- sum((pp$te_ee_arc_p * ee_frac + pp$te_er_arc_p +
                    pp$m0 * (pp$c_er_db_imp + pp$c_ira_out_trad_ded)) * W, na.rm = TRUE) / 1e9
  future_B  <- sum((fut_B + roth_B) * W, na.rm = TRUE) / 1e9
  bench <- rbind(bench, data.frame(
    quantity = c("SIPP CY2024 income-tax TE (CBO-comparable: +DB accrual, Roth split, outside IRA)",
                 "SIPP CY2024 payroll-tax TE (CBO-comparable: +DB employer accrual)",
                 "SIPP CY2024 combined TE (CBO-comparable, CBO parameters r=3.5%)",
                 "CO-HEADLINE: income-tax TE, CBO-comparable at r=6% (2024 rate environment)",
                 "CO-HEADLINE: combined TE, CBO-comparable at r=6% (2024 rate environment)",
                 "  sensitivity: income TE with actual-cash DB concept",
                 "  sensitivity: payroll TE with actual-cash DB concept",
                 "  sensitivity: income TE, withdrawal MTR = 0.9x contribution MTR",
                 "  sensitivity: income TE, withdrawal MTR = 0.8x contribution MTR",
                 "  sensitivity: income TE, withdrawal MTR = 0.7x contribution MTR",
                 "  decomposition: upfront legs (arcs + linear imputed upfront)",
                 "  decomposition: PV future component (inside buildup net of withdrawal tax)",
                 "Imputed employer DB (accrued) / outside-IRA trad+Roth dollars"),
    value_B = round(c(totB_inc, totB_pay, totB_inc + totB_pay,
                      totB6_inc, totB6_inc + totB_pay,
                      sum(te_income_B_dbactual * W, na.rm = TRUE)/1e9,
                      sum(te_payroll_B_dbactual * W, na.rm = TRUE)/1e9,
                      te_inc_B_mr[1], te_inc_B_mr[2], te_inc_B_mr[3],
                      upfront_B, future_B,
                      sum((pp$c_er_db_imp + pp$c_ira_out_trad_ded + pp$c_ira_out_trad_nonded +
                           pp$c_ira_out_roth) * W, na.rm = TRUE)/1e9), 1),
    source = "this pipeline (11 + 10, Stage 5)"))
  ## Roth-dollar-scale sensitivity: the participant->dollar factor (TY2020 IRS W-2
  ## statistics; stage5_scalars.csv) is the one stale external parameter with headline leverage
  ## and previously had NO published sensitivity. Rescale the Roth share of 401(k) dollars to
  ## alternative factors and recompute the income leg. Arc legs and the payroll wedge are held
  ## fixed (same convention as the r=6% and mr-scale sensitivities); Roth dollars are capped at
  ## each person's total 401(k) contribution.
  scal10 <- read.csv(file.path(path_project, "data", "raw", "external_benchmarks",
                               "stage5_scalars.csv"), stringsAsFactors = FALSE)
  roth_scale_base <- scal10$value[scal10$param == "roth401k_dollar_scale"]
  stopifnot(length(roth_scale_base) == 1, roth_scale_base > 0)
  c401_tot <- pp$c_ee_401_roth + pp$c_ee_401_trad
  ROTH_SCALES <- c(0.90, 1.00)
  te_inc_B_roth <- vapply(ROTH_SCALES, function(s) {
    roth_alt      <- pmin(pp$c_ee_401_roth * (s / roth_scale_base), c401_tot)
    c_ee_trad_alt <- pp$c_ee - roth_alt
    ee_frac_alt   <- ifelse(pp$c_ee > 0, c_ee_trad_alt / pp$c_ee, 1)
    trad_alt      <- c_ee_trad_alt + pp$c_er + pp$c_er_db_imp + pp$c_ira_out_trad_ded
    roth_Calt     <- roth_alt + pp$c_ira_out_roth + pp$c_ira_out_trad_nonded
    fut_a <- numeric(nrow(pp)); roth_a <- numeric(nrow(pp))
    fut_a[contribB]  <- trad_alt[contribB]  * fgB(pp$age[contribB], pp$m0[contribB], "fut_trad")
    roth_a[contribB] <- roth_Calt[contribB] * fgB(pp$age[contribB], pp$m0[contribB], "te_roth1")
    sum((pp$te_ee_arc_p * ee_frac_alt + pp$te_er_arc_p +
         pp$m0 * (pp$c_er_db_imp + pp$c_ira_out_trad_ded) + fut_a + roth_a) * W, na.rm = TRUE) / 1e9
  }, numeric(1))
  bench <- rbind(bench, data.frame(
    quantity = sprintf("  sensitivity: income TE, Roth 401(k) dollar scale %.2f (base %.2f, TY2020 W-2)",
                       ROTH_SCALES, roth_scale_base),
    value_B = round(te_inc_B_roth, 1),
    source = "this pipeline (11 + 10, Stage 5; arc legs held fixed)"))
  cat(sprintf("  Roth-scale sensitivity (income leg; base %.2f): %s\n", roth_scale_base,
              paste(sprintf("%.2fx $%.1fB", ROTH_SCALES, te_inc_B_roth), collapse = " | ")))
  ## Owner-wide IRA smear variant: footnote 5 claims the smear alternative leaves
  ## "totals, decile shares, and means essentially unchanged" -- previously asserted, not computed.
  ## Recompute the CBO-comparable TE with the *_smear IRA columns (11) in place of the
  ## concentrated ones and publish the comparison. The deductible share of smeared traditional
  ## dollars is applied at the aggregate rate (person-level ded shares are class-wide in 11).
  for (v in c("c_ira_out_trad_smear", "c_ira_out_roth_smear")) pp[[v]] <- z(pp[[v]])
  ded_frac_sm <- sum(pp$c_ira_out_trad_ded * W, na.rm = TRUE) /
                 max(sum((pp$c_ira_out_trad_ded + pp$c_ira_out_trad_nonded) * W, na.rm = TRUE), 1e-9)
  trad_sm_ded    <- pp$c_ira_out_trad_smear * ded_frac_sm
  trad_sm_nonded <- pp$c_ira_out_trad_smear - trad_sm_ded
  trad_C_sm <- pp$c_ee_trad + pp$c_er + pp$c_er_db_imp + trad_sm_ded
  roth_C_sm <- pp$c_ee_401_roth + pp$c_ira_out_roth_smear + trad_sm_nonded
  contribB_sm <- (trad_C_sm + roth_C_sm) > 0 & !is.na(pp$age)
  fgBsm <- pv_factor_grid(pp$age[contribB_sm], pp$m0[contribB_sm], r = PV_RETURN)
  fut_sm <- numeric(nrow(pp)); roth_sm <- numeric(nrow(pp))
  fut_sm[contribB_sm]  <- trad_C_sm[contribB_sm] * fgBsm(pp$age[contribB_sm], pp$m0[contribB_sm], "fut_trad")
  roth_sm[contribB_sm] <- roth_C_sm[contribB_sm] * fgBsm(pp$age[contribB_sm], pp$m0[contribB_sm], "te_roth1")
  pp$te_income_B_smear <- pp$te_ee_arc_p * ee_frac + pp$te_er_arc_p +
                          pp$m0 * (pp$c_er_db_imp + trad_sm_ded) +
                          fut_sm + roth_sm
  pp$te_total_B_smear  <- pp$te_income_B_smear + pp$te_payroll_B
  dec_sm <- decile_table(pp$te_income_B_smear, pp$te_payroll_B)
  ben_h  <- !is.na(pp$te_total_B) & pp$te_total_B > 0
  ben_s  <- !is.na(pp$te_total_B_smear) & pp$te_total_B_smear > 0
  smear_cmp <- data.frame(
    quantity = c("Combined TE total ($B)", "Mean TE per person ($)",
                 "Mean TE per beneficiary ($)", "Share of persons benefiting",
                 paste0("Decile ", 1:10, " share of combined TE")),
    headline_concentrated = round(c(sum(pp$te_total_B * W, na.rm = TRUE) / 1e9,
                                    sum(pp$te_total_B * W, na.rm = TRUE) / sum(W),
                                    sum(pp$te_total_B[ben_h] * W[ben_h]) / sum(W[ben_h]),
                                    sum(W[ben_h]) / sum(W),
                                    decile_tab_cbo$total_share), 4),
    smear_owner_wide      = round(c(sum(pp$te_total_B_smear * W, na.rm = TRUE) / 1e9,
                                    sum(pp$te_total_B_smear * W, na.rm = TRUE) / sum(W),
                                    sum(pp$te_total_B_smear[ben_s] * W[ben_s]) / sum(W[ben_s]),
                                    sum(W[ben_s]) / sum(W),
                                    dec_sm$total_share), 4))
  write.csv(smear_cmp, file.path(path_tables, "te_smear_sensitivity.csv"), row.names = FALSE)
  cat(sprintf("  IRA smear variant: combined TE $%.1fB vs headline $%.1fB; max decile-share shift %.2fpp; mean per beneficiary $%.0f vs $%.0f (see te_smear_sensitivity.csv)\n",
              smear_cmp$smear_owner_wide[1], smear_cmp$headline_concentrated[1],
              100 * max(abs(dec_sm$total_share - decile_tab_cbo$total_share)),
              smear_cmp$smear_owner_wide[3], smear_cmp$headline_concentrated[3]))
  write.csv(decile_tab_cbo,  file.path(path_tables, "te_decile_cbo_comparable.csv"), row.names = FALSE)
  write.csv(decile_tab_cbo6, file.path(path_tables, "te_decile_cbo_comparable_r6.csv"), row.names = FALSE)
}
write.csv(decile_tab, file.path(path_tables, "te_decile.csv"), row.names = FALSE)
write.csv(quint,      file.path(path_tables, "te_quintile_cbo_compare.csv"), row.names = FALSE)
write.csv(bench,      file.path(path_tables, "te_benchmarks.csv"), row.names = FALSE)
## Standing caveats for every te_* table: published as a sidecar so the notes
## travel with the CSVs instead of living only in this console.
writeLines(c(
  "Notes for te_decile*.csv, te_quintile_cbo_compare.csv, te_benchmarks.csv, te_se.csv:",
  "- SIPP public-use earnings/income/contribution amounts are topcoded and the survey top tail is",
  "  compressed relative to tax-return data, biasing top-decile/quintile TE shares (and all rows)",
  "  DOWNWARD relative to tax-return-based estimates (CBO/Treasury). A SOI top-tail calibration is",
  "  a documented future extension, deliberately not applied. Exact SIPP topcode rules per variable",
  "  are pending verification in the dataset registry.",
  "- Replicate-weight SEs (te_se.csv) condition on point-estimate tax parameters and imputation",
  "  shares: they capture sampling variability only. The dominant uncertainty is the scenario range",
  "  (r = 3.5% vs 6%; DB accrued vs actual cash; withdrawal-MTR scale; Roth dollar scale), published",
  "  as sensitivity rows in te_benchmarks.csv.",
  "- All dollar values are nominal CY2024."
), file.path(path_tables, "te_notes.txt"))

## =============================================================================
## Review
## =============================================================================
cat("\n================ 10 TAX EXPENDITURE: HEADLINE (observed flows, PV method) ================\n")
cat(sprintf("Income-tax TE:  $%.1fB   Payroll-tax TE: $%.1fB   Combined: $%.1fB\n",
            totals["income_te_B"], totals["payroll_te_B"], totals["total_te_B"]))
cat(sprintf("  (r = 6%% sensitivity income-tax TE: $%.1fB;  JCT-style cash-flow: $%.1fB)\n",
            totals["income_te6_B"], totals["cashflow_te_B"]))
cat(sprintf("Observed contributions ($B): employee %.1f + employer DC/IRA %.1f\n",
            totals["contrib_ee_B"], totals["contrib_er_B"]))
cat("\nDecile table (shares of each leg):\n")
print(within(decile_tab, { income_share <- round(income_share,3); payroll_share <- round(payroll_share,3)
                           total_share <- round(total_share,3); pct_benefiting <- round(pct_benefiting,3)
                           income_te_B <- round(income_te_B,2); payroll_te_B <- round(payroll_te_B,2)
                           total_te_B <- round(total_te_B,2); mean_te_benef <- round(mean_te_benef) }),
      row.names = FALSE)
if (HAS_S5) {
  cat("\n================ 10 CBO-COMPARABLE HEADLINE RANGE (Stage-5 imputations ON) ================\n")
  cat(sprintf("CBO parameters (r=3.5%%):        income $%.1fB + payroll $%.1fB = $%.1fB\n",
              totB_inc, totB_pay, totB_inc + totB_pay))
  cat(sprintf("2024 rate environment (r=6%%):   income $%.1fB + payroll $%.1fB = $%.1fB\n",
              totB6_inc, totB_pay, totB6_inc + totB_pay))
  cat(sprintf("  [CBO CY2019 benchmark: $202B income + $74B payroll = $276B; the r=3.5%% row is the\n   methodology-faithful replication, the r=6%% row the rate-environment-consistent estimate]\n"))
  cat(sprintf("  DB actual-cash sensitivity: income $%.1fB, payroll $%.1fB\n",
              sum(te_income_B_dbactual * W, na.rm = TRUE)/1e9,
              sum(te_payroll_B_dbactual * W, na.rm = TRUE)/1e9))
  cat(sprintf("  Withdrawal-MTR sensitivity (income leg; CBO base mr = m0): 0.9x $%.1fB | 0.8x $%.1fB | 0.7x $%.1fB\n",
              te_inc_B_mr[1], te_inc_B_mr[2], te_inc_B_mr[3]))
  cat(sprintf("  Income-leg decomposition: upfront $%.1fB + PV future $%.1fB = $%.1fB\n",
              upfront_B, future_B, upfront_B + future_B))
  ## Top-tail diagnostic, COMPUTED. Both figures were previously hardcoded ("22.3%", "~$491k")
  ## and the MTR had gone stale by 1.4pp across the RY2024 + PolicyEngine refreshes
  ## (external RA review, 2026-08-27).
  .tt   <- pp$c_all > 0 & !is.na(pp$frate_base)
  .mtr  <- sum((pp$frate_base * pp$c_all * W)[.tt]) / sum((pp$c_all * W)[.tt])
  .p99w <- wtd_quantile(units$pwages + units$swages, units$wgt_unit, 0.99)
  cat(sprintf(paste0("  NOTE (top-tail clarification, not corrected here): SIPP's compressed top income tail\n",
                     "   (contribution-weighted MTR %.1f%%; unit p99 wages ~$%.0fk) biases all rows DOWNWARD\n",
                     "  relative to tax-return-based estimates; a SOI top-tail calibration is a documented\n",
                     "  future extension, deliberately not applied (user decision 2026-07-11).\n"),
              .mtr, .p99w / 1000))
  cat("\nCBO-comparable decile table:\n")
  print(within(decile_tab_cbo, { income_share <- round(income_share,3)
        payroll_share <- round(payroll_share,3); total_share <- round(total_share,3)
        pct_benefiting <- round(pct_benefiting,3); income_te_B <- round(income_te_B,2)
        payroll_te_B <- round(payroll_te_B,2); total_te_B <- round(total_te_B,2)
        mean_te_benef <- round(mean_te_benef) }), row.names = FALSE)
}
cat("\nQuintile shares vs CBO 2019:\n"); print(round(quint, 3), row.names = FALSE)
cat("\nWrote te_decile.csv, te_quintile_cbo_compare.csv, te_benchmarks.csv",
    if (HAS_S5) ", te_decile_cbo_comparable.csv" else "", "\n")
