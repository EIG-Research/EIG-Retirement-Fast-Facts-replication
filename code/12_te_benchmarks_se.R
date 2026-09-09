# code/12_te_benchmarks_se.R
# TE Stage 6: (a) external calibration of SIPP contribution aggregates against verified
# administrative benchmarks; (b) replicate-weight standard errors (SIPP Fay's method,
# rho = 0.5, 240 replicates - confirmed vs 2025 SIPP Users' Guide sec 7.2.3 in 07) for the
# headline tax-expenditure totals and the CBO-comparable decile shares.
# Inputs : data/processed/te_person_results.parquet, rw2025_extract.parquet (built by 07)
# Outputs: output/tables/te_calibration.csv, output/tables/te_se.csv
#
# SE design note: TE values are held FIXED at their point-estimate tax parameters (TAXSIM is
# not re-run per replicate); the SEs capture sampling variability of the weighted aggregation
# only, not tax-model or imputation uncertainty. This is the standard approach for survey
# statistics built on modeled person-level values; stated in the table notes.

suppressWarnings(suppressMessages({
  library(arrow); library(dplyr)
}))

path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
path_processed <- file.path(path_project, "data", "processed")
path_tables    <- file.path(path_project, "output", "tables")

pp <- as.data.frame(read_parquet(file.path(path_processed, "te_person_results.parquet")))
z <- function(x) ifelse(is.na(x), 0, x)
W <- pp$WPFINWGT
tot <- function(x) sum(z(x) * W)

## =============================================================================
## (a) Calibration: SIPP weighted aggregates vs verified external benchmarks
## Benchmarks verified against primary files on 2026-07-11; NIPA aggregates refreshed to
## CY2024 on 2026-07-15 (BEA 2025 annual update, via the DBnomics BEA mirror the prior pass
## used; every 2023 value reproduced exactly, confirming the same series). NIPA DB employee
## contributions 2024: private 0.525 + federal 7.752 + state/local 76.447 = 84.724 $B
## (FRED Y241RC/Y276RC/S251201). IRS SOI SEP 16.596 + SIMPLE 12.802 = 29.398 $B
## (23in01ira.xlsx) is TY2023 - the LATEST published; IRS SOI TY2024 is not yet released
## (SOI lags ~2 yrs), so this benchmark lags the SIPP reference year by one (flagged).
## IRS W-2 TY2020 context (no calibration row uses it): all elective deferrals 379.244 $B
## (20in04w2all.xlsx Table 4.D; "all elective deferrals" includes 403(b)/457, hence a larger
## denominator than the $310.162B 401(k)-only base behind the Roth dollar scale).
## =============================================================================
## DB benchmark DERIVED from the same file that drives the stage-5 imputation, so the "equal by
## construction" row stays structurally true across vintage bumps (a hand-entered
## 354.2 here was the CY2024 military-INCLUSIVE total while the imputation input was CY2023 -
## ratio 0.96 under a true-by-construction label). Federal scope is CIVILIAN (CSRS/FERS) per the
## civilian-labor-force frame; see stage5_db_employer.csv row notes.
db_bench <- read.csv(file.path(path_project, "data", "raw", "external_benchmarks",
                               "stage5_db_employer.csv"), stringsAsFactors = FALSE)
stopifnot(all(db_bench$year == REF_YEAR))
db_accrued_busd <- round(sum(db_bench$value_busd[db_bench$concept == "accrued"]), 1)

calib <- data.frame(
  component = c("401(k)/403(b)/TSP employee contributions (incl. Roth-designated)",
                "DB/cash-balance employee contributions",
                "Employer-provided IRA (SEP/SIMPLE), employee + employer",
                "Employer DC/IRA contributions",
                "Imputed employer DB contributions (accrued, by construction)"),
  sipp_busd = round(c(tot(pp$c_ee_401),
                      tot(pp$c_ee_pen),
                      tot(pp$c_ee_ira) + tot(pp$c_er_ira),
                      tot(pp$c_er),
                      tot(pp$c_er_db_imp)) / 1e9, 1),
  benchmark_busd = c(607.4, 84.7, 29.4, 309.6, db_accrued_busd),
  benchmark = c("BEA NIPA 2024 DC household contributions $607.4B (T7.25 Y344RC, all sectors; BEA 2025 annual update; NIPA includes some plan types SIPP's 401(k) item may not)",
                "BEA NIPA 2024 DB employee contributions: $84.7B (0.5 pvt Y241RC + 7.8 fed Y276RC + 76.4 s&l S25120; BEA 2025 annual update; federal household DB contributions are entirely civilian - military line Y278RC = 0)",
                "IRS SOI TY2023 SEP+SIMPLE contributions: $29.4B (incl. employer share; TY2023 is the LATEST published, TY2024 not yet released, so this benchmark lags the SIPP year by one)",
                "BEA NIPA 2024 DC employer contributions $309.6B (T7.25 Y340RC; BEA 2025 annual update; SIPP row also includes SEP/SIMPLE employer dollars NIPA books elsewhere)",
                paste0("BEA NIPA ", REF_YEAR, " employer DB normal cost, federal CIVILIAN scope: $",
                       db_accrued_busd, "B (actual+imputed employer contribs, NIPA T7.22 / T7.23",
                       " civilian lines / T7.24; the same file drives the stage-5 imputation, so",
                       " equal by construction)")),
  stringsAsFactors = FALSE)
calib$ratio <- round(calib$sipp_busd / calib$benchmark_busd, 2)
write.csv(calib, file.path(path_tables, "te_calibration.csv"), row.names = FALSE)

## =============================================================================
## (b) Replicate-weight SEs (Fay, rho = 0.5): Var = (4/240) * sum_r (theta_r - theta_0)^2
## =============================================================================
rw_pq <- file.path(path_processed, RW_EXTRACT_PARQUET)
if (!file.exists(rw_pq)) stop("12: ", RW_EXTRACT_PARQUET, " not found - run 07_replicate_weights.R first.")
rw <- as.data.frame(read_parquet(rw_pq))
## Note: this join keys on SSUID+PNUM only (07 keys on five columns). Assert the December
## replicate extract is unique on that pair across the pooled 2022-2025 panels, so the inner
## join cannot fan out and silently duplicate TE dollars.
stopifnot(anyDuplicated(rw[, c("SSUID", "PNUM")]) == 0,
          anyDuplicated(pp[, c("SSUID", "PNUM")]) == 0)
d <- inner_join(pp, rw[, c("SSUID","PNUM", paste0("REPWGT", 0:240))], by = c("SSUID","PNUM"))
stopifnot(nrow(d) <= nrow(pp))
match_rate <- nrow(d) / nrow(pp)
message(sprintf("12: %.2f%% of persons matched to replicate weights.", 100 * match_rate))
stopifnot(match_rate > 0.99)
stopifnot(max(abs(d$REPWGT0 - d$WPFINWGT)) < 1e-3)

RW <- as.matrix(d[, paste0("REPWGT", 1:240)])              # n x 240
fay_se <- function(theta_r, theta_0) sqrt((4 / 240) * sum((theta_r - theta_0)^2))

## totals ($B) and their SEs for the headline quantities
se_total <- function(x) {
  x <- z(x); t0 <- sum(x * d$WPFINWGT) / 1e9
  tr <- as.numeric(crossprod(x, RW)) / 1e9                  # 240 replicate totals
  c(estimate = t0, se = fay_se(tr, t0))
}
rows <- list(
  c(name = "Income-tax TE, observed flows",        se_total(d$te_income)),
  c(name = "Payroll-tax TE, observed flows",       se_total(d$te_payroll)),
  c(name = "Combined TE, observed flows",          se_total(d$te_total)),
  c(name = "Income-tax TE, CBO-comparable",        se_total(d$te_income_B)),
  c(name = "Payroll-tax TE, CBO-comparable",       se_total(d$te_payroll_B)),
  c(name = "Combined TE, CBO-comparable",          se_total(d$te_total_B)),
  c(name = "Income-tax TE, CBO-comparable r=6% (2024 rate environment)", se_total(d$te_income_B6)),
  c(name = "Combined TE, CBO-comparable r=6% (2024 rate environment)",   se_total(d$te_total_B6))
)
se_tab <- do.call(rbind, lapply(rows, function(r)
  data.frame(quantity = r["name"], estimate_busd = as.numeric(r["estimate"]),
             se_busd = as.numeric(r["se"]))))
se_tab$ci95_lo <- se_tab$estimate_busd - 1.96 * se_tab$se_busd
se_tab$ci95_hi <- se_tab$estimate_busd + 1.96 * se_tab$se_busd

## decile shares of the CBO-comparable combined TE, with SEs (ratio statistic per replicate)
x <- z(d$te_total_B)
t0_dec <- tapply(x * d$WPFINWGT, d$decile, sum); s0 <- t0_dec / sum(t0_dec)
dec_mat <- matrix(0, 10, 240)
for (k in 1:10) dec_mat[k, ] <- as.numeric(crossprod(x * (d$decile == k), RW))
share_r <- sweep(dec_mat, 2, colSums(dec_mat), "/")         # 10 x 240 replicate shares
dec_se <- vapply(1:10, function(k) fay_se(share_r[k, ], s0[k]), numeric(1))
dec_tab <- data.frame(quantity = paste0("Decile ", 1:10, " share of combined TE (CBO-comparable)"),
                      estimate_busd = as.numeric(s0), se_busd = dec_se,
                      ci95_lo = as.numeric(s0) - 1.96 * dec_se,
                      ci95_hi = as.numeric(s0) + 1.96 * dec_se)

## =============================================================================
## (c) Pillar cross-cut SEs: the draft's bolded
## claims - group means, TE-dollar shares, and share-benefiting by access and
## match status - as Fay replicate ratio statistics on the WORKER frame.
## =============================================================================
ff <- as.data.frame(read_parquet(file.path(path_processed, "sipp_fastfacts.parquet")))
dw <- inner_join(d, ff[, c("SSUID","PNUM","lacks_access","match_receipt")], by = c("SSUID","PNUM"))
RWw <- as.matrix(dw[, paste0("REPWGT", 1:240)])
xw  <- z(dw$te_total_B)

## replicate-safe helpers: ratio of two weighted totals across the 240 replicates
ratio_se <- function(num_vec, den_vec) {
  t0 <- sum(num_vec * dw$WPFINWGT) / sum(den_vec * dw$WPFINWGT)
  tr <- as.numeric(crossprod(num_vec, RWw)) / as.numeric(crossprod(den_vec, RWw))
  c(estimate = t0, se = fay_se(tr, t0))
}
grp_rows <- function(flag, label) {
  g1 <- as.numeric(flag); g0 <- as.numeric(!flag)
  rbind(
    data.frame(quantity = paste0("Mean TE per worker, ", label), t(ratio_se(xw * g1, g1))),
    data.frame(quantity = paste0("Mean TE per worker, NOT ", label), t(ratio_se(xw * g0, g0))),
    data.frame(quantity = paste0("Share of worker TE dollars, ", label),
               t(ratio_se(xw * g1, xw))),
    data.frame(quantity = paste0("Share benefiting (TE>0), ", label),
               t(ratio_se(as.numeric(xw > 0) * g1, g1))),
    data.frame(quantity = paste0("Share benefiting (TE>0), NOT ", label),
               t(ratio_se(as.numeric(xw > 0) * g0, g0))))
}
cross_tab <- rbind(grp_rows(!dw$lacks_access, "has employer access"),
                   grp_rows(dw$match_receipt, "receives employer match"))
names(cross_tab) <- c("quantity","estimate_busd","se_busd")   # column names reused; units are $ or shares
cross_tab$ci95_lo <- cross_tab$estimate_busd - 1.96 * cross_tab$se_busd
cross_tab$ci95_hi <- cross_tab$estimate_busd + 1.96 * cross_tab$se_busd

se_out <- rbind(se_tab, dec_tab, cross_tab)
num_cols <- c("estimate_busd","se_busd","ci95_lo","ci95_hi")
se_out[num_cols] <- lapply(se_out[num_cols], function(v) round(v, 4))
write.csv(se_out, file.path(path_tables, "te_se.csv"), row.names = FALSE)

## =============================================================================
## (d) Summary statistics for key analysis variables
## =============================================================================
wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])
wsd   <- function(x, w) { m <- wmean(x, w); sqrt(wmean((x - m)^2, w)) }
## Weighted quantile (type-1 inverse CDF) for the percentile columns: dollar variables
## here are heavily right-skewed, so mean/SD alone hides the mean-vs-median gap and extremes.
wq <- function(x, w, p) {
  ok <- !is.na(x); x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(p, function(pp_) x[which(cw >= pp_)[1]], numeric(1))
}
sumrow <- function(df, v, label, w = df$WPFINWGT) {
  x <- df[[v]]
  q <- wq(x, w, c(0.10, 0.50, 0.90, 0.99))
  data.frame(variable = label, n = sum(!is.na(x)), missing_share = round(mean(is.na(x)), 4),
             wtd_mean = round(wmean(x, w), 2), wtd_sd = round(wsd(x, w), 2),
             wtd_p10 = round(q[1], 2), wtd_p50 = round(q[2], 2),
             wtd_p90 = round(q[3], 2), wtd_p99 = round(q[4], 2),
             units = "nominal CY2024 dollars unless 0/1 share")
}
ffn <- ff |> mutate(lacks_access_n = as.numeric(lacks_access),
                    participates_n = as.numeric(participates),
                    match_receipt_n = as.numeric(match_receipt))
sumstats <- rbind(
  sumrow(ffn, "lacks_access_n",  "Lacks employer access (0/1, workers)"),
  sumrow(ffn, "participates_n",  "Participates (0/1, workers)"),
  sumrow(ffn, "match_receipt_n", "Receives employer match (0/1, workers)"),
  sumrow(ffn, "earn_annual",     "Annual earnings ($, workers)"),
  ## Restrict to workers who report RECEIVING a match, matching matching_dollars.csv's
  ## universe (match_receipt & non-NA amount); the old row averaged all reporters incl. non-matched.
  sumrow(ffn[ffn$match_receipt, , drop = FALSE], "emp_contrib_amt", "Employer contribution ($, matched workers)"),
  sumrow(pp,  "c_ee",            "Employee retirement contributions ($, all persons)"),
  sumrow(pp,  "c_er",            "Employer DC/IRA contributions ($, all persons)"),
  sumrow(pp,  "te_total_B",      "Combined TE, CBO-comparable ($, all persons)"),
  sumrow(pp,  "te_total_B6",     "Combined TE at r=6% ($, all persons)"))
write.csv(sumstats, file.path(path_tables, "summary_stats.csv"), row.names = FALSE)

## =============================================================================
## Review
## =============================================================================
cat("\n================ 12 CALIBRATION (SIPP vs external) ================\n")
print(calib[, c("component","sipp_busd","benchmark_busd","ratio")], row.names = FALSE)
cat("\n================ 12 REPLICATE-WEIGHT SEs (Fay rho=0.5, 240 reps) ================\n")
print(within(se_out, { estimate_busd <- round(estimate_busd, 2); se_busd <- round(se_busd, 2)
                       ci95_lo <- round(ci95_lo, 2); ci95_hi <- round(ci95_hi, 2) }),
      row.names = FALSE)
cat("\nNote: SEs condition on point-estimate tax parameters and imputation shares;",
    "\nthey capture sampling variability only.\n")
cat("\n================ 12 SUMMARY STATISTICS ================\n")
print(sumstats, row.names = FALSE)
cat("\nWrote te_calibration.csv, te_se.csv, summary_stats.csv\n")
