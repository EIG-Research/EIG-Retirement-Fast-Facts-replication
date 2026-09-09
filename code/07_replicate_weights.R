# code/07_replicate_weights.R
# Proper standard errors via the SIPP 2025 replicate weights (Fay's method).
# Builds a December replicate-weight extract from rw2025.dta, merges to the analysis frame, declares a
# Fay replicate design, and reports SEs + 95% CIs for the headline and pillars.

suppressWarnings(suppressMessages({ library(haven); library(arrow); library(dplyr); library(survey) }))
path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
path_raw <- file.path(path_project, "data", "raw")
path_processed <- file.path(path_project, "data", "processed")
path_tables <- file.path(path_project, "output", "tables")

## SIPP uses Fay's modified BRR with a perturbation factor k = 0.5, giving Var = {1/(240*0.5^2)} *
## sum_{i=1}^{240}(theta_i - theta_0)^2 = (4/240) * sum(...). CONFIRMED against the 2025 SIPP Users'
## Guide sec 7.2.3 (Replicate Weights, p. 153), which prescribes svrepdesign(..., type="Fay", rho=0.5). Publishable.
FAY_RHO <- 0.5

## --- Build/refresh the December replicate-weight extract from the raw rw .dta -
## Filenames come from params.R (single vintage source), so a new SIPP release is drop-in.
rw_dta <- file.path(path_raw, RW_DTA_FILE)
rw_pq  <- file.path(path_processed, RW_EXTRACT_PARQUET)
if (isTRUE(REBUILD_EXTRACT) || !file.exists(rw_pq)) {
  if (!file.exists(rw_dta)) stop(RW_DTA_FILE, " not found in data/raw/; unzip the matching rw*.zip.")
  message("I4: reading replicate weights from ", RW_DTA_FILE, " ...")
  rw <- read_dta(rw_dta)
  rw <- haven::zap_labels(rw)               # strip Stata value labels (parity with 01 Stage 1)
  names(rw) <- toupper(names(rw))
  rw <- rw[!is.na(rw$MONTHCODE) & rw$MONTHCODE == REF_MONTH, c("SSUID","PNUM","MONTHCODE","SPANEL","SWAVE",
                                        paste0("REPWGT", 0:240))]
  write_parquet(as.data.frame(rw), rw_pq)
  message(sprintf("I4: wrote %s (%d Dec person rows x %d cols)", basename(rw_pq), nrow(rw), ncol(rw)))
}
rw <- as.data.frame(read_parquet(rw_pq))

## --- Merge to the analysis frame --------------------------------------------
d <- as.data.frame(read_parquet(file.path(path_processed, "sipp_fastfacts.parquet")))
n0 <- nrow(d)
d <- inner_join(d, rw, by = c("SSUID","PNUM","MONTHCODE","SPANEL","SWAVE"))
drop <- n0 - nrow(d)
if (drop > 0) message(sprintf("I4: %d of %d analysis rows (%.2f%%) lacked a replicate-weight record (rw file covers slightly fewer persons); proceeding on matched.",
                              drop, n0, 100 * drop / n0))
stopifnot(nrow(d) / n0 > 0.99)                            # require >99% of analysis rows matched

## --- Validate: REPWGT0 must equal the final person weight -------------------
stopifnot(max(abs(d$REPWGT0 - d$WPFINWGT)) < 1e-3)        # REPWGT0 == WPFINWGT (dictionary)

## --- Declare the Fay replicate design ---------------------------------------
repcols <- paste0("REPWGT", 1:240)
for (v in c("lacks_access","participates","match_receipt")) d[[v]] <- as.integer(d[[v]])
## P1 numerators/denominators as integer columns for svyratio() (conditional shares are ratio
## statistics under the replicate design; decision of 2026-08-27):
d$p1_num_obs <- as.integer(d$participates == 1L & d$access_emp_obs)
d$p1_den_obs <- as.integer(d$access_emp_obs)
d$p1_num_all <- as.integer(d$participates == 1L & d$access_emp)
d$p1_den_all <- as.integer(d$access_emp)
des <- svrepdesign(data = d, weights = ~WPFINWGT, repweights = d[, repcols],
                   type = "Fay", rho = FAY_RHO, combined.weights = TRUE)

## --- Estimates + SEs + 95% CIs ----------------------------------------------
grab <- function(fmla, label, group = NULL) {
  if (is.null(group)) {
    m <- svymean(fmla, des); ci <- confint(m)
    data.frame(measure = label, group = "All workers", estimate = as.numeric(m),
               se = as.numeric(SE(m)), ci_low = ci[1], ci_high = ci[2])
  } else {
    b <- svyby(fmla, as.formula(paste0("~", group)), des, svymean, vartype = c("se","ci"))
    data.frame(measure = label, group = as.character(b[[group]]), estimate = b[[2]],
               se = b$se, ci_low = b$ci_l, ci_high = b$ci_u)
  }
}
## Conditional shares (P1) are RATIOS of weighted totals under the replicate design.
grab_ratio <- function(num, den, label) {
  r  <- svyratio(as.formula(paste0("~", num)), as.formula(paste0("~", den)), des)
  ci <- confint(r)
  data.frame(measure = label, group = "All workers", estimate = as.numeric(coef(r)),
             se = as.numeric(SE(r)), ci_low = ci[1], ci_high = ci[2])
}
res <- rbind(
  grab(~lacks_access, "H1 lacks employer access"),
  grab(~lacks_access, "H1 by class", group = "worker_class"),
  grab(~participates, "P2 participation (all workers)"),
  grab(~match_receipt, "M1 employer-match receipt (all workers)"),
  grab_ratio("p1_num_obs", "p1_den_obs", "P1 participation | access (plan type observed)"),
  grab_ratio("p1_num_all", "p1_den_all", "P1 participation | access (incl. type-unobserved; conservative floor)")
)
res[, c("estimate","se","ci_low","ci_high")] <- round(res[, c("estimate","se","ci_low","ci_high")], 4)
write.csv(res, file.path(path_tables, "standard_errors.csv"), row.names = FALSE)

cat("\n=== REPLICATE-WEIGHT STANDARD ERRORS (Fay, rho=", FAY_RHO, "; 240 replicates) ===\n", sep = "")
print(res, row.names = FALSE)
cat("\nREPWGT0 == WPFINWGT validated; ", nrow(d), " workers; 240 replicate weights.\n", sep = "")
cat("Fay rho = 0.5 confirmed vs 2025 SIPP Users' Guide sec 7.2.3 (k=0.5; Var = 4/240 * sum of squares).\n")

## ===========================================================================
## Breakout SEs for every published figure/table share (2026-09-08).
## Direct Fay arithmetic on the replicate-weight matrix (fast; avoids svyby over
## dozens of subsets): each published share is a ratio of weighted totals,
## theta = sum(w*num)/sum(w*den); Var = (4/240) * sum_r (theta_r - theta_0)^2.
## ===========================================================================
Wrep <- as.matrix(d[, repcols])
w0   <- d$WPFINWGT
fay_stats <- function(num, den) {          # num, den: logical; num assumed subset of den
  num <- num & den
  t0 <- sum(w0[num]) / sum(w0[den])
  tr <- as.numeric(crossprod(Wrep, num)) / as.numeric(crossprod(Wrep, den))
  se <- sqrt((4 / 240) * sum((tr - t0)^2))
  list(est = t0, se = se, reps = tr, n_num = sum(num), n_den = sum(den))
}
row_of <- function(measure, dimension, group, fs)
  data.frame(measure = measure, dimension = dimension, group = group,
             estimate = fs$est, se = fs$se,
             ci_low = fs$est - 1.96 * fs$se, ci_high = fs$est + 1.96 * fs$se,
             n_num = fs$n_num, n_den = fs$n_den, stringsAsFactors = FALSE)

## Saver's Match flags (03 runs before 07 in the pipeline order)
smf <- as.data.frame(read_parquet(file.path(path_processed, "sipp_savers_match.parquet")))
d <- dplyr::left_join(d, smf[, c("SSUID", "PNUM", "sm_elig_any", "sm_wedge")], by = c("SSUID", "PNUM"))

priv <- d$worker_class == "Private"
empl <- d$worker_class %in% c("Private", "Government")
gov  <- d$worker_class == "Government"
alltrue <- rep(TRUE, nrow(d))
no_match <- d$match_receipt == 0L

bres <- list()
for (k in 1:10) {
  dk <- !is.na(d$earn_decile) & d$earn_decile == k
  bres[[length(bres)+1]] <- row_of("Lacks access (private)",            "earn_decile", k, fay_stats(d$lacks_access == 1L,  priv & dk))
  bres[[length(bres)+1]] <- row_of("Participates (private)",            "earn_decile", k, fay_stats(d$participates == 1L,  priv & dk))
  bres[[length(bres)+1]] <- row_of("Employer contribution (private)",   "earn_decile", k, fay_stats(d$match_receipt == 1L, priv & dk))
  bres[[length(bres)+1]] <- row_of("Participates | no employer contribution (private)", "earn_decile", k,
                                   fay_stats(d$participates == 1L, priv & dk & no_match))
}
half_bot <- !is.na(d$earn_decile) & d$earn_decile <= 5
half_top <- !is.na(d$earn_decile) & d$earn_decile >= 6
bres[[length(bres)+1]] <- row_of("Participates | no employer contribution (private)", "earn_half", "Bottom half", fay_stats(d$participates == 1L, priv & half_bot & no_match))
bres[[length(bres)+1]] <- row_of("Participates | no employer contribution (private)", "earn_half", "Top half",    fay_stats(d$participates == 1L, priv & half_top & no_match))
for (g in unique(na.omit(d$race_eth)))
  bres[[length(bres)+1]] <- row_of("Employer contribution (employees)", "race_eth", g, fay_stats(d$match_receipt == 1L, empl & d$race_eth %in% g))
for (g in unique(na.omit(d$educ_grp)))
  bres[[length(bres)+1]] <- row_of("Employer contribution (employees)", "educ_grp", g, fay_stats(d$match_receipt == 1L, empl & d$educ_grp %in% g))
for (r in c("White (NH)", "Black (NH)", "Hispanic", "Asian (NH)")) {
  for (g in unique(na.omit(d$educ_grp)))
    bres[[length(bres)+1]] <- row_of("Lacks access (private)", paste0("race_educ: ", r), g,
                                     fay_stats(d$lacks_access == 1L, priv & d$race_eth %in% r & d$educ_grp %in% g))
  bres[[length(bres)+1]] <- row_of("Lacks access (private)", "race_eth", r, fay_stats(d$lacks_access == 1L, priv & d$race_eth %in% r))
}
## Government detail (draft footnote 3)
for (g in c("Federal", "State", "Local"))
  bres[[length(bres)+1]] <- row_of("Lacks access (government)", "gov_subclass", g, fay_stats(d$lacks_access == 1L, gov & d$gov_subclass %in% g))
for (g in c("Full-time", "Part-time"))
  bres[[length(bres)+1]] <- row_of("Lacks access (government)", "ft_pt", g, fay_stats(d$lacks_access == 1L, gov & d$ft_pt %in% g))
bres[[length(bres)+1]] <- row_of("Lacks access (government)", "age_band", "18-24", fay_stats(d$lacks_access == 1L, gov & d$age_band %in% "18-24"))
bres[[length(bres)+1]] <- row_of("Lacks access (government)", "educ_grp", "Less than HS", fay_stats(d$lacks_access == 1L, gov & d$educ_grp %in% "Less than HS"))
## Saver's Match shares
elig_ok <- d$sm_elig_any %in% TRUE
bres[[length(bres)+1]] <- row_of("SM income-eligible (share of workers)", "Overall", "All workers", fay_stats(elig_ok, alltrue))
bres[[length(bres)+1]] <- row_of("SM wedge (share of eligible)",          "Overall", "Eligible",    fay_stats(d$sm_wedge %in% TRUE, elig_ok))
bres <- do.call(rbind, bres)
bres[, c("estimate","se","ci_low","ci_high")] <- round(bres[, c("estimate","se","ci_low","ci_high")], 4)
write.csv(bres, file.path(path_tables, "se_breakouts.csv"), row.names = FALSE)

## The 27x-style ratio: decile-10 / decile-1 participation among the unmatched, with a
## Fay CI computed on the ratio-of-ratios directly (each replicate re-forms the full ratio).
f1  <- fay_stats(d$participates == 1L, priv & !is.na(d$earn_decile) & d$earn_decile == 1  & no_match)
f10 <- fay_stats(d$participates == 1L, priv & !is.na(d$earn_decile) & d$earn_decile == 10 & no_match)
r0  <- f10$est / f1$est
rr  <- f10$reps / f1$reps
r_se <- sqrt((4 / 240) * sum((rr - r0)^2))
ratio_row <- data.frame(measure = "Decile-10 / decile-1 participation ratio | no employer contribution (private)",
                        dimension = "earn_decile", group = "10 vs 1",
                        estimate = round(r0, 2), se = round(r_se, 2),
                        ci_low = round(r0 - 1.96 * r_se, 2), ci_high = round(r0 + 1.96 * r_se, 2),
                        n_num = f10$n_num, n_den = f1$n_num, stringsAsFactors = FALSE)
write.csv(rbind(bres, ratio_row), file.path(path_tables, "se_breakouts.csv"), row.names = FALSE)
cat(sprintf("\n27x-claim uncertainty: decile-10/decile-1 ratio = %.1f (SE %.1f; 95%% CI %.1f-%.1f); unweighted participants n=%d (decile 1) and n=%d (decile 10).\n",
            r0, r_se, r0 - 1.96 * r_se, r0 + 1.96 * r_se, f1$n_num, f10$n_num))
cat("Wrote se_breakouts.csv (replicate SEs for every published breakout share).\n")

## ===========================================================================
## Pooled-panel consistency diagnostic: H1/P2/M1 by SPANEL, with SEs.
## If the by-panel spread sits within sampling error, the pooled headline is not
## an attrition/time-in-sample artifact; the draft methods note can say so.
## ===========================================================================
pres <- list()
for (pn in sort(unique(d$SPANEL))) {
  inp <- d$SPANEL == pn
  pres[[length(pres)+1]] <- row_of("H1 lacks employer access", "SPANEL", pn, fay_stats(d$lacks_access == 1L,  inp))
  pres[[length(pres)+1]] <- row_of("P2 participation",         "SPANEL", pn, fay_stats(d$participates == 1L,  inp))
  pres[[length(pres)+1]] <- row_of("M1 employer contribution", "SPANEL", pn, fay_stats(d$match_receipt == 1L, inp))
}
pres <- do.call(rbind, pres)
pres[, c("estimate","se","ci_low","ci_high")] <- round(pres[, c("estimate","se","ci_low","ci_high")], 4)
write.csv(pres, file.path(path_tables, "pillars_by_panel.csv"), row.names = FALSE)
h1sp <- pres[pres$measure == "H1 lacks employer access", ]
cat(sprintf("Cross-panel H1 spread: %.1f%%-%.1f%% across panels %s (pooled %.1f%%); see pillars_by_panel.csv.\n",
            100 * min(h1sp$estimate), 100 * max(h1sp$estimate),
            paste(sort(unique(d$SPANEL)), collapse = "/"),
            100 * res$estimate[res$measure == "H1 lacks employer access" & res$group == "All workers"]))
