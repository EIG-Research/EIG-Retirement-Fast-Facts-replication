# code/14_te_crosscuts.R
# TE cross-cuts: join the person-level retirement tax expenditure (10) to the Fast Facts
# worker frame (01) and tabulate the TE across the brief's three pillars (access,
# participation, matching) and its demographic/socioeconomic breakdowns.
# Inputs : data/processed/te_person_results.parquet, sipp_fastfacts.parquet
# Outputs: output/tables/te_crosscuts.csv (one row per group x measure)
#          output/tables/te_crosscut_highlights.csv (the punchline ratios)
#
# TE measure: the CBO-comparable row (te_total_B; income te_income_B + payroll te_payroll_B,
# r = 3.5%), with the r = 6% co-headline (te_total_B6) carried alongside. Includes the
# Stage-5 imputed components (employer DB accrual, Roth split, outside IRA), so DB-covered
# government workers carry their imputed employer-DB benefit.
# Universe note: the TE is estimated for ALL December persons; this script tabulates the
# subset in the Fast Facts working population (ages 18-64, employed, December job). The
# coverage share of total TE dollars captured by that frame is reported.

suppressWarnings(suppressMessages({
  library(arrow); library(dplyr)
}))

path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
source(file.path(path_project, "code", "_shared", "helpers.R"))
path_processed <- file.path(path_project, "data", "processed")
path_tables    <- file.path(path_project, "output", "tables")

te <- as.data.frame(read_parquet(file.path(path_processed, "te_person_results.parquet")))
ff <- as.data.frame(read_parquet(file.path(path_processed, "sipp_fastfacts.parquet")))
stopifnot(all(c("te_total_B","te_income_B","te_payroll_B","te_total_B6") %in% names(te)))

z <- function(x) ifelse(is.na(x), 0, x)
te_total_all <- sum(z(te$te_total_B) * te$WPFINWGT)          # all December persons

d <- inner_join(ff,
                te |> select(SSUID, PNUM, decile_hh = decile, te_income_B, te_payroll_B,
                             te_total_B, te_total_B6, c_ee, c_er),
                by = c("SSUID","PNUM"))
stopifnot(nrow(d) / nrow(ff) > 0.99)                          # worker frame must match through
for (v in c("te_income_B","te_payroll_B","te_total_B","te_total_B6")) d[[v]] <- z(d[[v]])
W <- d$WPFINWGT
te_total_workers <- sum(d$te_total_B * W)
coverage <- te_total_workers / te_total_all

## --- Group tabulator ----------------------------------------------------------
## For each level of `var`: weighted population share, share of worker TE dollars,
## mean TE per worker, % with any benefit, mean among beneficiaries, mean at r=6%.
grp_tab <- function(var, label = var) {
  g <- d[[var]]
  lv <- sort(unique(g[!is.na(g)]))
  do.call(rbind, lapply(lv, function(l) {
    s <- !is.na(g) & g == l
    ben <- s & d$te_total_B > 0
    data.frame(dimension = label, group = as.character(l),
               pop_share      = sum(W[s]) / sum(W),
               te_share       = sum(d$te_total_B[s] * W[s]) / te_total_workers,
               mean_te        = sum(d$te_total_B[s] * W[s]) / sum(W[s]),
               pct_benefiting = sum(W[ben]) / sum(W[s]),
               mean_te_benef  = if (sum(W[ben]) > 0) sum(d$te_total_B[ben] * W[ben]) / sum(W[ben]) else 0,
               mean_te_r6     = sum(d$te_total_B6[s] * W[s]) / sum(W[s]))
  }))
}

d$access_status <- ifelse(d$lacks_access, "Lacks employer access", "Has employer access")
d$partic_status <- ifelse(d$participates, "Participates", "Does not participate")
d$match_status  <- ifelse(d$match_receipt, "Receives employer match", "No employer match")

cc <- rbind(
  grp_tab("access_status", "Pillar 1: access"),
  grp_tab("partic_status", "Pillar 2: participation"),
  grp_tab("match_status",  "Pillar 3: matching"),
  grp_tab("worker_class",  "Worker class"),
  grp_tab("ft_pt",         "Full-/part-time"),
  grp_tab("sex",           "Sex"),
  grp_tab("race_eth",      "Race/ethnicity"),
  grp_tab("educ_grp",      "Education"),
  grp_tab("age_band",      "Age band"),
  grp_tab("disability",    "Disability"),
  grp_tab("earn_decile",   "Worker earnings decile"),
  grp_tab("decile_hh",     "Household income decile (CBO-style)")
)
num <- c("pop_share","te_share","mean_te","pct_benefiting","mean_te_benef","mean_te_r6")
cc[num] <- lapply(cc[num], function(x) round(x, 4))
write.csv(cc, file.path(path_tables, "te_crosscuts.csv"), row.names = FALSE)

## --- Punchline highlights ------------------------------------------------------
pick <- function(dim, grp, col) cc[cc$dimension == dim & cc$group == grp, col]
hl <- data.frame(
  highlight = c(
    "Worker frame share of ALL TE dollars (rest: older/non-working contributors, imputed IRA owners)",
    "TE dollar share captured by workers WITH employer access (pop share shown alongside)",
    "TE dollar share captured by match recipients",
    "Mean TE per worker: has vs lacks employer access ($/yr)",
    "Mean TE per worker: match recipients vs non-recipients ($/yr)",
    "Mean TE per worker: bachelor's+ vs less than HS ($/yr)",
    "Mean TE per worker: White (NH) vs Hispanic ($/yr)",
    "Mean TE per worker: top vs bottom worker earnings decile ($/yr)",
    "Share benefiting: has vs lacks employer access"),
  value = c(
    sprintf("%.1f%%", 100 * coverage),
    sprintf("%.1f%% of TE dollars vs %.1f%% of workers",
            100 * pick("Pillar 1: access", "Has employer access", "te_share"),
            100 * pick("Pillar 1: access", "Has employer access", "pop_share")),
    sprintf("%.1f%% of TE dollars vs %.1f%% of workers",
            100 * pick("Pillar 3: matching", "Receives employer match", "te_share"),
            100 * pick("Pillar 3: matching", "Receives employer match", "pop_share")),
    sprintf("$%s vs $%s",
            format(round(pick("Pillar 1: access", "Has employer access", "mean_te")), big.mark = ","),
            format(round(pick("Pillar 1: access", "Lacks employer access", "mean_te")), big.mark = ",")),
    sprintf("$%s vs $%s",
            format(round(pick("Pillar 3: matching", "Receives employer match", "mean_te")), big.mark = ","),
            format(round(pick("Pillar 3: matching", "No employer match", "mean_te")), big.mark = ",")),
    sprintf("$%s vs $%s",
            format(round(pick("Education", "Bachelor's+", "mean_te")), big.mark = ","),
            format(round(pick("Education", "Less than HS", "mean_te")), big.mark = ",")),
    sprintf("$%s vs $%s",
            format(round(pick("Race/ethnicity", "White (NH)", "mean_te")), big.mark = ","),
            format(round(pick("Race/ethnicity", "Hispanic", "mean_te")), big.mark = ",")),
    sprintf("$%s vs $%s",
            format(round(pick("Worker earnings decile", "10", "mean_te")), big.mark = ","),
            format(round(pick("Worker earnings decile", "1", "mean_te")), big.mark = ",")),
    sprintf("%.1f%% vs %.1f%%",
            100 * pick("Pillar 1: access", "Has employer access", "pct_benefiting"),
            100 * pick("Pillar 1: access", "Lacks employer access", "pct_benefiting"))))
write.csv(hl, file.path(path_tables, "te_crosscut_highlights.csv"), row.names = FALSE)

## --- Sensitivity: the lacks-access "share benefiting" under the SMEARED
## (owner-wide) IRA allocation, for disclosure next to the concentrated headline.
## Approximation: a worker counts as benefiting under the smear if they have observed
## contributions, an imputed DB accrual, or a positive smeared outside-IRA allocation.
s5 <- as.data.frame(read_parquet(file.path(path_processed, "stage5_imputed.parquet")))
da <- left_join(d, s5[, c("SSUID","PNUM","c_er_db_imp","c_ira_out_trad_smear","c_ira_out_roth_smear")],
                by = c("SSUID","PNUM"))
## Index ONE frame. The person-level join must not fan out or reorder; assert, then
## take every vector (flags, weights, TE) from the joined frame.
stopifnot(nrow(da) == nrow(d), identical(da$SSUID, d$SSUID), identical(da$PNUM, d$PNUM))
ben_smear <- z(da$c_ee) + z(da$c_er) + z(da$c_er_db_imp) +
             z(da$c_ira_out_trad_smear) + z(da$c_ira_out_roth_smear) > 0
la <- da$lacks_access
pct_smear <- sum(da$WPFINWGT[la & ben_smear]) / sum(da$WPFINWGT[la])
pct_prim  <- sum(da$WPFINWGT[la & da$te_total_B > 0]) / sum(da$WPFINWGT[la])

## Persist the smear-sensitivity value cited in the draft appendix so it is reproducible from
## pipeline output, not console-only (the exact share lives in this CSV, never in comments).
hl <- rbind(hl, data.frame(
  highlight = "Share benefiting, lacks access: HEADLINE (concentrated IRA) vs owner-wide smear sensitivity",
  value = sprintf("%.1f%% vs %.1f%%", 100 * pct_prim, 100 * pct_smear)))
write.csv(hl, file.path(path_tables, "te_crosscut_highlights.csv"), row.names = FALSE)

## --- Review ---------------------------------------------------------------------
cat("\n================ 14 TE CROSS-CUTS (CBO-comparable row, r=3.5%) ================\n")
cat(sprintf("Worker-frame TE: $%.1fB of $%.1fB total (%.1f%% coverage; workers n=%d)\n",
            te_total_workers/1e9, te_total_all/1e9, 100*coverage, nrow(d)))
cat("\nHighlights:\n")
for (i in seq_len(nrow(hl))) cat(" -", hl$highlight[i], ":", hl$value[i], "\n")
cat("\nFull table by dimension (mean TE $/worker, TE share vs pop share):\n")
print(cc[, c("dimension","group","pop_share","te_share","mean_te","pct_benefiting")],
      row.names = FALSE)
cat(sprintf("\nSensitivity: lacks-access share benefiting = %.1f%% under the HEADLINE (contributor-\ncount-concentrated) IRA allocation; the owner-wide smear sensitivity gives %.1f%%.\n",
            100 * pct_prim, 100 * pct_smear))
cat("\nWrote te_crosscuts.csv, te_crosscut_highlights.csv\n")
