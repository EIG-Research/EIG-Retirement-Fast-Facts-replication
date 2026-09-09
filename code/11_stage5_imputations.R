# code/11_stage5_imputations.R
# TE Stage 5: gap-closing imputations for the CBO-comparable headline row.
# Inputs : data/processed/te_persons.parquet, tax_units.parquet, pu2025_extract.parquet,
#          data/raw/external_benchmarks/*.csv (verified public aggregates; see its README)
# Output : data/processed/stage5_imputed.parquet (person-level imputed components)
#
# Three imputations, each preserving its source aggregate by construction:
#  (1) EMPLOYER DB contributions - unobserved in SIPP (no TECNTAMT_PEN exists). National
#      sector aggregates allocated across SIPP December DB participants (EMJOB_PEN = 1)
#      proportional to weighted earnings within sector. Concept choice: "accrued" (normal
#      cost of this year's benefit accrual) is the primary - it matches the PV estimand
#      (benefit earned THIS year); "actual" cash contributions (which include amortization
#      of past underfunding, ~2/3 of state-local cash) is stored for sensitivity.
#  (2) ROTH split of observed 401(k)-type employee contributions - SIPP has no Roth
#      variables. Vanguard HAS 2024 usage-if-offered by income band, scaled by the
#      participant->dollar factor from IRS W-2 statistics (TY2020: Roth = 10.4% of 401(k)
#      dollars vs ~13.6% of instances -> 0.75).
#  (3) OUTSIDE-employer IRA contributions - SIPP contribution amounts are anchored to the
#      main employer; personal traditional/Roth IRA dollars (IRS SOI IRA study TY2023, by
#      AGI class) are allocated to SIPP IRA owners without an employer-provided IRA,
#      weight-proportional within AGI class. Deductible share of traditional by AGI from
#      Pub 1304 Table 1.4. SEP/SIMPLE aggregates are NOT imported (observed in SIPP).

suppressWarnings(suppressMessages({
  library(arrow); library(dplyr)
}))

path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
source(file.path(path_project, "code", "_shared", "helpers.R"))
path_processed <- file.path(path_project, "data", "processed")
path_bench     <- file.path(path_project, "data", "raw", "external_benchmarks")

## --- Load and validate benchmark inputs (fail loud) ---------------------------
read_bench <- function(f, required_cols) {
  p <- file.path(path_bench, f)
  if (!file.exists(p)) stop("11: missing benchmark input ", p,
                            " - see data/raw/external_benchmarks/README.md")
  x <- read.csv(p, stringsAsFactors = FALSE)
  miss <- setdiff(required_cols, names(x))
  if (length(miss)) stop("11: ", f, " lacks required columns: ", paste(miss, collapse = ", "))
  x
}
roth_tab <- read_bench("stage5_roth401k_by_income.csv",
                       c("income_lo","income_hi","roth_usage_if_offered"))
ira_tab  <- read_bench("stage5_ira_by_agi.csv",
                       c("agi_lo","agi_hi","trad_musd","roth_musd","trad_n","roth_n"))  # trad_n/roth_n drive conc_alloc()
ded_tab  <- read_bench("stage5_ira_deduction_by_agi.csv", c("agi_lo","agi_hi","ded_musd"))
db_tab   <- read_bench("stage5_db_employer.csv", c("sector","concept","value_busd","year"))
scal_tab <- read_bench("stage5_scalars.csv", c("param","value"))
scal <- setNames(scal_tab$value, scal_tab$param)
stopifnot("roth401k_dollar_scale" %in% names(scal))
stopifnot(all(c("Private","StateLocal","Federal") %in% db_tab$sector))
## Vintage guard: the DB benchmark must carry the SIPP reference year;
## refresh data/raw/external_benchmarks/stage5_db_employer.csv on every vintage bump.
stopifnot(all(db_tab$year == REF_YEAR))

## --- Person frame with December job attributes --------------------------------
persons <- as.data.frame(read_parquet(file.path(path_processed, "te_persons.parquet")))
units   <- as.data.frame(read_parquet(file.path(path_processed, "tax_units.parquet")))

raw <- read_parquet(file.path(path_processed, EXTRACT_PARQUET),
                    col_select = c("SSUID","PNUM","MONTHCODE","ERESIDENCEID","EMJOB_PEN","EOWN_IRAKEO",
                                   "EMJOB_IRA", paste0("EJB",1:6,"_CLWRK"))) |> as.data.frame()
## Civilian-labor-force scope: drop households with any active-duty military member (helpers.R),
## consistent with 01/08, so the DB sector allocation base excludes them.
mil_hh <- if (isTRUE(EXCLUDE_MILITARY)) military_household_ids(raw) else character(0)
raw    <- drop_military_households(raw, mil_hh)
dec <- raw[!is.na(raw$MONTHCODE) & raw$MONTHCODE == REF_MONTH, ]
clw_cols <- lapply(paste0("EJB",1:6,"_CLWRK"), function(v) { x <- dec[[v]]; x[x == NA_CAT] <- NA; x })
dec$clw <- Reduce(dplyr::coalesce, clw_cols)
dec$sector <- dplyr::case_when(
  dec$clw %in% CLWRK_PRIVATE                 ~ "Private",
  dec$clw %in% c(3L, 4L)                     ~ "StateLocal",
  dec$clw == 1L                              ~ "Federal",     # federal-civilian (active-duty military excluded upstream)
  dec$clw %in% CLWRK_SELFEMP                 ~ "Private",     # SE with a DB plan: treat as private
  TRUE                                       ~ NA_character_)
p <- persons |>
  left_join(dec |> select(SSUID, PNUM, EMJOB_PEN, EOWN_IRAKEO, EMJOB_IRA, sector),
            by = c("SSUID","PNUM"))
z <- function(x) ifelse(is.na(x), 0, x)

## =============================================================================
## (1) Employer DB contributions by sector (both concepts carried)
## =============================================================================
db_part <- !is.na(p$EMJOB_PEN) & p$EMJOB_PEN == 1 & !is.na(p$sector)
alloc_db <- function(concept) {
  out <- numeric(nrow(p))
  for (s in c("Private","StateLocal","Federal")) {
    agg <- db_tab$value_busd[db_tab$sector == s & db_tab$concept == concept] * 1e9
    stopifnot(length(agg) == 1, is.finite(agg), agg >= 0)
    sel <- db_part & p$sector == s
    base <- pmax(0, z(p$earn_ann[sel])) * p$WPFINWGT[sel]      # weighted earnings shares
    if (sum(base) <= 0) stop("11: no allocation base for DB sector ", s)
    ## per-person dollars: aggregate x earnings share of the weighted base. Algebraically
    ## agg * (earn*w / sum(earn*w)) / w, written weight-free so a zero-weight December person
    ## gets a finite value instead of 0/0 = NaN.
    out[sel] <- agg * pmax(0, z(p$earn_ann[sel])) / sum(base)
  }
  out
}
p$c_er_db_imp        <- alloc_db("accrued")   # PRIMARY: normal cost of this year's accrual
p$c_er_db_imp_actual <- alloc_db("actual")    # sensitivity: cash concept incl. amortization

## =============================================================================
## (2) Roth split of observed 401(k)-type employee contributions
## =============================================================================
scale_d <- unname(scal["roth401k_dollar_scale"])
band <- findInterval(pmax(0, z(p$earn_ann)), c(roth_tab$income_lo, Inf), rightmost.closed = FALSE)
band <- pmin(pmax(band, 1L), nrow(roth_tab))
p$s_roth401k <- roth_tab$roth_usage_if_offered[band] * scale_d
p$c_ee_401_roth <- z(p$c_ee_401) * p$s_roth401k
p$c_ee_401_trad <- z(p$c_ee_401) - p$c_ee_401_roth

## =============================================================================
## (3) Outside-employer personal IRA contributions by AGI class
## =============================================================================
## AGI proxy = tax-unit gross income (return level, like SOI classes)
units$unit_agi_proxy <- with(units, pwages + swages + psemp + ssemp + dividends +
                                    intrec + otherprop + pensions + 0.5 * gssi)
p <- p |> left_join(units |> select(filing_unit_id, unit_agi_proxy), by = "filing_unit_id")
p$unit_agi_proxy <- z(p$unit_agi_proxy)

## eligible receivers: IRA/Keogh owners without an employer-provided IRA (those flows are observed)
elig <- !is.na(p$EOWN_IRAKEO) & p$EOWN_IRAKEO == 1 & (is.na(p$EMJOB_IRA) | p$EMJOB_IRA != 1)

ira_tab$ded_musd <- ded_tab$ded_musd[match(paste(ira_tab$agi_lo, ira_tab$agi_hi),
                                           paste(ded_tab$agi_lo, ded_tab$agi_hi))]
if (anyNA(ira_tab$ded_musd)) stop("11: deduction-by-AGI rows do not align with IRA-by-AGI rows")
## deductible share of traditional, capped at 1 (different SOI samples can exceed slightly)
ira_tab$ded_share <- pmin(1, ira_tab$ded_musd / pmax(ira_tab$trad_musd, 1e-9))

## Deductible share applies class-wide (set for every eligible owner in the class;
## both allocations below inherit it).
p$ira_ded_share <- 0
for (k in seq_len(nrow(ira_tab))) {
  sel <- elig & p$unit_agi_proxy >= ira_tab$agi_lo[k] & p$unit_agi_proxy <= ira_tab$agi_hi[k]
  p$ira_ded_share[sel] <- ira_tab$ded_share[k]
}

## --- PRIMARY (HEADLINE): contributor-count-CONCENTRATED allocation ---------------
## (user decision 2026-07-13). Each AGI class's dollars go
## to a recipient mass equal to the SOI CONTRIBUTOR COUNT for that class (trad_n /
## roth_n from stage5_ira_by_agi.csv), selecting recipients by
## descending IRA/Keogh balance (owners with larger balances are likelier active
## contributors). Anchored to the administrative fact that most IRA owners do not
## contribute in a given year; conservative for "share benefiting" statistics.
conc_alloc <- function(target_n, dollars_musd) {
  out <- numeric(nrow(p))
  for (k in seq_len(nrow(ira_tab))) {
    sel <- which(elig & p$unit_agi_proxy >= ira_tab$agi_lo[k] & p$unit_agi_proxy <= ira_tab$agi_hi[k])
    if (!length(sel) || dollars_musd[k] <= 0) next
    ord <- sel[order(-z(p$bal_ira[sel]), -z(p$WPFINWGT[sel]))]
    cw  <- cumsum(p$WPFINWGT[ord])
    take <- ord[cw <= max(target_n[k], cw[1])]          # at least one recipient per class
    out[take] <- dollars_musd[k] * 1e6 / sum(p$WPFINWGT[take])
  }
  out
}
p$c_ira_out_trad <- conc_alloc(ira_tab$trad_n, ira_tab$trad_musd)
p$c_ira_out_roth <- conc_alloc(ira_tab$roth_n, ira_tab$roth_musd)
p$c_ira_out_trad_ded    <- p$c_ira_out_trad * p$ira_ded_share
p$c_ira_out_trad_nonded <- p$c_ira_out_trad * (1 - p$ira_ded_share)

## --- ALTERNATIVE (disclosed sensitivity): owner-wide SMEAR -----------------------
## Spreads each class's dollars evenly across ALL eligible owners. Assumption-free
## about which owners contribute, but manufactures tens of millions of sliver
## recipients (upper bound on "anyone touched by the subsidy").
p$c_ira_out_trad_smear <- 0; p$c_ira_out_roth_smear <- 0
for (k in seq_len(nrow(ira_tab))) {
  sel <- elig & p$unit_agi_proxy >= ira_tab$agi_lo[k] & p$unit_agi_proxy <= ira_tab$agi_hi[k]
  wsum <- sum(p$WPFINWGT[sel])
  if (wsum <= 0) { message("11: NOTE no eligible IRA owners in AGI class ", k, "; dollars dropped: $",
                           round((ira_tab$trad_musd[k] + ira_tab$roth_musd[k])), "M"); next }
  p$c_ira_out_trad_smear[sel] <- ira_tab$trad_musd[k] * 1e6 / wsum
  p$c_ira_out_roth_smear[sel] <- ira_tab$roth_musd[k] * 1e6 / wsum
}

## =============================================================================
## Verify aggregates are preserved, persist, review
## =============================================================================
W <- p$WPFINWGT
tot <- function(x) sum(x * W, na.rm = TRUE)
db_prim_target <- sum(db_tab$value_busd[db_tab$concept == "accrued"]) * 1e9
stopifnot(abs(tot(p$c_er_db_imp) - db_prim_target) / db_prim_target < 1e-6)
ira_target <- sum(ira_tab$trad_musd + ira_tab$roth_musd) * 1e6
ira_alloc  <- tot(p$c_ira_out_trad + p$c_ira_out_roth)
if (ira_alloc / ira_target < 0.95) message("11: NOTE ", sprintf("%.1f%%", 100*(1 - ira_alloc/ira_target)),
    " of SOI IRA dollars undistributed (AGI classes with no eligible owners).")

out_cols <- c("SSUID","PNUM","c_er_db_imp","c_er_db_imp_actual","s_roth401k",
              "c_ee_401_roth","c_ee_401_trad","c_ira_out_trad_ded","c_ira_out_trad_nonded",
              "c_ira_out_roth","c_ira_out_trad_smear","c_ira_out_roth_smear","sector")
write_parquet(p[, out_cols], file.path(path_processed, "stage5_imputed.parquet"))

cat("\n================ 11 STAGE-5 IMPUTATIONS: REVIEW ================\n")
cat("DB participants (Dec, EMJOB_PEN=1):", sum(db_part), "persons; weighted",
    round(sum(W[db_part])/1e6, 1), "M\n")
cat(sprintf("Imputed employer DB (accrued): $%.1fB  [actual-cash sensitivity: $%.1fB]\n",
            tot(p$c_er_db_imp)/1e9, tot(p$c_er_db_imp_actual)/1e9))
by_sec <- tapply((p$c_er_db_imp * W)[db_part], p$sector[db_part], sum)
print(round(by_sec / 1e9, 1))
cat(sprintf("Roth 401(k) split: weighted Roth share of 401(k) employee dollars = %.1f%% ($%.1fB of $%.1fB)\n",
            100 * tot(p$c_ee_401_roth) / max(tot(z(p$c_ee_401)), 1),
            tot(p$c_ee_401_roth)/1e9, tot(z(p$c_ee_401))/1e9))
cat(sprintf("Outside-IRA allocated: trad $%.1fB (deductible $%.1fB) + Roth $%.1fB  [SOI target $%.1fB]\n",
            tot(p$c_ira_out_trad)/1e9, tot(p$c_ira_out_trad_ded)/1e9,
            tot(p$c_ira_out_roth)/1e9, ira_target/1e9))
cat("Eligible outside-IRA receivers (owner universe):", sum(elig), "persons\n")
prim_any <- p$c_ira_out_trad + p$c_ira_out_roth > 0
cat(sprintf("IRA allocation: HEADLINE = count-concentrated, %.1fM weighted recipients (smeared sensitivity would reach %.1fM); smear columns carried as *_smear\n",
            sum(W[prim_any])/1e6, sum(W[elig])/1e6))
cat("\nWrote stage5_imputed.parquet\n")
