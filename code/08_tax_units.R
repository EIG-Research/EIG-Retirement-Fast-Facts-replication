# code/08_tax_units.R
# TE Stage 1-2: annualize CY2024 income components, build CBO-style income deciles for ALL
# persons, and construct TY2024 tax-filing units from the December household roster.
# Inputs : data/processed/pu2025_extract.parquet (188 vars, all person-months)
# Outputs: data/processed/te_persons.parquet  (person-level: annual components, decile, unit id)
#          data/processed/tax_units.parquet   (unit-level TAXSIM-35 input frame)
#
# Design decisions implemented here (all from the approved plan; disclosed in 10's limitations):
#  - Population: persons present in the December reference month (MONTHCODE 12).
#  - Deciles: CBO-comparable "income before transfers and taxes" ~= annualized household sum of
#    (TPTOTINC - TPTRNINC), size-adjusted by sqrt(household size), EQUAL-PERSON weighted deciles.
#    Deviations from CBO (no imputed Medicare value, no realized capital gains) are disclosed.
#  - Tax units: married-joint via EMS/EPNSPOUSE; qualifying children by age/enrollment + parent
#    pointers; dependent filers (earned income > threshold) file their own mstat-8 return;
#    qualifying relatives NOT modeled (v1, disclosed); cohabiters file separately.
#  - Filing status: this frame emits only single / married-jointly / dependent-child. HEAD OF
#    HOUSEHOLD is deliberately NOT a frame value -- both engines DERIVE it from unit structure
#    (verified 2026-08-27: PolicyEngine-US 1.772.0 returns filing_status = HEAD_OF_HOUSEHOLD and the
#    $21,900 TY2024 standard deduction for a single filer with two children; TAXSIM-35 returns the
#    $20,800 TY2023 HoH zero-bracket amount). Roughly 6% of units (~13M weighted) are treated as
#    HoH this way. CAVEAT: married-filing-separately is likewise never emitted -- every non-jointly
#    person becomes "single" -- so a small group (~0.9% of units, ~1.7M weighted) who are
#    married-spouse-absent (EMS 2) or separated (EMS 5) receive HoH treatment they may not be
#    entitled to in law. An MFS branch is a documented future sensitivity (external RA review,
#    2026-08-27). Exact counts are printed by this script's review block each run.
#  - Wages vs self-employment: pwages = sum of employee-type job-line earnings (CLWRK 1-7);
#    psemp = residual TPEARN - pwages (captures unincorporated SE profits/losses and odd jobs).

suppressWarnings(suppressMessages({
  library(arrow); library(dplyr)
}))

path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
source(file.path(path_project, "code", "_shared", "helpers.R"))
path_processed <- file.path(path_project, "data", "processed")

raw <- as.data.frame(read_parquet(file.path(path_processed, EXTRACT_PARQUET)))
need <- c("SSUID","PNUM","MONTHCODE","WPFINWGT","ERESIDENCEID","THHLDSTATUS","TAGE","TAGE_EHC",
          "EMS","EPNSPOUSE","EPNPAR1","EPNPAR2","ERELRPE","EEDENROLL","RENROLL",
          "TPEARN","TPTOTINC","TPTRNINC","TRETINCAMT","TSSSAMT","TUC1AMT","TUC2AMT",
          "TPPRPINC","TINC_BANK","TINC_BOND","TINC_STMF","TINC_RENT","TINC_OTH",
          paste0("TJB",1:6,"_MSUM"), paste0("EJB",1:6,"_CLWRK"),
          "TSCNTAMT_401","TSCNTAMT_IRA","TSCNTAMT_PEN","TSCNTAMT",
          "TECNTAMT_401","TECNTAMT_IRA","TECNTAMT","TCNTAMT",
          "TIRAKEOVAL","TTHR401VAL","TVAL_RET","TIRA_INC_AMT","TTHR_INC_AMT",
          "TST_INTV","EFSTATUS","EEITC","EDEPCLM")
miss <- setdiff(need, names(raw))
if (length(miss)) stop("08_tax_units: extract is missing required columns: ", paste(miss, collapse=", "),
                       "\n  Re-run 01_build_dataset.R with the 188-var EXTRACT_VARS (2026-07-11).")

## Civilian-labor-force scope: drop all person-months of households with any active-duty military
## member (helpers.R), consistent with 01/11, BEFORE tax-unit construction so no unit is ever split
## and the tax-expenditure totals are civilian-only.
mil_hh <- if (isTRUE(EXCLUDE_MILITARY)) military_household_ids(raw) else character(0)
raw    <- drop_military_households(raw, mil_hh)

## --- 1. Sentinel cleaning ----------------------------------------------------
## Dollar amounts are >= 0 per the dictionary except the AMT_ALLOW_NEGATIVE set; the exact
## NA-Missing sentinels for the two big recodes are cleaned explicitly, everything else
## defensively (negatives -> NA for nonneg-domain amounts).
clean_amt <- function(x, allow_negative = FALSE, sentinels = NULL) {
  if (!is.null(sentinels)) x[x %in% sentinels] <- NA
  if (!allow_negative)     x[!is.na(x) & x < 0] <- NA
  x
}
raw$TPEARN    <- clean_amt(raw$TPEARN,   TRUE,  NA_TPEARN)
raw$TPTOTINC  <- clean_amt(raw$TPTOTINC, TRUE,  NA_TPTOTINC)
for (v in c("TPTRNINC","TRETINCAMT","TSSSAMT","TUC1AMT","TUC2AMT",
            "TINC_BANK","TINC_BOND","TINC_STMF",
            paste0("TJB",1:6,"_MSUM"),
            "TSCNTAMT_401","TSCNTAMT_IRA","TSCNTAMT_PEN","TSCNTAMT",
            "TECNTAMT_401","TECNTAMT_IRA","TECNTAMT","TCNTAMT",
            "TIRAKEOVAL","TTHR401VAL","TVAL_RET","TIRA_INC_AMT","TTHR_INC_AMT"))
  raw[[v]] <- clean_amt(raw[[v]], allow_negative = FALSE)
## allow-negative members of AMT_ALLOW_NEGATIVE handled here (TPEARN/TPTOTINC above
## use their exact documented sentinels; TFTOTINC/THTOTINC are not used in this script)
for (v in c("TPPRPINC","TINC_RENT","TINC_OTH")) {
  stopifnot(v %in% AMT_ALLOW_NEGATIVE)
  raw[[v]] <- clean_amt(raw[[v]], allow_negative = TRUE, sentinels = c(NA_TPTOTINC, NA_TPEARN, NA_DOLLAR_SMALL))
}

## --- 2. Wage vs self-employment earnings per person-month --------------------
## Employee-type wages: job lines whose class of worker is 1-7 (employees + SE-incorporated,
## who pay themselves W-2 wages). Residual person earnings (TPEARN - wages) -> self-employment.
wage_m <- rep(0, nrow(raw))
for (n in 1:6) {
  msum <- raw[[paste0("TJB",n,"_MSUM")]]
  clw  <- raw[[paste0("EJB",n,"_CLWRK")]]
  is_emp <- !is.na(clw) & clw %in% 1:7
  wage_m <- wage_m + ifelse(is_emp & !is.na(msum), msum, 0)
}
raw$wage_m <- wage_m

## --- 3. Annualize (sum monthly x 12/n_months; annual-as-stored via max) ------
## Reviewed, no change: sum(..., na.rm=TRUE) over months / n() is CORRECT here.
## Monthly earnings/income items (TPEARN, wage_m, TPTOTINC, ...) are NA precisely when the
## person is OUT of that month's universe -- e.g. TPEARN's universe is "held a job during
## the reference month," so a jobless month is NA, not 0 (verified: effectively all interior-NA
## TPEARN months are RMESR>=6 no-job months). na.rm treats those months as $0 earned, which
## is right. Do NOT rescale by non-NA months: that would fabricate earnings for jobless
## months and wrongly lift part-year workers up the distribution.
## The 12/n_months factor is ~1 except for the few partial-PRESENCE persons (n_months<12),
## which is the intended partial-year adjustment.
mx <- function(x) { m <- suppressWarnings(max(x, na.rm = TRUE)); if (is.infinite(m)) NA_real_ else m }
z0 <- function(x) ifelse(is.na(x), 0, x)
ann <- raw |>
  group_by(SSUID, PNUM) |>
  summarise(
    n_months   = n(),
    wages      = sum(wage_m,             na.rm = TRUE) * 12 / n_months,
    earn_tot   = sum(TPEARN,             na.rm = TRUE) * 12 / n_months,
    ptotinc    = sum(TPTOTINC,           na.rm = TRUE) * 12 / n_months,
    trninc     = sum(TPTRNINC,           na.rm = TRUE) * 12 / n_months,
    pensions   = sum(TRETINCAMT,         na.rm = TRUE) * 12 / n_months,
    gssi       = sum(TSSSAMT,            na.rm = TRUE) * 12 / n_months,
    ## zero-guard BOTH UI components: NA + value = NA would drop the month's dollars
    ui         = sum(z0(TUC1AMT) + z0(TUC2AMT)) * 12 / n_months,
    ## annual-as-stored (reference-year totals repeated on the person's monthly records)
    prpinc     = mx(TPPRPINC),
    intrec     = z0(mx(TINC_BANK)) + z0(mx(TINC_BOND)),
    dividends  = mx(TINC_STMF),
    rent_oth   = z0(mx(TINC_RENT)) + z0(mx(TINC_OTH)),   # TINC_OTH already includes annuity income (registry note) - do not add TANNINC
    c_ee_401   = mx(TSCNTAMT_401), c_ee_ira = mx(TSCNTAMT_IRA), c_ee_pen = mx(TSCNTAMT_PEN),
    c_ee_tot   = mx(TSCNTAMT),
    c_er_401   = mx(TECNTAMT_401), c_er_ira = mx(TECNTAMT_IRA), c_er_tot = mx(TECNTAMT),
    bal_ira    = mx(TIRAKEOVAL),   bal_401  = mx(TTHR401VAL),   bal_ret  = mx(TVAL_RET),
    wd_ira     = mx(TIRA_INC_AMT), wd_401   = mx(TTHR_INC_AMT),
    .groups = "drop")
## psemp = residual earnings not classified as employee wages (SE profits/losses, other arrangements)
ann$psemp <- ann$earn_tot - ann$wages

## --- 4. December person frame ------------------------------------------------
dec <- raw[!is.na(raw$MONTHCODE) & raw$MONTHCODE == REF_MONTH, ]
dec <- dec[!is.na(dec$THHLDSTATUS) & dec$THHLDSTATUS %in% 1:4, ]  # in-universe household members
p <- dec |>
  transmute(SSUID, PNUM, ERESIDENCEID, WPFINWGT,
            age    = ifelse(!is.na(TAGE_EHC), TAGE_EHC, TAGE),
            ems    = EMS,
            sp     = ifelse(!is.na(EPNSPOUSE) & EPNSPOUSE >= 101, EPNSPOUSE, NA_integer_),
            par1   = ifelse(!is.na(EPNPAR1)   & EPNPAR1   >= 101, EPNPAR1,   NA_integer_),
            par2   = ifelse(!is.na(EPNPAR2)   & EPNPAR2   >= 101, EPNPAR2,   NA_integer_),
            enrolled = (!is.na(EEDENROLL) & EEDENROLL == 1) | (!is.na(RENROLL) & RENROLL == 1),
            ## TST_INTV is a zero-padded string FIPS ("06"); normalize to integer for the map
            state_fips = suppressWarnings(as.integer(TST_INTV)), EFSTATUS, EEITC, EDEPCLM) |>
  left_join(ann, by = c("SSUID","PNUM"))
p$hh_id <- paste(p$SSUID, p$ERESIDENCEID, sep = "-")
stopifnot(!anyNA(p$age), all(p$WPFINWGT >= 0))

## --- 5. CBO-style deciles (ALL December persons, equal-person weighted) ------
## income before transfers and taxes ~= person total income minus means-tested transfers,
## summed to the household, size-adjusted by sqrt(hh size). Person-weighted deciles.
p$ibt <- (ifelse(is.na(p$ptotinc), 0, p$ptotinc) - ifelse(is.na(p$trninc), 0, p$trninc))
hh <- p |> group_by(hh_id) |>
  summarise(hh_ibt = sum(ibt), hh_size = n(), .groups = "drop") |>
  mutate(hh_ibt_adj = hh_ibt / sqrt(hh_size))
p <- left_join(p, hh, by = "hh_id")
p$decile <- wtd_decile(p$hh_ibt_adj, p$WPFINWGT)
dec_share <- tapply(p$WPFINWGT, p$decile, sum) / sum(p$WPFINWGT)
stopifnot(length(dec_share) == 10, all(abs(dec_share - 0.10) < 0.005))

## --- 6. Tax-unit construction (TY2024) ---------------------------------------
## (a) married couples filing jointly: EMS=1 with a valid spouse pointer resolving in-household.
p$key    <- paste(p$hh_id, p$PNUM)
p$sp_key <- ifelse(is.na(p$sp), NA, paste(p$hh_id, p$sp))
p$sp_ok  <- !is.na(p$sp_key) & p$sp_key %in% p$key & !is.na(p$ems) & p$ems == 1
## couple id = household + min person number of the pair (symmetric)
p$couple_id <- ifelse(p$sp_ok, paste0(p$hh_id, "-C", pmin(p$PNUM, p$sp)), NA)

## (b) qualifying-child candidates: under 19, or under 24 and enrolled, with a parent present
## in the household. Never a dependent if married-joint themselves.
p$par_key1 <- ifelse(is.na(p$par1), NA, paste(p$hh_id, p$par1))
p$par_key2 <- ifelse(is.na(p$par2), NA, paste(p$hh_id, p$par2))
par_present <- (!is.na(p$par_key1) & p$par_key1 %in% p$key) |
               (!is.na(p$par_key2) & p$par_key2 %in% p$key)
age_ok <- p$age <= CHILD_DEP_AGE_MAX | (p$age <= STUDENT_DEP_AGE_MAX & p$enrolled)
p$is_dep <- age_ok & par_present & !p$sp_ok

## dependent's claiming parent: par1 if present, else par2
p$dep_parent_key <- ifelse(p$is_dep,
                           ifelse(!is.na(p$par_key1) & p$par_key1 %in% p$key, p$par_key1, p$par_key2),
                           NA)

## (c) unit id: dependents attach to the parent's unit (parent's couple unit when married);
## everyone else heads their own unit (couple or single).
own_unit <- ifelse(!is.na(p$couple_id), p$couple_id, paste0(p$hh_id, "-S", p$PNUM))
parent_unit <- own_unit[match(p$dep_parent_key, p$key)]
p$unit_id <- ifelse(p$is_dep, parent_unit, own_unit)
stopifnot(!anyNA(p$unit_id))                       # every December person in exactly one unit

## (d) dependent filers: dependents whose own earned income exceeds the TY2024 dependent
## standard deduction file their own mstat-8 return (still counted in the parent's depx).
p$earn_ann <- ifelse(is.na(p$earn_tot), 0, p$earn_tot)
p$dep_files <- p$is_dep & p$earn_ann > DEP_FILING_THRESH
p$filing_unit_id <- ifelse(p$dep_files, paste0(p$unit_id, "-D", p$PNUM), p$unit_id)

## --- 7. Unit-level TAXSIM-35 input frame -------------------------------------
z <- function(x) ifelse(is.na(x), 0, x)
adults <- p[!p$is_dep | p$dep_files, ]     # heads/spouses + dependent filers
adults$is_couple <- !is.na(adults$couple_id) & !adults$dep_files

## primary = higher annual earnings within a couple; singles are their own primary
adults <- adults |> group_by(filing_unit_id) |> mutate(prim = earn_ann == max(earn_ann)) |>
  mutate(prim = prim & !duplicated(prim)) |> ungroup()   # exactly one primary per unit

deps <- p[p$is_dep, ] |> group_by(filing_unit_id = unit_id) |>
  summarise(depx = n(),
            age1 = sort(age)[1], age2 = sort(age)[2], age3 = sort(age)[3],
            ## ALL dependents' observed ages, ";"-joined: the PolicyEngine path builds every
            ## dependent from this (age 0 included; ages are never NA -- asserted on p above),
            ## TAXSIM keeps its depx/age1-3 interface.
            dep_ages = paste(sort(age), collapse = ";"),
            .groups = "drop")

units <- adults |>
  group_by(filing_unit_id) |>
  summarise(
    hh_id    = first(hh_id),
    mstat    = if (first(dep_files)) "dependent child"
               else if (any(is_couple)) "married, jointly" else "single",
    page     = age[prim][1],
    sage     = if (any(is_couple) && sum(!prim) > 0) age[!prim][1] else 0,
    pwages   = sum(z(wages[prim])),   swages = sum(z(wages[!prim])),
    psemp    = sum(z(psemp[prim])),   ssemp  = sum(z(psemp[!prim])),
    dividends= sum(z(dividends)),     intrec = sum(z(intrec)),
    otherprop= sum(z(rent_oth)),      pensions = sum(z(pensions)),
    gssi     = sum(z(gssi)),
    pui      = sum(z(ui[prim])),      sui    = sum(z(ui[!prim])),
    transfers= sum(z(trninc)),
    state_fips = first(state_fips),
    ## unit contribution/balance/withdrawal aggregates for 09/10
    c_ee_401 = sum(z(c_ee_401)), c_ee_ira = sum(z(c_ee_ira)), c_ee_pen = sum(z(c_ee_pen)),
    c_er_401 = sum(z(c_er_401)), c_er_ira = sum(z(c_er_ira)),
    ## primary/spouse split of pre-tax contributions (09 adjusts pwages/swages separately)
    c_ee_prim = sum(z(c_ee_401[prim]) + z(c_ee_ira[prim]) + z(c_ee_pen[prim])),
    c_ee_sp   = sum(z(c_ee_401[!prim]) + z(c_ee_ira[!prim]) + z(c_ee_pen[!prim])),
    c_er_prim = sum(z(c_er_401[prim]) + z(c_er_ira[prim])),
    c_er_sp   = sum(z(c_er_401[!prim]) + z(c_er_ira[!prim])),
    bal_ret  = sum(z(bal_ret)),  wd_ret   = sum(z(wd_ira) + z(wd_401)),
    wgt_unit = sum(WPFINWGT[prim]),
    .groups = "drop") |>
  left_join(deps, by = "filing_unit_id") |>
  mutate(depx = z(depx),
         age1 = z(age1), age2 = z(age2), age3 = z(age3),
         dep_ages = ifelse(is.na(dep_ages), "", dep_ages),
         state = unname(FIPS_TO_STATE[as.character(state_fips)]),
         year  = TAX_YEAR,
         taxsimid = row_number())
## dependents' own depx must be zero; ages of youngest three only
units$depx[units$mstat == "dependent child"] <- 0
units$dep_ages[units$mstat == "dependent child"] <- ""
## dep_ages must list exactly depx ages for every unit (09_policyengine_te.py consumes dep_ages;
## TAXSIM consumes depx/age1-3 -- the two interfaces must stay structurally identical)
n_dep_tok <- ifelse(units$dep_ages == "", 0L,
                    lengths(strsplit(units$dep_ages, ";", fixed = TRUE)))
stopifnot(all(n_dep_tok == units$depx))
## TST_INTV is 50 states + DC; a small number of December records carry an empty string
## (noninterview/Type-2). The HEADLINE is federal-only (09 runs TAXSIM with state = 0 for all
## units); `state` is retained here for the future state-tax extension. Tolerate but report NA.
if (anyNA(units$state)) message("08: NOTE ", sum(is.na(units$state)),
                                " units (", sprintf('%.2f%%', 100*mean(is.na(units$state))),
                                ") have no state FIPS; federal headline unaffected.")
stopifnot(sum(units$mstat == "married, jointly") > 0,
          !anyNA(units$page), all(units$pwages >= 0), all(units$swages >= 0))

## --- 8. Persist + review -----------------------------------------------------
persons_out <- p |>
  select(SSUID, PNUM, hh_id, unit_id, filing_unit_id, WPFINWGT, age, decile,
         hh_ibt, hh_ibt_adj, hh_size, is_dep, dep_files, earn_ann, wages, psemp,
         c_ee_401, c_ee_ira, c_ee_pen, c_ee_tot, c_er_401, c_er_ira, c_er_tot,
         bal_ira, bal_401, bal_ret, wd_ira, wd_401, pensions, gssi,
         state_fips, EFSTATUS, EEITC, EDEPCLM)
write_parquet(persons_out, file.path(path_processed, "te_persons.parquet"))
write_parquet(units,       file.path(path_processed, "tax_units.parquet"))

cat("\n================ 08 TAX UNITS: REVIEW ================\n")
cat("December persons:", nrow(p), " | households:", length(unique(p$hh_id)),
    " | filing units:", nrow(units), "\n")
## Note: partial-year persons whose incomes are scaled by 12/n_months
py <- !is.na(p$n_months) & p$n_months < 12
cat(sprintf("Partial-year persons (income scaled 12/n): %.1f%% of December persons (weighted %.1f%%)\n",
            100 * mean(py), 100 * sum(p$WPFINWGT[py]) / sum(p$WPFINWGT)))
## Note: zero-coded missing income entering the household ranking sums. Children under 15
## are out of universe for income (legitimately zero); the concern is item nonresponse
## among persons 15+, reported separately.
zc <- is.na(p$ptotinc) & p$age >= 15
cat(sprintf("Zero-coded missing income (persons 15+, item nonresponse): %.1f%% of December persons (weighted %.1f%%)\n",
            100 * mean(zc), 100 * sum(p$WPFINWGT[zc]) / sum(p$WPFINWGT)))
cat("Weighted December population (M):", round(sum(p$WPFINWGT)/1e6, 1), "\n")
cat("Decile person-shares:", paste(sprintf('%.1f%%', 100*dec_share), collapse=" "), "\n")
tab <- table(units$mstat)
cat("Units by mstat:\n"); print(tab)
## HoH-derivation disclosure (see header): the frame emits single/married-jointly/dependent-child
## only; both engines DERIVE head-of-household for single filers with dependents. COMPUTED here so
## the header's caveat counts stay reproducible run-to-run.
prim_rows <- adults[adults$prim, c("filing_unit_id", "ems")]
units_ems <- prim_rows$ems[match(units$filing_unit_id, prim_rows$filing_unit_id)]
hoh_derived <- units$mstat == "single" & units$depx > 0
hoh_ems_sep <- hoh_derived & !is.na(units_ems) & units_ems %in% c(2L, 5L)
cat(sprintf("HoH-derived units (single filer + dependents; engines derive HoH): %d (%.2f%%; %.2fM weighted)\n",
            sum(hoh_derived), 100 * mean(hoh_derived), sum(units$wgt_unit[hoh_derived]) / 1e6))
cat(sprintf("  of which head is EMS spouse-absent/separated (MFS-entitlement caveat): %d (%.2f%%; %.2fM weighted)\n",
            sum(hoh_ems_sep), 100 * mean(hoh_ems_sep), sum(units$wgt_unit[hoh_ems_sep]) / 1e6))
cat("Weighted filing units (M, primary weights):", round(sum(units$wgt_unit)/1e6,1), "\n")
cat("Units with any retirement contribution:", sum(units$c_ee_401+units$c_ee_ira+units$c_ee_pen+
    units$c_er_401+units$c_er_ira > 0), "\n")
cat("\nWrote te_persons.parquet (", nrow(persons_out), ") and tax_units.parquet (", nrow(units), ")\n")
