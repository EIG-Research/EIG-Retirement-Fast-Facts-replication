# code/_shared/helpers.R
# Weighting + breakout + funnel helpers for the SIPP Fast Facts pipeline.
# All estimates use the SIPP person weight WPFINWGT.

## Replace exact NA sentinels with NA (keeps legitimate negatives, e.g. TPEARN losses).
na_sentinel <- function(x, sentinels) {
  x[x %in% sentinels] <- NA
  x
}

## TRUE where a SIPP Yes/No item == 1 (Yes); NA (-9) stays FALSE unless na_true.
is_yes <- function(x) !is.na(x) & x == 1L

## Weighted count (sum of weights where condition TRUE), NA in cond treated as FALSE.
wtd_count <- function(cond, w) sum(w[!is.na(cond) & cond], na.rm = TRUE)

## Weighted share of `num` among `denom` (both logical), returns list(share, n_num, n_den, wt_num, wt_den).
## An EMPTY denominator returns share = NA (with n_den = 0), by design: a group with no
## in-universe members has no defined rate. Breakout CSVs therefore carry NA for structurally empty
## cells (e.g. self-employed in the participation-given-access table, where access is forced FALSE);
## consumers must treat NA-share rows as "no universe," not zero.
wtd_share <- function(num, denom, w) {
  d <- !is.na(denom) & denom
  n <- d & !is.na(num) & num
  wt_den <- sum(w[d], na.rm = TRUE)
  wt_num <- sum(w[n], na.rm = TRUE)
  list(share   = if (wt_den > 0) wt_num / wt_den else NA_real_,
       n_num   = sum(n), n_den = sum(d),
       wt_num  = wt_num, wt_den = wt_den)
}

## Weighted quantiles (type: cumulative-weight, lower-bound). x numeric, w weights, probs in [0,1].
wtd_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

## Assign weighted decile (1..10) to a numeric vector.
## Guards: refuse all-NA/zero-weight input, and verify the resulting
## deciles are approximately balanced (heavy ties at a cut point silently dump mass
## into one bin; findInterval cannot split a tie).
wtd_decile <- function(x, w, balance_tol = 0.03) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) stop("wtd_decile: no usable observations (all NA or zero-weight).")
  cuts <- wtd_quantile(x, w, probs = seq(0.1, 0.9, by = 0.1))
  d <- findInterval(x, cuts, rightmost.closed = FALSE) + 1L   # 0..9 -> 1..10
  sh <- tapply(w[ok], d[ok], sum) / sum(w[ok])
  if (max(abs(sh - 0.10)) > balance_tol)
    stop(sprintf("wtd_decile: deciles unbalanced (max deviation %.1fpp > %.0fpp tolerance);",
                 100 * max(abs(sh - 0.10)), 100 * balance_tol),
         " heavy ties at a cut point - inspect the input distribution.")
  d
}

## Breakout: compute wtd_share of (num within denom) overall and within each level of `by`.
## Returns a data.frame with columns: dimension, group, share, n_num, n_den, wt_num, wt_den.
breakout <- function(df, num, denom, by_vars, w = df$WPFINWGT, overall_label = "All workers") {
  rows <- list()
  o <- wtd_share(df[[num]], df[[denom]], w)
  rows[[1]] <- data.frame(dimension = "Overall", group = overall_label,
                          share = o$share, n_num = o$n_num, n_den = o$n_den,
                          wt_num = o$wt_num, wt_den = o$wt_den, stringsAsFactors = FALSE)
  for (bv in by_vars) {
    g <- df[[bv]]
    for (lev in sort(unique(g[!is.na(g)]))) {
      sel <- !is.na(g) & g == lev
      s <- wtd_share(df[[num]][sel], df[[denom]][sel], w[sel])
      rows[[length(rows) + 1]] <- data.frame(
        dimension = bv, group = as.character(lev),
        share = s$share, n_num = s$n_num, n_den = s$n_den,
        wt_num = s$wt_num, wt_den = s$wt_den, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

## Two-way breakout: compute wtd_share of (num within denom) within each cell of the
## by1 x by2 cross-tab (every observed combination of levels, not just marginals).
## Returns a data.frame with columns: dim1, group1, dim2, group2, share, n_num, n_den,
## wt_num, wt_den. NA levels of either variable are dropped (not a "cell").
breakout2 <- function(df, num, denom, by1, by2, w = df$WPFINWGT) {
  g1 <- df[[by1]]; g2 <- df[[by2]]
  rows <- list()
  for (lev1 in sort(unique(g1[!is.na(g1)]))) {
    for (lev2 in sort(unique(g2[!is.na(g2)]))) {
      sel <- !is.na(g1) & !is.na(g2) & g1 == lev1 & g2 == lev2
      s <- wtd_share(df[[num]][sel], df[[denom]][sel], w[sel])
      rows[[length(rows) + 1]] <- data.frame(
        dim1 = by1, group1 = as.character(lev1),
        dim2 = by2, group2 = as.character(lev2),
        share = s$share, n_num = s$n_num, n_den = s$n_den,
        wt_num = s$wt_num, wt_den = s$wt_den, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

## Simple funnel logger: append a (step, n_rows, wt_total) row.
funnel_row <- function(label, df, w_col = "WPFINWGT") {
  data.frame(step = label, n_persons = nrow(df),
             wt_total = sum(df[[w_col]], na.rm = TRUE), stringsAsFactors = FALSE)
}

## --- Active-duty military household exclusion (civilian-labor-force scope) -----------------------
## Project decision 2026-07-16: exclude the ENTIRE SIPP household wherever any active-duty military
## member appears (ANY of the EJB1-6_CLWRK job lines == CLWRK_MILITARY in the December reference
## month), not just the military person -- so no tax unit is ever split and the civilian labor force
## is measured cleanly. Household key = SSUID-ERESIDENCEID, matching the tax-unit grouping in
## 08_tax_units.R. Requires params.R (CLWRK_MILITARY, NA_CAT, REF_MONTH, EXCLUDE_MILITARY) sourced
## first. Applied at every stage that reads the raw extract independently: 01, 08, 11.
##
## The job-line test is an ANY across all six lines, NOT a coalesce of them (external RA review,
## 2026-08-27). `Reduce(coalesce, ...)` returns the FIRST non-missing line, so a person whose job 1
## was civilian and job 2 active duty was silently kept: it missed 7 active-duty persons (weighted
## 122,799) and 7 of 56 military households, and those 7 persons carried a civilian worker_class
## (Private 4, Self-employed 2, Government 1) into the "civilian labor force" frame.
hh_key <- function(df) {
  ## paste() over a NULL column silently yields "<SSUID>-" for every row, which matches no
  ## household id and would make drop_military_households() a silent no-op. Fail loud instead.
  miss <- setdiff(c("SSUID", "ERESIDENCEID"), names(df))
  if (length(miss)) stop("hh_key: frame lacks required column(s): ", paste(miss, collapse = ", "))
  paste(df$SSUID, df$ERESIDENCEID, sep = "-")
}

## TRUE where the person holds an active-duty military job on ANY December job line 1-6.
## Shared by military_household_ids() (which households to drop) and by 01's impact-disclosure CSV
## (how to attribute who was dropped), so the disclosure can never disagree with the rule applied.
## NOTE: this is intentionally NOT the coalesced `clw` that 01 uses for worker_class -- that one is
## the deliberate I3 primary-job classification, and using it here under-counted active-duty members
## and over-counted civilians in the disclosure (external RA review, 2026-08-27).
is_military_person <- function(df) {
  cols <- paste0("EJB", 1:6, "_CLWRK")
  ## A missing job-line column would make this logical(0) and silently disable the whole
  ## civilian-labor-force exclusion -- the same failure family hh_key() guards. Fail loud.
  miss <- setdiff(cols, names(df))
  if (length(miss)) stop("is_military_person: frame lacks job-line column(s): ",
                         paste(miss, collapse = ", "))
  per_line <- lapply(cols,
                     function(v) { x <- df[[v]]; x[x == NA_CAT] <- NA
                                   !is.na(x) & x == CLWRK_MILITARY })
  Reduce(`|`, per_line)
}

military_household_ids <- function(raw) {
  dec <- raw[!is.na(raw$MONTHCODE) & raw$MONTHCODE == REF_MONTH, ]
  unique(hh_key(dec)[is_military_person(dec)])
}

## Drop all persons in `mil_hh` households; no-op if EXCLUDE_MILITARY is FALSE or the set is empty.
drop_military_households <- function(df, mil_hh) {
  if (!isTRUE(EXCLUDE_MILITARY) || length(mil_hh) == 0) return(df)
  df[!(hh_key(df) %in% mil_hh), , drop = FALSE]
}
