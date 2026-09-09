# code/_shared/pv_te.R
# Present-value tax-expenditure pieces (CBO 57413 appendix parameters via params.R).
# Decomposed so traditional and Roth contributions combine the SAME pieces differently:
#
#   pieces(age, m0, mr, r):
#     etax1 : PV of the ANNUAL taxes on investment returns per $1 of initial AFTER-TAX basis
#             in the taxable counterfactual account (grows at r(1-m0); returns taxed at m0
#             annually; balance drawn evenly over the withdrawal window). CBO's counterfactual
#             asset is interest-bearing securities taxed as ordinary income.
#     rwd1  : PV of withdrawal taxes per $1 of PRE-TAX contribution in the retirement account
#             (grows at r untaxed to PV_RET_AGE, level-annuity withdrawals to PV_END_AGE,
#             taxed at mr).
#
#   TRADITIONAL TE(C_pretax) = m0*C + (1-m0)*C*etax1 - C*rwd1
#     (upfront exclusion; counterfactual invests (1-m0)C after-tax; future withdrawal tax offsets)
#   ROTH TE(C_aftertax)      = C*etax1
#     (no upfront benefit; counterfactual invests the same after-tax C; neither side taxes
#      withdrawals, so the whole benefit is the untaxed inside buildup)
#
# Invariants (tested): r=0 & m0=mr -> traditional TE = 0; m0=0 -> both TEs = 0;
# both increasing in m0 and in horizon length.

pv_pieces <- function(age, m0, mr, r = PV_RETURN,
                      ret_age = PV_RET_AGE, end_age = PV_END_AGE) {
  n_acc <- max(0L, as.integer(ret_age - age))
  n_wd  <- max(PV_MIN_WITHDRAW_YRS, as.integer(end_age - max(age, ret_age)))
  disc  <- function(t) (1 + r)^(-t)

  ## retirement account: withdrawal-tax PV per $1 pre-tax contribution
  bal_R <- (1 + r)^n_acc
  wd    <- if (r == 0) bal_R / n_wd else bal_R * r / (1 - (1 + r)^(-n_wd))
  rwd1  <- 0; b <- bal_R
  for (t in seq_len(n_wd)) {
    w <- min(wd, b * (1 + r)); rwd1 <- rwd1 + mr * w * disc(n_acc + t)
    b <- b * (1 + r) - w
  }

  ## taxable counterfactual: annual-earnings-tax PV per $1 initial after-tax basis
  g <- r * (1 - m0)
  etax1 <- 0; bT <- 1
  for (t in seq_len(n_acc)) { etax1 <- etax1 + m0 * r * bT * disc(t); bT <- bT * (1 + g) }
  wdT <- bT / n_wd
  for (t in seq_len(n_wd)) {
    etax1 <- etax1 + m0 * r * bT * disc(n_acc + t)
    bT    <- max(0, bT * (1 + g) - wdT * (1 + g))
  }
  c(etax1 = etax1, rwd1 = rwd1)
}

te_trad <- function(C, age, m0, mr = m0, r = PV_RETURN) {
  if (is.na(C) || C <= 0 || is.na(m0) || is.na(age)) return(0)
  p <- pv_pieces(age, m0, mr, r)
  unname(m0 * C + (1 - m0) * C * p["etax1"] - C * p["rwd1"])
}

te_roth <- function(C, age, m0, r = PV_RETURN) {
  if (is.na(C) || C <= 0 || is.na(m0) || is.na(age)) return(0)
  unname(pv_pieces(age, m0, m0, r)["etax1"]) * C
}

## Grid-evaluated per-$1 factors over unique (age x mtr) pairs; returns lookup functions.
## upfront-excluded: fut_trad = TE_trad per $1 MINUS the m0 upfront (the arc legs replace it).
## mr_scale scales the WITHDRAWAL-year MTR relative to the contribution-year MTR (CBO's base
## assumption is mr = m0, i.e. mr_scale = 1; the sensitivity analysis rescales it alone).
pv_factor_grid <- function(ages, mtrs, r = PV_RETURN, mr_scale = 1) {
  grid <- expand.grid(age = sort(unique(ages)), m = sort(unique(round(mtrs, 4))))
  pc <- t(mapply(function(a, m) pv_pieces(a, m, m * mr_scale, r), grid$age, grid$m))
  grid$fut_trad  <- (1 - grid$m) * pc[, "etax1"] - pc[, "rwd1"]   # per $1 pre-tax, ex-upfront
  grid$te_trad1  <- grid$m + grid$fut_trad                        # per $1 pre-tax, total
  grid$te_roth1  <- pc[, "etax1"]                                 # per $1 after-tax basis
  key <- paste(grid$age, grid$m)
  function(a, m, what = c("fut_trad", "te_trad1", "te_roth1")) {
    what <- match.arg(what)
    out <- grid[[what]][match(paste(a, round(m, 4)), key)]
    ## Note: match() returns silent NA for an (age, mtr) pair not in the grid, and downstream
    ## sums use na.rm = TRUE -- an unguarded query would silently DROP dollars. Fail loud instead;
    ## queries on NA inputs must be masked by the caller (the contribB pattern in 10).
    if (anyNA(out)) stop("pv_factor_grid: ", sum(is.na(out)),
                         " lookup(s) outside the grid (age x mtr pair not built); ",
                         "mask the query to the vectors the grid was built from.")
    out
  }
}
