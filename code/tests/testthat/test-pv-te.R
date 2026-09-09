# Unit tests for the shared PV tax-expenditure module (code/_shared/pv_te.R).

path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
## params.R sources extract_vars.R relative to path_project, which exists here, so this is safe
source(file.path(path_project, "code", "_shared", "params.R"))
source(file.path(path_project, "code", "_shared", "pv_te.R"))

test_that("traditional TE limiting cases", {
  expect_equal(te_trad(1000, 40, 0.22, 0.22, r = 0), 0, tolerance = 1e-10)  # pure deferral, no return
  expect_equal(te_trad(1000, 40, 0), 0)                                     # zero MTR
  expect_equal(te_trad(0,    40, 0.22), 0)                                  # zero contribution
  expect_equal(te_trad(NA,   40, 0.22), 0)                                  # NA guarded
})

test_that("Roth TE is the inside-buildup benefit only", {
  expect_equal(te_roth(1000, 40, 0), 0)
  expect_gt(te_roth(1000, 40, 0.22), 0)
  ## Roth/traditional EQUIVALENCE: with discount = return and m0 = mr, $1,000 pre-tax in a
  ## traditional account and its after-tax-equivalent $780 in a Roth confer the SAME benefit
  ## (PV of level withdrawals at discount = growth returns exactly mr per $1, cancelling the
  ## upfront exclusion). The traditional advantage arises only from mr < m0 or bracket effects.
  expect_equal(te_roth(780, 40, 0.22), te_trad(1000, 40, 0.22), tolerance = 1e-9)
  expect_gt(te_trad(1000, 40, 0.24, mr = 0.12), te_roth(760, 40, 0.24))  # rate arbitrage breaks it
  ## Roth TE at r=0 is exactly zero (no returns, nothing to shelter)
  expect_equal(te_roth(1000, 40, 0.22, r = 0), 0, tolerance = 1e-10)
})

test_that("monotonicity in MTR, horizon, and return", {
  expect_gt(te_trad(1000, 40, 0.24), te_trad(1000, 40, 0.12))
  expect_gt(te_trad(1000, 30, 0.22), te_trad(1000, 60, 0.22))
  expect_gt(te_roth(1000, 30, 0.22), te_roth(1000, 60, 0.22))
  expect_gt(te_trad(1000, 40, 0.22, r = 0.06), te_trad(1000, 40, 0.22, r = 0.035))
})

test_that("withdrawal-MTR asymmetry has the right sign", {
  ## lower rate in retirement than at contribution -> larger benefit
  expect_gt(te_trad(1000, 40, 0.24, mr = 0.12), te_trad(1000, 40, 0.24, mr = 0.24))
})

test_that("grid factors agree with direct evaluation", {
  fg <- pv_factor_grid(c(30, 45, 60), c(0.12, 0.24))
  expect_equal(fg(45, 0.24, "te_trad1") * 1000, te_trad(1000, 45, 0.24), tolerance = 1e-8)
  expect_equal(fg(30, 0.12, "te_roth1") * 1000, te_roth(1000, 30, 0.12), tolerance = 1e-8)
  ## fut_trad + m0 = te_trad1 by construction
  expect_equal(fg(60, 0.24, "fut_trad") + 0.24, fg(60, 0.24, "te_trad1"), tolerance = 1e-10)
})

test_that("worker at or past retirement age still gets a defined window", {
  expect_gt(te_trad(1000, 70, 0.22), 0)   # withdraws over remaining years to 85
  expect_gt(te_trad(1000, 84, 0.22), 0)   # floor of PV_MIN_WITHDRAW_YRS applies
})
