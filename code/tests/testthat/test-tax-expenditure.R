# Tests for the retirement tax-expenditure pipeline (08/09/10).
# Runs only when the pipeline outputs exist (mirrors the suite's pending-output pattern).

path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))   # TAX_YEAR / TAX_ENGINE (engine-aware)
path_processed <- file.path(path_project, "data", "processed")

pq <- function(f) file.path(path_processed, f)
have_08 <- file.exists(pq("te_persons.parquet")) && file.exists(pq("tax_units.parquet"))
have_09 <- file.exists(pq("taxsim_results.parquet"))

test_that("08: every December person belongs to exactly one tax unit; deciles are equal-person", {
  skip_if_not(have_08, "08 outputs not built")
  p <- as.data.frame(arrow::read_parquet(pq("te_persons.parquet")))
  expect_false(anyNA(p$unit_id))
  expect_false(anyNA(p$filing_unit_id))
  expect_false(anyNA(p$decile))
  sh <- tapply(p$WPFINWGT, p$decile, sum) / sum(p$WPFINWGT)
  expect_length(sh, 10)
  expect_true(all(abs(sh - 0.10) < 0.005))
  ## Note: the previous assertion here ended in `| TRUE` and could never fail. Real
  ## invariant: every non-filing dependent belongs to a filing unit that exists in the unit
  ## frame (headed by an adult or a dependent filer). Documented exception: dependents of
  ## NON-FILING dependents never reach the tax engine (10 zeroes their TE and prints the count).
  u_ids <- arrow::read_parquet(pq("tax_units.parquet"), col_select = "filing_unit_id")$filing_unit_id
  dep_nf <- p$is_dep & !p$dep_files
  expect_lt(mean(!(p$filing_unit_id[dep_nf] %in% u_ids)), 0.01)
})

test_that("08: tax-unit frame is TAXSIM-valid", {
  skip_if_not(have_08, "08 outputs not built")
  u <- as.data.frame(arrow::read_parquet(pq("tax_units.parquet")))
  expect_false(any(duplicated(u$taxsimid)))
  expect_true(all(u$mstat %in% c("single", "married, jointly", "dependent child")))
  expect_true(all(u$pwages >= 0 & u$swages >= 0))
  expect_true(all(u$depx >= 0))
  expect_true(all(u$year == TAX_YEAR))   # engine-aware: taxsim -> 2023 (engine frontier); policyengine -> REF_YEAR (2024)
  ## dependent-filer units claim no dependents of their own
  expect_true(all(u$depx[u$mstat == "dependent child"] == 0))
})

test_that("09: arc tax differences behave (repealing an exclusion cannot cut taxes en masse)", {
  skip_if_not(have_09, "09 outputs not built")
  r <- as.data.frame(arrow::read_parquet(pq("taxsim_results.parquet")))
  has_c <- with(r, c_ee_401 + c_ee_ira + c_ee_pen + c_er_401 + c_er_ira > 0)
  ## units with no contributions have exactly zero arc TE
  expect_true(all(abs(r$te_ee_arc[!has_c]) < 1e-6))
  expect_true(all(abs(r$te_er_arc[!has_c]) < 1e-6))
  ## negative arcs are possible (EITC phase-in oddities) but must be rare among contributors
  expect_lt(mean((r$te_ee_arc + r$te_er_arc)[has_c] < -1e-6), 0.02)
  ## aggregate arc TE is positive
  expect_gt(sum((r$te_ee_arc + r$te_er_arc) * r$wgt_unit), 0)
})

test_that("12: SE table is internally consistent", {
  se_path <- file.path(path_project, "output", "tables", "te_se.csv")
  skip_if_not(file.exists(se_path), "12 outputs not built")
  se <- read.csv(se_path)
  expect_true(all(se$se_busd >= 0))
  expect_true(all(se$ci95_lo <= se$estimate_busd & se$estimate_busd <= se$ci95_hi))
  dec <- se[grepl("^Decile", se$quantity), ]
  expect_equal(nrow(dec), 10)
  expect_equal(sum(dec$estimate_busd), 1, tolerance = 1e-3)   # shares sum to 1
  ## headline totals must be estimated far more precisely than their magnitude
  head_rows <- se[grepl("Combined TE", se$quantity), ]
  expect_true(all(head_rows$se_busd / abs(head_rows$estimate_busd) < 0.10))
})

test_that("14: cross-cut table is internally consistent", {
  cc_path <- file.path(path_project, "output", "tables", "te_crosscuts.csv")
  skip_if_not(file.exists(cc_path), "14 outputs not built")
  cc <- read.csv(cc_path)
  ## population shares and TE shares sum to ~1 within every dimension
  for (dm in unique(cc$dimension)) {
    s <- cc[cc$dimension == dm, ]
    expect_equal(sum(s$pop_share), 1, tolerance = 0.02, label = paste(dm, "pop_share"))
    expect_equal(sum(s$te_share),  1, tolerance = 0.02, label = paste(dm, "te_share"))
  }
  ## the access pillar must show the concentration the draft claims
  acc <- cc[cc$dimension == "Pillar 1: access", ]
  expect_gt(acc$te_share[acc$group == "Has employer access"], 0.90)
  expect_gt(acc$mean_te[acc$group == "Has employer access"],
            20 * acc$mean_te[acc$group == "Lacks employer access"])
})

test_that("10: decile table is internally consistent", {
  ## The pv_te() invariants (r=0 & m0=mr -> TE=0; MTR=0 -> TE=0; monotone in MTR and horizon)
  ## are enforced as stopifnot() guards inside 10_tax_expenditure.R at every run.
  tab_path <- file.path(path_project, "output", "tables", "te_decile.csv")
  skip_if_not(file.exists(tab_path), "10 outputs not built")
  tab <- read.csv(tab_path)
  expect_equal(nrow(tab), 10)
  expect_equal(sum(tab$income_share),  1, tolerance = 1e-6)
  expect_equal(sum(tab$payroll_share), 1, tolerance = 1e-6)
  expect_true(all(tab$payroll_te_B >= 0))                 # payroll leg is a nonneg wedge
  expect_true(all(diff(tab$total_te_B[4:10]) > 0))        # rises across upper deciles
  expect_false(anyNA(tab$pct_benefiting))
})
