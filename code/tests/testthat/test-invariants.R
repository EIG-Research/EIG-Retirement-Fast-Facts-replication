# Invariants + regression baselines for the v1 analysis frame.
# These lock current behavior; update the baselines DELIBERATELY when I3 (job-line reclassification)
# changes the universe.
suppressWarnings(suppressMessages({ library(arrow); library(testthat) }))
root <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(root, "code", "_shared", "helpers.R"))
d <- as.data.frame(read_parquet(file.path(root, "data", "processed", "sipp_fastfacts.parquet")))
d$.univ <- TRUE

test_that("sample size and weighted total match the locked baseline (post-I3 reclassification)", {
  # Baselines re-locked 2026-08-27: the military-household test in helpers.R was a coalesce of the
  # six job lines (first non-missing only) and is now an ANY across them, so 7 more households and
  # 11 more workers are correctly dropped (external RA review). Was 12341 / 147542786 under the
  # coalesce test, and 12423 / 148.6M before any military drop. SIPP 2025 vintage, reference year 2024.
  expect_equal(nrow(d), 12330L)                             # RMESR-employed w/ a Dec job, military households dropped
  expect_equal(sum(d$WPFINWGT), 147316148, tolerance = 1)   # ~147.3M civilian workers 18-64 employed, Dec ref
})

test_that("every worker has a class (no Unknown) after the Dec-job restriction", {
  expect_false(any(d$worker_class == "Unknown"))
  expect_setequal(unique(d$worker_class), c("Private", "Government", "Self-employed"))
})

test_that("all pillar measures are proportions in [0, 1]", {
  for (v in c("lacks_access", "access_emp", "participates", "match_receipt", "access_ownership")) {
    s <- wtd_share(d[[v]], d$.univ, d$WPFINWGT)$share
    expect_gte(s, 0); expect_lte(s, 1)
  }
})

test_that("pillar nesting holds: participation and matching are subsets of (raw) access", {
  acc  <- wtd_count(d$access_emp_raw, d$WPFINWGT)   # true 'has access' (pre self-employed forcing)
  part <- wtd_count(d$participates,   d$WPFINWGT)
  mat  <- wtd_count(d$match_receipt,  d$WPFINWGT)
  expect_lte(part, acc)
  expect_lte(mat,  acc)
})

test_that("headline H1 regression: 51.7% lack employer-provided access (offered+eligible)", {
  s <- wtd_share(d$lacks_access, d$.univ, d$WPFINWGT)$share
  expect_equal(s, 0.517, tolerance = 0.003)               # civilian labor force (RY2024), 0.5171 at the 2026-08-27 military fix; was 0.516 all-worker, 0.509 RY2023
})

test_that("self-employed are all coded as lacking employer-provided access (headline decision)", {
  se <- d$worker_class == "Self-employed"
  expect_true(all(d$lacks_access[se]))
})
