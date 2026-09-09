# Invariant checks on the built analysis frame and the Saver's Match summary.
suppressWarnings(suppressMessages({ library(testthat); library(arrow) }))
.root <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
.d <- as.data.frame(read_parquet(file.path(.root, "data", "processed", "sipp_fastfacts.parquet")))

test_that("I3 reclassification: no Unknown class; workers classified from any Dec job line 1-6", {
  expect_false(any(.d$worker_class == "Unknown"))
  expect_equal(nrow(.d), 12330L)                 # civilian labor force (military households dropped), RY2024
  expect_true(sum(.d$WPFINWGT) > 147e6)          # ~147.3M civilian workers 18-64 employed (RY2024)
})

test_that("I2 dependent-refined Saver's Match: dependents observed; eligibility_v2 <= v1", {
  s <- read.csv(file.path(.root, "output", "tables", "savers_match_summary.csv"))
  val <- function(k) s$value[s$metric == k]
  expect_gt(val("n_dep_obs"), 0)
  expect_lte(val("elig_any_v2_wt"), val("elig_any_v1_wt"))
  expect_gt(val("wedge_wt"), 0)
})

test_that("I1 employer-match dollars: matched-with-amount subset of matched; median in documented range", {
  n_matched  <- sum(.d$match_receipt)
  n_with_amt <- sum(.d$match_receipt & !is.na(.d$emp_contrib_amt))
  expect_lte(n_with_amt, n_matched)
  dol <- read.csv(file.path(.root, "output", "tables", "matching_dollars.csv"))
  med <- dol$value[dol$measure == "median_employer_$"]
  expect_gt(med, 0); expect_lte(med, 19999998)          # documented TECNTAMT maximum
})

test_that("I4 replicate-weight SEs: positive SE and CI brackets the estimate for H1", {
  se <- read.csv(file.path(.root, "output", "tables", "standard_errors.csv"))
  h1 <- se[se$measure == "H1 lacks employer access" & se$group == "All workers", ]
  expect_equal(nrow(h1), 1L)
  expect_gt(h1$se, 0)
  expect_lte(h1$ci_low, h1$estimate); expect_gte(h1$ci_high, h1$estimate)
})

