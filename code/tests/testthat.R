# code/tests/testthat.R — test runner for the SIPP Fast Facts pipeline.
# Run:  "<Rscript>" code/tests/testthat.R   (or via run_all.R's run_tests flag)
suppressWarnings(suppressMessages(library(testthat)))
path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
res <- test_dir(file.path(path_project, "code", "tests", "testthat"),
                reporter = "summary", stop_on_failure = TRUE)
