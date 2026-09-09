# ==============================================================================
# 15_chart_specs.R  --  spec-driven chart-payload builder
#
# Builds the chart specification for every figure and table exhibit: title,
# intro, annotations, source line, token colors, and the tidy data rows. Each
# spec is written as a JSON payload under output/datawrapper/dryrun/.
#
# This is the authoritative source for three exhibits that have no counterpart in
# 05_make_figures.R or 13_te_figures.R:
#   te_fig1b_decile_share  -- tax-expenditure shares by income decile
#   access_gap_table       -- the appendix access-gap table
#   table1_definitions     -- the appendix definitions table
#
# Reads   : output/figures/*.csv     (tidy data from 05_ and 13_)
#           output/tables/*.csv      (analysis tables from 02_, 03_, 10_, 16_)
#           code/_shared/eig_style_public.R   (token colors)
# Writes  : output/datawrapper/dryrun/<figure_key>.json
#
# NOTE ON SCOPE: internally these payloads are pushed to Datawrapper to render
# the published charts. The publishing half of this script (API authentication,
# chart create/update, PNG export, and the chart-id registry) is EIG-internal
# tooling and is not part of this replication package. What remains is the
# payload construction, which is the part that determines what each exhibit
# shows. The published renders are in output/figures/ and output/datawrapper/.
#
# Run: Rscript code/15_publish_datawrapper.R
# ==============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

# ---- 0. Config ---------------------------------------------------------------
PALETTE_MODE <- "primary_2022"
# Optional comma-separated figure_key filter, e.g. DW_ONLY="fig2_income_ladder".
ONLY <- Sys.getenv("DW_ONLY", "")
ONLY <- if (nzchar(ONLY)) trimws(strsplit(ONLY, ",")[[1]]) else character(0)

# ---- 1. Paths ----------------------------------------------------------------
.find_root <- function() {
  a <- commandArgs(trailingOnly = FALSE); f <- grep("^--file=", a, value = TRUE)
  if (length(f) == 1) return(dirname(dirname(normalizePath(sub("^--file=", "", f),
    winslash = "/", mustWork = FALSE))))
  getwd()
}
ROOT       <- .find_root()
FIG_DIR    <- file.path(ROOT, "output", "figures")
STYLE_FILE <- file.path(ROOT, "code", "_shared", "eig_style_public.R")
DW_OUT     <- file.path(ROOT, "output", "datawrapper")
DRYRUN_DIR <- file.path(DW_OUT, "dryrun")
for (d in c(DW_OUT, DRYRUN_DIR))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
RUN_TS <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

# ---- 2. Token-derived colors -------------------------------------------------
TOK <- local({
  env <- new.env(parent = baseenv())
  sys.source(STYLE_FILE, envir = env)
  tk <- env$eig_load_tokens()
  list(version = tk$EIG_TOKEN_VERSION, hex = tk$EIG_COLORS)
})
GREEN <- unname(TOK$hex[["eig_green_700"]])   # #19644D
GOLD  <- unname(TOK$hex[["eig_gold_600"]])    # #E1AD28
GREY  <- "#6E6E6E"  # documented neutral (no 2022 token); darkened from #8C8C8C so
                    # white labels on grey clear WCAG AA and grey separates from gold

# SRC_SIPP / SRC_TE and the vintage constants (SIPP_PANEL, REF_YEAR) come from the single
# source of truth (params.R) so a vintage bump propagates here automatically -- no standalone
# literals to drift. params.R is pure constants (loads no data), safe to source here.
path_project <- ROOT
source(file.path(ROOT, "code", "_shared", "params.R"))
# "December 2024" as a DERIVED label, not a standalone literal: a vintage bump in
# params.R propagates into every chart intro/annotation that names the reference month.
REF_LABEL <- paste0(month.name[REF_MONTH], " ", REF_YEAR)
readf <- function(key) read_csv(file.path(FIG_DIR, paste0(key, ".csv")),
  show_col_types = FALSE)

# Format helper: returns visualize number-format keys for a chart type + fmt.
# Datawrapper uses numeral.js-style tokens ("0%", "$0,0"), NOT d3-format.
# In this team theme "%" appends the sign WITHOUT multiplying by 100, so percent
# data is pre-multiplied by 100 (see the post-processing block after build_specs).
fmt_keys <- function(type, fmt) {
  code <- switch(fmt, pct = "0%", pct1 = "0.0%", dollar = "$0,0",
                 dollarB = "$0,0", NULL)
  if (is.null(code)) return(list())
  # Value axis is x for horizontal bars, dot plots, and range/dumbbell plots.
  axis_key <- if (type %in% c("d3-bars", "d3-bars-stacked", "d3-dot-plot",
                              "d3-range-plot"))
    "x-grid-format" else "y-grid-format"
  out <- list(); out[["value-label-format"]] <- code; out[[axis_key]] <- code
  out
}

# Informative decile-axis endpoints (author request 2026-09-08): Datawrapper has no axis
# titles, so the endpoint categories carry the scale's meaning ("Bottom 10%" ... "Top 10%")
# and intros drop the "(1 = lowest)" decoder. NBSP ( ) keeps each label on one line —
# DW floats a negative group's label above the baseline, and a two-line label there is
# obtrusive. Charts using these must also set `rotate-labels` = "off": the "auto" default
# flips EVERY category label vertical once one label is long (both diagnosed on te_fig1).
LAB_BOTTOM10 <- "Bottom 10%"
LAB_TOP10    <- "Top 10%"
decile_labels <- function(x) dplyr::case_when(
  as.integer(x) == 1L  ~ LAB_BOTTOM10,
  as.integer(x) == 10L ~ LAB_TOP10,
  TRUE                 ~ as.character(x))
relabel_decile_cols <- function(d) {   # for pivoted wide frames (columns "1".."10")
  names(d)[names(d) == "1"]  <- LAB_BOTTOM10
  names(d)[names(d) == "10"] <- LAB_TOP10
  d
}

# ==============================================================================
# 4. FIGURE SPECS
# Each: figure_key, type, title, intro, annotate, source, data (df),
#       colors (named col->hex or NULL), fmt, h (export height px).
# ==============================================================================
build_specs <- function() {
  specs <- list()

  # ---- fig4_savers_match_wedge (pilot; single 100%-stacked bar) --------------
  local({
    r <- readf("fig4_savers_match_wedge")
    mil <- setNames(r$count_millions, r$segment)
    ## 1:1 with the PNG's in-segment labels (05): full descriptive segment names. Fresh column
    ## names also shed the chart's old UI-side series rename ("No Qualifying Account"), which was
    ## keyed to the previous column name and survived API updates.
    ## Comma-free labels: a comma inside a column name interferes with the series color-key
    ## matching on upload (round-2 fix; the parenthetical-with-comma version fell back to
    ## auto-shaded default colors).
    lock_lab  <- "Locked out — no qualifying account"
    claim_lab <- "Can claim — has a qualifying account"
    d <- tibble(Group = "Income-eligible workers")
    d[[lock_lab]]  <- round(unname(mil["Locked out"]), 1)
    d[[claim_lab]] <- round(unname(mil["Can claim"]),  1)
    fig4_colors <- setNames(list(GOLD, GREY), c(lock_lab, claim_lab))
    specs[["fig4_savers_match_wedge"]] <<- list(
      type = "d3-bars-stacked",
      title = "Eligible for the Saver's Match, but locked out",
      intro = sprintf("Of %s million income-eligible workers, %s million lack a qualifying account to receive it",
                      round(sum(r$count_millions), 1), round(unname(mil["Locked out"]), 1)),
      annotate = "The Saver's Match is a federal matching contribution for eligible lower-income savers, beginning in 2027. A qualifying account (a 401(k)-type or IRA) is needed to receive it; defined-benefit pensions cannot receive the match. Counts in millions of workers; shares are of the income-eligible. Eligibility is approximated: SIPP income proxies AGI, filing status is the prior tax year, and statutory thresholds are applied unindexed to a 2024 population.",
      source = SRC_SIPP, data = d,
      colors = fig4_colors,
      ## d3-bars-stacked reads series colors from color-category$map, NOT custom-colors
      ## (diagnosed against live metadata, round 3): without this the renamed series fall
      ## back to auto-tinted defaults.
      visualize = list(`stack-percentages` = TRUE,
                       `color-category` = list(map = fig4_colors)),
      fmt = NULL, h = 260)
  })

  # ---- fig1_access_by_class (horizontal bars; gold highlight) ----------------
  local({
    r <- readf("fig1_access_by_class")
    ord <- c("All workers", "Self-employed", "Private", "Government",
             "Part-time", "Full-time")
    r <- r %>% mutate(group = factor(group, levels = ord)) %>% arrange(group)
    d <- tibble(Category = as.character(r$group), Share = r$share)
    # Blank spacer rows separate the three groups (total / class / schedule),
    # which a flat bar list otherwise reads as six parallel categories.
    sep <- function(pad) tibble(Category = strrep(" ", pad), Share = NA_real_)
    # Rows selected BY CATEGORY NAME, not position: a new category row in the
    # CSV would silently shift positional slices into the wrong visual group.
    stopifnot(setequal(d$Category, ord))
    d <- bind_rows(d[d$Category == "All workers", ], sep(1),
                   d[d$Category %in% c("Self-employed", "Private", "Government"), ], sep(2),
                   d[d$Category %in% c("Part-time", "Full-time"), ])
    specs[["fig1_access_by_class"]] <<- list(
      type = "d3-bars",
      title = "Half of workers lack retirement-plan access",
      intro = "Share of workers without access to an employer-provided retirement plan",
      annotate = "Gold marks the all-worker total; grey marks the self-employed, who lack employer access by definition. Grouped by worker class (Self-employed, Private, Government) and by work schedule (Part-time, Full-time).",
      source = SRC_SIPP, data = d,
      # Gold = the all-worker headline bar; grey mutes the definitional self-employed 100%.
      colors = list(`All workers` = GOLD, `Self-employed` = GREY),
      base_color = GREEN, visualize = list(), fmt = "pct1", h = 460)
  })

  # ---- fig2_income_ladder (grouped columns) ---------------------------------
  # Datawrapper grouped columns: x-groups = columns, series = rows. So the data
  # is transposed vs. ggplot: rows = the 3 measures (series), columns = deciles.
  # Spec re-synced 2026-09-08: the CSV carries THREE series on a
  # private-sector-employee population (2026-08-31 Figure 2 rebuild); the prior
  # spec still described the old two-series all-worker/employee mix.
  local({
    d <- readf("fig2_income_ladder") %>% arrange(earn_decile) %>%
      pivot_wider(names_from = earn_decile, values_from = share)  # measure | 1..10
    stopifnot(setequal(d$measure, c("Lacks employer access", "Doesn't participate",
                                    "No employer contribution")))
    d <- relabel_decile_cols(d)   # informative endpoints (shared helper; see its header notes)
    specs[["fig2_income_ladder"]] <<- list(
      type = "grouped-column-chart",
      title = "Low earners lag on access, participation, and contributions",
      intro = "Share of private-sector employees who lack each, by earnings decile",
      annotate = "“Doesn't participate” and “No employer contribution” are one minus the participation and employer-contribution-receipt rates. All three series are among private-sector employees only (the self-employed and government workers are excluded). Deciles are of individual annual earnings among all workers (columns 2–9 are the middle deciles, lowest to highest earnings), so decile bins hold unequal numbers of private-sector employees.",
      source = SRC_SIPP, data = d,
      colors = list(`Lacks employer access` = GREEN, `Doesn't participate` = GREY,
                    `No employer contribution` = GOLD),
      visualize = list(`rotate-labels` = "off"), fmt = "pct", h = 420)
  })

  # ---- fig2b_participation_without_match (column chart, gold) ----------------
  # 1:1 replication of 05's Figure 2B: participation among private-sector employees
  # with no employer contribution, by earnings decile. Caption values interpolated
  # from the current tables (by convention: never restated as literals).
  local({
    r <- readf("fig2b_participation_without_match") %>% arrange(earn_decile)
    half <- readr::read_csv(file.path(ROOT, "output", "tables",
      "participation_given_no_match_half_private.csv"), show_col_types = FALSE)
    pb <- half$share[half$group == "Bottom half (deciles 1-5)"]
    pt <- half$share[half$group == "Top half (deciles 6-10)"]
    stopifnot(length(pb) == 1, length(pt) == 1, pb > 0)
    specs[["fig2b_participation_without_match"]] <<- list(
      type = "column-chart",
      title = "High earners are far likelier to participate without a match",
      intro = "Participation rate among those with no employer contribution, by earnings decile",
      annotate = sprintf(paste0(
        "Denominator is private-sector employees who receive no employer contribution; ",
        "numerator is the share of that group who still participate in a plan. Bottom-half ",
        "(deciles 1–5) employees participate at %.1f percent versus %.1f percent for top-half ",
        "(deciles 6–10) employees — about %.0f times as likely. Deciles are of individual annual ",
        "earnings among all workers. Decile-level bars rest on small unweighted participant ",
        "counts (as few as %d in the lowest decile); treat decile-to-decile contrasts as noisy."),
        100 * pb, 100 * pt, pt / pb, min(r$n_num)),
      source = SRC_SIPP,
      data = tibble(Decile = decile_labels(r$earn_decile), `Participation rate` = r$share),
      colors = NULL, base_color = GOLD,
      ## Per-bar value labels, matching the PNG. Diagnosed against the live renderer (2026-09-08):
      ## column-chart honors the camelCase `valueLabels` object; the kebab-case variants alone do
      ## not render. Both are sent so the metadata stays coherent either way.
      visualize = list(valueLabels = list(show = "always", enabled = TRUE, placement = "outside"),
                       `value-labels` = list(show = "always", enabled = TRUE),
                       `rotate-labels` = "off"),
      fmt = "pct1", h = 400)
  })

  # ---- fig3alt_access_by_race_educ (flat bars, race section headers) ---------
  # 1:1-as-possible replication of 05's four-panel grid: Datawrapper has no facets,
  # so the four race/ethnicity panels become UPPERCASE header rows over their four
  # education bars (the fig1 spacer technique), preserving the per-cell gold
  # highlight for cells above the private-sector lacks-access benchmark. Education
  # labels are suffixed with 0-3 non-breaking spaces so identical labels stay
  # unique across sections (Datawrapper keys row colors by label).
  local({
    r <- readf("fig3alt_access_by_race_educ")
    acc_hd <- readr::read_csv(file.path(ROOT, "output", "tables", "access_headline.csv"),
                              show_col_types = FALSE)
    nat <- acc_hd$share[acc_hd$dimension == "worker_class" & acc_hd$group == "Private"]
    stopifnot(length(nat) == 1)
    races <- c("White (NH)", "Black (NH)", "Hispanic", "Asian (NH)")
    stopifnot(setequal(unique(r$race), races))
    rows <- list(); colmap <- list(); sp <- 0L
    for (i in seq_along(races)) {
      if (i > 1) { sp <- sp + 1L
        rows[[length(rows) + 1]] <- tibble(Category = strrep(" ", sp), Share = NA_real_) }
      rows[[length(rows) + 1]] <- tibble(Category = toupper(races[i]), Share = NA_real_)
      sub <- r %>% filter(race == races[i])
      for (j in seq_len(nrow(sub))) {
        lab <- paste0(sub$educ[j], strrep(" ", i - 1))
        rows[[length(rows) + 1]] <- tibble(Category = lab, Share = sub$lacks_share[j])
        colmap[[lab]] <- if (sub$hue[j] == "above_avg") GOLD else GREEN
      }
    }
    d3a <- bind_rows(rows)
    blk <- r %>% filter(race == "Black (NH)",
                        educ %in% c("Less than high school", "High school graduate"))
    small <- r %>% filter(n_den < 45) %>% mutate(cell = paste0(race, ", ", tolower(educ)))
    specs[["fig3alt_access_by_race_educ"]] <<- list(
      type = "d3-bars",
      title = "Lack of employer plan access falls with education",
      intro = "Share of private-sector employees lacking plan access, by education, within race/ethnicity",
      annotate = sprintf(paste0(
        "Gold bars sit above the %.1f%% private-sector average lacking access; green bars are at ",
        "or below it. Private-sector employees only (government and self-employed workers ",
        "excluded, so the population matches the private-sector benchmark). “Other (NH)” ",
        "race/ethnicity is excluded (smallest, most heterogeneous category); “NH” denotes ",
        "non-Hispanic. For Black (NH) workers, the two lowest education groups have nearly ",
        "identical lack rates (%.1f%% vs. %.1f%%) rather than a clear step down; the smallest ",
        "cells (%s) have unweighted n below 45, so treat those bars as noisier than the rest."),
        100 * nat,
        100 * blk$lacks_share[blk$educ == "Less than high school"],
        100 * blk$lacks_share[blk$educ == "High school graduate"],
        paste(small$cell, collapse = "; ")),
      source = SRC_SIPP, data = d3a,
      colors = colmap, base_color = GREEN,
      visualize = list(), fmt = "pct1", h = 900)   # tall enough that the note never clips
  })

  # ---- fig3a/b match by group (two bar charts) ------------------------------
  local({
    r <- readf("fig3_match_by_group")
    ra <- r %>% filter(panel == "By race and ethnicity") %>%
      arrange(desc(match_receipt_share))
    ed <- r %>% filter(panel == "By education") %>%
      arrange(desc(match_receipt_share))
    specs[["fig3a_match_by_race"]] <<- list(
      type = "d3-bars",
      title = "Who gets an employer contribution or match, by race and ethnicity",
      intro = "Share of employees receiving any employer contribution to an employer-provided retirement plan",
      annotate = "“Contribution” is any employer money reported to a 401(k)-type or IRA/Keogh account — SIPP does not distinguish matching from non-matching employer contributions. The lowest group is highlighted in gold. “NH” denotes non-Hispanic.",
      source = SRC_SIPP,
      data = tibble(Group = ra$group, Share = ra$match_receipt_share),
      # Highlight the lowest-receipt group in gold so color carries the disparity
      # (COMPUTED, not hardcoded: the lowest group can change across vintages).
      colors = setNames(list(GOLD), ra$group[which.min(ra$match_receipt_share)]),
      base_color = GREEN,
      visualize = list(), fmt = "pct1", h = 340)
    specs[["fig3b_match_by_education"]] <<- list(
      type = "d3-bars",
      title = "Who gets an employer contribution or match, by education",
      intro = "Share of employees receiving any employer contribution to an employer-provided retirement plan",
      annotate = "“Contribution” is any employer money reported to a 401(k)-type or IRA/Keogh account — SIPP does not distinguish matching from non-matching employer contributions. The lowest group is highlighted in gold.", source = SRC_SIPP,
      data = tibble(Group = ed$group, Share = ed$match_receipt_share),
      colors = setNames(list(GOLD), ed$group[which.min(ed$match_receipt_share)]),
      base_color = GREEN,
      visualize = list(), fmt = "pct1", h = 320)
  })

  # ---- te_fig1_decile_total (grouped columns, $B) ---------------------------
  # Transposed: rows = the 2 measures (series), columns = deciles (x-groups).
  # 1:1 with 13's PNG: full series name for the CBO-comparable measure, the PNG's
  # title/subtitle, and its decile note + negative-decile-1 line.
  local({
    d <- readf("te_fig1_decile_total") %>%
      mutate(total_te_B = round(total_te_B, 1)) %>%
      arrange(decile) %>%
      pivot_wider(names_from = decile, values_from = total_te_B)  # measure | 1..10
    d <- relabel_decile_cols(d)   # informative endpoints (shared helper; see its header notes)
    nm_cbo <- "CBO-comparable (incl. imputed DB, Roth, IRA)"
    stopifnot(setequal(d$measure, c("Observed flows", nm_cbo)))
    specs[["te_fig1_decile_total"]] <<- list(
      type = "grouped-column-chart",
      title = "The retirement tax expenditure flows overwhelmingly to top-income deciles",
      intro = "Total retirement tax expenditure by income decile, billions of dollars, 2024",
      annotate = "Deciles are of size-adjusted household income before transfers and taxes; each contains an equal number of people (columns 2–9 are the middle deciles, poorest to richest). The bottom decile is slightly negative under both measures.",
      source = SRC_TE, data = d,
      colors = setNames(list(GREEN, GOLD), c("Observed flows", nm_cbo)),
      ## rotate-labels "auto" flips ALL category labels vertical once one label is long;
      ## force horizontal (the one-line endpoint labels fit at this width).
      visualize = list(`rotate-labels` = "off"), fmt = "dollarB", h = 430)
  })

  # ---- te_fig1b_decile_share (grouped columns, % of each measure's total) ----
  # Companion to te_fig1 (author request 2026-09-08): the same two measures as SHARES of
  # each measure's own total, which makes the observed-vs-CBO-comparable CONTRAST readable
  # (the imputed DB/Roth/IRA flows sit lower in the distribution, so the CBO-comparable
  # series is less top-concentrated than observed flows). Shares computed from the same
  # CSV as te_fig1, so the two charts cannot drift apart; headline share interpolated,
  # never restated as a literal, by convention.
  local({
    r <- readf("te_fig1_decile_total")
    nm_cbo <- "CBO-comparable (incl. imputed DB, Roth, IRA)"
    stopifnot(setequal(unique(r$measure), c("Observed flows", nm_cbo)))
    r <- r %>% group_by(measure) %>%
      mutate(share = total_te_B / sum(total_te_B)) %>% ungroup()
    top_obs <- r$share[r$measure == "Observed flows" & r$decile == 10]
    top_cbo <- r$share[r$measure == nm_cbo & r$decile == 10]
    d <- r %>% select(measure, decile, share) %>% arrange(decile) %>%
      pivot_wider(names_from = decile, values_from = share)  # measure | 1..10
    d <- relabel_decile_cols(d)
    specs[["te_fig1b_decile_share"]] <<- list(
      type = "grouped-column-chart",
      ## Title quotes the HEADLINE (CBO-comparable) measure; both series' exact shares are in
      ## the note.
      title = sprintf("The top income decile captures about %.0f percent of the retirement tax expenditure",
                      100 * top_cbo),
      intro = "Share of each measure's total retirement tax expenditure by income decile, 2024",
      annotate = sprintf(paste0(
        "Shares sum to 100 percent within each series. Deciles are of size-adjusted household ",
        "income before transfers and taxes; each contains an equal number of people (columns ",
        "2–9 are the middle deciles, poorest to richest). The bottom decile is slightly ",
        "negative under both measures. Adding the imputed DB, Roth, and IRA flows makes the ",
        "distribution somewhat less top-concentrated: the top decile's share is %.0f percent ",
        "of observed flows versus %.0f percent of the CBO-comparable total."),
        100 * top_obs, 100 * top_cbo),
      source = SRC_TE, data = d,
      colors = setNames(list(GREEN, GOLD), c("Observed flows", nm_cbo)),
      visualize = list(`rotate-labels` = "off"), fmt = "pct", h = 430)
  })

  # ---- te_fig2_share_vs_cbo (dot plot, 3 series) ----------------------------
  local({
    r <- readf("te_fig2_share_vs_cbo") %>%
      pivot_wider(names_from = series, values_from = share) %>% arrange(quintile)
    nm_cboc <- paste0("SIPP ", SIPP_PANEL, " CBO-comparable")  # vintage-driven (params.R)
    nm_obs  <- paste0("SIPP ", SIPP_PANEL, " observed")
    d <- tibble(Quintile = r$quintile, `CBO 2019` = r$`CBO 2019`)
    d[[nm_cboc]] <- r[[nm_cboc]]
    d[[nm_obs]]  <- r[[nm_obs]]
    specs[["te_fig2_share_vs_cbo"]] <<- list(
      type = "d3-dot-plot",
      ## Note (2026-09-08): "closely matches", not "replicates" -- the two series differ by five
      ## years and income-concept details; the annotate names the gaps (mirrors 13's caption).
      title = paste0("SIPP ", REF_YEAR, " closely matches CBO's 2019 distribution"),
      intro = "Share of the income-tax retirement expenditure accruing to each income quintile",
      annotate = "Quintiles are of size-adjusted household income before transfers and taxes. CBO-comparable adds imputed DB, Roth, and IRA flows to match CBO's measure. The two series differ by five years and by income-concept details (no imputed Medicare value or realized capital gains in the SIPP ranking; survey top incomes are compressed relative to CBO's tax-return data). CBO, The Distribution of Major Tax Expenditures in 2019 (2021).",
      source = SRC_TE, data = d,
      colors = setNames(list(GOLD, GREEN, GREY), c("CBO 2019", nm_cboc, nm_obs)),
      # Dot plots do not auto-show a legend; enable the color key so the three
      # series are identifiable without relying on color discrimination alone.
      visualize = list(`show-color-key` = TRUE, `color-key-enabled` = TRUE,
                       `color-key` = list(enabled = TRUE)),
      fmt = "pct", h = 420)
  })

  # ---- te_fig3a/b benefiting + mean (two column charts) ---------------------
  local({
    r <- readf("te_fig3_benefiting_mean") %>% arrange(decile)
    specs[["te_fig3a_pct_benefiting"]] <<- list(
      type = "column-chart",
      title = "Who benefits from the retirement tax expenditure",
      intro = "Share of people with any benefit, by income decile, CBO-comparable measure, 2024",
      annotate = "Deciles are of size-adjusted household income before transfers and taxes (columns 2–9 are the middle deciles, poorest to richest).",
      source = SRC_TE,
      data = tibble(Decile = decile_labels(r$decile), `Share benefiting` = r$pct_benefiting),
      colors = NULL, base_color = GREEN, visualize = list(`rotate-labels` = "off"),
      fmt = "pct", h = 400)
    specs[["te_fig3b_mean_benefit"]] <<- list(
      type = "column-chart",
      title = "And by how much",
      intro = "Mean benefit among beneficiaries ($), by income decile, CBO-comparable measure, 2024",
      annotate = "Deciles are of size-adjusted household income before transfers and taxes (columns 2–9 are the middle deciles, poorest to richest).",
      source = SRC_TE,
      data = tibble(Decile = decile_labels(r$decile), `Mean benefit` = round(r$mean_te_benef)),
      colors = NULL, base_color = GOLD, visualize = list(`rotate-labels` = "off"),
      fmt = "dollar", h = 400)
  })

  # ---- te_fig4a/b crosscut (column + grouped columns) -----------------------
  local({
    r <- readf("te_fig4_crosscut")
    dec <- r %>% filter(dimension == "Worker earnings decile") %>%
      mutate(decile = as.integer(group)) %>% arrange(decile)
    specs[["te_fig4a_mean_by_decile"]] <<- list(
      type = "column-chart",
      title = "The tax subsidy rises steeply with earnings",
      intro = "Mean retirement tax benefit per worker ($/year), by worker earnings decile, 2024",
      annotate = "Tax benefit = the present value of federal income- and payroll-tax savings on a worker's 2024 retirement contributions (CBO method), including an estimate of employer pension (defined-benefit) contributions. Columns 2–9 are the middle earnings deciles, lowest to highest.",
      source = SRC_TE,
      data = tibble(Decile = decile_labels(dec$decile), `Mean tax benefit` = round(dec$mean_te)),
      colors = NULL, base_color = GREEN, visualize = list(`rotate-labels` = "off"),
      fmt = "dollar", h = 400)
    # pillar dot plot: one row per pillar, two dots (grey = workers,
    # gold = dollars). The horizontal gap is the disproportion. Dot plots honor
    # custom-colors, show a legend, and give a readable value axis — which the
    # grouped-column and range-plot (broken endpoint colors) forms did not.
    pil <- r %>% filter(grepl("^Pillar", dimension))
    ## Note: SIPP's EECNTYN_* is ANY employer contribution, not specifically a match.
    labmap <- c("Pillar 1: access" = "Has employer access",
                "Pillar 2: participation" = "Participates",
                "Pillar 3: matching" = "Receives employer contribution or match")
    pil <- pil %>% mutate(P = factor(labmap[dimension], levels = unname(labmap))) %>%
      arrange(P)
    d4b <- tibble(Pillar = as.character(pil$P),
                  `Share of workers` = pil$pop_share,
                  `Share of subsidy dollars` = pil$te_share)
    specs[["te_fig4b_workers_vs_dollars"]] <<- list(
      type = "d3-dot-plot",
      title = "The tax subsidy follows access, participation, and matching",
      intro = "Share of workers vs. share of subsidy dollars, by pillar, 2024",
      annotate = "Each pillar shows the group that has the benefit; the gap between the dots is how far the dollars outrun the people.",
      source = SRC_TE, data = d4b,
      colors = list(`Share of workers` = GREY, `Share of subsidy dollars` = GOLD),
      visualize = list(`show-color-key` = TRUE, `color-key-enabled` = TRUE,
                       `color-key` = list(enabled = TRUE), `show-value-labels` = TRUE),
      fmt = "pct", h = 300)
  })

  # ---- access_gap_table (Census-style descriptive TABLE) --------------------
  # A native Datawrapper TABLE (not a chart) of the access gap across every
  # subset, sourced from output/tables/access_gap_descriptive.csv (16_...).
  # Panel headers and the overall row are bolded via markdown; percent columns
  # are pre-multiplied x100 per the team theme "%" quirk (see fmt notes above).
  local({
    desc <- readr::read_csv(file.path(ROOT, "output", "tables",
      "access_gap_descriptive.csv"), show_col_types = FALSE)
    # Values are pre-formatted to text so cells display verbatim: percent columns
    # avoid the team-theme "%" quirk, and section-header rows show blank (not "NA").
    fM <- function(x) sprintf("%.1f", x / 1e6)      # millions, 1 dp
    fP <- function(x) sprintf("%.1f%%", x * 100)     # percent, 1 dp
    mkrow <- function(label, r) tibble(
      `Worker characteristic`     = label,
      `Workers (millions)`        = if (nrow(r)) fM(r$workers_weighted) else "",
      `Share of all workers`      = if (nrow(r)) fP(r$share_of_workers) else "",
      `Lacking access (millions)` = if (nrow(r)) fM(r$lacking_access_weighted) else "",
      `Lacking-access rate`       = if (nrow(r)) fP(r$lacking_access_rate) else "",
      `Share of total gap`        = if (nrow(r)) fP(r$share_of_total_gap) else "")
    disp <- function(p) sub(" \\(1 = lowest\\)$", "", p)  # shorten decile panel label
    # Boundary separation via the data (this team's table renderer honors the global
    # `striped` toggle but ignores per-row/per-column style overrides sent through the
    # API — verified against live metadata). So each demographic section is set off by
    # a blank spacer row (the fig1 technique) and an UPPERCASE section header, which,
    # with zebra striping, reads as a clear boundary between panels.
    sp_n <- 0L
    spacer <- function() { sp_n <<- sp_n + 1L
      mkrow(strrep(" ", sp_n), desc[0, ]) }   # unique blank-ish label, empty cells
    # Focus panels (drops Disability status and Annual earnings decile).
    KEEP_PANELS <- c("Class of worker", "Work schedule", "Sex",
                     "Race and ethnicity", "Educational attainment", "Age")
    out <- mkrow("All workers", desc[desc$panel == "All workers", ])
    for (p in intersect(KEEP_PANELS, unique(desc$panel))) {
      out <- bind_rows(out, spacer(), mkrow(toupper(disp(p)), desc[0, ]))
      sub <- desc[desc$panel == p, ]
      for (i in seq_len(nrow(sub))) out <- bind_rows(out, mkrow(sub$group[i], sub[i, ]))
    }
    # Every column is text (pre-formatted); force text so DW does not re-parse.
    data_meta <- list(`column-format` = setNames(
      lapply(names(out), function(nm) list(type = "text")), names(out)))
    colcfg <- list(`Worker characteristic` = list(width = 0.34))
    specs[["access_gap_table"]] <<- list(
      type = "tables",
      title = "The retirement access gap across the workforce",
      intro = paste0("Share of workers who lack employer-provided access to a retirement plan, by worker characteristic. Employed workers ages 18–64, ", REF_LABEL, "."),
      annotate = "“Lacking access” means the worker lacks employer-provided access to a qualifying plan (one the employer offers and the worker is eligible for); the self-employed are counted as lacking employer access by construction. Lacking-access rate is the share within each group. Share of all workers and share of total gap are shares of the full population and may not sum to 100 percent within a characteristic because a few workers do not report it.",
      source = SRC_SIPP, data = out, colors = NULL,
      data_meta = data_meta,
      visualize = list(columns = colcfg, striped = TRUE,
        perPage = 100, pagination = list(enabled = FALSE)),
      fmt = NULL, h = 1350, w = 760)
  })

  # ---- table1_definitions (appendix Table 1: how measures are defined) -------
  # A native Datawrapper TABLE mirroring the appendix "Table 1". The Workers and
  # Share columns are pulled LIVE from the pipeline outputs so the table tracks the
  # vintage; the rule/universe columns are static definitions.
  local({
    rd  <- function(f) readr::read_csv(file.path(ROOT, "output", "tables", f), show_col_types = FALSE)
    ah  <- rd("access_headline.csv");            aad <- rd("access_alt_definitions.csv")
    paw <- rd("participation_all_workers.csv");  pga <- rd("participation_given_access.csv")
    maw <- rd("matching_all_workers.csv");       mgp <- rd("matching_given_participation.csv")
    md  <- rd("matching_dollars.csv")
    ov  <- function(d) d[d$dimension == "Overall", ][1, ]
    fM  <- function(x) sprintf("%.1fM", x / 1e6)
    fP  <- function(x) sprintf("%.1f%%", x * 100)
    pop       <- ov(ah)$wt_den
    lacks     <- ov(ah)
    offeronly <- aad[aad$definition == "H3 offer-only (lacks)"      & aad$dimension == "Overall", ][1, ]
    owner     <- aad[aad$definition == "H4 ownership DC/IRA (lacks)" & aad$dimension == "Overall", ][1, ]
    part      <- ov(paw); part_ga <- ov(pga)$share
    matc      <- ov(maw); matc_gp <- ov(mgp)$share
    medamt    <- md$value[md$measure == "median_employer_$"]
    t1 <- tibble::tribble(
      ~`Measure`, ~`SIPP rule (variables)`, ~`Universe (per data dictionary)`, ~`Workers`, ~`Share*`,
      "Working population (denominator)",
        "MONTHCODE = 12; ages 18–64; employed (RMESR ∈ 1–5); has a December job; weighted by WPFINWGT",
        paste0("All employed workers ages 18–64 in the ", REF_LABEL, " reference month"),
        fM(pop), "100%",
      "Lacks access — offered and eligible (headline)",
        "NOT [ holds a plan through the employer (EMJOB_401 / IRA / PEN = Yes) OR employer sponsors a plan and the worker is included (EPENSNYN = Yes and EINCPENS = Yes) ]; the self-employed count as lacking",
        "Offer and inclusion are asked of workers with a December job who do not already hold an employer plan",
        fM(lacks$wt_num), fP(lacks$share),
      "Lacks access — offer-only (companion)",
        "NOT [ holds a plan through the employer (EMJOB_401 / IRA / PEN = Yes) OR employer sponsors any plan (EPENSNYN = Yes) ], regardless of the worker's own eligibility",
        "Workers with a December job",
        fM(offeronly$wt_num), fP(offeronly$share),
      "Lacks a qualifying account — ownership (companion)",
        "NOT [ holds a DC or IRA account: EOWN_THR401 = Yes OR EOWN_IRAKEO = Yes ]; not employer-dependent (own IRAs count)",
        "Persons 15+ as of the reference period's last day",
        fM(owner$wt_num), fP(owner$share),
      "Participates (contributes)",
        "Contributes to an employer plan: ESCNTYN_401 / IRA / PEN = Yes",
        "Workers who hold a plan through their employer (EMJOB_* = Yes)",
        fM(part$wt_num), paste0(fP(part$share), " †"),
      "Receives an employer contribution or match",
        "Employer contributes to the account (any employer money; matching is not distinguished): EECNTYN_401 = Yes OR EECNTYN_IRA = Yes (DC / IRA plans only)",
        "Workers who hold a plan through their employer (EMJOB_* = Yes)",
        fM(matc$wt_num), paste0(fP(matc$share), " ‡"),
      "Employer-contribution amount",
        "Dollars the employer contributed over the reference period: TECNTAMT (annual)",
        "Workers receiving an employer contribution, with a reported amount",
        fM(matc$wt_num), sprintf("median ≈ $%s/yr", formatC(round(medamt), format = "d", big.mark = ","))
    )
    data_meta <- list(`column-format` = setNames(
      lapply(names(t1), function(nm) list(type = "text")), names(t1)))
    colcfg <- list(`Measure` = list(width = 0.16),
                   `SIPP rule (variables)` = list(width = 0.34),
                   `Universe (per data dictionary)` = list(width = 0.30))
    specs[["table1_definitions"]] <<- list(
      type = "tables",
      title = "Table 1. How access, participation, and matching are defined and counted",
      intro = paste0("Shares are of the working population (", fM(pop),
                     "). † ", fP(part_ga), " of workers with employer-provided access whose plan",
                     " type is observed. ‡ ", fP(matc_gp), " of participants."),
      annotate = paste0("Variable names, universes, and question wording follow the SIPP Data Dictionary; counts are weighted (WPFINWGT) to the ", REF_LABEL, " reference month."),
      source = SRC_SIPP, data = t1, colors = NULL, data_meta = data_meta,
      visualize = list(columns = colcfg, striped = TRUE, perPage = 100,
        pagination = list(enabled = FALSE)),
      fmt = NULL, h = 520, w = 900)
  })

  specs
}

SPECS <- build_specs()

# ---- Post-process every spec's data for Datawrapper conventions -------------
for (k in names(SPECS)) {
  # (a) The label column must be text, else DW mis-reads numeric labels as a
  #     data series (the "A..J" bug on column/dot charts).
  SPECS[[k]]$data[[1]] <- as.character(SPECS[[k]]$data[[1]])
  # (b) This team theme's "%" format appends the sign without multiplying, so
  #     pre-multiply percent value columns by 100.
  if ((SPECS[[k]]$fmt %||% "") %in% c("pct", "pct1")) {
    d <- SPECS[[k]]$data
    for (j in 2:ncol(d)) if (is.numeric(d[[j]])) d[[j]] <- d[[j]] * 100
    SPECS[[k]]$data <- d
  }
}

if (length(ONLY)) SPECS <- SPECS[intersect(names(SPECS), ONLY)]

# ==============================================================================
# 5. Write one JSON payload per exhibit
# ==============================================================================
{
  for (key in names(SPECS)) {
    s <- SPECS[[key]]
    preview <- list(figure_key = key, chart_type = s$type,
      palette_mode = PALETTE_MODE, token_version = TOK$version,
      colors = s$colors, base_color = s$base_color %||% NA,
      title = s$title, intro = s$intro, annotate = s$annotate, source = s$source,
      data_rows = s$data)
    write_json(preview, file.path(DRYRUN_DIR, paste0(key, ".json")),
               pretty = TRUE, auto_unbox = TRUE, na = "string")
  }
  message(length(SPECS), " chart payloads written to ", DRYRUN_DIR)
}

