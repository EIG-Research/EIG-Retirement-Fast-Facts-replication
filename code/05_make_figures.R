# ==============================================================================
# 05_make_figures.R
# Build the four figures for "The U.S. Retirement System: Fast Facts".
#
# Reads   : output/tables/*.csv
# Writes  : output/figures/figN_*.png  (300 dpi, ragg) + figN_*.csv (background data)
#
# Style   : EIG 2022 primary palette + Tufte graphical-quality layer.
#           Sources the palette and ggplot theme from code/_shared/eig_style_public.R
#           (colors are NOT reinvented). EIG's licensed brand OTFs are not
#           redistributed with this package, so the script falls back to a system
#           serif/sans stack and reports which fonts it used at run time.
#
# Run     : Rscript code/05_make_figures.R
#           (orchestrator wires the run_all flag; do not edit run_all.R here)
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# ---- 0. Locate project root (robust to working directory) --------------------
.find_root <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  if (length(f) == 1) {
    sp <- normalizePath(sub("^--file=", "", f), winslash = "/", mustWork = FALSE)
    return(dirname(dirname(sp)))   # code/05_make_figures.R -> repo root
  }
  getwd()
}
ROOT        <- .find_root()
TABLES_DIR  <- file.path(ROOT, "output", "tables")
FIG_DIR     <- file.path(ROOT, "output", "figures")
THEME_DIR   <- file.path(ROOT, "code", "_shared")
FONTS_DIR   <- file.path(ROOT, "code", "_shared", "fonts")  # brand OTFs are not redistributed
if (!dir.exists(FIG_DIR)) dir.create(FIG_DIR, recursive = TRUE)

# ---- 1. Source canonical EIG theme + tokens ----------------------------------
source(file.path(THEME_DIR, "eig_style_public.R"))
tokens <- eig_load_tokens()

# Vintage + Source: line come from the single source of truth (params.R).
path_project <- ROOT
source(file.path(ROOT, "code", "_shared", "params.R"))

# Token-derived colors only (2022 primary palette).
EIG_FOREST <- tokens$EIG_COLORS[["eig_green_700"]]  # #19644D  primary single series
EIG_GOLD   <- tokens$EIG_COLORS[["eig_gold_600"]]   # #E1AD28  highlight / 2nd series
EIG_INK    <- tokens$EIG_COLORS[["eig_black"]]      # text
GRID_GREY  <- "#D9D9D9"                             # theme gridline gray (canonical)
# Neutral gray is explicitly permitted by style-figure-rule 11 / Tufte ("one or two
# series colors PLUS neutral gray"); it carries no series encoding. The 2022 palette
# has no gray token, so a documented neutral gray is used for de-emphasis only.
# Darkened from #8C8C8C to #6E6E6E (2026-07-13) so white value labels on grey fills
# clear WCAG AA (5.1:1, was 3.36:1) and grey separates better from gold.
NEUTRAL_GREY <- "#6E6E6E"

# ---- 2. Fonts: register shipped EIG brand OTFs, else documented fallback ------
FONT_NOTE <- ""
resolve_fonts <- function() {
  brand_files <- c(
    tiempos_reg  = file.path(FONTS_DIR, "TiemposText-Regular.otf"),
    tiempos_semi = file.path(FONTS_DIR, "TiemposText-Semibold.otf"),
    tiempos_ital = file.path(FONTS_DIR, "TiemposText-RegularItalic.otf"),
    gp_book      = file.path(FONTS_DIR, "GalaxiePolaris-Book.otf"),
    gp_bold      = file.path(FONTS_DIR, "GalaxiePolaris-Bold.otf"),
    gp_light     = file.path(FONTS_DIR, "GalaxiePolaris-Light.otf")
  )
  have_brand <- requireNamespace("systemfonts", quietly = TRUE) &&
    all(file.exists(brand_files))

  if (have_brand) {
    ok <- tryCatch({
      systemfonts::register_font(
        name  = "Tiempos Text",
        plain = brand_files[["tiempos_reg"]],
        bold  = brand_files[["tiempos_semi"]],
        italic = brand_files[["tiempos_ital"]],
        bolditalic = brand_files[["tiempos_semi"]]
      )
      systemfonts::register_font(
        name  = "Galaxie Polaris",
        plain = brand_files[["gp_book"]],
        bold  = brand_files[["gp_bold"]],
        italic = brand_files[["gp_book"]],
        bolditalic = brand_files[["gp_bold"]]
      )
      systemfonts::register_font(
        name  = "Galaxie Polaris Light",
        plain = brand_files[["gp_light"]]
      )
      TRUE
    }, error = function(e) FALSE)

    if (ok) {
      FONT_NOTE <<- paste0(
        "EIG brand fonts rendered: Tiempos Text (headline) and Galaxie Polaris ",
        "(body), registered from repo assets via systemfonts and drawn by ragg."
      )
      return(list(headline = "Tiempos Text", body = "Galaxie Polaris",
                  source = "Galaxie Polaris Light"))
    }
  }

  # Fallback: prefer token primaries if system-installed, else generic stack.
  avail <- if (requireNamespace("systemfonts", quietly = TRUE)) {
    unique(systemfonts::system_fonts()$family)
  } else character(0)
  headline <- if (tokens$EIG_FONT_HEADLINE_PRIMARY %in% avail) {
    tokens$EIG_FONT_HEADLINE_PRIMARY
  } else if ("Georgia" %in% avail) "Georgia" else "serif"
  body <- if (tokens$EIG_FONT_BODY_PRIMARY %in% avail) {
    tokens$EIG_FONT_BODY_PRIMARY
  } else if ("Arial" %in% avail) "Arial" else "sans"
  FONT_NOTE <<- paste0(
    "EIG brand OTFs unavailable; fell back per style docs to headline='", headline,
    "', body='", body, "'."
  )
  list(headline = headline, body = body, source = body)
}
FONTS <- resolve_fonts()

# Override the token font families with the resolved (registered/fallback) ones,
# then reuse the canonical eig_theme_ggplot() so all other styling stays canonical.
tokens$EIG_FONT_HEADLINE_PRIMARY <- FONTS$headline
tokens$EIG_FONT_BODY_PRIMARY     <- FONTS$body

SRC_LINE <- paste0("Source: ", SRC_SIPP)   # SRC_SIPP defined in params.R (vintage-driven)

# EIG base theme + shared caption/label styling used by every figure.
eig_fig_theme <- function(base_size = 10) {
  eig_theme_ggplot(tokens, base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(family = FONTS$headline, face = "bold",
                                size = base_size * 1.25, color = EIG_INK,
                                margin = margin(b = 2)),
      plot.subtitle = element_text(family = FONTS$body, size = base_size * 1.0,
                                   color = "#333333", margin = margin(b = 8)),
      plot.caption = element_text(family = FONTS$source, size = base_size * 0.72,
                                  color = "#5A5A5A", hjust = 0, lineheight = 1.15,
                                  margin = margin(t = 10)),
      axis.text  = element_text(family = FONTS$body, size = base_size * 0.85,
                                color = EIG_INK),
      axis.title = element_text(family = FONTS$body, face = "bold",
                                size = base_size * 0.85, color = EIG_INK),
      legend.position = "none"
    )
}

save_fig <- function(plot, file, width = 6.5, height = 3.5) {
  path <- file.path(FIG_DIR, file)
  ragg::agg_png(path, width = width, height = height, units = "in", res = 300,
                background = "white")
  print(plot)
  invisible(dev.off())
  message("wrote ", path)
}

lab_pct <- function(x, acc = 0.1) scales::percent(x, accuracy = acc)

# ==============================================================================
# FIGURE 1 — Lack of employer-provided access (all workers, by class, by schedule)
# ==============================================================================
acc <- read_csv(file.path(TABLES_DIR, "access_headline.csv"), show_col_types = FALSE)
se  <- read_csv(file.path(TABLES_DIR, "standard_errors.csv"), show_col_types = FALSE)

f1_base <- bind_rows(
  acc |> filter(dimension == "Overall") |> mutate(group_cat = "All workers"),
  acc |> filter(dimension == "worker_class") |> mutate(group_cat = "By worker class"),
  acc |> filter(dimension == "ft_pt") |> mutate(group_cat = "By work schedule")
) |>
  select(group_cat, group, share, n_den, wt_den)

# CIs for the rows we have them (All workers, by class).
se_ci <- se |>
  filter(measure %in% c("H1 lacks employer access", "H1 by class")) |>
  select(group, ci_low, ci_high, se)

fig1 <- f1_base |>
  left_join(se_ci, by = "group") |>
  mutate(
    group_cat = factor(group_cat,
      levels = c("All workers", "By worker class", "By work schedule")),
    group = factor(group),
    # Highlight the headline all-workers reference in gold; mute the self-employed
    # bar to grey because its 100% is definitional (no employer access by
    # construction), not a behavioral finding, so it should not out-weigh the
    # all-workers headline in the visual hierarchy; all other bars forest green.
    hue = ifelse(group == "Self-employed", "muted",
                 ifelse(group_cat == "All workers", "highlight", "series")),
    lab_x = ifelse(!is.na(ci_high), ci_high, share)
  )

write_csv(
  fig1 |> select(group_cat, group, share, se, ci_low, ci_high, n_den, wt_den) |>
    arrange(group_cat, desc(share)),
  file.path(FIG_DIR, "fig1_access_by_class.csv")
)

p1 <- ggplot(fig1, aes(x = share, y = reorder(group, share))) +
  geom_col(aes(fill = hue), width = 0.68) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high),
                orientation = "y", width = 0.18, linewidth = 0.4,
                color = EIG_INK, na.rm = TRUE) +
  geom_text(aes(x = lab_x, label = lab_pct(share)),
            hjust = -0.18, family = FONTS$body, size = 2.75, color = EIG_INK) +
  scale_fill_manual(values = c(series = EIG_FOREST, highlight = EIG_GOLD,
                               muted = NEUTRAL_GREY)) +
  scale_x_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0.16))) +
  facet_grid(rows = vars(group_cat), scales = "free_y", space = "free_y",
             switch = "y",
             # Drop the redundant "All workers" group header: for that single-row
             # group the left strip label duplicated the bar-row axis label. Keep the
             # bar-row label (every bar stays labeled with its data category) and blank
             # only the strip. Eraser + collision tests (style-figure-rules 9-10).
             labeller = as_labeller(c(
               "All workers"      = "",
               "By worker class"  = "By worker class",
               "By work schedule" = "By work schedule"))) +
  labs(
    title = "Half of workers lack retirement-plan access",
    subtitle = "Share of workers without access to an employer-provided retirement plan",
    x = "Share of workers (%)", y = NULL,
    caption = paste0(
      "Note: Gold marks the all-worker total; the self-employed bar is grey ",
      "(no employer access by definition). Whiskers show 95% confidence\n",
      "intervals from replicate weights, shown for all workers and by class; ",
      "full-time/part-time CIs not shown.\n",
      SRC_LINE)
  ) +
  eig_fig_theme() +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(family = FONTS$body, face = "bold",
                                     angle = 0, hjust = 1, size = 8,
                                     color = "#333333"),
    panel.spacing = unit(6, "pt")
  )

save_fig(p1, "fig1_access_by_class.png", width = 6.5, height = 4.0)

# ==============================================================================
# FIGURE 2 — The gap widens down the income ladder (earnings deciles)
# ==============================================================================
## Matching (fig3, below) is presented on an EMPLOYEE basis (self-employed reported
## separately; 2026-07-16), so `mm` reads the Private+Government employee table.
mm <- read_csv(file.path(TABLES_DIR, "matching_employees.csv"), show_col_types = FALSE)

## Figure 2's three series read the PRIVATE-SECTOR-EMPLOYEE decile tables (2026-08-31): access,
## participation, and employer contribution are now one consistent population (private-sector
## employees), rather than the prior mix of all-worker access with Private+Government match.
acc_priv_dec   <- read_csv(file.path(TABLES_DIR, "access_by_decile_private.csv"), show_col_types = FALSE)
part_priv_dec  <- read_csv(file.path(TABLES_DIR, "participation_by_decile_private.csv"), show_col_types = FALSE)
match_priv_dec <- read_csv(file.path(TABLES_DIR, "matching_by_decile_private.csv"), show_col_types = FALSE)

acc_dec <- acc_priv_dec |>
  filter(dimension == "earn_decile") |>
  transmute(earn_decile = as.integer(group), measure = "Lacks employer access",
            share = share)
part_dec <- part_priv_dec |>
  filter(dimension == "earn_decile") |>
  transmute(earn_decile = as.integer(group), measure = "Doesn't participate",
            share = 1 - share)   # 1 - participation share
match_dec <- match_priv_dec |>
  filter(dimension == "earn_decile") |>
  transmute(earn_decile = as.integer(group), measure = "No employer contribution",
            share = 1 - share)   # 1 - contribution-receipt share

fig2 <- bind_rows(acc_dec, part_dec, match_dec) |>
  mutate(measure = factor(measure,
    levels = c("Lacks employer access", "Doesn't participate", "No employer contribution"))) |>
  arrange(measure, earn_decile)

write_csv(fig2, file.path(FIG_DIR, "fig2_income_ladder.csv"))

pal2 <- c("Lacks employer access" = EIG_FOREST, "Doesn't participate" = NEUTRAL_GREY,
          "No employer contribution" = EIG_GOLD)

p2 <- ggplot(fig2, aes(x = factor(earn_decile), y = share, fill = measure)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.70) +
  scale_fill_manual(values = pal2, name = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(
    title = "Low earners lag on access, participation, and contributions",
    subtitle = "Share of private-sector employees who lack each, by earnings decile",
    x = "Earnings decile (1 = lowest earnings)", y = "Share (%)",
    caption = paste0(
      "Note: “Doesn't participate” and “No employer contribution” are one minus the participation ",
      "and employer-contribution-receipt rates. All three series are among private-sector employees ",
      "only (the self-employed and government workers are excluded). Deciles are of individual annual\n",
      "earnings among ALL workers (one common earnings ladder across figures), so decile bins hold ",
      "unequal numbers of private-sector employees.\n",
      SRC_LINE)
  ) +
  eig_fig_theme() +
  theme(legend.position = "top", legend.justification = "left",
        legend.key.size = unit(0.85, "lines"),
        plot.margin = margin(6, 6, 6, 6))

save_fig(p2, "fig2_income_ladder.png", width = 6.5, height = 4.0)

# ==============================================================================
# FIGURE 2B — Among the unmatched, high earners are far likelier to participate on their own
# ==============================================================================
## Companion to Figure 2 (2026-08-31): the "No employer contribution" gold bar above shows who
## LACKS a match; this figure zooms into that unmatched population and asks whether they still
## participate in a plan on their own -- the direct test of the liquidity-constraint hypothesis
## (higher earners can self-fund saving even without employer money on the table). Same
## population (private-sector employees), same x-axis (earnings decile), same 0-100% y-axis and
## theme as Figure 2 so the two read as one pair. Gold carries over from Figure 2's "No employer
## contribution" series to visually tie this figure to that unmatched subgroup.
part_no_match_dec <- read_csv(file.path(TABLES_DIR, "participation_given_no_match_private.csv"),
                              show_col_types = FALSE)

fig2b <- part_no_match_dec |>
  filter(dimension == "earn_decile") |>
  transmute(earn_decile = as.integer(group), share, n_num, n_den)

write_csv(fig2b, file.path(FIG_DIR, "fig2b_participation_without_match.csv"))

## Caption values interpolated from the current tables, never restated as literals:
## hard-coded caption statistics silently go stale on a vintage bump.
half2b <- read_csv(file.path(TABLES_DIR, "participation_given_no_match_half_private.csv"),
                   show_col_types = FALSE)
p_bot2b <- half2b$share[half2b$group == "Bottom half (deciles 1-5)"]
p_top2b <- half2b$share[half2b$group == "Top half (deciles 6-10)"]
stopifnot(length(p_bot2b) == 1, length(p_top2b) == 1, p_bot2b > 0)
n_min2b <- min(fig2b$n_num)
## Axis limit derived from the data (label headroom); a hardcoded cap can silently clip a bar
## when a vintage moves the values.
ymax2b <- min(1, ceiling(max(fig2b$share) * 1.18 * 20) / 20)   # next 5%-step above data+headroom

p2b <- ggplot(fig2b, aes(x = factor(earn_decile), y = share)) +
  geom_col(fill = EIG_GOLD, width = 0.70) +
  geom_text(aes(label = lab_pct(share)), vjust = -0.5, family = FONTS$body,
            size = 2.7, color = EIG_INK) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, ymax2b), breaks = seq(0, ymax2b, 0.10),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(
    title = "High earners are far likelier to participate without a match",
    subtitle = "Participation rate among those with no employer contribution, by earnings decile",
    x = "Earnings decile (1 = lowest earnings)", y = "Share (%)",
    caption = paste0(
      "Note: Denominator is private-sector employees who receive no employer contribution; ",
      "numerator is the share of that group who still participate in a plan. Bottom-half ",
      sprintf("(deciles 1-5) employees participate at %.1f percent versus %.1f percent for top-half ",
              100 * p_bot2b, 100 * p_top2b),
      sprintf("(deciles 6-10) employees -- about %.0f times as likely. Deciles are of individual annual\n",
              p_top2b / p_bot2b),
      "earnings among ALL workers. Decile-level bars rest on small unweighted participant counts ",
      sprintf("(as few as %d in the lowest decile); treat decile-to-decile contrasts as noisy.\n", n_min2b),
      SRC_LINE)
  ) +
  eig_fig_theme() +
  theme(plot.margin = margin(6, 6, 6, 6))

save_fig(p2b, "fig2b_participation_without_match.png", width = 6.5, height = 4.0)

# ==============================================================================
# FIGURE 3 — Who gets an employer match (by race/ethnicity and by education)
# ==============================================================================
fig3 <- mm |>
  filter(dimension %in% c("race_eth", "educ_grp")) |>
  transmute(
    panel = ifelse(dimension == "race_eth", "By race and ethnicity", "By education"),
    group, match_receipt_share = share, n_den
  ) |>
  mutate(
    panel = factor(panel,
      levels = c("By race and ethnicity", "By education")),
    # Spell out cramped education abbreviations where space allows (clear, detailed
    # labeling defeats distortion, Tufte §2). Race "(NH)" is kept compact and defined
    # in the figure note to reconcile with the draft prose ("non-Hispanic").
    group = dplyr::recode(group,
      "Some college/assoc" = "Some college / associate",
      "HS grad"            = "High school graduate",
      "Less than HS"       = "Less than high school")) |>
  # The figure's point is the disparity, so let color carry it: highlight the
  # lowest-receipt group in each panel in gold, the rest forest green.
  group_by(panel) |>
  mutate(hue = ifelse(match_receipt_share == min(match_receipt_share),
                      "low", "series")) |>
  ungroup()

write_csv(
  fig3 |> arrange(panel, desc(match_receipt_share)),
  file.path(FIG_DIR, "fig3_match_by_group.csv")
)

p3 <- ggplot(fig3, aes(x = match_receipt_share,
                       y = reorder(group, match_receipt_share))) +
  geom_col(aes(fill = hue), width = 0.7) +
  scale_fill_manual(values = c(series = EIG_FOREST, low = EIG_GOLD)) +
  geom_text(aes(label = lab_pct(match_receipt_share)),
            hjust = -0.18, family = FONTS$body, size = 2.7, color = EIG_INK) +
  # Upper limit derived from the data (label headroom); a hardcoded cap can silently
  # clip a bar when a vintage moves the values.
  scale_x_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, min(1, max(fig3$match_receipt_share) * 1.15)),
                     breaks = seq(0, 0.6, 0.2),
                     expand = expansion(mult = c(0, 0.12))) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  labs(
    ## Note: SIPP's EECNTYN_* records ANY employer contribution (matching, non-elective, or
    ## profit-sharing are indistinguishable), so titles/notes say "contribution or match", never
    ## "match" alone.
    title = "Who gets an employer contribution or match",
    subtitle = "Share of employees receiving any employer contribution to an employer-provided retirement plan",
    x = "Share of employees (%)", y = NULL,
    caption = paste0("Note: Among employees (the self-employed are reported separately). “Contribution” is any ",
                     "employer money reported to a 401(k)-type or IRA/Keogh\naccount — SIPP does not distinguish ",
                     "matching from non-matching employer contributions. The lowest group in each panel is ",
                     "highlighted in gold.\n“NH” denotes non-Hispanic.\n", SRC_LINE)
  ) +
  eig_fig_theme() +
  theme(
    strip.text = element_text(family = FONTS$body, face = "bold", hjust = 0,
                              size = 9, color = "#333333"),
    panel.spacing.x = unit(16, "pt")
  )

save_fig(p3, "fig3_match_by_group.png", width = 6.5, height = 3.4)

## Single-panel exports of Figure 3: the draft embeds the race and education panels SEPARATELY,
## and the two-panel PNG cannot be split after the fact. Same data, palette, and limits as the
## combined figure.
p3_panel <- function(pnl, fname, note_nh = FALSE) {
  d1 <- fig3 |> filter(panel == pnl)
  p <- ggplot(d1, aes(x = match_receipt_share, y = reorder(group, match_receipt_share))) +
    geom_col(aes(fill = hue), width = 0.7) +
    scale_fill_manual(values = c(series = EIG_FOREST, low = EIG_GOLD)) +
    geom_text(aes(label = lab_pct(match_receipt_share)),
              hjust = -0.18, family = FONTS$body, size = 2.7, color = EIG_INK) +
    scale_x_continuous(labels = label_percent(accuracy = 1),
                       limits = c(0, min(1, max(fig3$match_receipt_share) * 1.15)),
                       breaks = seq(0, 0.6, 0.2),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(title = paste0("Who gets an employer contribution or match, ", tolower(pnl)),
         subtitle = "Share of employees receiving any employer contribution to an employer-provided retirement plan",
         x = "Share of employees (%)", y = NULL,
         caption = paste0("Note: Among employees (the self-employed are reported separately). “Contribution” is any ",
                          "employer money reported to a 401(k)-type\nor IRA/Keogh account — SIPP does not ",
                          "distinguish matching from non-matching employer contributions. The lowest group is ",
                          "highlighted in gold.",
                          if (note_nh) " “NH” denotes non-Hispanic." else "",
                          "\n", SRC_LINE)) +
    eig_fig_theme()
  save_fig(p, fname, width = 6.5, height = 2.9)
}
p3_panel("By race and ethnicity", "fig3a_match_by_race.png", note_nh = TRUE)
p3_panel("By education",          "fig3b_match_by_education.png")

# ==============================================================================
# FIGURE 3-ALT — Lack of access (not matching) by education, within race/ethnicity
# Four-cell grid: one panel per race/ethnicity, bars = share LACKING employer access by
# education group, AMONG PRIVATE-SECTOR EMPLOYEES ONLY (2026-08-31 decision). Government
# and self-employed workers are dropped from every cell so the population matches the
# national benchmark below (H1's private-sector row) exactly. "Other (NH)" is excluded
# (smallest/most heterogeneous race_eth category; keeps a clean 2x2 rather than a fifth panel).
# Framed around the LACKS-access share throughout (2026-08-31), matching the draft's own framing
# (the H1 headline and Figure 1's waffle/bar chart are both built around the share who lack
# access, not the share who have it) rather than the positively-framed "access share" used in
# this figure's first draft.
# ==============================================================================
race_educ <- read_csv(file.path(TABLES_DIR, "access_by_race_educ.csv"), show_col_types = FALSE)

## National reference line: the PRIVATE-SECTOR lacks-access share (H1's private-sector row,
## 49.1%), from the same `acc` table Figure 1 reads. Matches the population in every cell below
## (private-sector employees only) rather than the all-worker or employees-combined figure, which
## would benchmark a private-sector breakdown against a different population's average.
nat_lacks <- acc$share[acc$dimension == "worker_class" & acc$group == "Private"]

fig3alt <- race_educ |>
  transmute(
    race = factor(group1, levels = c("White (NH)", "Black (NH)", "Hispanic", "Asian (NH)")),
    educ = dplyr::recode(group2,
      "Some college/assoc" = "Some college / associate",
      "HS grad"            = "High school graduate",
      "Less than HS"       = "Less than high school"),
    lacks_share = share, n_den
  ) |>
  mutate(
    # Education is an ordered scale (the story is monotonic), so panels keep this fixed
    # order rather than sorting bars by value within each facet (unlike fig3's group
    # panels, where categories have no inherent order).
    educ = factor(educ, levels = c("Less than high school", "High school graduate",
                                   "Some college / associate", "Bachelor's+")),
    # Gold marks WORSE-than-average cells (a higher lacks-access rate), mirroring fig3's
    # per-panel disparity highlight; here the reference is the fixed national benchmark
    # rather than each panel's own minimum.
    hue = ifelse(lacks_share > nat_lacks, "above_avg", "series")
  )

write_csv(
  fig3alt |> arrange(race, educ),
  file.path(FIG_DIR, "fig3alt_access_by_race_educ.csv")
)

pal3alt <- c(series = EIG_FOREST, above_avg = EIG_GOLD)
lab3alt <- c(series = "At or below private-sector average",
            above_avg = paste0("Above private-sector average (", lab_pct(nat_lacks), ")"))

p3alt <- ggplot(fig3alt, aes(x = lacks_share, y = forcats::fct_rev(educ))) +
  geom_col(aes(fill = hue), width = 0.7) +
  geom_text(aes(label = lab_pct(lacks_share)),
            hjust = -0.18, family = FONTS$body, size = 2.6, color = EIG_INK) +
  scale_fill_manual(values = pal3alt, labels = lab3alt, name = NULL,
                    breaks = c("series", "above_avg")) +
  scale_x_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, min(1, max(fig3alt$lacks_share) * 1.2)),
                     breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0.12))) +
  facet_wrap(~ race, ncol = 2) +
  labs(
    title = "Lack of employer plan access falls with education",
    subtitle = "Share of private-sector employees lacking plan access, by education, within race/ethnicity",
    x = "Share lacking access (%)", y = NULL,
    ## Caption values interpolated from the loaded tables: literals go stale on a
    ## vintage bump. The small-cell sentence names whichever cells fall under the n threshold today.
    caption = {
      blk2 <- fig3alt |> filter(race == "Black (NH)",
                                educ %in% c("Less than high school", "High school graduate"))
      small_n  <- 45L
      small_cells <- fig3alt |> filter(n_den < small_n) |>
        mutate(cell = paste0(race, ", ", tolower(educ)))
      small_txt <- if (nrow(small_cells) > 0) paste0(
        "the\nsmallest cells (", paste(small_cells$cell, collapse = "; "),
        ") have unweighted n below ", small_n, ",\nso treat those bars as noisier than the rest.") else
        "no cell falls below the small-sample flag threshold."
      paste0(
        "Note: Private-sector employees only (government and self-employed workers excluded, so the\n",
        "population matches the ", lab_pct(nat_lacks), " private-sector lacks-access benchmark). “Other (NH)” race/\n",
        "ethnicity is excluded from this four-panel grid (smallest, most heterogeneous category); “NH”\n",
        "denotes non-Hispanic. For Black (NH) workers, the two lowest education groups have nearly\n",
        sprintf("identical lack rates (%.1f%% vs. %.1f%%, no CIs shown) rather than a clear step down; ",
                100 * blk2$lacks_share[blk2$educ == "Less than high school"],
                100 * blk2$lacks_share[blk2$educ == "High school graduate"]),
        small_txt, "\n",
        "Source: U.S. Census Bureau, Survey of Income and Program Participation, ", SIPP_PANEL,
        " release\n(pooled ", SIPP_PANEL_LABEL, " panels), ", month.name[REF_MONTH], " ", REF_YEAR,
        " reference month; EIG analysis.")
    }
  ) +
  eig_fig_theme() +
  theme(
    strip.text = element_text(family = FONTS$body, face = "bold", hjust = 0,
                              size = 9, color = "#333333"),
    panel.spacing = unit(14, "pt"),
    # Discrete y (education) draws one horizontal gridline per category by default, which
    # collides with the data labels sitting just past each bar (style-figure-rule 10,
    # collision test). Drop them; the x gridlines still anchor the value scale.
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.key.size = unit(0.85, "lines"),
    legend.text = element_text(family = FONTS$body, size = 8, color = EIG_INK)
  )

save_fig(p3alt, "fig3alt_access_by_race_educ.png", width = 6.5, height = 5.2)

# ==============================================================================
# FIGURE 4 — Eligible, but locked out (the Saver's Match wedge)
# ==============================================================================
sm <- read_csv(file.path(TABLES_DIR, "savers_match_summary.csv"), show_col_types = FALSE)
sm_v <- setNames(sm$value, sm$metric)

## Runtime values from savers_match_summary.csv (03). Deliberately NOT restated as literals here:
## the previous comments carried figures from two different vintages and none of the three matched
## the output (external RA review, 2026-08-27). For current values read
## output/tables/savers_match_summary.csv or output/figures/fig4_savers_match_wedge.csv.
elig     <- unname(sm_v["elig_any_v2_wt"])    # income-eligible for any match (dependent-refined)
wedge    <- unname(sm_v["wedge_wt"])          # eligible but NO qualifying account
canclaim <- elig - wedge                      # eligible AND holding a qualifying account

fig4 <- tibble::tibble(
  segment = c("Can claim", "Locked out"),
  count = c(canclaim, wedge)
) |>
  mutate(
    count_millions = count / 1e6,
    share_of_eligible = count / elig,
    segment = factor(segment, levels = c("Can claim", "Locked out"))
  )

write_csv(
  fig4 |> select(segment, count, count_millions, share_of_eligible),
  file.path(FIG_DIR, "fig4_savers_match_wedge.csv")
)

# Explicit geometry: locked-out wedge (focus) on the left in gold, can-claim in
# neutral gray on the right. geom_rect avoids stack-order ambiguity, so labels and
# fills are deterministic.
plot4 <- tibble::tibble(
  segment = factor(c("Locked out", "Can claim"),
                   levels = c("Locked out", "Can claim")),
  xmin = c(0, wedge),
  xmax = c(wedge, elig),
  count_millions = c(wedge, canclaim) / 1e6,
  share_of_eligible = c(wedge, canclaim) / elig
) |>
  mutate(
    mid = (xmin + xmax) / 2,
    seg_label = c("Locked out\n(eligible, no qualifying account)",
                  "Can claim\n(has a qualifying account)"),
    val_label = paste0(scales::number(count_millions, accuracy = 0.1),
                       "M · ", lab_pct(share_of_eligible, 1))
  )

pal4 <- c("Can claim" = NEUTRAL_GREY, "Locked out" = EIG_GOLD)
txt4 <- c("Can claim" = "white",     "Locked out" = EIG_INK)

p4 <- ggplot(plot4) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0.75, ymax = 1.25,
                fill = segment)) +
  # segment name (bold) — direct label inside each segment
  geom_text(aes(x = mid, y = 1, label = seg_label, color = segment),
            family = FONTS$body, fontface = "bold", size = 2.9,
            lineheight = 1.0, vjust = -0.2) +
  # count + share of eligible, beneath the name, inside each segment
  geom_text(aes(x = mid, y = 1, label = val_label, color = segment),
            family = FONTS$body, size = 2.9, vjust = 1.9) +
  scale_fill_manual(values = pal4) +
  scale_color_manual(values = txt4) +
  scale_y_continuous(limits = c(0.6, 1.4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, elig), expand = expansion(mult = c(0, 0.005))) +
  labs(
    title = "Eligible for the Saver's Match, but locked out",
    subtitle = paste0("Of ", scales::number(elig / 1e6, accuracy = 0.1),
      " million income-eligible workers, ", scales::number(wedge / 1e6, accuracy = 0.1),
      " million lack a qualifying account"),
    x = NULL, y = NULL,
    ## The eligibility proxies travel WITH the figure, not only in 03's console.
    caption = paste0(
      "Note: The Saver's Match is a federal matching contribution for eligible ",
      "lower-income savers, beginning in 2027. A qualifying account\n(a 401(k)-type ",
      "or IRA) is needed to receive it; defined-benefit pensions cannot receive the match. ",
      "Counts in millions of workers; shares are\nof the income-eligible. Eligibility is ",
      "approximated: SIPP income proxies AGI, filing status is the prior tax year, statutory ",
      "thresholds are\napplied unindexed, and a post-2026 program is applied to a ", REF_YEAR,
      " population — treat counts as estimates.\n", SRC_LINE)
  ) +
  eig_fig_theme() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

save_fig(p4, "fig4_savers_match_wedge.png", width = 6.5, height = 2.6)

message("\nFont status: ", FONT_NOTE)
message("All figures written to ", FIG_DIR)
