# ==============================================================================
# 13_te_figures.R
# Build the four figures for the retirement tax-expenditure (TE) analysis.
#
# Reads   : output/tables/te_decile.csv
#           output/tables/te_decile_cbo_comparable.csv
#           output/tables/te_quintile_cbo_compare.csv
#           output/tables/te_crosscuts.csv
# Writes  : output/figures/te_figN_*.png (300 dpi, ragg) + te_figN_*.csv
#           (background data), mirroring 05_make_figures.R conventions.
#
# Style   : EIG 2022 primary palette + Tufte graphical-quality layer.
#           Sources the palette and ggplot theme from code/_shared/eig_style_public.R
#           (colors are NOT reinvented). EIG's licensed brand OTFs are not
#           redistributed with this package, so the script falls back to a system
#           serif/sans stack and reports which fonts it used at run time.
#
# Run     : Rscript code/13_te_figures.R
#           (self-contained: resolves its own paths; safe to wire into run_all.R
#           as a flag later; no rm(list = ls()))
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# ---- 0. Locate project root (robust to working directory) --------------------
.find_root <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  if (length(f) == 1) {
    sp <- normalizePath(sub("^--file=", "", f), winslash = "/", mustWork = FALSE)
    return(dirname(dirname(sp)))   # code/13_te_figures.R -> repo root
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
EIG_FOREST <- tokens$EIG_COLORS[["eig_green_700"]]  # #19644D  primary series
EIG_GOLD   <- tokens$EIG_COLORS[["eig_gold_600"]]   # #E1AD28  2nd series / highlight
EIG_INK    <- tokens$EIG_COLORS[["eig_black"]]      # text
# Neutral gray is explicitly permitted by style-figure-rule 11 ("one or two series
# colors PLUS neutral gray"); it de-emphasizes a context series. The 2022 palette
# has no gray token, so the same documented neutral gray as 05_make_figures.R is
# used, carrying no primary-series encoding. Darkened to #6E6E6E (2026-07-13) to
# match 05_make_figures.R (WCAG AA on grey fills; better grey/gold separation).
NEUTRAL_GREY <- "#6E6E6E"

# ---- 2. Fonts: register shipped EIG brand OTFs, else documented fallback ------
# (Mirrors 05_make_figures.R exactly; the font block is script-local there, so it
# is reproduced rather than re-invented.)
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

# Source lines for the TE figures (per figure spec).
SRC_TE <- paste0("Source: ", SRC_TE)   # params.R SRC_TE (vintage-driven), prefixed with "Source: "
DECILE_NOTE <- paste0(
  "Note: Deciles are of size-adjusted household income before transfers and ",
  "taxes; each contains an equal number of people."
)

# EIG base theme + shared caption/label styling (same helper as 05_make_figures.R).
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

# "$51B" style dollar-billions labels; percent labels with 0 decimals.
lab_bil <- label_dollar(accuracy = 1, suffix = "B")
lab_pct0 <- label_percent(accuracy = 1)

# ---- 3. Load TE tables --------------------------------------------------------
te_obs <- read_csv(file.path(TABLES_DIR, "te_decile.csv"), show_col_types = FALSE)
te_cbo <- read_csv(file.path(TABLES_DIR, "te_decile_cbo_comparable.csv"),
                   show_col_types = FALSE)
te_qnt <- read_csv(file.path(TABLES_DIR, "te_quintile_cbo_compare.csv"),
                   show_col_types = FALSE)
te_cc  <- read_csv(file.path(TABLES_DIR, "te_crosscuts.csv"),
                   show_col_types = FALSE)

# ==============================================================================
# FIGURE 1 — Total TE ($B) by income decile, observed vs. CBO-comparable
# ==============================================================================
MEAS_OBS <- "Observed flows"
MEAS_CBO <- "CBO-comparable (incl. imputed DB, Roth, IRA)"

fig1 <- bind_rows(
  te_obs |> transmute(decile, measure = MEAS_OBS, total_te_B),
  te_cbo |> transmute(decile, measure = MEAS_CBO, total_te_B)
) |>
  mutate(measure = factor(measure, levels = c(MEAS_OBS, MEAS_CBO)),
         decile = factor(decile, levels = 1:10))

write_csv(fig1 |> arrange(measure, decile),
          file.path(FIG_DIR, "te_fig1_decile_total.csv"))

pal1 <- setNames(c(EIG_FOREST, EIG_GOLD), c(MEAS_OBS, MEAS_CBO))

# Decile 1 is slightly negative under both measures; the y scale spans the data
# (including the negative value) with a zero baseline drawn — no truncation.
p1 <- ggplot(fig1, aes(x = decile, y = total_te_B, fill = measure)) +
  geom_hline(yintercept = 0, color = EIG_INK, linewidth = 0.35) +
  geom_col(position = position_dodge(width = 0.78), width = 0.70) +
  scale_fill_manual(values = pal1, name = NULL) +
  scale_y_continuous(labels = lab_bil, breaks = seq(0, 75, 25),
                     expand = expansion(mult = c(0.04, 0.05))) +
  labs(
    # Explicit wraps: ggplot does not wrap long titles/captions at 6.5 in.
    title = paste0("The retirement tax expenditure flows overwhelmingly\n",
                   "to top-income deciles"),
    subtitle = "Total retirement tax expenditure by income decile, billions of dollars, 2024",
    x = "Income decile (1 = lowest)", y = "Tax expenditure ($B)",
    caption = paste0(
      DECILE_NOTE, "\n",
      "Decile 1 is slightly negative under both measures.\n",
      SRC_TE)
  ) +
  eig_fig_theme() +
  theme(legend.position = "top", legend.justification = "left",
        legend.key.size = unit(0.85, "lines"))

save_fig(p1, "te_fig1_decile_total.png", width = 6.5, height = 3.8)

# ==============================================================================
# FIGURE 2 — Quintile shares of the income-tax expenditure vs. CBO 2019
# ==============================================================================
S_CBO19 <- "CBO 2019"
S_SIPP  <- paste0("SIPP ", SIPP_PANEL, " observed")        # vintage-driven (params.R)
S_SIPPC <- paste0("SIPP ", SIPP_PANEL, " CBO-comparable")  # vintage-driven (params.R)

fig2 <- te_qnt |>
  transmute(
    quintile,
    !!S_CBO19 := cbo2019_income_share,
    !!S_SIPP  := sipp_income_share,
    !!S_SIPPC := sipp_cbo_comparable_income
  ) |>
  pivot_longer(-quintile, names_to = "series", values_to = "share") |>
  mutate(series = factor(series, levels = c(S_CBO19, S_SIPPC, S_SIPP)),
         quintile = factor(quintile, levels = 1:5))

write_csv(fig2 |> arrange(series, quintile),
          file.path(FIG_DIR, "te_fig2_share_vs_cbo.csv"))

# Two series colors + neutral gray (style-figure-rule 11): the headline comparison
# (CBO 2019 vs. the like-for-like SIPP measure) carries the two palette colors;
# the observed-flows series is context, de-emphasized in neutral gray.
pal2 <- setNames(c(EIG_GOLD, EIG_FOREST, NEUTRAL_GREY), c(S_CBO19, S_SIPPC, S_SIPP))

p2 <- ggplot(fig2, aes(x = quintile, y = share, color = series)) +
  geom_hline(yintercept = 0, color = EIG_INK, linewidth = 0.35) +
  geom_point(position = position_dodge(width = 0.55), size = 2.6) +
  scale_color_manual(values = pal2, name = NULL) +
  scale_y_continuous(labels = lab_pct0, breaks = seq(0, 0.6, 0.2),
                     expand = expansion(mult = c(0.05, 0.07))) +
  labs(
    ## Note: "replicates" overstates a comparison across a five-year gap and known concept
    ## differences (no Medicare imputation or realized capital gains in the SIPP ranking; a
    ## censored survey top tail vs tax-return data). "Closely matches" + the named gaps is the
    ## defensible claim.
    title = paste0("SIPP ", REF_YEAR, " closely matches CBO's 2019 distribution"),
    subtitle = "Share of the income-tax retirement expenditure accruing to each income quintile",
    x = "Income quintile (1 = lowest)", y = "Share of expenditure (%)",
    caption = paste0(
      "Note: Quintiles are of size-adjusted household income before transfers ",
      "and taxes; each contains an equal number of people.\nCBO-comparable adds ",
      "imputed DB, Roth, and IRA flows to match CBO's measure. The two series differ ",
      "by five years and by income-concept\ndetails (no imputed Medicare value or ",
      "realized capital gains in the SIPP ranking; survey top incomes are compressed ",
      "relative\nto CBO's tax-return data).\n",
      SRC_TE, "\n",
      "CBO, The Distribution of Major Tax Expenditures in 2019 (2021).")
  ) +
  eig_fig_theme() +
  theme(legend.position = "top", legend.justification = "left",
        legend.key.size = unit(0.85, "lines"))

save_fig(p2, "te_fig2_share_vs_cbo.png", width = 6.5, height = 3.8)

# ==============================================================================
# FIGURE 3 — Who benefits (share) and by how much (mean $ among beneficiaries)
# ==============================================================================
fig3 <- te_cbo |>
  transmute(decile = factor(decile, levels = 1:10),
            pct_benefiting, mean_te_benef)

write_csv(fig3, file.path(FIG_DIR, "te_fig3_benefiting_mean.csv"))

# Two aligned panels (patchwork): same deciles on both x axes; each panel is a
# single-color bar chart (different measures, not competing series).
p3a <- ggplot(fig3, aes(x = decile, y = pct_benefiting)) +
  geom_col(fill = EIG_FOREST, width = 0.7) +
  geom_text(aes(label = lab_pct0(pct_benefiting)),
            vjust = -0.45, family = FONTS$body, size = 2.5, color = EIG_INK) +
  # Upper limit derived from the data (label headroom): a hardcoded cap can silently clip
  # a bar when a vintage moves the values.
  scale_y_continuous(labels = lab_pct0, breaks = seq(0, 0.6, 0.2),
                     limits = c(0, min(1, max(fig3$pct_benefiting) * 1.12)),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(subtitle = "Share of people with any benefit (%)",
       x = "Income decile (1 = lowest)", y = NULL) +
  eig_fig_theme()

p3b <- ggplot(fig3, aes(x = decile, y = mean_te_benef)) +
  geom_col(fill = EIG_GOLD, width = 0.7) +
  # Compact "$4.5K" labels: full "$4,517" labels collide horizontally across
  # 10 narrow bars (collision test, style-figure-rule 10).
  geom_text(aes(label = label_dollar(accuracy = 0.1, scale = 1e-3,
                                     suffix = "K")(mean_te_benef)),
            vjust = -0.45, family = FONTS$body, size = 2.5, color = EIG_INK) +
  # Upper limit derived from the data (label headroom): the former hardcoded caps clipped
  # decile 10 once (at 5000) and later left almost no headroom (at 6000).
  scale_y_continuous(labels = label_dollar(accuracy = 1, scale = 1e-3, suffix = "K"),
                     breaks = seq(0, 6000, 2000),
                     limits = c(0, max(fig3$mean_te_benef) * 1.12),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(subtitle = "Mean benefit among beneficiaries ($)",
       x = "Income decile (1 = lowest)", y = NULL) +
  eig_fig_theme()

p3 <- p3a + p3b +
  plot_annotation(
    title = "Who benefits, and by how much",
    subtitle = "Retirement tax expenditure by income decile, CBO-comparable measure, 2024",
    caption = paste0(
      DECILE_NOTE, "\n",
      "CBO-comparable measure includes imputed DB, Roth, and IRA flows.\n",
      SRC_TE),
    theme = eig_fig_theme()
  )

save_fig(p3, "te_fig3_benefiting_mean.png", width = 6.5, height = 3.4)

# ==============================================================================
# FIGURE 4 — The tax subsidy follows access, participation, and matching
# ==============================================================================
# Panel A: mean tax benefit per worker ($/yr) by worker earnings decile.
fig4a <- te_cc |>
  filter(dimension == "Worker earnings decile") |>
  transmute(decile = factor(group, levels = as.character(1:10)), mean_te)

# Panel B: each pillar's "positive" group only (has access / participates /
# receives match) — its share of workers vs. its share of TE dollars. Negative
# groups are the complements and would double the bar count without adding
# information (eraser test, style-figure-rule 9).
S_WORK <- "Share of workers"
S_DOLL <- "Share of subsidy dollars"
PILLAR_POS <- c("Has employer access", "Participates", "Receives employer match")

fig4b <- te_cc |>
  filter(grepl("^Pillar", dimension), group %in% PILLAR_POS) |>
  transmute(group = factor(group, levels = PILLAR_POS),
            !!S_WORK := pop_share, !!S_DOLL := te_share) |>
  pivot_longer(-group, names_to = "series", values_to = "share") |>
  mutate(series = factor(series, levels = c(S_WORK, S_DOLL)))

# Background data: the crosscut rows each panel draws on, with used columns.
write_csv(
  te_cc |>
    filter(dimension == "Worker earnings decile" |
             (grepl("^Pillar", dimension) & group %in% PILLAR_POS)) |>
    select(dimension, group, pop_share, te_share, mean_te),
  file.path(FIG_DIR, "te_fig4_crosscut.csv")
)

# Compact bar labels: "$838" below $1,000, "$1.4K" above — full "$4,506" labels
# collide across 10 half-width bars (collision test, style-figure-rule 10).
lab_dollar_mix <- function(x) {
  ifelse(x < 1000,
         label_dollar(accuracy = 1)(x),
         label_dollar(accuracy = 0.1, scale = 1e-3, suffix = "K")(x))
}

p4a <- ggplot(fig4a, aes(x = decile, y = mean_te)) +
  geom_col(fill = EIG_FOREST, width = 0.7) +
  geom_text(aes(label = lab_dollar_mix(mean_te)),
            vjust = -0.45, family = FONTS$body, size = 2.4, color = EIG_INK) +
  scale_y_continuous(labels = label_dollar(accuracy = 1, scale = 1e-3, suffix = "K"),
                     breaks = seq(0, 6000, 2000),
                     limits = c(0, max(fig4a$mean_te) * 1.12),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(subtitle = "Mean tax benefit per worker ($/year)",
       x = "Worker earnings decile (1 = lowest)", y = NULL) +
  eig_fig_theme()

# Second series color (gold) marks the subsidy dollars — the punchline; the
# workers baseline is context in the documented neutral gray (rule 11).
pal4 <- setNames(c(NEUTRAL_GREY, EIG_GOLD), c(S_WORK, S_DOLL))

p4b <- ggplot(fig4b, aes(x = group, y = share, fill = series)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.70) +
  geom_text(aes(label = lab_pct0(share)),
            position = position_dodge(width = 0.78),
            vjust = -0.45, family = FONTS$body, size = 2.4, color = EIG_INK) +
  scale_fill_manual(values = pal4, name = NULL) +
  scale_x_discrete(labels = c("Has employer\naccess", "Participates",
                              "Receives\nemployer match")) +
  scale_y_continuous(labels = lab_pct0, breaks = seq(0, 1, 0.25),
                     limits = c(0, 1.05),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(subtitle = "Share of workers vs. subsidy dollars (%)",
       x = NULL, y = NULL) +
  eig_fig_theme() +
  theme(legend.position = "top", legend.justification = "left",
        legend.key.size = unit(0.85, "lines"),
        legend.margin = margin(t = 0, b = 2))

p4 <- p4a + p4b +
  plot_annotation(
    title = "The tax subsidy follows access, participation, and matching",
    subtitle = "Retirement tax benefit among workers, 2024",
    caption = paste0(
      "Note: Tax benefit = the present value of federal income- and payroll-tax ",
      "savings on 2024 retirement contributions (CBO method),\nincluding an ",
      "estimate of employer pension (defined-benefit) contributions.\n",
      SRC_TE),
    theme = eig_fig_theme()
  )

save_fig(p4, "te_fig4_crosscut.png", width = 6.5, height = 3.7)

message("\nFont status: ", FONT_NOTE)
message("TE figures written to ", FIG_DIR)
