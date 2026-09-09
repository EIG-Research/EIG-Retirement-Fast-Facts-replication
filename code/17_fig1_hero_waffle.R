# ==============================================================================
# 17_fig1_hero_waffle.R
# Candidate hero (Figure 1): a population cascade + waffle.
#   Top: four stacked bars whose LENGTH is proportional to each group's weighted
#   size (all workers -> employees only -> government -> private sector) and whose
#   gold segment is the share lacking employer-provided retirement access — so the
#   gold length is directly comparable across bars (it is the lacking headcount).
#   Below: a 100-square waffle of the PRIVATE-SECTOR workforce, split into
#   full-time and part-time blocks, gold = lacks access.
#
# Reads   : data/processed/sipp_fastfacts.parquet   (built by 01_build_dataset.R)
# Writes  : output/figures/fig1_private_access_waffle.png   (300 dpi, ragg)
#           output/figures/fig1_private_access_waffle.csv   (tidy background data)
#
# Style   : EIG 2022 primary palette + brand fonts (Tiempos Text / Galaxie Polaris),
#           Tufte graphical-quality layer — one emphasis hue (gold) + neutral grey,
#           direct labels (no legend clutter), no axes/gridlines/box, lie factor 1.
# Run     : Rscript code/17_fig1_hero_waffle.R
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(ggplot2); library(scales)
})

# ---- 0. Locate project root (robust to working directory) --------------------
.find_root <- function() {
  a <- commandArgs(trailingOnly = FALSE); f <- grep("^--file=", a, value = TRUE)
  if (length(f) == 1) return(dirname(dirname(normalizePath(sub("^--file=", "", f),
    winslash = "/", mustWork = FALSE))))
  getwd()
}
ROOT      <- .find_root()
PROC_DIR  <- file.path(ROOT, "data", "processed")
FIG_DIR   <- file.path(ROOT, "output", "figures")
THEME_DIR <- file.path(ROOT, "code", "_shared")
FONTS_DIR <- file.path(ROOT, "code", "_shared", "fonts")  # brand OTFs are not redistributed
if (!dir.exists(FIG_DIR)) dir.create(FIG_DIR, recursive = TRUE)

# ---- 1. Tokens + brand fonts (same registration path as 05_make_figures.R) ---
source(file.path(THEME_DIR, "eig_style_public.R"))
tokens <- eig_load_tokens()

# Vintage + Source: line come from the single source of truth (params.R).
path_project <- ROOT
source(file.path(ROOT, "code", "_shared", "params.R"))
GOLD <- unname(tokens$EIG_COLORS[["eig_gold_600"]])   # #E1AD28  emphasis: lacks access
TEAL <- unname(tokens$EIG_COLORS[["eig_teal_900"]])   # #024140  deck / accent
INK  <- "#1A1A1A"                                     # near-black text (softer than pure)
HAS  <- "#DAD9D2"                                     # neutral warm grey: has access (recedes)
SUBINK <- "#5A5A5A"

resolve_fonts <- function() {
  bf <- c(tiempos_reg = file.path(FONTS_DIR, "TiemposText-Regular.otf"),
          tiempos_semi = file.path(FONTS_DIR, "TiemposText-Semibold.otf"),
          tiempos_ital = file.path(FONTS_DIR, "TiemposText-RegularItalic.otf"),
          gp_book = file.path(FONTS_DIR, "GalaxiePolaris-Book.otf"),
          gp_bold = file.path(FONTS_DIR, "GalaxiePolaris-Bold.otf"),
          gp_light = file.path(FONTS_DIR, "GalaxiePolaris-Light.otf"))
  if (requireNamespace("systemfonts", quietly = TRUE) && all(file.exists(bf))) {
    ok <- tryCatch({
      systemfonts::register_font("Tiempos Text", plain = bf[["tiempos_reg"]],
        bold = bf[["tiempos_semi"]], italic = bf[["tiempos_ital"]],
        bolditalic = bf[["tiempos_semi"]])
      systemfonts::register_font("Galaxie Polaris", plain = bf[["gp_book"]],
        bold = bf[["gp_bold"]], italic = bf[["gp_book"]], bolditalic = bf[["gp_bold"]])
      systemfonts::register_font("Galaxie Polaris Light", plain = bf[["gp_light"]])
      TRUE }, error = function(e) FALSE)
    if (ok) return(list(headline = "Tiempos Text", body = "Galaxie Polaris",
                        light = "Galaxie Polaris Light", note = "brand OTFs"))
  }
  avail <- if (requireNamespace("systemfonts", quietly = TRUE))
    unique(systemfonts::system_fonts()$family) else character(0)
  hl <- if ("Georgia" %in% avail) "Georgia" else "serif"
  bd <- if ("Arial" %in% avail) "Arial" else "sans"
  list(headline = hl, body = bd, light = bd, note = "fallback fonts")
}
FNT <- resolve_fonts()   # not `F`: that masks base R's FALSE alias

# ---- 2. Data: group sizes + lacks-access rates, weighted ----------------------
df <- as.data.frame(read_parquet(file.path(PROC_DIR, "sipp_fastfacts.parquet")))
wrate <- function(sel) sum(df$WPFINWGT[sel & df$lacks_access]) / sum(df$WPFINWGT[sel])
all_sel  <- rep(TRUE, nrow(df))
emp_sel  <- df$worker_class %in% c("Private", "Government")   # employees only: excl. self-employed
gov_sel  <- df$worker_class == "Government"
priv_sel <- df$worker_class == "Private"
all_M  <- sum(df$WPFINWGT) / 1e6;            rate_all  <- wrate(all_sel)
emp_M  <- sum(df$WPFINWGT[emp_sel]) / 1e6;   emp_rate  <- wrate(emp_sel)
gov_M  <- sum(df$WPFINWGT[gov_sel]) / 1e6;   gov_rate  <- wrate(gov_sel)
priv_M <- sum(df$WPFINWGT[priv_sel]) / 1e6;  priv_rate <- wrate(priv_sel)

# Private-sector schedule split for the waffle
p  <- df[priv_sel, ]
w  <- p$WPFINWGT; tot <- sum(w)
ft <- !is.na(p$ft_pt) & p$ft_pt == "Full-time"
pt <- !is.na(p$ft_pt) & p$ft_pt == "Part-time"
ft_share <- sum(w[ft]) / tot;                 pt_share <- sum(w[pt]) / tot
ft_rate  <- sum(w[ft & p$lacks_access]) / sum(w[ft])
pt_rate  <- sum(w[pt & p$lacks_access]) / sum(w[pt])

# Largest-remainder rounding of the 2x2 to exactly 100 squares.
raw <- c(ftHas = ft_share*(1-ft_rate), ftLacks = ft_share*ft_rate,
         ptHas = pt_share*(1-pt_rate), ptLacks = pt_share*pt_rate) * 100
fl <- floor(raw); need <- 100 - sum(fl)
fl[order(raw - fl, decreasing = TRUE)[seq_len(need)]] <-
  fl[order(raw - fl, decreasing = TRUE)[seq_len(need)]] + 1
n_ft <- unname(fl["ftHas"] + fl["ftLacks"]); n_pt <- unname(fl["ptHas"] + fl["ptLacks"])

# ---- 3. Geometry ---------------------------------------------------------------
NC <- 10
x0 <- 0.5; x1 <- NC + 0.5      # full grid width = the all-workers bar
BH <- 0.42                     # bar half-height
sz <- function(pt) pt / .pt    # convert pt -> ggplot mm size
LX <- -0.35                    # right edge of the left label gutter

## Cascade bars (top of the figure). Length is proportional to weighted group
## size; the gold segment is the group's lacks-access share, so ACROSS bars the
## gold length is the lacking headcount on one common scale.
bars <- data.frame(
  name = c("All U.S. workers", "Employees only", "Government", "Private sector"),
  note = sprintf("%.1f million", c(all_M, emp_M, gov_M, priv_M)),
  frac = c(all_M, emp_M, gov_M, priv_M) / all_M,
  rate = c(rate_all, emp_rate, gov_rate, priv_rate),
  y    = c(1.05, -0.70, -2.45, -4.20)
)
bars$xmax  <- x0 + (x1 - x0) * bars$frac
bars$goldx <- x0 + (x1 - x0) * bars$frac * bars$rate
## % label sits at the gold edge when the grey remainder has room, else past the bar end
bars$labx  <- ifelse(bars$xmax - bars$goldx >= 1.3, bars$goldx, bars$xmax) + 0.15
bars$lab   <- percent(bars$rate, 0.1)

## Waffle blocks (below the bars; gold clusters at the FT/PT seam)
mk <- function(n_total, n_lacks, y_top, lacks_first) {
  i <- 0:(n_total - 1)
  status <- if (lacks_first) ifelse(i < n_lacks, "Lacks access", "Has access")
            else            ifelse(i < (n_total - n_lacks), "Has access", "Lacks access")
  data.frame(col = i %% NC + 1, y = y_top - (i %/% NC), status = status)
}
ft_top  <- -6.8                                   # below the last bar + its sub-label
ft_rows <- ceiling(n_ft / NC)
ft_blk  <- mk(n_ft, unname(fl["ftLacks"]), y_top = ft_top,  lacks_first = FALSE) # gold at bottom
pt_top  <- ft_top - (ft_rows - 1) - 2                                            # 1-row gap
pt_blk  <- mk(n_pt, unname(fl["ptLacks"]), y_top = pt_top,  lacks_first = TRUE)  # gold at top
waf <- rbind(ft_blk, pt_blk)
ft_mid <- ft_top - (ft_rows - 1) / 2
pt_rows <- ceiling(n_pt / NC)
pt_mid <- pt_top - (pt_rows - 1) / 2
y_bot  <- pt_top - (pt_rows - 1) - 0.9

# ---- 4. Plot -----------------------------------------------------------------
pl <- ggplot(waf, aes(col, y, fill = status)) +
  geom_tile(width = 0.9, height = 0.9) +
  scale_fill_manual(values = c("Lacks access" = GOLD, "Has access" = HAS)) +
  # legend key (top-left of panel, direct-labeled)
  annotate("rect", xmin = 1 - 0.45, xmax = 1 + 0.45, ymin = 2.6, ymax = 3.5, fill = GOLD) +
  annotate("text", x = 1.75, y = 3.05, label = "Lacks access", hjust = 0,
           family = FNT$body, size = sz(9.5), color = INK) +
  annotate("rect", xmin = 4.55, xmax = 5.45, ymin = 2.6, ymax = 3.5, fill = HAS) +
  annotate("text", x = 5.75, y = 3.05, label = "Has access", hjust = 0,
           family = FNT$body, size = sz(9.5), color = INK) +
  # cascade bars: grey base, gold lacking segment, group + size labels left, rate at gold edge
  geom_rect(data = bars, aes(xmin = x0, xmax = xmax, ymin = y - BH, ymax = y + BH),
            fill = HAS, inherit.aes = FALSE) +
  geom_rect(data = bars, aes(xmin = x0, xmax = goldx, ymin = y - BH, ymax = y + BH),
            fill = GOLD, inherit.aes = FALSE) +
  geom_text(data = bars, aes(x = LX, y = y + 0.42, label = name),
            hjust = 1, family = FNT$body, fontface = "bold", size = sz(10),
            color = INK, inherit.aes = FALSE) +
  geom_text(data = bars, aes(x = LX, y = y - 0.5, label = note),
            hjust = 1, family = FNT$body, size = sz(9), color = SUBINK,
            inherit.aes = FALSE) +
  geom_text(data = bars, aes(x = labx, y = y, label = lab),
            hjust = 0, family = FNT$body, fontface = "bold", size = sz(9.5),
            color = INK, inherit.aes = FALSE) +
  # full-time block label (left gutter)
  annotate("text", x = LX, y = ft_mid + 0.55, label = "Full-time", hjust = 1,
           family = FNT$body, fontface = "bold", size = sz(11), color = INK) +
  annotate("text", x = LX, y = ft_mid - 0.55, label = paste0(percent(ft_rate, 1), " lack access"),
           hjust = 1, family = FNT$body, size = sz(9.5), color = SUBINK) +
  # part-time block label
  annotate("text", x = LX, y = pt_mid + 0.55, label = "Part-time", hjust = 1,
           family = FNT$body, fontface = "bold", size = sz(11), color = INK) +
  annotate("text", x = LX, y = pt_mid - 0.55, label = paste0(percent(pt_rate, 1), " lack access"),
           hjust = 1, family = FNT$body, size = sz(9.5), color = SUBINK) +
  # section header for the waffle detail
  annotate("text", x = x0, y = ft_top + 1.2,
           label = "The private-sector workforce, square by square",
           hjust = 0, family = FNT$body, fontface = "bold", size = sz(9.5), color = SUBINK) +
  coord_equal(clip = "off") +
  scale_x_continuous(limits = c(-5.9, NC + 0.8), expand = c(0, 0)) +
  scale_y_continuous(limits = c(y_bot, 3.9), expand = c(0, 0)) +
  labs(
    title = "Half of workers have no retirement plan at work",
    subtitle = "Bar length is each group's share of all workers; gold marks those lacking access\nto an employer-provided plan. Each square below is 1% of the private sector.",
    tag = "Figure 1.",
    caption = paste0(
      "Note: Workers ages 18–64 employed in December ", REF_YEAR,
      ", weighted (", format(round(all_M, 1), nsmall = 1), " million).",
      " Bars share one scale, so the gold length is\nthe number of workers lacking access.",
      " “Employees only” = private + government wage-and-salary workers, excluding\nthe ",
      "self-employed, who lack employer-provided access by definition.",
      " “Lacks access” means no employer-provided\nretirement plan the worker is offered and eligible for.\n",
      "Source: ", sub("Participation, ", "Participation,\n", SRC_SIPP, fixed = TRUE))
  ) +
  theme_void(base_family = FNT$body) +
  theme(
    legend.position = "none",
    plot.title.position   = "plot",
    plot.caption.position  = "plot",
    plot.tag.position = c(0, 1),
    plot.tag = element_text(family = FNT$body, face = "bold", size = 10, color = TEAL, hjust = 0),
    plot.title = element_text(family = FNT$headline, face = "bold", size = 16, color = INK,
                              hjust = 0, lineheight = 1.05, margin = margin(t = 8, b = 6)),
    plot.subtitle = element_text(family = FNT$body, size = 10.5, color = "#333333", hjust = 0,
                                 lineheight = 1.2, margin = margin(b = 6)),
    plot.caption = element_text(family = FNT$light, size = 7.6, color = SUBINK, hjust = 0,
                                lineheight = 1.25, margin = margin(t = 14)),
    plot.margin = margin(14, 16, 12, 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ---- 5. Save PNG + tidy background CSV ---------------------------------------
png_path <- file.path(FIG_DIR, "fig1_private_access_waffle.png")
ragg::agg_png(png_path, width = 7.2, height = 9.5, units = "in", res = 300, background = "white")
print(pl); invisible(dev.off())

bg <- data.frame(
  group = c("All U.S. workers", "Employees only (private + government)", "Government",
            "Private (all)", "Full-time (private)", "Part-time (private)"),
  workers_millions = c(all_M, emp_M, gov_M, priv_M, sum(w[ft])/1e6, sum(w[pt])/1e6),
  share_of_all_workers = c(1, emp_M, gov_M, priv_M, NA, NA) / c(1, all_M, all_M, all_M, NA, NA),
  share_of_private = c(NA, NA, NA, 1, ft_share, pt_share),
  lacking_access_rate = c(rate_all, emp_rate, gov_rate, priv_rate, ft_rate, pt_rate),
  icons_total = c(NA, NA, NA, 100, n_ft, n_pt),
  icons_lacking = c(NA, NA, NA, unname(fl["ftLacks"] + fl["ptLacks"]),
                    unname(fl["ftLacks"]), unname(fl["ptLacks"])))
write.csv(bg, file.path(FIG_DIR, "fig1_private_access_waffle.csv"), row.names = FALSE)

# ---- 6. Interactive HTML version (website embed) -------------------------------
# Self-contained inline-SVG + vanilla-JS page generated from the SAME computed values
# as the PNG (living-repo convention: a vintage bump regenerates both; the interactive
# and static figures cannot drift). No external dependencies — deliberately NOT Plotly:
# a waffle is plain rectangles, and a dependency-free file embeds anywhere via one
# iframe. Brand fonts are referenced by name with system fallbacks (the embedding site
# loads the webfonts; font files are not distributed with this HTML).
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  message("17: jsonlite not installed; skipping the interactive HTML export.")
} else {
  payload <- list(
    tag      = "Figure 1.",
    title    = "Half of workers have no retirement plan at work",
    subtitle = paste0("Bar length is each group's share of all workers; gold marks those ",
                      "lacking access to an employer-provided plan. Each square below is ",
                      "1% of the private sector."),
    section  = "The private-sector workforce, square by square",
    note     = paste0("Note: Workers ages 18–64 employed in December ", REF_YEAR,
                      ", weighted (", format(round(all_M, 1), nsmall = 1), " million). ",
                      "Bars share one scale, so the gold length is the number of workers ",
                      "lacking access. “Employees only” = private + government ",
                      "wage-and-salary workers, excluding the self-employed, who lack ",
                      "employer-provided access by definition. “Lacks access” ",
                      "means no employer-provided retirement plan the worker is offered ",
                      "and eligible for."),
    source   = paste0("Source: ", SRC_SIPP),
    colors   = list(gold = GOLD, has = HAS, ink = INK, subink = SUBINK, teal = TEAL),
    bars     = data.frame(
      name = bars$name, millions = round(c(all_M, emp_M, gov_M, priv_M), 1),
      frac = round(bars$frac, 4), rate = round(bars$rate, 4)),
    waffle   = list(
      nc = NC, perSquareM = round(priv_M / 100, 2),
      blocks = data.frame(
        id     = c("ft", "pt"),
        label  = c("Full-time", "Part-time"),
        n      = c(n_ft, n_pt),
        lacks  = unname(c(fl["ftLacks"], fl["ptLacks"])),
        goldAt = c("end", "start"),          # gold clusters at the FT/PT seam
        rate   = round(c(ft_rate, pt_rate), 4),
        millionsLack = round(c(sum(w[ft & p$lacks_access]), sum(w[pt & p$lacks_access])) / 1e6, 1),
        millions     = round(c(sum(w[ft]), sum(w[pt])) / 1e6, 1)))
  )
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA)

  tpl <- r"---(<!-- fig1_private_access_waffle: interactive version. GENERATED by code/17_fig1_hero_waffle.R - do not hand-edit. -->
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
  .eigwaf * { box-sizing: border-box; margin: 0; }
  .eigwaf { max-width: 680px; margin: 0 auto; padding: 18px 16px 14px;
    background: #fff; color: #1A1A1A;
    font-family: "Galaxie Polaris", "Helvetica Neue", Helvetica, Arial, sans-serif; }
  .eigwaf .tag { font-weight: 700; font-size: 13px; letter-spacing: .02em; }
  .eigwaf h2 { font-family: "Tiempos Text", Georgia, "Times New Roman", serif;
    font-size: 24px; line-height: 1.12; margin: 6px 0 8px; font-weight: 700; }
  .eigwaf .sub { font-size: 13.5px; line-height: 1.35; color: #333; margin-bottom: 10px; }
  .eigwaf .legend { display: flex; gap: 18px; font-size: 12.5px; margin: 2px 0 10px; }
  .eigwaf .legend span { display: inline-flex; align-items: center; gap: 6px; }
  .eigwaf .legend i { width: 13px; height: 13px; display: inline-block; border-radius: 2px; }
  .eigwaf svg { width: 100%; height: auto; display: block; }
  .eigwaf svg text { font-family: inherit; }
  .eigwaf .note { font-size: 10.5px; line-height: 1.45; color: #5A5A5A; margin-top: 12px; }
  .eigwaf .src { font-size: 10.5px; color: #5A5A5A; margin-top: 4px; }
  .eigwaf .tip { position: fixed; pointer-events: none; z-index: 99; max-width: 240px;
    background: #1A1A1A; color: #fff; font-size: 12px; line-height: 1.4;
    padding: 7px 9px; border-radius: 4px; opacity: 0; transition: opacity .08s; }
  .eigwaf .tip b { color: #E1AD28; }
  .eigwaf rect.hoverable { cursor: pointer; }
  .eigwaf.dimming rect.waffle { opacity: .22; transition: opacity .1s; }
  .eigwaf.dimming rect.waffle.hl { opacity: 1; }
</style>
<figure class="eigwaf" id="eigwaf-fig1">
  <div class="tag" id="ew-tag"></div>
  <h2 id="ew-title"></h2>
  <div class="sub" id="ew-sub"></div>
  <div class="legend" id="ew-legend"></div>
  <svg id="ew-svg" role="img" aria-label="Population cascade and waffle chart of retirement-plan access"></svg>
  <figcaption>
    <div class="note" id="ew-note"></div>
    <div class="src" id="ew-src"></div>
  </figcaption>
  <div class="tip" id="ew-tip"></div>
</figure>
<script>
(function () {
  var D = __DATA__;
  var fig = document.getElementById("eigwaf-fig1");
  var svg = document.getElementById("ew-svg");
  var tip = document.getElementById("ew-tip");
  var NS = "http://www.w3.org/2000/svg";
  document.getElementById("ew-tag").textContent = D.tag;
  document.getElementById("ew-tag").style.color = D.colors.teal;
  document.getElementById("ew-title").textContent = D.title;
  document.getElementById("ew-sub").textContent = D.subtitle;
  document.getElementById("ew-note").textContent = D.note;
  document.getElementById("ew-src").textContent = D.source;
  document.getElementById("ew-legend").innerHTML =
    '<span><i style="background:' + D.colors.gold + '"></i>Lacks access</span>' +
    '<span><i style="background:' + D.colors.has + '"></i>Has access</span>';

  // ---- geometry (all in viewBox units) ----
  var GUT = 178, W = 650, CELL = 41, GAP = 6, STEP = CELL + GAP;
  var gridW = D.waffle.nc * STEP - GAP, gx = GUT + 4;
  var barH = 24, barStep = 58, y = 6;
  function el(tag, at, parent) { var e = document.createElementNS(NS, tag);
    for (var k in at) e.setAttribute(k, at[k]); (parent || svg).appendChild(e); return e; }
  function txt(x, yy, s, at, parent) { var a = { x: x, y: yy, fill: D.colors.ink,
      "font-size": 12.5 }; for (var k in (at||{})) a[k] = at[k];
    var e = el("text", a, parent); e.textContent = s; return e; }
  function fmtM(m) { return m.toFixed(1) + " million"; }
  function pct(r, d) { return (100 * r).toFixed(d === undefined ? 1 : d) + "%"; }

  // ---- cascade bars ----
  D.bars.forEach(function (b) {
    var wFull = gridW * b.frac, wGold = wFull * b.rate;
    txt(GUT - 8, y + 10, b.name, { "text-anchor": "end", "font-weight": "bold", "font-size": 13 });
    txt(GUT - 8, y + 25, fmtM(b.millions), { "text-anchor": "end", fill: D.colors.subink, "font-size": 11 });
    var tipHtml = "<b>" + b.name + "</b><br>" + fmtM(b.millions) + " workers; " +
      fmtM(b.millions * b.rate) + " (" + pct(b.rate) + ") lack employer-provided access.";
    [[gx, wFull, D.colors.has], [gx, wGold, D.colors.gold]].forEach(function (seg) {
      var r = el("rect", { x: seg[0], y: y + 2, width: seg[1], height: barH,
        fill: seg[2], "class": "hoverable" });
      r.dataset.tip = tipHtml;
    });
    var labX = (wFull - wGold >= 52 ? gx + wGold : gx + wFull) + 6;
    txt(labX, y + 2 + barH / 2 + 4.5, pct(b.rate), { "font-weight": "bold", "font-size": 12 });
    y += barStep;
  });

  // ---- waffle section header ----
  y += 8;
  txt(gx, y + 6, D.section, { "font-weight": "bold", fill: D.colors.subink, "font-size": 12 });
  y += 18;

  // ---- waffle blocks ----
  D.waffle.blocks.forEach(function (blk) {
    var rows = Math.ceil(blk.n / D.waffle.nc);
    var blockTop = y;
    for (var i = 0; i < blk.n; i++) {
      var lacks = (blk.goldAt === "start") ? (i < blk.lacks) : (i >= blk.n - blk.lacks);
      var cx = gx + (i % D.waffle.nc) * STEP,
          cy = blockTop + Math.floor(i / D.waffle.nc) * STEP;
      var r = el("rect", { x: cx, y: cy, width: CELL, height: CELL, rx: 2,
        fill: lacks ? D.colors.gold : D.colors.has,
        "class": "waffle hoverable", "data-grp": blk.id + (lacks ? "L" : "H") });
      r.dataset.tip = "<b>" + blk.label + ", " + (lacks ? "lacks" : "has") +
        " access</b><br>Each square ≈ " + D.waffle.perSquareM +
        " million workers (1% of the private sector).<br>" + pct(blk.rate, 0) +
        " of " + blk.label.toLowerCase() + " private-sector employees (" +
        blk.millionsLack + " of " + blk.millions + " million) lack access.";
    }
    var mid = blockTop + (rows * STEP - GAP) / 2;
    txt(GUT - 8, mid - 3, blk.label, { "text-anchor": "end", "font-weight": "bold", "font-size": 13.5 });
    txt(GUT - 8, mid + 13, pct(blk.rate, 0) + " lack access",
        { "text-anchor": "end", fill: D.colors.subink, "font-size": 11 });
    y = blockTop + rows * STEP - GAP + 34;
  });
  svg.setAttribute("viewBox", "0 0 " + W + " " + (y - 24));

  // ---- tooltip + segment highlighting ----
  function move(ev) { var pad = 14,
      x = Math.min(ev.clientX + pad, window.innerWidth - tip.offsetWidth - 8),
      yy = Math.min(ev.clientY + pad, window.innerHeight - tip.offsetHeight - 8);
    tip.style.left = x + "px"; tip.style.top = yy + "px"; }
  svg.addEventListener("mousemove", function (ev) {
    var t = ev.target;
    if (t.dataset && t.dataset.tip) {
      tip.innerHTML = t.dataset.tip; tip.style.opacity = 1; move(ev);
      if (t.dataset.grp) { fig.classList.add("dimming");
        svg.querySelectorAll("rect.waffle").forEach(function (r) {
          r.classList.toggle("hl", r.dataset.grp === t.dataset.grp); }); }
      else fig.classList.remove("dimming");
    } else { tip.style.opacity = 0; fig.classList.remove("dimming"); }
  });
  svg.addEventListener("mouseleave", function () {
    tip.style.opacity = 0; fig.classList.remove("dimming"); });
})();
</script>
)---"

  html_path <- file.path(FIG_DIR, "fig1_private_access_waffle.html")
  writeLines(sub("__DATA__", json, tpl, fixed = TRUE), html_path, useBytes = TRUE)
  cat("Wrote ", html_path, " (interactive; self-contained, no dependencies)\n", sep = "")
}

cat(sprintf("Fonts: %s\n", FNT$note))
cat(sprintf("Cascade: all %.1fM %.1f%% | employees %.1fM %.1f%% | gov %.1fM %.1f%% | private %.1fM %.1f%%\n",
            all_M, 100*rate_all, emp_M, 100*emp_rate, gov_M, 100*gov_rate, priv_M, 100*priv_rate))
cat(sprintf("Waffle: FT %.0f%% lack (%d icons, %d gold) | PT %.0f%% lack (%d icons, %d gold)\n",
            100*ft_rate, n_ft, fl["ftLacks"], 100*pt_rate, n_pt, fl["ptLacks"]))
cat("Wrote ", png_path, "\n", sep = "")
