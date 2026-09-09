# ==============================================================================
# 16_access_gap_table.R
# One Census-style descriptive table of the retirement ACCESS GAP for all workers,
# stacking every subset the pipeline breaks out (class of worker, work schedule,
# sex, race/ethnicity, education, age, disability, earnings decile) into a single
# table with, per subset: total workers, share of all workers, number lacking
# employer-provided access, the lacking-access rate, and each subset's share of
# the total access gap.
#
# Reads   : data/processed/sipp_fastfacts.parquet   (built by 01_build_dataset.R)
# Writes  : output/tables/access_gap_descriptive.csv   (machine-readable, full precision)
#           output/tables/access_gap_descriptive.html  (self-contained, EIG-styled)
#
# Measure : "lacks_access" (H1) = lacks employer-provided access (offered & eligible).
#           Self-employed count as lacking employer-provided access by construction
#           (see 01_build_dataset.R). Universe: employed workers 18-64, the December
#           reference month of the current SIPP vintage (labels derive from params.R
#           REF_YEAR / SIPP_PANEL). All estimates weighted (WPFINWGT).
#
# Run     : Rscript code/16_access_gap_table.R
#           (run_all.R wires the flag; do not edit run_all.R here)
# ==============================================================================

suppressWarnings(suppressMessages(library(arrow)))

path_project <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
source(file.path(path_project, "code", "_shared", "params.R"))
source(file.path(path_project, "code", "_shared", "helpers.R"))
path_processed <- file.path(path_project, "data", "processed")
path_tables    <- file.path(path_project, "output", "tables")
dir.create(path_tables, showWarnings = FALSE, recursive = TRUE)

df <- as.data.frame(read_parquet(file.path(path_processed, "sipp_fastfacts.parquet")))
df$.univ <- TRUE

## --- Dimension display metadata (panel label + preferred group order) ---------
## Groups not listed in `order` are appended (never silently dropped) so a new
## category (e.g. a residual "Unknown" worker class) still surfaces.
DIM_META <- list(
  list(var = "worker_class", label = "Class of worker",
       order = c("Private", "Government", "Self-employed")),
  list(var = "ft_pt",        label = "Work schedule",
       order = c("Full-time", "Part-time")),
  list(var = "sex",          label = "Sex",
       order = c("Male", "Female")),
  list(var = "race_eth",     label = "Race and ethnicity",
       order = c("White (NH)", "Black (NH)", "Hispanic", "Asian (NH)", "Other (NH)")),
  list(var = "educ_grp",     label = "Educational attainment",
       order = c("Less than HS", "HS grad", "Some college/assoc", "Bachelor's+")),
  list(var = "age_band",     label = "Age",
       order = c("18-24", "25-34", "35-44", "45-54", "55-64")),
  list(var = "disability",   label = "Disability status",
       order = c("No disability", "Disability")),
  list(var = "earn_decile",  label = "Annual earnings decile (1 = lowest)",
       order = as.character(1:10))
)
DIMS <- vapply(DIM_META, function(m) m$var, character(1))

## --- Core computation: one breakout of lacks_access over all dimensions -------
bo <- breakout(df, "lacks_access", ".univ", DIMS)
overall <- bo[bo$dimension == "Overall", ]
tot_workers <- overall$wt_den   # weighted count of all workers
tot_gap     <- overall$wt_num   # weighted count lacking access (the total access gap)

## Enrich a breakout slice into the descriptive columns.
enrich <- function(rows, panel_label) {
  data.frame(
    panel                    = panel_label,
    group                    = rows$group,
    n_unweighted             = rows$n_den,
    workers_weighted         = rows$wt_den,
    share_of_workers         = rows$wt_den / tot_workers,
    lacking_access_weighted  = rows$wt_num,
    lacking_access_rate      = rows$share,
    share_of_total_gap       = rows$wt_num / tot_gap,
    stringsAsFactors = FALSE
  )
}

## --- Assemble the stacked table: overall anchor row, then one panel per dim ---
tab <- enrich(overall, "All workers")
tab$group <- "All workers"
for (m in DIM_META) {
  slice <- bo[bo$dimension == m$var, ]
  present <- slice$group
  ord <- c(intersect(m$order, present), setdiff(present, m$order))
  slice <- slice[match(ord, slice$group), , drop = FALSE]
  tab <- rbind(tab, enrich(slice, m$label))
}

## --- Write the machine-readable CSV (full precision) --------------------------
write.csv(tab, file.path(path_tables, "access_gap_descriptive.csv"), row.names = FALSE)

## --- Render the self-contained, EIG-styled HTML table -------------------------
## EIG 2022 primary palette (see code/_shared/eig_style_public.R):
EIG_TEAL <- "#024140"; EIG_GREEN <- "#19644D"; EIG_CREAM <- "#FEECD6"
EIG_GOLD <- "#E1AD28"; EIG_INK <- "#000000";  EIG_RULE <- "#D9D9D9"

esc  <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
fmtM <- function(x) formatC(x / 1e6, format = "f", digits = 1, big.mark = ",")
fmtP <- function(x) sprintf("%.1f%%", 100 * x)
fmtN <- function(x) formatC(x, format = "d", big.mark = ",")

data_row <- function(r, bold = FALSE, indent = TRUE) {
  cls <- if (bold) " class=\"tot\"" else ""
  pad <- if (indent && !bold) " style=\"padding-left:22px\"" else ""
  paste0(
    "<tr", cls, ">",
    "<td", pad, ">", esc(r$group), "</td>",
    "<td class=\"num\">", fmtN(r$n_unweighted), "</td>",
    "<td class=\"num\">", fmtM(r$workers_weighted), "</td>",
    "<td class=\"num\">", fmtP(r$share_of_workers), "</td>",
    "<td class=\"num\">", fmtM(r$lacking_access_weighted), "</td>",
    "<td class=\"num rate\">", fmtP(r$lacking_access_rate), "</td>",
    "<td class=\"num\">", fmtP(r$share_of_total_gap), "</td>",
    "</tr>")
}

body <- data_row(tab[1, ], bold = TRUE, indent = FALSE)   # All workers anchor row
for (m in DIM_META) {
  body <- c(body, paste0(
    "<tr class=\"panel\"><td colspan=\"7\">", esc(m$label), "</td></tr>"))
  slice <- tab[tab$panel == m$label, ]
  for (i in seq_len(nrow(slice))) body <- c(body, data_row(slice[i, ]))
}

html <- paste0(
'<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">',
'<meta name="viewport" content="width=device-width, initial-scale=1">',
'<title>Retirement access gap by worker characteristic</title><style>',
'body{font-family:"Galaxie Polaris","Helvetica Neue",Arial,sans-serif;color:', EIG_INK,
';margin:0;padding:32px;background:#fff;-webkit-font-smoothing:antialiased}',
'.wrap{max-width:900px;margin:0 auto}',
'h1{font-size:22px;line-height:1.25;margin:0 0 4px}',
'.sub{font-size:14px;color:#3d3d3d;margin:0 0 18px}',
'.scroll{overflow-x:auto}',
'table{border-collapse:collapse;width:100%;font-size:13px}',
'caption{caption-side:top;text-align:left}',
'thead th{background:', EIG_TEAL, ';color:#fff;font-weight:600;text-align:right;',
'padding:9px 10px;vertical-align:bottom;line-height:1.2}',
'thead th.lbl{text-align:left}',
'th.grp{width:34%}',
'td{padding:6px 10px;border-bottom:1px solid ', EIG_RULE, '}',
'td.num{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}',
'td.rate{font-weight:600;color:', EIG_GREEN, '}',
'tr.tot td{font-weight:700;background:', EIG_CREAM, ';border-top:2px solid ', EIG_TEAL,
';border-bottom:2px solid ', EIG_TEAL, '}',
'tr.tot td.rate{color:', EIG_TEAL, '}',
'tr.panel td{background:', EIG_GREEN, ';color:#fff;font-weight:600;font-size:12px;',
'letter-spacing:.02em;text-transform:uppercase;padding:6px 10px}',
'tr:hover td{background:#f6f6f4}tr.tot:hover td{background:', EIG_CREAM, '}',
'tr.panel:hover td{background:', EIG_GREEN, '}',
'.notes{font-size:11.5px;color:#555;margin-top:14px;line-height:1.5}',
'.notes b{color:#333}.src{margin-top:8px;font-size:11.5px;color:#555}',
'</style></head><body><div class="wrap">',
'<h1>Who is left out: the retirement access gap by worker characteristic</h1>',
'<p class="sub">Employed workers ages 18&ndash;64, December ', REF_YEAR, ' reference month. ',
## Pooled-release citation: no SPANEL/SWAVE filter exists anywhere in the
## pipeline, so the label names the release and the pooled panel range, never a single panel/wave.
'SIPP ', SIPP_PANEL, ' release (pooled ', SIPP_PANEL_LABEL, ' panels). ',
'All figures weighted (person weight <code>WPFINWGT</code>).</p>',
'<div class="scroll"><table>',
'<thead><tr>',
'<th class="lbl grp">Worker characteristic</th>',
'<th>Sample<br>(unweighted)</th>',
'<th>Workers<br>(millions)</th>',
'<th>Share of<br>all workers</th>',
'<th>Lacking access<br>(millions)</th>',
'<th>Lacking-<br>access rate</th>',
'<th>Share of<br>total gap</th>',
'</tr></thead><tbody>',
paste(body, collapse = ""),
'</tbody></table></div>',
'<div class="notes">',
'<p><b>Lacking access</b> means the worker lacks employer-provided access to a ',
'qualifying, tax-advantaged retirement plan (a plan that is both offered by the ',
'employer and one the worker is eligible for). Self-employed workers are counted as ',
'lacking employer-provided access by construction.</p>',
'<p><b>Lacking-access rate</b> is the share <i>within</i> each subset that lacks access. ',
'<b>Share of total gap</b> is each subset&rsquo;s share of the ',
fmtM(tot_gap), ' million workers who lack access. <b>Share of all workers</b> and ',
'<b>share of total gap</b> are shares of the full population; within a characteristic ',
'they may not sum to 100 percent because a small number of workers do not report that ',
'characteristic (e.g. work schedule).</p>',
'<p class="src">Source: Economic Innovation Group analysis of the U.S. Census Bureau ',
'Survey of Income and Program Participation (SIPP), ', SIPP_PANEL,
' release (pooled ', SIPP_PANEL_LABEL, ' panels), December ', REF_YEAR, ' reference month.</p>',
'</div></div></body></html>')

writeLines(html, file.path(path_tables, "access_gap_descriptive.html"), useBytes = TRUE)

## --- Console summary ----------------------------------------------------------
cat("\n========== ACCESS-GAP DESCRIPTIVE TABLE ==========\n")
cat(sprintf("All workers: %s million; lacking access: %s million (%s).\n",
            fmtM(tot_workers), fmtM(tot_gap), fmtP(tot_gap / tot_workers)))
cat(sprintf("Rows: %d (1 overall + %d dimensions). Sample N = %s.\n",
            nrow(tab), length(DIM_META), fmtN(overall$n_den)))
cat("Wrote output/tables/access_gap_descriptive.csv and .html\n")

## --- Government access-gap detail (draft footnote 3) --------------------------
## Reproduces, from a committed script, the within-government figures the brief cites:
## the federal/state/local split (share of the government access gap and each group's
## lacking-access rate), plus lacking-access rates by work schedule, all-worker earnings
## decile, age, and education, and the full-time headcount share of the government gap.
## "Government" = worker_class "Government" = federal-civilian + state + local (active-duty military
## excluded upstream at the household level, EXCLUDE_MILITARY 2026-07-16; params.R CLWRK_GOVERNMENT).
## gov_subclass (Federal/State/Local) is built in 01_build_dataset.R. Addresses the question of
## flagged these numbers as having no supporting code.
gov <- df[df$worker_class == "Government", , drop = FALSE]
gov$.univ <- TRUE
GOV_DIMS <- c("gov_subclass", "ft_pt", "earn_decile", "age_band", "educ_grp")
gbo     <- breakout(gov, "lacks_access", ".univ", by_vars = GOV_DIMS)
gov_all <- gbo[gbo$dimension == "Overall", ]
gov_n   <- gov_all$wt_den          # weighted government workers
gov_gap <- gov_all$wt_num          # weighted government workers lacking access

## Per-group rows: within-group lacking-access rate and each group's share of the
## government access gap (headcount of government workers lacking access).
gov_detail <- do.call(rbind, lapply(GOV_DIMS, function(dim) {
  s <- gbo[gbo$dimension == dim, ]
  data.frame(
    dimension           = dim,
    group               = s$group,
    n_unweighted        = s$n_den,
    workers_weighted    = s$wt_den,
    lacking_weighted    = s$wt_num,
    lacking_access_rate = s$share,               # rate within the group
    share_of_gov_gap    = s$wt_num / gov_gap,    # group's share of the government access gap
    stringsAsFactors = FALSE)
}))

## Government-level summary + comparison to the full population and the private sector.
priv <- df[df$worker_class == "Private", , drop = FALSE]
ft_gap_share <- sum(gov$WPFINWGT[gov$lacks_access & !is.na(gov$ft_pt) &
                                 gov$ft_pt == "Full-time"], na.rm = TRUE) / gov_gap
gov_summary <- data.frame(
  metric = c("gov_workers_weighted", "gov_share_of_all_workers",
             "gov_lacking_weighted", "gov_lacking_rate", "gov_share_of_total_gap",
             "ft_headcount_share_of_gov_gap", "private_lacking_rate"),
  value  = c(gov_n, gov_n / tot_workers,
             gov_gap, gov_gap / gov_n, gov_gap / tot_gap,
             ft_gap_share,
             sum(priv$WPFINWGT[priv$lacks_access], na.rm = TRUE) / sum(priv$WPFINWGT)),
  stringsAsFactors = FALSE)

write.csv(gov_detail,  file.path(path_tables, "government_access_detail.csv"),  row.names = FALSE)
write.csv(gov_summary, file.path(path_tables, "government_access_summary.csv"), row.names = FALSE)

cat("\n========== GOVERNMENT ACCESS-GAP DETAIL (draft footnote 3) ==========\n")
cat(sprintf("Government: %s million workers (%s of all); lacking access %s million (%s of gov; %s of the total gap).\n",
            fmtM(gov_n), fmtP(gov_n / tot_workers), fmtM(gov_gap),
            fmtP(gov_gap / gov_n), fmtP(gov_gap / tot_gap)))
sub <- gov_detail[gov_detail$dimension == "gov_subclass", ]
for (i in seq_len(nrow(sub)))
  cat(sprintf("  %-8s lacking-access rate %s, share of gov gap %s\n",
              sub$group[i], fmtP(sub$lacking_access_rate[i]), fmtP(sub$share_of_gov_gap[i])))
cat(sprintf("  Full-time headcount share of gov gap %s; private-sector lacking rate %s.\n",
            fmtP(ft_gap_share), fmtP(gov_summary$value[gov_summary$metric == "private_lacking_rate"])))
cat("Wrote output/tables/government_access_detail.csv and government_access_summary.csv\n")
