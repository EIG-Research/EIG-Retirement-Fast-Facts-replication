# code/

Analysis pipeline for *The U.S. Retirement System: Fast Facts* (SIPP 2025 release, pooled
2022–2025 panels, **reference year 2024**; civilian labor force). Orchestrated by [`run_all.R`](run_all.R), which sources each flagged
numbered script in a fresh environment and then runs the `testthat` suite
(`stop_on_failure = TRUE`). Every script resolves the repo root with `here::here()` and reads
shared configuration from [`_shared/params.R`](_shared/params.R) — the single switch for the
vintage, policy thresholds, the tax-engine selector, and filenames.

The per-script reads and writes are listed in the table below, and the exhibit table in
[`../README.md`](../README.md) maps each published exhibit to the script that produces it. For how
the access / participation / matching pillar metrics are defined and justified, see
[`_shared/README.md`](_shared/README.md).

## Run it

```bash
Rscript code/run_all.R          # from the repo root
```

Requires R 4.4.x with `here`, `arrow`, `dplyr`, `ggplot2`, `survey`, and (for the default tax
engine) Python 3.14 with `policyengine-us==1.772.0` + `pyarrow` at `params.R::PYTHON_BIN`.
`REBUILD_EXTRACT <- TRUE` (the committed default) re-reads the raw `.dta` on every run; set it
`FALSE` to reuse the cached extract. Toggle individual stages with the `run_*` flags at the top of
`run_all.R`. A timestamped log is written to `output/logs/`.

## Pipeline stages (execution order)

| # | Script | Does |
|---|--------|------|
| 01 | `01_build_dataset.R` | Read raw SIPP, restrict to employed Dec workers 18–64, derive pillar flags + `gov_subclass`; write the analysis frame |
| 02 | `02_analysis.R` | Access / participation / matching pillar tables |
| 03 | `03_savers_match.R` | Saver's Match income-eligibility and the eligibility×access wedge |
| 07 | `07_replicate_weights.R` | Fay replicate-weight standard errors (240 replicates) |
| 05 | `05_make_figures.R` | Pillar figures (PNG + background CSV), EIG theme |
| 08 | `08_tax_units.R` | Tax-expenditure stage 1–2: annualize income, CBO deciles, TY2024 filing units |
| 09 | `09_tax_engine.R` | Runs the tax stage → `09_policyengine_te.py` (PolicyEngine-US) |
| 11 | `11_stage5_imputations.R` | Impute employer DB, Roth split, outside IRA (external benchmarks) |
| 10 | `10_tax_expenditure.R` | CBO present-value tax expenditure by decile + benchmarks |
| 12 | `12_te_benchmarks_se.R` | External calibration + replicate-weight SEs + summary stats |
| 14 | `14_te_crosscuts.R` | Tax expenditure crossed with pillars + demographics |
| 13 | `13_te_figures.R` | Tax-expenditure figures (PNG + background CSV) |
| 16 | `16_access_gap_table.R` | Census-style access-gap table + within-government detail (footnote 3) |
| 17 | `17_fig1_hero_waffle.R` | Hero waffle figure (Figure 1) |
| 15 | `15_publish_datawrapper.R` | Chart payload specs for every figure/table exhibit |

Notes: `15_publish_datawrapper.R` runs last and builds the chart specification payloads, including
the three exhibits it alone produces (`te_fig1b_decile_share`, `access_gap_table`,
`table1_definitions`). Ordering that is not purely numeric: `14` before `13`, `11` before `10`,
and `07` before `12` and `05`. Each inversion is a real data dependency.

## `_shared/` and `tests/`

- [`_shared/`](_shared) — `params.R` (config), `extract_vars.R` (the 188 SIPP variables),
  `helpers.R` (weighting/breakout/funnel), `pv_te.R` (CBO present-value math),
  `eig_style_public.R` (palette + ggplot theme), and [`README.md`](_shared/README.md)
  (metric definitions).
- `tests/` — `testthat.R` runs `test-invariants.R` (locked RY2024 civilian-labor-force baselines:
  N = 12,330; 147.3M weighted; H1 = 51.7%), `test-wavec-pending.R`, `test-pv-te.R`, and
  `test-tax-expenditure.R`.
