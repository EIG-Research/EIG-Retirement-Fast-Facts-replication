# The U.S. Retirement System: Fast Facts — Replication Package

Public replication materials for the Economic Innovation Group analysis *The U.S.
Retirement System: Fast Facts*. This repository holds the analysis code, the exhibits
that appear in the publication, and the documentation needed to obtain and prepare the
input data.

The analysis characterizes the U.S. retirement savings system along three pillars —
**access**, **participation**, and **employer contributions/matching** — for the full
civilian working population (not only private-sector employees), using the Census
Bureau's Survey of Income and Program Participation (SIPP). It then places those facts
in policy context with an estimate of the federal retirement tax expenditure on the
Congressional Budget Office's present-value basis, and a measure of the gap between
Saver's Match income eligibility and account access.

**Vintage:** SIPP 2025 release (pooled 2022–2025 panels), December 2024 reference month,
calendar-year 2024 income. Population: 147.3 million civilian workers ages 18–64
(unweighted N = 12,330).

## What is here

| Path | Contents |
|---|---|
| `code/` | The analysis pipeline. Entry point is `code/run_all.R`. |
| `code/_shared/` | Constants (`params.R`), helpers, the present-value math (`pv_te.R`), the extract variable list, and the chart style module. `code/_shared/README.md` documents how every pillar metric is constructed from SIPP variables. |
| `code/tests/` | `testthat` suite of invariant and tax-expenditure checks, run at the end of `run_all.R`. |
| `output/figures/` | The R-generated figures, each with the tidy CSV behind it. |
| `output/tables/` | Every analysis table that sources a number in the publication. |
| `data/raw/external_benchmarks/` | Small, hand-entered aggregate benchmarks (BEA, IRS SOI, Vanguard) read by the imputation stage. Each row carries its source and vintage. |
| `data/raw/irs_soi/` | IRS Statistics of Income tables kept as a filer-basis reference. |
| `DATA.md` | Every dataset used, its source and vintage, and how to obtain it. |

## The publication's exhibits, and the code behind each

The charts in the publication were finished in Datawrapper, and those rendered images are
not redistributed here. What this repository provides is the code and the tidy data behind
every one of them. "Figure shipped" means an image exists in `output/figures/`; where it
does not, the exhibit's data and its full chart specification still ship, and
`code/15_publish_datawrapper.R` rebuilds the specification — titles, notes, and rows.

| # | Exhibit | Key | Produced by | Figure shipped |
|---|---|---|---|---|
| 1 | Half of workers have no retirement plan at work | `fig1_private_access_waffle` | `17_fig1_hero_waffle.R` | yes |
| 2 | Lack of employer plan access falls with education | `fig3alt_access_by_race_educ` | `05_make_figures.R`, `15_publish_datawrapper.R` | yes |
| 3 | Low earners lag on access, participation, and contributions | `fig2_income_ladder` | `05_make_figures.R`, `15_publish_datawrapper.R` | yes |
| 4 | High earners are far likelier to participate without a match | `fig2b_participation_without_match` | `05_make_figures.R`, `15_publish_datawrapper.R` | yes |
| 5 | The top income decile captures about 33 percent of the tax expenditure | `te_fig1b_decile_share` | `15_publish_datawrapper.R` | no — spec + `te_decile_cbo_comparable.csv` |
| 6 | Eligible for the Saver's Match, but locked out | `fig4_savers_match_wedge` | `05_make_figures.R`, `15_publish_datawrapper.R` | yes |
| 7 | The retirement access gap across the workforce (appendix) | `access_gap_table` | `16_access_gap_table.R`, `15_publish_datawrapper.R` | no — `access_gap_descriptive.csv` / `.html` |
| 8 | How access, participation, and matching are defined and counted (appendix) | `table1_definitions` | `15_publish_datawrapper.R` | no — spec + `access_headline.csv`, `access_alt_definitions.csv` |

## How to replicate

1. **Obtain the SIPP microdata.** The two large input files are not redistributed here.
   Follow `DATA.md` to download the 2025 SIPP public-use file and its replicate-weight
   file, and place them at `data/raw/pu2025.dta` and `data/raw/rw2025.dta`.

2. **Install the R dependencies.** R 4.4 or later. The pipeline uses `here`, `arrow`,
   `dplyr`, `tidyr`, `readr`, `ggplot2`, `scales`, `jsonlite`, `ragg`, `systemfonts`, and
   `testthat`. Each script loads its own libraries; `run_all.R` checks the core three up
   front. Versions are **not** pinned — see *Environment used for the published run*
   below for what was installed when the published estimates were produced.

3. **Install the tax engine.** The tax-expenditure stage calls PolicyEngine-US from
   Python. Install `policyengine-us` at the version pinned in
   `code/_shared/params.R` (`POLICYENGINE_VERSION`) and point `SIPP_PYTHON_BIN` at that
   interpreter:

   ```bash
   pip install policyengine-us==1.772.0
   export SIPP_PYTHON_BIN=/path/to/python
   ```

4. **Run the pipeline** from the repository root:

   ```bash
   Rscript code/run_all.R
   ```

   Every stage is behind a `TRUE`/`FALSE` flag at the top of `run_all.R`, so a targeted
   rerun is a matter of flipping flags. The first run needs `REBUILD_EXTRACT <- TRUE` in
   `code/_shared/params.R` to build the cached extract from the raw `.dta`. Outputs land
   in `output/`, and a per-run log is written to `output/logs/`.

Stage order in `run_all.R` is deliberately not numeric — `14` runs before `13`, `11`
before `10`, `07` before `12` and `05`. Each inversion is a real data dependency
documented in `code/README.md`; renaming the scripts to "fix" the order will break it.
In particular `15_publish_datawrapper.R` runs last, because it consumes the tidy figure
CSVs that `05` and `13` write.

## Environment used for the published run

Recorded for reference, not pinned. The pipeline is not locked to these versions and no
lockfile is shipped; later versions are expected to work, and any that do not is a bug
worth reporting.

| Component | Version |
|---|---|
| R | 4.4.3 |
| `arrow` | 23.0.1.2 |
| `dplyr` | 1.2.1 |
| `ggplot2` | 4.0.3 |
| `here` | 1.0.2 |
| `jsonlite` | 2.0.0 |
| `ragg` | 1.5.2 |
| `readr` | 2.2.0 |
| `scales` | 1.4.0 |
| `systemfonts` | 1.3.2 |
| `testthat` | 3.3.2 |
| `tidyr` | 1.3.2 |
| Python | 3.14.3 |
| `policyengine-us` | 1.772.0 |

`policyengine-us` is the one component whose version materially changes the numbers, since
it encodes the tax law used to compute the expenditure. It is asserted at run time: the
Python worker compares the installed version against `POLICYENGINE_VERSION` in
`code/_shared/params.R` and stops if they differ, so a silent engine drift cannot pass
unnoticed.

## Reproducibility notes

- **This package has been run end to end.** Starting from the raw SIPP microdata in a
  clean tree containing nothing but these published files plus the two Census `.dta`
  inputs, `code/run_all.R` completed all 15 stages in 3 minutes 37 seconds; the
  `testthat` suite passed all 103 assertions; and every one of the 47 CSVs in `output/`
  was reproduced cell for cell, along with `te_notes.txt` and both HTML outputs.

- **All estimates are weighted** by the SIPP final person weight `WPFINWGT`
  (`WHFNWGT` for household-level quantities). Confidence intervals come from SIPP's 240
  Fay replicate weights, following the 2025 SIPP Users' Guide.
- **Active-duty military are excluded.** The pipeline drops every household containing an
  active-duty member (`EXCLUDE_MILITARY` in `code/_shared/params.R`), which narrows the
  frame from roughly 148.6 million all-worker to 147.3 million civilian workers. The
  sample impact is written to `output/tables/military_exclusion_impact.csv`. Set the flag
  to `FALSE` to restore the all-worker frame.
- **Two stages require the SIPP microdata**, not just the shipped tables:
  `16_access_gap_table.R` and `17_fig1_hero_waffle.R` read the derived person-level frame.
  The remaining figure and chart stages (`05`, `13`, `15`) run from the CSVs already in
  `output/`, so figures can be regenerated without SIPP access.
- **Typography will differ.** EIG's brand typefaces are commercially licensed and are not
  redistributed. The figure scripts detect their absence and fall back to a system
  serif/sans stack, reporting which fonts they used. Regenerated figures match the
  published PNGs in color and layout but not in typeface.

## Citation

See `CITATION.cff`. Please cite the publication rather than this repository alone when
referring to the findings.

## License

MIT — see `LICENSE`. The SIPP microdata are public-domain U.S. government data; the
third-party benchmark aggregates under `data/raw/` remain the property of their sources
(BEA, IRS, Vanguard), which are cited per row.
