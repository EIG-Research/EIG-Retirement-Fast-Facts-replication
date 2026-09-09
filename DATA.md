# Data Documentation — The U.S. Retirement System: Fast Facts

Every dataset the analysis uses, its source and vintage, how it was used, and how to
obtain it. The two SIPP microdata files are not redistributed here; everything else the
pipeline reads is included in this repository.

## Datasets

| Dataset | Source / agency | Vintage | How it was used | Access |
|---|---|---|---|---|
| SIPP public-use file (`pu2025.dta`) | U.S. Census Bureau | 2025 release, pooling SPANEL 2022–2025; December 2024 reference month, calendar-year 2024 income | The analysis frame. `01_build_dataset.R` reads it directly and derives access, participation, matching, worker class, income, and demographics. | Download (~2.76 GB). Not included. |
| SIPP replicate weights (`rw2025.dta`) | U.S. Census Bureau | 2025 release, reference year 2024 | 240 Fay replicate weights for standard errors and 95 percent confidence intervals (`07_replicate_weights.R`, `12_te_benchmarks_se.R`). | Download (~737 MB). Not included. |
| Employer DB contributions by sector | BEA National Income and Product Accounts, tables 7.22 / 7.23 / 7.24 | CY2024; federal scope is civilian (CSRS/FERS) | Allocates a national defined-benefit normal-cost total across workers reporting a DB plan, in proportion to earnings (`11_stage5_imputations.R`). | Included: `data/raw/external_benchmarks/stage5_db_employer.csv`. |
| IRA contributions and deductions by AGI class | IRS Statistics of Income — IRA study Table 3; Publication 1304 Table 1.4 | TY2023 | Imputes personal IRA contributions made outside work, assigned to a population matched to IRS contributor counts (`11_stage5_imputations.R`). | Included: `stage5_ira_by_agi.csv`, `stage5_ira_deduction_by_agi.csv`. |
| Roth 401(k) usage by participant income | Vanguard, *How America Saves 2024*, Fig. 43 | PY2023 | Splits observed 401(k) deferrals into pre-tax and Roth for the present-value calculation. | Included: `stage5_roth401k_by_income.csv`. |
| Derived scalar parameters | Per-row; see `data/raw/external_benchmarks/README.md` | Mixed | Roth dollar-share scale, deductible totals, and related single-value parameters. | Included: `stage5_scalars.csv`. |
| External pillar benchmarks | Per-row (incl. BLS National Compensation Survey) | Per-row | Benchmarks the SIPP access rate against establishment-survey measures (`pillar_benchmarks.csv`). | Included: `pillar_external.csv`. |
| IRS SOI individual tables | Internal Revenue Service | Table 1 TY2022; monthly tables through TY2023 | Filer-basis reference only. **Not read by any script.** | Included: `data/raw/irs_soi/*.xls`. |

## Obtaining the SIPP microdata

1. Go to the Census Bureau's SIPP datasets page:
   <https://www.census.gov/programs-surveys/sipp/data/datasets.html> and select **2025**.

2. Download the public-use file and the replicate-weight file in Stata format. Place them
   at:

   ```
   data/raw/pu2025.dta
   data/raw/rw2025.dta
   ```

   The replicate-weight download arrives zipped with the `.dta` nested inside; extract it
   flat (for example `unzip -j rw2025.dta.zip`).

3. In `code/_shared/params.R`, set `REBUILD_EXTRACT <- TRUE` for the first run so
   `01_build_dataset.R` builds the cached column-selected extract from the raw file. The
   file names are centralized there (`RAW_DTA_FILE`, `RW_DTA_FILE`), so a later vintage is
   a drop-in change.

4. Run `Rscript code/run_all.R`.

Reference documentation for the release — the SIPP Data Dictionary, Users' Guide, and
release notes — is available from the Census Bureau's SIPP technical documentation page:
<https://www.census.gov/programs-surveys/sipp/tech-documentation/complete-documents.html>.

## Key variables

`code/_shared/README.md` documents, per metric, the exact SIPP variables, their universes,
and the question wording behind each statistic. In brief:

- **Access** — `EMJOB_401`, `EMJOB_IRA`, `EMJOB_PEN` (holds a plan through the main
  employer), else `EPENSNYN` (employer sponsors any plan) and `EINCPENS` (worker is
  included in it). Ownership gates are `EOWN_THR401`, `EOWN_IRAKEO`, `EOWN_PENSION`.
- **Participation** — `ESCNTYN_401`, `ESCNTYN_IRA`, `ESCNTYN_PEN`, asked only of workers
  who hold that plan type through their main employer.
- **Employer contribution / match** — `EECNTYN_401`, `EECNTYN_IRA`, with dollars in
  `TECNTAMT`. SIPP records no employer-contribution item for defined-benefit pensions.
- **Weights** — `WPFINWGT` (person), `WHFNWGT` (household), plus 240 replicate weights.

## Notes and limitations

- SIPP is **self-reported**, and the access question asks whether the worker is personally
  eligible for and included in a plan. This yields a wider access gap than the
  establishment-reported, offer-based BLS National Compensation Survey measure (roughly 49
  percent versus 28 percent for private-sector workers). Both are reported in
  `output/tables/pillar_benchmarks.csv`.
- Access, participation, and matching are measured in the **December reference month**;
  income is a full calendar-year total. Seasonal patterns are therefore not observable.
- Saver's Match eligibility is an **approximation**: SIPP income proxies adjusted gross
  income, filing status comes from the prior tax year, and statutory thresholds are applied
  unindexed. The specific caveats are enumerated in the publication's technical appendix.
- Three elements of the tax-expenditure estimate are **imputed rather than observed** —
  employer contributions to defined-benefit pensions, the Roth share of 401(k) deferrals,
  and personal IRA contributions made outside work. The benchmark inputs above drive those
  imputations, and `output/tables/te_calibration.csv` records the calibration.
