# External benchmark inputs for Stage-5 imputations

Hand-entered from the verified primary sources listed below (2026-07-11); every value
carries its source and vintage in-row. Consumed by `code/11_stage5_imputations.R` (Stage 5 of the
tax-expenditure replication).

| File | Content | Primary source |
|---|---|---|
| `stage5_roth401k_by_income.csv` | Roth 401(k) usage-if-offered by participant income band, PY2023 | Vanguard *How America Saves 2024*, Fig. 43 p. 44 |
| `stage5_ira_by_agi.csv` | Traditional and Roth IRA contribution dollars ($M) by AGI class, TY2023 | IRS SOI IRA study, Table 3 (`23in03ira.xlsx`) |
| `stage5_ira_deduction_by_agi.csv` | IRA deduction (Form 1040) by AGI class, TY2023 | IRS SOI Pub 1304 Table 1.4 (`23in14ar.xls`) cols 123–124 |
| `stage5_db_employer.csv` | Employer DB contributions by sector (CY2024), actual + accrual concepts; federal scope is CIVILIAN (CSRS/FERS) to match the civilian-labor-force frame | BEA NIPA tables 7.22 / 7.23 (civilian lines 6+9) / 7.24, DBnomics-verified 2026-08-27 |
| `stage5_scalars.csv` | Single-value parameters (Roth dollar-share scale, deductible totals) | per-row |

Key derived parameters and their justification:

- **Roth participant→dollar scale 0.75**: IRS SOI W-2 statistics TY2020 (`20in04w2all.xlsx`,
  Table 4.D): Roth = 10.4% of 401(k) elective-deferral dollars vs ~13.6% of contributor
  instances → dollars/participants ≈ 0.76. Applied to the Vanguard participant-share gradient
  (upper-bound proxy) with disclosure.
- **SEP/SIMPLE excluded from the IRA imputation**: SIPP already observes employer-provided
  IRA contributions (`TSCNTAMT_IRA`/`TECNTAMT_IRA`); importing SOI SEP/SIMPLE would double count.
- **Deductible vs non-deductible traditional IRA**: deductible dollars follow the traditional
  PV formula (upfront exclusion); non-deductible remainder is approximated with the Roth formula
  (upper bound; Treasury values that line at only ~$0.8B PV).
